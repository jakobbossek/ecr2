#' @title Export results of statistical tests to LaTeX table(s).
#'
#' @description Returns high-quality LaTeX-tables of the test results of
#' statistical tests performed with function \code{test}
#' on per-instance basis. I.e., a table is returned for each instances combining
#' the results of different indicators.
#'
#' @param stats [\code{list}]\cr
#'   Data frame (return value of \code{\link{computeIndicators}}) or named list of list as returned by \code{test}.
#' @param stat.cols [\code{character}]\cr
#'   Names of the indicators to consider.
#'   Defaults to all indicators available in \code{stats}.
#' @param probs [\code{character}]\cr
#'   Filtering: vector of problem instances. This way one can restrict the
#'   size of the table(s).
#'   Defaults to all problems available in \code{stats}.
#'   Ignored if \code{stats} is a data frame.
#' @param type [\code{character(1)}]\cr
#'   Type of tables. At the moment only option \dQuote{by.instance} is available.
#'   I.e., a separate LaTeX-table is generated for each instance specified via \code{probs}.
#'   Ignored if \code{stats} is a data frame.
#' @param cell.formatter [\code{function(cell, ...)}]\cr
#'   Function which is used to format table cells. This function is applied to each
#'   table cell and may be used to customize the output. Default is \code{niceCellFormater}.
#'   Ignored if \code{stats} is a data frame.
#' @return [\code{list}] Named list of strings (LaTeX tables). Names correspond to the
#'   selected problem instances in \code{probs}.
#' @family EMOA performance assessment tools
#' @name toLatex
#' @rdname toLatex
#' @export
toLatex = function(stats, stat.cols = NULL, probs = NULL, type = "by.instance", cell.formatter = NULL) {
  UseMethod("toLatex")
}

#' @rdname toLatex
#' @export
toLatex.list = function(stats, stat.cols = NULL, probs = NULL, type = "by.instance", cell.formatter = NULL) {
  assertList(stats)
  assertChoice(type, choices = "by.instance")

  if (is.null(probs))
    probs = unique(names(stats))

  if (is.null(stat.cols))
    stat.cols = unique(names(stats[[1L]]))

  if (is.null(cell.formatter))
    cell.formatter = niceCellFormater

  all.probs = names(stats)
  all.inds  = names(stats[[1L]])

  alpha = attr(stats, "alpha")
  unary.inds.funs = attr(stats, "unary.inds")

  assertSubset(probs, choices = all.probs)
  assertSubset(stat.cols, choices = all.inds)
  assertFunction(cell.formatter, args = "cell")

  # now filter relevant stats, i.e., for selected problems
  stats = stats[which(all.probs %in% probs)]

  tables = list()
  if (type == "by.instance") {
    for (prob in probs) {
      catf("Problem: %s", prob)
      # extract relevant indicators
      res.stats = stats[[prob]][which(names(stats[[prob]]) %in% stat.cols)]
      stat.cols = names(res.stats)
      res.stats = do.call(cbind, res.stats)

      # format cells
      res.stats = apply(res.stats, 2L, function(column)
        sapply(column, cell.formatter, alpha = alpha)
      )

      n.inds = length(stat.cols)
      n.algos = nrow(res.stats)

      # build nice LaTeX table
      align.vec = c("l", rep("c", n.inds * n.algos))
      # align.vec[length(align.vec)] = "c"
      # align.vec[seq(2, length(align.vec) - 1, by = n.algos)] = "c||"
      dd = knitr::kable(res.stats, align = align.vec, format = "latex", booktabs = TRUE, escape = FALSE)
      dd = kableExtra::kable_styling(dd, position = "center")
      header.probs = c(1, rep(n.algos, n.inds))

      # catf("Indicators: %s", collapse(inds, sep = ","))
      # catf("Nmaes in stats: %s", collapse(names(unary.inds.funs)))
      inds.latex = sapply(stat.cols, function(ind.name) attr(unary.inds.funs[[ind.name]]$fun, "latex.name"))
      inds.latex = paste0("$", inds.latex, "$")
      # catf("Latex names: %s", collapse(inds.latex))
      #print(unary.inds.funs)
      #inds.latex = c("$I_{HV}$", "$I_{\\\\epsilon}^{+}$")
      group.titles = paste(rep(prob, n.inds), "/", inds.latex)
      names(header.probs) = c(" ", group.titles)

      # add some nice additions
      dd = kableExtra::add_header_above(dd, header.probs, bold = TRUE, escape = FALSE)
      dd = kableExtra::footnote(dd, general = sprintf("Bold font entries are significant to significance level $\\\\alpha = %.2f$ (adjusted for multiple testing).", alpha), general_title = "Note: ", footnote_as_chunk = TRUE, escape = FALSE)

      tables[[prob]] = dd
      # dd = group_rows(dd, "$I_{HV}^{+}$", 1, 4, escape = FALSE)
      # dd = group_rows(dd, "$I_{\\eps}^b$", 5, 8, escape = FALSE)
    }
  }
  return(tables)
}

#' @rdname toLatex
#' @export
toLatex.data.frame = function(stats, stat.cols = NULL, probs = NULL, type = "by.instance", cell.formatter = NULL) {
  assertDataFrame(stats)

  res.stats = dplyr::group_by_(stats, "algorithm", "prob")
  print(res.stats)

  res = lapply(stat.cols, function(stat.col) {
    tmp = dplyr::summarise_(
      res.stats,
      mean = lazyeval::interp(~mean(x), x = as.name(stat.col)),
      sd   = lazyeval::interp(~sd(x),   x = as.name(stat.col))
    )
    tmp = dplyr::ungroup(tmp)
    names(tmp)[names(tmp) == "mean"] = paste0(stat.col, ".mean")
    names(tmp)[names(tmp) == "sd"]   = paste0(stat.col, ".sd")
    return(tmp)
  })

  # now cbind the ugly way
  res2 = res[[1L]]
  if (length(res) > 1L) {
    for (i in 2:length(res)) {
      drop.cols = c("prob", "algorithm")
      drop.cols = which(colnames(res[[i]]) %in% drop.cols)
      res2 = cbind(res2, res[[i]][, -drop.cols, drop = FALSE])
    }
  }

  latex.name.mapping = list("HV" = "$I_{HV}$", "EPS" = "$I_{\\\\epsilon}$")

  res2 = dplyr::arrange_(res2, "prob")
  meta = res2[, c("prob", "algorithm")]
  res2$prob = res2$algorithm = NULL
  res2 = cbind(meta, res2)

  prob.col = which(colnames(res2) == "prob")
  n.probs = unique(res2[[prob.col]])

  #res2 = boldify(res2, group.by = "prob", col.names = c(3, 4, 5, 6), dir.funs = c(min, min, min, min))

  align.vec = c("l", "l", rep(c("r", "r"), length(stat.cols)))
  col.names = c("Problem", "Algorithm", rep(c("Mean", "StdDev"), length(stat.cols)))

  dd = knitr::kable(res2, align = align.vec, col.names = col.names, format = "latex", booktabs = TRUE, escape = FALSE)

  group.cols = c(1, 1, rep(2L, length(stat.cols)))
  names(group.cols) = c(" ", " ", latex.name.mapping[stat.cols])
  dd = kableExtra::kable_styling(dd)
  dd = kableExtra::add_header_above(dd, group.cols, escape = FALSE)
  dd = kableExtra::column_spec(dd, column = 1L, bold = TRUE)
  dd = kableExtra::collapse_rows(dd, columns = 1L, latex_hline = "major")

  return(dd)
}

boldify = function(df, group.by = NULL, col.names, dir.funs) {
  n.cols = length(col.names)
  if (is.numeric(col.names))
    col.names = names(df)[as.integer(col.names)]
  if (length(dir.funs) == 1L & n.cols > 1L)
    dir.funs = rep(dir.funs, n.cols)
  if (is.null(group.by)) {
    df$groupby = "DUMMY"
    group.by = "groupby"
  }

  for (i in 1:n.cols) {
    col.name = col.names[i]
    the.dir.fun = dir.funs[i][[1L]]

    names(df)[names(df) == col.name] = "tmpname"
    # get min or max value or some other for each group
    tmp = df %>%
      dplyr::group_by_(group.by) %>%
      dplyr::summarize(best = the.dir.fun("tmpname")) %>%
      dplyr::ungroup()
    names(df)[names(df) == "tmpname"] = col.name

    df = dplyr::left_join(df, tmp, by = group.by)
    is.best = df[[col.name]] == df[["best"]]
    pre.tex = ifelse(is.best, "\\cellcolor{gray!15}\\textbf{", "")
    pre.tex = ifelse(is.best, "\\textbf{", "")
    post.tex = ifelse(is.best, "}", "")
    df[[col.name]] = sprintf("%s%.3f%s", pre.tex, df[[col.name]], post.tex)
    df$best = NULL
  }
  if (group.by == "groupby")
    df$groupby = NULL
  return(df)
}


makeBoldHeader = function(x) {
  paste('{\\textbf{',x,'}}', sep ='')
}
