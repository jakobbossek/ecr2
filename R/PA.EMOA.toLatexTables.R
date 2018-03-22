#' @title Export results of statistical tests to LaTeX table(s).
#'
#' @description Returns high-quality LaTeX-tables of the test results of
#' statistical tests performed with function \code{\link{applyStatisticalTests}}
#' on per-instance basis. I.e., a table is returned for each instances combining
#' the results of different indicators.
#'
#' @param stats [\code{list}]\cr
#'   Named list of list as returned by \code{\link{applyStatisticalTests}}.
#' @param probs [\code{character}]\cr
#'   Filtering: vector of problem instances. This way one can restrict the
#'   size of the table(s).
#'   Defaults to all problems available in \code{stats}.
#' @param inds [\code{character}]\cr
#'   Names of the indicators to consider.
#'   Defaults to all indicators available in \code{stats}.
#' @param type [\code{character(1)}]\cr
#'   Type of tables. At the moment only option \dQuote{by.instance} is available.
#'   I.e., a separate LaTeX-table is generated for each instance specified via \code{probs}.
#' @param cell.formatter [\code{function(cell, ...)}]\cr
#'   Function which is used to format table cells. This function is applied to each
#'   table cell and may be used to customize the output. Default is \code{niceCellFormater}.
#' @return [\code{list}] Named list of strings (LaTeX tables). Names correspond to the
#'   selected problem instances in \code{probs}.
#' @family EMOA performance assessment tools
#' @export
toLatexTables = function(stats, probs = NULL, inds = NULL, type = "by.instance", cell.formatter = NULL) {
  assertList(stats)
  assertChoice(type, choices = "by.instance")

  if (is.null(probs))
    probs = unique(names(stats))

  if (is.null(inds))
    inds = unique(names(stats[[1L]]))

  if (is.null(cell.formatter))
    cell.formatter = niceCellFormater

  all.probs = names(stats)
  all.inds  = names(stats[[1L]])

  alpha = attr(stats, "alpha")
  unary.inds.funs = attr(stats, "unary.inds")

  assertSubset(probs, choices = all.probs)
  assertSubset(inds, choices = all.inds)
  assertFunction(cell.formatter, args = "cell")

  # now filter relevant stats, i.e., for selected problems
  stats = stats[which(all.probs %in% probs)]

  tables = list()
  if (type == "by.instance") {
    for (prob in probs) {
      catf("Problem: %s", prob)
      # extract relevant indicators
      res.stats = stats[[prob]][which(names(stats[[prob]]) %in% inds)]
      inds = names(res.stats)
      res.stats = do.call(cbind, res.stats)

      # format cells
      res.stats = apply(res.stats, 2L, function(column)
        sapply(column, cell.formatter, alpha = alpha)
      )

      n.inds = length(inds)
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
      inds.latex = sapply(inds, function(ind.name) attr(unary.inds.funs[[ind.name]]$fun, "latex.name"))
      inds.latex = paste0("$", inds.latex, "$")
      # catf("Latex names: %s", collapse(inds.latex))
      #print(unary.inds.funs)
      #inds.latex = c("$I_{HV}$", "$I_{\\\\epsilon}^{+}$")
      group.titles = paste(rep(prob, n.inds), "/", inds.latex)
      names(header.probs) = c(" ", group.titles)

      # add some nice additions
      dd = kableExtra::add_header_above(dd, header.probs, bold = TRUE, escape = FALSE)
      dd = kableExtra::footnote(dd, general = sprintf("Bold font entries are significant to significance level $\\\\alpha = %.2f$ (adjusted for multiple testing).", alpha), general_title = "Note: ", footnote_as_chunk = TRUE, escape = FALSE)
      print(dd)

      tables[[prob]] = dd
      # dd = group_rows(dd, "$I_{HV}^{+}$", 1, 4, escape = FALSE)
      # dd = group_rows(dd, "$I_{\\eps}^b$", 5, 8, escape = FALSE)
    }
  }
  return(tables)
}
