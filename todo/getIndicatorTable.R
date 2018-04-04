# # this will be fucking awesome!!!
getIndicatorTable = function(df,
  caption = "Unary indicators",
  ind.cols,
  group.by = NULL,
  digits = 2L,
  file = "",
  meta = NULL,
  col.align = NULL,
  add.sd = TRUE,
  longtable = FALSE,
  rotate = FALSE,
  alpha = 0.05) {

  assertDataFrame(df)
  assertString(caption)
  assertSubset(ind.cols, colnames(df))
  #assertList(group.by)
  assertFlag(longtable)
  assertFlag(rotate)
  assertFlag(add.sd)
  assertNumber(alpha, lower = 0, upper = 1, na.ok = FALSE)

  #print(colnames(df))

  id.cols = c("prob", "algorithm", "repl")

  probs = unique(df$prob)

  if (is.null(meta)) {
    meta.cols = setdiff(names(df), c(id.cols, ind.cols))
    if (length(meta.cols) > 0L)
      meta = df[, c(id.cols[1L], meta.cols)]
  } else {
    assertSubset(id.cols[1L], names(meta))
  }

  res = lapply(ind.cols, function(ind.col) {
    sel.cols = c(id.cols, ind.col)
    catf("Selected cols: %s", BBmisc::collapse(sel.cols, sep = ","))
    tmp = dplyr::select_(.data = df, .dots = as.list(sel.cols))
    # workaround: rename column so that dplyr::summarize works
    names(tmp)[names(tmp) == ind.col] = "Indicator"
    tmp = dplyr::group_by_(tmp, .dots = id.cols[1:2])
    tmp = dplyr::summarize(tmp, AVG = mean(Indicator, na.rm = TRUE), SD = sd(Indicator, na.rm = TRUE))
    tmp = dplyr::ungroup(tmp)
    #print(tmp)
    names(tmp)[names(tmp) == "AVG"] = sprintf("avg.%s", ind.col)
    names(tmp)[names(tmp) == "SD"] = sprintf("sd.%s", ind.col)
    return(tmp)
  })

  # reduce
  #FIXME: must be possible more elegant
  n.res = length(res)
  if (n.res > 1L) {
    res2 = res[[1L]]
    for (i in 2:n.res) {
      res2 = dplyr::full_join(res2, res[[i]], by = id.cols[1:2])
    }
  } else {
    res2 = res[[1L]]
  }
  res = res2

  n.cols.meta = 0
  if (!is.null(meta)) {
    n.cols.meta = ncol(meta)
    res = left_join(meta, res, by = "prob")
  }

  print(as_tibble(res))

  # store some vars
  probs = unique(res$prob)
  algos = unique(res$algorithm)
  n.probs = length(probs)
  n.algos = length(algos)
  n.inds = length(ind.cols)

  res = res[order(res$prob, res$algorithm), , drop = FALSE]

  # debug
  xx <<- res

  # do some fine magic
  stats = c("avg")
  cnames = paste(stats, ind.cols, sep = ".")

  # asure that values are highlighted according to their importance
  res = boldify(res, group.by = "prob", col.names = cnames, dir.funs = c(max, max))

  #res$prob = as.integer(as.factor(res$prob))

  # make multirow stuff
  prob.freq = table(res$prob)
  prob.col = rep("", nrow(res))
  multirow.inds = cumsum(c(1L, as.integer(prob.freq[-length(prob.freq)])))
  multirow.names = names(prob.freq)
  prob.col[multirow.inds] = sprintf("\\multirow{%i}{*}{%s}", prob.freq, multirow.names)
  res$prob = prob.col

  if (!is.null(group.by)) {
    group.freq = table(res[[group.by]])
    group.col = rep("", nrow(res))
    multirow.inds2 = cumsum(c(1L, as.integer(group.freq[-length(group.freq)])))
    multirow.names = names(group.freq)
    group.col[multirow.inds2] = sprintf("\\multirow{%i}{*}{$n=%s$}", group.freq, multirow.names)
    res[[group.by]] = group.col
  }

  xtab = xtable::xtable(res, digits = digits, caption = caption,
    align = c("c", "|l|", rep("c|", ncol(res) - 1L)))
  xtable::print.xtable(xtab,
    file = file,
    sanitize.text.function = identity,
    sanitize.colnames.function = makeBoldHeader,
    #booktabs = TRUE,
    add.to.row = list(
      pos = as.list(multirow.inds - 1L),
      command = rep("\\cline{2-7}", length(multirow.inds))
    ),
    floating = FALSE,
    include.rownames = FALSE,
    NA.string = "-")

  # for each problem, apply KW-test to check if there is a significant
  # shift in location in for least one algorithm
  kw.test = lapply(probs, function(prob2) {
    tmp = dplyr::filter(.data = df, prob == prob2)
    res = sapply(ind.cols, function(ind.col) {
      p.val = stats::kruskal.test(tmp[[ind.col]], g = tmp$algorithm)$p.value
      if (is.na(p.val) | is.nan(p.val))
        return(1.0)
      return(p.val)
    })
    names(res) = ind.cols
    return(res)
  })
  names(kw.test) = probs

  print(kw.test)

  stop("xtable rocks!")

  algo.cols = lapply(algos, function(algo) {
    n.empty = 2 * n.inds - 1L
    c(rep(sprintf("\\multicolumn{%i}{|c|}{%s}", 2 * n.inds, algo), 2 * n.inds))#, rep("", n.empty))
  })
  algo.cols = do.call(c, algo.cols)

  print(algo.cols)

  header = c("Problem", algo.cols)

  # reshape magic
  y = reshape2::melt(res, id.vars = 1:2)
  z = reshape2::dcast(y, formula = prob ~ algorithm + variable)

  # now adapt colnames
  stats = c("sd", "avg")
  stat.names = as.character(sapply(stats, function(stat) paste(stat, ind.cols, sep = ".")))

  cnames = colnames(z)
  print(stat.names)
  for (stat.name in stat.names) {
    cnames = gsub(paste0("_", stat.name), "", cnames, fixed = TRUE)
  }
  catf("Cleaned names: %s", collapse(cnames, ","))

  z = rbind(cnames, z)
  z = rbind(z[1L, , drop = FALSE], z)
  z[1L, ] = header
  print(colnames(z))
  col.ns = sapply(ind.cols, function(x) paste0(stats, x))
  colnames(z) = c("Problem", rep(col.ns, n.algos))
  z$Problem = NULL
  xtab = xtable::xtable(z, digits = digits, caption = caption)
  xtable::print.xtable(xtab,
    file = "latex/tables/stats.tex",# append = TRUE,
    sanitize.text.function = identity,
    sanitize.colnames.function = makeBoldHeader,
    include.rownames = FALSE)

  #stop("xtable rocks!")
}

#FIXME: this is soooo ugly!!!
# rewrite with xtable
getIndicatorTable2 = function(df,
  caption = "Unary indicators",
  ind.cols,
  group.by = NULL,
  digits = 2L,
  meta = NULL,
  col.align = NULL,
  add.sd = TRUE,
  longtable = FALSE,
  rotate = FALSE,
  alpha = 0.05) {

  assertDataFrame(df)
  assertString(caption)
  assertSubset(ind.cols, colnames(df))
  #assertList(group.by)
  assertFlag(longtable)
  assertFlag(rotate)
  assertFlag(add.sd)
  assertNumber(alpha, lower = 0, upper = 1, na.ok = FALSE)

  #print(colnames(df))

  id.cols = c("prob", "algorithm", "repl")

  probs = unique(df$prob)

  if (is.null(meta)) {
    meta.cols = setdiff(names(df), c(id.cols, ind.cols))
    if (length(meta.cols) > 0L)
      meta = df[, c(id.cols[1L], meta.cols)]
  } else {
    assertSubset(id.cols[1L], names(meta))
  }

  res = lapply(ind.cols, function(ind.col) {
    sel.cols = c(id.cols, ind.col)
    catf("Selected cols: %s", BBmisc::collapse(sel.cols, sep = ","))
    tmp = dplyr::select_(.data = df, .dots = as.list(sel.cols))
    # workaround: rename column so that dplyr::summarize works
    names(tmp)[names(tmp) == ind.col] = "Indicator"
    tmp = dplyr::group_by_(tmp, .dots = id.cols[1:2])
    tmp = dplyr::summarize(tmp, AVG = mean(Indicator, na.rm = TRUE), SD = sd(Indicator, na.rm = TRUE))
    tmp = dplyr::ungroup(tmp)
    #print(tmp)
    names(tmp)[names(tmp) == "AVG"] = sprintf("avg.%s", ind.col)
    names(tmp)[names(tmp) == "SD"] = sprintf("sd.%s", ind.col)
    return(tmp)
  })

  # for each problem, apply KW-test to check if there is a significant
  # shift in location in for least one algorithm
  kw.test = lapply(probs, function(prob2) {
    tmp = dplyr::filter(.data = df, prob == prob2)
    res = sapply(ind.cols, function(ind.col) {
      p.val = stats::kruskal.test(tmp[[ind.col]], g = tmp$algorithm)$p.value
      if (is.na(p.val) | is.nan(p.val))
        return(1.0)
      return(p.val)
    })
    names(res) = ind.cols
    return(res)
  })
  names(kw.test) = probs

  # print(kw.test)
  # stop()

  n.res = length(res)
  if (n.res > 1L) {
    res2 = res[[1L]]
    for (i in 2:n.res) {
      res2 = dplyr::full_join(res2, res[[i]], by = id.cols[1:2])
    }
  } else {
    res2 = res[[1L]]
  }
  res = res2
  print(dim(res))
  if (!is.null(meta))
    res = dplyr::left_join(res, meta, by = id.cols[1L])
  print(dim(res))
  #print(as.data.frame(res))

#  stop()


  probs = unique(res$prob)
  algos = unique(res$algorithm)
  n.probs = length(probs)
  n.algos = length(algos)
  n.inds = length(ind.cols)
  format.string = collapse(rep("|c", n.algos * n.inds + 1L + as.integer(!is.null(group.by))), sep = "|")
  tab.envir = ifelse(longtable, "longtable", "tabular")
  latex.tab = cat(paste0("\\begin{", tab.envir, "}{", format.string, "}\\hline"))

  # build header
  if (!is.null(group.by))
    header = cat("\\multicolumn{2}{|c|}{Problem} & ")
  else
    header = cat("Problem & ")
  #header = paste0(header, collapse(algos, " & "), "\\")
  if (n.inds == 1L) {
    header = cat(paste0(header, collapse(algos, " & "), "\\\\"))
  } else {
    for (algo in algos) {
      header = cat(paste0(header, sprintf("\\multicolumn{%i}{|c|}{%s}", n.inds, algo)))
      if (algo == algos[n.algos]) header = cat(paste0(header, "\\\\")) else header = cat(paste0(header, " & "))
    }
  }
  header = cat(paste0(header, "\\hline\\hline"))
  if (!is.null(group.by))
    header = cat(paste0(header, " & & "))
  else
    header = cat(paste0(header, " & "))
  if (n.inds == 1L) {
    header = cat(paste0(header, collapse(rep(ind.cols, n.algos), " & "), "\\\\"))
  } else {
    for (algo in algos) {
      header = cat(paste0(header, collapse(ind.cols, sep = " & ")))
      if (algo == algos[n.algos])
        header = cat(paste0(header, "\\\\"))
      else
        header = cat(paste0(header, " & "))
    }
  }
  header = cat(paste0(header, "\\hline\\hline"))

  if (!is.null(group.by)) {
    prob.meta = dplyr::select_(res, .dots = c("prob", group.by))
    prob.meta = prob.meta[!duplicated(prob.meta$prob), ]
    #print(prob.meta)
    n.per.group = dplyr::group_by_(prob.meta, .dots = group.by) %>% dplyr::count() %>% ungroup()
    #print(n.per.group)
    #print(n.per.group)
    #print(as_tibble(res))
    #res = dplyr::arrange(res, .dots = group.by)
  }

  latex.tab = cat(paste0(latex.tab, header))
  cur.group = -1
  for (prob2 in probs) {
    if (!is.null(group.by)) {
      if (cur.group != prob.meta[prob.meta$prob == prob2, group.by]) {
        cur.line = ""
        if (cur.group != -1)
          cur.line = cat(paste0(cur.line, "\\hline\\hline"))
        cur.group = prob.meta[prob.meta$prob == prob2, group.by]
        # catf("current group: %s", cur.group)
        # catf("N rows: %i", as.integer(n.per.group[n.per.group[[group.by]] == as.integer(cur.group), "n"]))
        cur.line = cat(paste0(cur.line, sprintf("\\multirow{%i}{*}{N = %s} & %s &", as.integer(n.per.group[n.per.group[[group.by]] == as.integer(cur.group), "n"]), cur.group, prob2)))
      } else {
        cur.line = cat(sprintf(" & %s & ", prob2))
      }
    } else {
      cur.line = cat(sprintf("%s & ", prob2))
    }
    #cur.line = paste0(prob2, " & ")
    for (algo2 in algos) {
      tmp2 = dplyr::filter(res, prob == prob2)
      for (ind2 in ind.cols) {
        # check if indicator location shift is significant according to KW-test
        is.signif.test = kw.test[[prob2]][[ind2]] < alpha
        tmp = dplyr::filter(res, prob == prob2, algorithm == algo2)
        # avg value
        avg.col.name = sprintf("avg.%s", ind2)
        sd.col.name = sprintf("sd.%s", ind2)
        max.value = max(tmp2[[avg.col.name]])
        avg.value = tmp[[avg.col.name]]
        sd.value = tmp[[sd.col.name]]
        # highlight maximal value
        if (abs(avg.value - max.value) < 1e-10) {
          stat.test.string = ifelse(is.signif.test, "^{\\mathbf{*}}", "")
          sd.value.string = ifelse(add.sd, sprintf(" (\\pm \\mathbf{%.2f})", sd.value), "")
          cur.line = cat(paste0(cur.line, sprintf("$\\mathbf{%.2f}%s%s$", avg.value, sd.value.string, stat.test.string)))
        } else {
          sd.value.string = ifelse(add.sd, sprintf(" (\\pm %.2f)", sd.value), "")
          cur.line = cat(paste0(cur.line, sprintf("$%.2f%s$", avg.value, sd.value.string)))
        }
        if (algo2 == algos[n.algos] & ind2 == ind.cols[n.inds])
          cur.line = cat(paste0(cur.line, "\\\\"))
        else
          cur.line = cat(paste0(cur.line, " & "))
      }
    }
    latex.tab = cat(paste0(latex.tab, cur.line))
  }
  latex.tab = cat(paste0(latex.tab, "\\hline\\end{", tab.envir, "}"))

  return(latex.tab)
  print(latex.tab)
  stop()

  return(res)
  # return(knitr::kable(res, caption = caption, digits = 2L, format = "latex"))
}

#FIXME: rename! can be used not only for bolding!
#FIMXE: add something like wrap.fun(x, condition = c(TRUE, FALSE), pre, post) (should be vectorized)
#FIXME: add sanity checks
#FIXME: add docs
#FIXME: add package:: prefixes
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
      group_by_(group.by) %>%
      summarize(best = the.dir.fun(tmpname)) %>%
      ungroup()
    names(df)[names(df) == "tmpname"] = col.name

    df = left_join(df, tmp, by = group.by)
    is.best = df[[col.name]] == df[["best"]]
    pre.tex = ifelse(is.best, "\\cellcolor{gray!15}\\textbf{", "")
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
