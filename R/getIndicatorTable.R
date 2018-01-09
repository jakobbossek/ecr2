getIndicatorTable = function(df,
  caption = "Unary indicators",
  ind.cols,
  group.by = NULL,
  digits = 2L,
  meta = NULL,
  add.var = FALSE,
  #group.by = list(),
  longtable = FALSE,
  rotate = FALSE) {
  assertDataFrame(df)
  assertString(caption)
  assertSubset(ind.cols, colnames(df))
  #assertList(group.by)
  assertFlag(longtable)
  assertFlag(rotate)

  #print(colnames(df))

  id.cols = c("prob", "algorithm", "repl")
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
  format.string = collapse(rep("c", n.algos * n.inds + 1L + as.integer(!is.null(group.by))), sep = "|")
  latex.tab = cat(paste0("\\begin{tabular}{", format.string, "}"))

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
        cur.line = cat(paste0(cur.line, sprintf("\\multirow{%i}{*}{%s} & Problem &", as.integer(n.per.group[n.per.group[[group.by]] == as.integer(cur.group), "n"]), cur.group)))
      } else {
        cur.line = cat(" & Problem & ")
      }
    } else {
      cur.line = cat("Problem & ")
    }
    #cur.line = paste0(prob2, " & ")
    for (algo2 in algos) {
      for (ind2 in ind.cols) {
        tmp = dplyr::filter(res, prob == prob2, algorithm == algo2)
        # avg value
        tmp2 = dplyr::filter(res, prob == prob2)
        avg.col.name = sprintf("avg.%s", ind2)
        sd.col.name = sprintf("sd.%s", ind2)
        max.value = max(tmp2[[avg.col.name]] / 1e5)
        avg.value = tmp[[avg.col.name]] / 1e5
        sd.value = tmp[[sd.col.name]]
        # highlight maximal value
        if (abs(avg.value - max.value) < 1e-10) {
          cur.line = cat(paste0(cur.line, sprintf("$\\mathbf{%.2f} (\\pm \\mathbf{%.2f})$", avg.value, sd.value)))
        } else {
          cur.line = cat(paste0(cur.line, sprintf("$%.2f (\\pm %.2f)$", avg.value, sd.value)))
        }
        if (algo2 == algos[n.algos] & ind2 == ind.cols[n.inds])
          cur.line = cat(paste0(cur.line, "\\\\"))
        else
          cur.line = cat(paste0(cur.line, " & "))
      }
    }
    latex.tab = cat(paste0(latex.tab, cur.line))
  }
  latex.tab = cat(paste0(latex.tab, "\\end{tabular}"))

  # latex.tab = gsub("\\b", "\b", latex.tab, fixed = TRUE)
  # latex.tab = gsub("\\m", "\m", latex.tab, fixed = TRUE, perl = TRUE)
  # latex.tab = gsub("\\end", "\end", latex.tab, fixed = TRUE)
  # latex.tab = gsub("\\hline", "\hline", latex.tab, fixed = TRUE)

  return(latex.tab)
  print(latex.tab)
  stop()

  return(res)
  # return(knitr::kable(res, caption = caption, digits = 2L, format = "latex"))
}
