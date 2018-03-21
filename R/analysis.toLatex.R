toLatex = function(stats, probs = NULL, inds = NULL, by.instance = TRUE, cell.formatter = NULL) {
  assertList(stats)
  assertFlag(by.instance)

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

  if (by.instance) {
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
      # dd = group_rows(dd, "$I_{HV}^{+}$", 1, 4, escape = FALSE)
      # dd = group_rows(dd, "$I_{\\eps}^b$", 5, 8, escape = FALSE)
    }
  }
}
