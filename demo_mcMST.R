library(devtools)

load_all(".")

data(mcMST)
obj.cols = c("f1", "f2")
sel.probs = c("instance-100-1", "instance-100-2", "instance-100-3")

# normalize to [1, 1] x [1, 2]
mcMST = ecr::normalize(mcMST, obj.cols = obj.cols, offset = c(1, 1))

stop("Dominance ranking")

tt = dplyr::filter(mcMST, prob %in% sel.probs)

res = by(tt, list(tt$prob),
  function(x) {
    # get grid the ugly way
    grid = dplyr::group_by(x, algorithm, repl)
    grid = dplyr::summarize(grid, rg = NA)
    grid = ungroup(grid)

    n = nrow(grid)
    for (i in 1:n) {
      rg = 1L
      for (j in 1:n) {
        cat(".")
        if (i == j)
          next
        approx.a = x[x$algorithm == as.character(grid[i, "algorithm"]) & x$repl == as.integer(grid[i, "repl"]), obj.cols, drop = FALSE]
        approx.b = x[x$algorithm == as.character(grid[j, "algorithm"]) & x$repl == as.integer(grid[j, "repl"]), obj.cols, drop = FALSE]
        rg = rg + as.integer(setDominates(t(approx.b), t(approx.a)))
      }
      grid[i, "rg"] = rg
    }
    return(grid)
  })
res = do.call(rbind, res)

# now apply rank test







# # define which unary indicators to use
unary.inds = list(
  list(fun = ecr::emoaIndHV),
  list(fun = ecr::emoaIndEps),
  list(fun = ecr::emoaIndDelta)
)

# FIXME: rename to computeEMOAIndicators
inds = computeIndicators(dplyr::filter(mcMST, grepl("NSGA", algorithm)), unary.inds = unary.inds)

unary.inds = dplyr::filter(inds$unary, prob %in% sel.probs)

pl = plotIndicatorDistribution(unary.inds, plot.type = "boxplot")
print(pl)

unary.inds = inds$unary
unary.inds$N = as.integer(sapply(strsplit(as.character(unary.inds$prob), "-"), "[[", 2L))

unary.inds %>% group_by(prob, repl) %>%
  mutate(rankHV = rank(HV, ties.method = "random"), rankEPS = rank(EPS, ties.method = "random"), rankDELTA = rank(-DELTA, ties.method = "random")) %>%
  ungroup() %>%
  select(prob, algorithm, N, rankHV, rankEPS, rankDELTA) %>%
  group_by(algorithm, N) %>%
  summarize_if(is.numeric, funs(median)) %>%
  ungroup() %>%
  arrange(N, algorithm)

aggregateIndicators = function(inds, aggr.funs = list("mean", "sd")) {
  unary.inds.funs = attr(inds, "unary.inds")
  inds.names = intersect(names(unary.inds.funs), colnames(inds))
  print(inds.names)

  aggr = dplyr::group_by_(inds, "prob", "algorithm")
  aggr = dplyr::summarize_at(aggr, inds.names, do.call(dplyr::funs, aggr.funs))
  aggr = dplyr::ungroup(aggr)
  aggr = dplyr::arrange_(aggr, "prob", "algorithm")
  return(aggr)
}

unary.inds.aggr = aggregateIndicators(unary.inds)
dd = knitr::kable(unary.inds.aggr, format = "latex", longtable = TRUE)
dd = kableExtra::collapse_rows(dd, columns = 1, latex_hline = "major")
dd = kableExtra::kable_styling(dd, latex_options = c("repeat_header"))


# # check ref-points/sets
# #ref.points = approximateRefPoints(mcMST, obj.cols = obj.cols, offset = 0.1, as.df = TRUE)
# #ref.sets = approximateRefSets(mcMST, obj.cols = obj.cols, as.df = FALSE)

# select subset of instances for examples
mcMST = dplyr::filter(mcMST,
  prob %in% sel.probs)

# #stop()
# # visualize approximation sets

# pl = plotScatter2d(dplyr::filter(mcMST, repl <= 2), title = "Test", colour = "algorithm", facet.type = "grid", facet.args = list(facets = formula(repl ~ prob)))
# print(pl)

# load_all()
unary.inds$algorithm = gsub("NSGA2.", "", unary.inds$algorithm, fixed = TRUE)
test.res = applyStatisticalTests(unary.inds, ind.names = c("HV", "EPS"))


print(toLatexTables(test.res, probs = c("instance-100-2", "instance-100-3"), type = "by.instance"))


# dd = kable(comb.res, format = "latex", booktabs = TRUE, escape = FALSE)
# dd = kable_styling(dd, latex_options = "striped", position = "center", font_size = 7)
# header.probs = c(1, rep(n.algos, n.probs))
# names(header.probs) = c(" ", probs)

# dd = add_header_above(dd, header.probs)
# dd = group_rows(dd, "$I_{HV}^{+}$", 1, 4, escape = FALSE)
# dd = group_rows(dd, "$I_{\\eps}^b$", 5, 8, escape = FALSE)
#print(dd)


stop("computed indicators")




binary.inds = inds$binary

plotHeatmap(binary.inds[[1L]])


stop()
# 3D EXAMPLE

mcMST3d = mcMST
mcMST3d$f3 = mcMST3d$f2
plotScatter3d(mcMST3d, package = "plot3D")
