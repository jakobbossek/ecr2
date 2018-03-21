library(devtools)

load_all(".")

data(mcMST)
obj.cols = c("f1", "f2")

# normalize to [1, 1] x [1, 2]
mcMST = ecr::normalize(mcMST, obj.cols = obj.cols, offset = c(1, 1))

# # check ref-points/sets
# #ref.points = approximateRefPoints(mcMST, obj.cols = obj.cols, offset = 0.1, as.df = TRUE)
# #ref.sets = approximateRefSets(mcMST, obj.cols = obj.cols, as.df = FALSE)

# select subset of instances for examples
mcMST = dplyr::filter(mcMST,
  prob %in% c("instance-100-1", "instance-100-2", "instance-100-3"))

# #stop()
# # visualize approximation sets

# pl = plotScatter2d(dplyr::filter(mcMST, repl <= 2), title = "Test", colour = "algorithm", facet.type = "grid", facet.args = list(facets = formula(repl ~ prob)))
# print(pl)

# # define which unary indicators to use
unary.inds = list(
  list(fun = ecr::emoaIndHV),
  list(fun = ecr::emoaIndEps),
  list(fun = ecr::emoaIndONVG),
  list(fun = ecr::emoaIndDelta)
)

# FIXME: rename to computeEMOAIndicators
inds = computeIndicators(dplyr::filter(mcMST, grepl("NSGA", algorithm)), unary.inds = unary.inds)

unary.inds = inds$unary

# plotIndicatorDistribution(unary.inds, plot.type = "boxplot")

# load_all()
unary.inds$algorithm = gsub("NSGA2.", "", unary.inds$algorithm, fixed = TRUE)
test.res = applyStatisticalTests(unary.inds, ind.names = c("HV", "EPS"))


print(toLatex(test.res, probs = c("instance-100-2", "instance-100-3"), by.instance = TRUE))


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
