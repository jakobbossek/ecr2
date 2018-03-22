library(devtools)

load_all(".")

# data import and preprocessing
data(mcMST)
print(head(mcMST))

obj.cols = c("f1", "f2")
sel.probs = c("instance-100-1", "instance-100-2", "instance-100-3")
sel.algos = c("NSGA2.MIXED", "NSGA2.SG", "NSGA2.EX", "NSGA2.ZHOU")
mcMST = dplyr::filter(mcMST, prob %in% sel.probs, algorithm %in% sel.algos)

# normalize to [1, 2] x [1, 2]
mcMST = ecr::normalize(mcMST, obj.cols = obj.cols, offset = c(1, 1))

## RANKING
## =======

# compute and visualize ranks
ranks = computeDominanceRanking(mcMST, obj.cols = obj.cols)
pl = plotDistribution(ranks) + ggplot2::scale_fill_grey(end = 0.8)
print(pl)
# Observations: seemingly, MIXED and SG have rank 1 in any case;
# EX and ZHOU ar far off with ZHOU being ranked worst for all three instances

## APPROXIMATION SETS
## ========

## Take a glimpse at examplary approximation sets
pl = plotScatter2d(dplyr::filter(mcMST, repl <= 2), facet.type = "grid",
  facet.args = list(facets = formula(repl ~ prob)))
print(pl)
# Observations: the results of the ranking are confirmed. ZHOU always performs
# worse with its approxiamtion sets beeing far away from the the others. EX is placed
# second while MIXED and SG approximation sets are incomparable in general. However,
# MIXED may be quantified better with ragard to, e.g., hypervolume (see replication 2)
# on instances instance-100-{2,3})

## COMPUTATION OF INDICATORS
## ========

# define which unary indicators to use
unary.inds = list(
  list(fun = ecr::emoaIndHV),
  list(fun = ecr::emoaIndEps),
  list(fun = ecr::emoaIndDelta)
)

# compute inidcators
inds = computeIndicators(
  mcMST, unary.inds = unary.inds
)

# visualize indicator distributions
pl = plotDistribution(inds$unary, plot.type = "boxplot")
pl = pl + ggplot2::scale_fill_grey(end = 0.8)
print(pl)
# Observations: MIXED and SG outperform EX and ZHOU with regard to the hypervolume
# and epsilon indicator with MIXED gaining a slight lead over SG. We will now check,
# whether the differences are significant with statistical rigor.

# apply non-parametrical tests
# shorter algorithm names
unary = inds$unary
unary$algorithm = gsub("NSGA2.", "", unary$algorithm, fixed = TRUE)
tests = applyStatisticalTests(unary, ind.names = c("HV", "EPS"))
print(toLatexTables(tests, probs = sel.probs[2:3]))

# binary.inds = inds$binary
# plotHeatmap(binary.inds[[1L]])

stop()
# 3D EXAMPLE

mcMST3d = mcMST
mcMST3d$f3 = mcMST3d$f2
plotScatter3d(mcMST3d, package = "plot3D")
