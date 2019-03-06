library(devtools)
library(ggplot2)

load_all()

# PERFORMANCE ASSESSMENT OF MULTI-OBJECTIVE EVOLUTIONARY ALGORITHMS
# =========================================================
# Here we demonstrate the functionality of the EMOA performance
# assessment functions which are integrated in ecr by example.
# The example data is a subset of data obtained during a study
# on the bi-criteria minimum spanning tree problem, where the
# goal is to approximate the set of non-dominated spanning trees,
# i.e., trees which span all graph nodes are have minimal sum of
# weights regarding two conflicting objectives. The study focused
# on multi-objective evolutionary algorithms for tackling this
# NP-hard problem. Multiple mutation operators and representations
# were compared using NSGA-II and SMS-EMOA as encapsulating
# wrappers.

# data import and preprocessing
data(mcMST)
print(head(mcMST))

# we focus on NSGA-2 results and three problem instances
obj.cols = c("f1", "f2")
mcMST = dplyr::filter(mcMST,
  grepl("100", prob),
  grepl("NSGA", algorithm)
)

# normalize to [1, 2] x [1, 2]
mcMST = ecr::normalize(mcMST, obj.cols = obj.cols, offset = 1)

## RANKING
## =======

# compute and visualize ranks
parallelStartMulticore(cpus = 3L, level = "ecr.computeDominanceRanking") # use 3 cpus
ranks = computeDominanceRanking(mcMST, obj.cols = obj.cols)
parallelStop()
pl = plotDistribution(ranks) + ggplot2::theme(legend.position = "none")
print(pl)

#ranks.test = test(ranks, "rank")
# Observations: seemingly, MIXED and SG have rank 1 in any case;
# EX and ZHOU are far off with ZHOU being ranked worst for all three instances

# Next, we restrict to NSGA-II only and continue detailed analysis


## APPROXIMATION SETS
## ========

## Take a glimpse at examplary approximation sets
pl = plotScatter2d(
  dplyr::filter(mcMST, repl <= 2),
  shape = "algorithm",
  facet.type = "grid",
  facet.args = list(facets = formula(repl ~ prob))) + ggplot2::scale_colour_grey(end = 0.8)
print(pl)

# Observations: the results of the ranking are confirmed. ZHOU always performs
# worse with its approxiamtion sets beeing far away from the the others. EX is placed
# second while MIXED and SG approximation sets are incomparable in general. However,
# MIXED may be quantified better with ragard to, e.g., hypervolume (see replication 2)
# on instances instance-100-{2,3})

## COMPUTATION OF INDICATORS
## ========

myONVG = makeEMOAIndicator(
  fun = function(points, ...) ncol(points),
  name = "ONVG",
  latex.name = "I_{O}",
  minimize = FALSE
)

# define which unary indicators to use
unary.inds = list(
  list(fun = ecr::emoaIndHV),
  list(fun = ecr::emoaIndEps),
  list(fun = myONVG)
)

# compute inidcators
parallelStartMulticore(cpus = 3L, level = "ecr.computeIndicators")
inds = computeIndicators(
  mcMST, unary.inds = unary.inds
)
parallelStop()

print(head(inds))

# visualize indicator distributions
pl = plotDistribution(inds$unary, plot.type = "boxplot") + ggplot2::scale_fill_grey(end = 0.8)
print(toLatex(inds$unary, stat.cols = c("HV", "EPS")))
print(pl)

# Observations: MIXED and SG outperform EX and ZHOU with regard to the hypervolume
# and epsilon indicator with MIXED gaining a slight lead over SG. We will now check,
# whether the differences are significant with statistical rigor.

# apply non-parametrical tests
# shorter algorithm names
unary = inds$unary
unary$algorithm = gsub("NSGA2.", "", unary$algorithm, fixed = TRUE)
tests = ecr::test(unary, cols = c("HV", "EPS", "ONVG"))
latex.tabs = toLatexTables(tests, probs = sel.probs)
print(latex.tabs)

stop()
# 3D EXAMPLE

mcMST3d = mcMST
mcMST3d$f3 = mcMST3d$f2
plotScatter3d(mcMST3d, package = "plot3D")
