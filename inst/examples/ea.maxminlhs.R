library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(ggplot2)

load_all(".")

# EA FOR THE GENERATION OF A POINT CLOUD MAXIMIZING THE MINIMAL
# DISTANCE BETWEEN POINTS (RESEMBLES A MAXIMIN-LATIN-HYPERCUBE-SAMPLING)
# ======================================================================
# This example illustrates custom representations. Here an individual is
# a (N x 2) matrix.

# reproducability
set.seed(1)

# defs
N = 30L

# we aim to maximize the minimal distance
fitness.fun = function(x) {
  min(dist(x))
}

# "Point-Shift mutation"
# Shift all points in a random direction
myMutator = makeMutator(
  mutator = function(ind, par.list) {
    idx = which(runif(nrow(ind)) < 0.1)
    ind[idx, ] = matrix(runif(2 * length(idx)), ncol = 2)
    return(ind)
  },
  supported = "custom"
)

MU = 100L
LAMBDA = 10L

initial.solutions = gen(matrix(runif(N * 2L), ncol = 2L), MU)

res = ecr(fitness.fun = fitness.fun, n.objectives = 1L, minimize = FALSE,
  representation = "custom",
  mu = MU, lambda = LAMBDA, survival.strategy = "plus",
  initial.solutions = initial.solutions,
  mutator = myMutator,
  terminators = list(stopOnIters(5000L)))

plot(res$best.x[[1L]])
