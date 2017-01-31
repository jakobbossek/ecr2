library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(ggplot2)

load_all(".")

set.seed(123)

# defs
N = 15L

#fitness = makeFitnessFunction(
fitness.fun = function(x, ...) {
  dists = dist(x)
  return(1 / sum(dists)) # since we want to maximise here
}

# control and ope
myGenerator = makeGenerator(
  generator = function(size, par.list) {
    lapply(seq(size), function(i) {
      matrix(runif(par.list$N * 2L), ncol = 2L)
    })
  },
  name = "Point generator",
  description = "Generates random point clouds in the euclidean space",
  supported = "custom"
)

myMutator = makeMutator(
  mutator = function(ind, par.list) {
    idx = which(runif(nrow(ind)) < 0.1)
    ind[idx, ] = matrix(runif(2 * length(idx)), ncol = 2)
    return(ind)
  },
  name = "Point-Shift mutation",
  description = "Shift all points in a random direction",
  supported = "custom"
)

res = ecr(fitness.fun = fitness.fun, n.objectives = 1L, par.list = list(N = N),
  representation = "custom",
  mu = 100L, lambda = 10L, survival.strategy = "plus",
  mutator = myMutator, recombinator = NULL,
  survival.selector = setupGreedySelector(), generator = myGenerator,
  terminators = list(stopOnIters(10000L)))

plot(res$best.x[[1L]])
