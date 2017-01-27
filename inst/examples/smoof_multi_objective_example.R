# In this example we aim to approximate the Pareto-optimal front/set of the ZDT1
# test function. Since ecr does not support 'real' Multi-Objective Evolutionary
# algorithms, we solve a sequence of scalarized single-objective functions. I. e.
# we solve the single objective function f(x) = w1 f1(x) + w2 f2(x) with w1 + w2 = 1
# consecutively for different weights w1, w2.

library(methods)
library(testthat)
library(devtools)
library(smoof)
library(ggplot2)
library(BBmisc)

load_all(".", reset = TRUE)

set.seed(1)

# generate test function
# We aim to approximate the Pareto-front of this one
# with a scalarization approach.
obj.fun = makeZDT1Function(dimensions = 2L)

# define a 'grid of weights'
weights = seq(0, 1, by = 0.05)
n.reps = length(weights)

# initialize storage for the pareto set and the 'scalarized fitness'
pareto.set = matrix(NA, ncol = 2L, nrow = n.reps)
pareto.fitness = numeric(n.reps)

# now approximate the front
for (i in seq_len(n.reps)) {

  # choose weight vector
  weight1 = weights[i]
  weight2 = 1 - weight1
  w = c(weight1, weight2)

  # generate scalarized fitness function with 'weights' w
  fitness.fun = function(x) {
    force(w)
    res = obj.fun(x)
    sum(res * w)
  }
  # assure that fitness.fun is smoof function as well
  attributes(fitness.fun) = attributes(obj.fun)
  fitness.fun = setAttribute(fitness.fun, "n.objectives", 1L)
  fitness.fun = setAttribute(fitness.fun, "minimize", TRUE)

  # do the evolutionary magic
  res = ecr(fitness.fun = fitness.fun,
    representation = "float",
    mu = 50L, lambda = 10L,
    terminators = list(stopOnIters(100L)),
    mutator = setupGaussMutator(sdev = 0.05))

  # save new non-inferior point
  pareto.set[i, ] = as.numeric(res$best.x[[1L]])
  pareto.fitness[i] = as.numeric(res$best.y)
}

pareto.front = as.data.frame(t(apply(pareto.set, 1L, obj.fun)))
colnames(pareto.front) = c("f1", "f2")

# visualize pareto front and the computed approximation
pl = smoof::visualizeParetoOptimalFront(obj.fun)
pl = pl + geom_point(data = pareto.front, mapping = aes(x = f1, y = f2), colour = "blue")

print(pareto.set)
# Conclusion: The approach found a set of non-dominated points,
# which are not well distributed over the whole Pareto-front. There is a higher
# concentration of points located in the area where f2 < 0.2.
