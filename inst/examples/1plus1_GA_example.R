# Here we use the ecr package to optimize the ONE-MIN function,
# i. e., a simple binary function. ONE-MIN(x) = x_1 + ... + x_n,
# where n is the number of parameters and x_i in {0,1} for i = 1,...,n.
# We apply a very simple evolutionary algorithm, namely the (1+1)-GA
# to solve the problem and plot the optimization trace.
library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(ggplot2)

load_all(".")

set.seed(123)

# the target function is the popular ONE-MIN function
onemin = function(x) sum(x)
n.bits = 50L

# Here we make use of mutations only! The nullRecombinator
# does nothing.
res = ecr(fitness.fun = onemin, n.objectives = 1L,
  n.bits = n.bits, representation = "binary",
  mu = 1L, lambda = 1L, terminators = list(stopOnIters(1000L)),
  mutator = setupBitflipMutator(p = 1 / n.bits), p.mut = 1L,
  recombinator = NULL)

print(res)
print(res$best.x)
print(res$best.y)

# plot optimization trace
#print(autoplot(res$log, log.fitness = FALSE))
