# Optimization of the famous Rastrigin function in one dimension
# using a simple Evolutionary Strategy (ES) and a custom
# monitoring function, which pauses the optimization process
# after each generation and plots the target fun as well as
# the entire population.

# We choose a (20 + 5) strategy with 1-elitism, a natural real-valued
# representation of the variables and 0.005 as the standard deviation
# of the normal mutation operator.
library(methods)
library(testthat)
library(devtools)
library(smoof)
library(ggplot2)
library(BBmisc)

load_all(".", reset = TRUE)

set.seed(123)

# Monitoring function. For details on the expected formal parameters
# see the help pages for makeMonitor and setupConsoleMonitor.
myMonitorStep = function(log, ...) {
  # control = opt.state$control
  # n.targets = control$n.targets
  # population = opt.state$population
  # task = opt.state$task

  # x = seq(-5, 5, by = 0.05)
  # df = expand.grid(x, x)
  # names(df) = paste0("x", 1:2)
  # df.points = as.data.frame(do.call(rbind, population$individuals))
  # names(df.points) = names(df)
  # df$y = apply(df, 1L, task$fitness.fun)

  # pl = ggplot(data = df, aes(x = x1, y = x2, z = y)) + geom_contour(colour = "gray")
  # pl = pl + geom_point(data = df.points, aes(z = NULL), colour = "tomato")
  # print(pl)
}

myMonitor = makeECRMonitor(step = myMonitorStep)

# generate objective function
fitness.fun = makeRastriginFunction(dimensions = 2L)

# no need to pass n.objectives, minimize, lower, upper and n.dim here since
# the fitness function is a smoof function. Thus all relevant information is
# automatically extracted.
res = ecr(fitness.fun = fitness.fun,
  representation = "float",
  mu = 100L, lambda = 10L,
  mutator = setupGaussMutator(p = 1, sdev = 0.15,
    lower = getLowerBoxConstraints(fitness.fun), upper = getUpperBoxConstraints(fitness.fun)),
  terminators = list(stopOnIters(100L)))

print(res$best.y)
print(getGlobalOptimum(fitness.fun)$value)
