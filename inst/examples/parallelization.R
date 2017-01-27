library(smoof)
library(parallelMap)
library(microbenchmark)
library(devtools)

load_all(".")

# In the following we perform a simple benchmark on an artificially slowed down
# objective function to show the effect of parallel function evaluation.
fitness.fun = makeSingleObjectiveFunction(
  fn = function(x) {
    Sys.sleep(runif(1, min = 0.03, max = 0.1)) # delay execution a bit
    return(sum(x^2))
  },
  par.set = makeNumericParamSet("x", len = 2L, lower = -5, upper = 5)
)

runEA = function() {
  ecr(fitness.fun = fitness.fun, representation = "float",
  mu = 10L, lambda = 10L,
  terminators = list(stopOnIters(30L)))
}

# wrapper for sequential evaluation
EAseq = function() {
  runEA()
}

# wrapper for parallel evaluation with 3 CPU cores
# Note: This will fail, if your system has less than 3 cores.
EApar = function() {
  parallelStartMulticore(cpus = 3L, level = "ecr2.evaluateFitness") # use 3 cpus
  runEA()
  parallelStop()
}

set.seed(1)
# Use the microbenchmark package to repeat both sequential and parallel EA
# each 10 times.
bench = microbenchmark(
  EAseq(),
  EApar(),
  times = 10L
)

# We see a speedup of about factor 3
print(bench, unit = "relative")
pl = autoplot(bench, log = FALSE)# + ylim(c(0, 50)) + ylab("Time [seconds]")
print(pl)
# Unit: relative
#     expr   min    lq  mean median    uq  max neval cld
#  EAseq() 2.429 2.349 2.379   2.36 2.346 2.42    10   b
#  EApar() 1.000 1.000 1.000   1.00 1.000 1.00    10  a
