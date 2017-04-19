library(smoof)
library(parallelMap)
library(microbenchmark)
library(devtools)

load_all(".")

# PARALLELIZATION
# ===============
# In the following we perform a simple benchmark on an artificially slowed down
# objective function to show the effect of parallel function evaluation.
fitness.fun = function(x) {
  Sys.sleep(runif(1, min = 0.03, max = 0.1)) # delay execution a bit
  return(sum(x^2))
}

lower = c(-5, -5)
upper = c(5, 5)

runEA = function() {
  ecr(fitness.fun = fitness.fun, n.objectives = 1L,
    representation = "float", n.dim = 2L,
    lower = lower, upper = upper,
    mu = 10L, lambda = 10L,
    mutator = setup(mutGauss, lower = lower, upper = upper),
    terminators = list(stopOnIters(30L)))
}

# wrapper for sequential evaluation
EAseq = function() {
  runEA()
}

# wrapper for parallel evaluation with 3 CPU cores
# Note: This will fail, if your system has less than 3 cores.
EApar = function() {
  parallelStartMulticore(cpus = 3L, level = "ecr.evaluateFitness") # use 3 cpus
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
ggsave("benchmark_parallelization.pdf", width = 7, height = 3, plot = pl)
# Unit: relative
#     expr   min    lq  mean median    uq  max neval cld
#  EAseq() 2.429 2.349 2.379   2.36 2.346 2.42    10   b
#  EApar() 1.000 1.000 1.000   1.00 1.000 1.00    10  a
