library(methods)
library(devtools)
library(testthat)
library(smoof)
library(ggplot2)
library(gridExtra)
library(microbenchmark)

load_all(".")

n.dim = 30L
fitness.fun = addCountingWrapper(smoof::makeZDT1Function(dimensions = n.dim))
par.set = getParamSet(fitness.fun)

# c1 = initECRControlFloat(smoof::makeSphereFunction(4L))
# c2 = initECRControlFloat(function(x) c(sum(x), sum(exp(x)/7)), minimize = c(TRUE, FALSE), n.dim = 20, n.objectives = 2L, lower = -5, upper = 3)
# c3 = initECRControlBinary(function(x) sum(x), n.objectives = 1L, n.bits = 100L)
# c4 = initECRControlPermutation(function(x) sum(x), perm = 10, n.objectives = 3L)

control = initECRControlFloat(fitness.fun)
control = registerMutator(control, fun = setupGaussMutator())#setupBitFlipMutator())
control = registerRecombinator(control, fun = setupIntermediateRecombinator())
control = registerGenerator(control, fun = setupUniformGenerator(len = n.dim, lower = getLower(par.set), upper = getUpper(par.set)))#setupBinaryGenerator(len = n.dim))
control = registerSurvivalSelector(control, fun = setupNondomSelector())
control = registerMatingSelector(control, fun = setupSimpleSelector())
control = registerLogger(control, logger = setupECRDefaultLogger(
  log.stats = list("mean", "sd", "hv" = list(fun = computeDominatedHypervolume, pars = list(ref.point = rep(11, 2L)))),
  log.pop = TRUE, init.size = 1000L)
)

MAX.GENS = 100L
MU = 100L
LAMBDA = 100L
st = system.time({
  init.solutions = control$generate(size = floor(MU / 2))
  population = initPopulation(mu = MU, init.solutions = init.solutions, control = control)
  population = control$generate(size = MU)
  fitness = evaluateFitness(population, control)
  # init logger
  control$logger$before()

  for (gen in seq_len(MAX.GENS)) {
    # generate offspring
    offspring = generateOffspring(control, population, fitness, lambda = MU, p.recomb = 1, p.mut = 1, mut.pars = list(lower = control$lower, upper = control$upper))
    #offspring = lapply(population, control$mutate, lower = control$lower, upper = control$upper)
    fitness.off = evaluateFitness(offspring, control)

    # select next generation
    merged.pop = c(population, offspring)
    merged.fit = cbind(fitness, fitness.off)
    surv.idx = control$selectForSurvival(merged.fit, n.select = MU)
    population = merged.pop[surv.idx]
    fitness = merged.fit[, surv.idx, drop = FALSE]

    # do some logging
    control$logger$step(control$logger, population, fitness, gen)
  }
})
log = control$logger$env$stats

pl = ggplot(log, aes(x = gen, y = hv)) + geom_line()
ff = as.data.frame(t(fitness))
colnames(ff) = c("f1", "f2")
pl.front = ggplot(ff, aes(x = f1, y = f2)) + geom_point()
print(grid.arrange(pl, pl.front, nrow = 1L))
print(st)

stop(123)
