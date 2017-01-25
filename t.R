library(methods)
library(devtools)
library(testthat)
library(smoof)
library(ggplot2)
library(gridExtra)
library(microbenchmark)

load_all(".")

MAX.GENS = 1200L
n.dim = 30L
fitness.fun = addCountingWrapper(smoof::makeZDT1Function(dimensions = n.dim))
par.set = getParamSet(fitness.fun)

fitness.fun = addCountingWrapper(smoof::makeAckleyFunction(dimensions = n.dim))
par.set = getParamSet(fitness.fun)

st = system.time({
  res = ecr(fitness.fun, lower = getLower(par.set), upper = getUpper(par.set), n.dim = n.dim,
  mu = 100L, lambda = 100L, representation = "float", survival.strategy = "plus", n.objectives = getNumberOfObjectives(fitness.fun),
  #survival.selector = setupDominatedHypervolumeSelector(ref.point = getRefPoint(fitness.fun)),
  survival.selector = setupTournamentSelector(k = 2L),
  recombinator = setupIntermediateRecombinator(),#setupSBXRecombinator(eta = 15, p = 0.7),
  mutator = setupUniformMutator(),#setupPolynomialMutator(p = 0.3, eta = 25),
  terminator = list(stopOnIters(MAX.GENS)))
})

print(res)
#plot(res$pareto.front)

log = res$log$env$stats
log = log[1:MAX.GENS, ]
library(reshape2)
log2 = melt(log, "gen", value.name = "value", variable.name = "stat")
pl = ggplot(log2, aes(x = gen, y = value, linetype = stat)) + geom_line()
print(pl)

# print(st)

stop(123)

# # c1 = initECRControlFloat(smoof::makeSphereFunction(4L))
# # c2 = initECRControlFloat(function(x) c(sum(x), sum(exp(x)/7)), minimize = c(TRUE, FALSE), n.dim = 20, n.objectives = 2L, lower = -5, upper = 3)
# # c3 = initECRControlBinary(function(x) sum(x), n.objectives = 1L, n.bits = 100L)
# # c4 = initECRControlPermutation(function(x) sum(x), perm = 10, n.objectives = 3L)

# control = initECRControlFloat(fitness.fun)
# control = registerMutator(control, operator.fun = setupPolynomialMutator(p = 0.3, eta = 25))#setupBitFlipMutator())
# control = registerRecombinator(control, setupSBXRecombinator(eta = 15, p = 0.7))#setupCrossoverRecombinator())
# control = registerGenerator(control, operator.fun = setupUniformGenerator(len = n.dim, lower = getLower(par.set), upper = getUpper(par.set)))#setupBinaryGenerator(len = n.dim))
# control = registerSurvivalSelector(control, operator.fun = setupDominatedHypervolumeSelector(ref.point = getRefPoint(fitness.fun)))
# control = registerMatingSelector(control, operator.fun = setupSimpleSelector())
# control = registerLogger(control, logger = setupECRDefaultLogger(
#   log.stats = list("mean", "sd", "hv" = list(fun = computeDominatedHypervolume, pars = list(ref.point = rep(11, 2L)))),
#   log.pop = TRUE, init.size = 10000L)
# )

n.dim = 50L
fitness.fun = smoof::makeAckleyFunction(dimensions = n.dim)
par.set = getParamSet(fitness.fun)

# c1 = initECRControlFloat(smoof::makeSphereFunction(4L))
# c2 = initECRControlFloat(function(x) c(sum(x), sum(exp(x)/7)), minimize = c(TRUE, FALSE), n.dim = 20, n.objectives = 2L, lower = -5, upper = 3)
# c3 = initECRControlBinary(function(x) sum(x), n.objectives = 1L, n.bits = 100L)
# c4 = initECRControlPermutation(function(x) sum(x), perm = 10, n.objectives = 3L)

control = initECRControlFloat(fitness.fun)
control = registerMutator(control, operator.fun = setupUniformMutator())#setupBitFlipMutator())
control = registerRecombinator(control, setupIntermediateRecombinator())#())
control = registerGenerator(control, operator.fun = setupUniformGenerator(len = n.dim, lower = getLower(par.set), upper = getUpper(par.set)))#setupBinaryGenerator(len = n.dim))
control = registerSurvivalSelector(control, operator.fun = setupTournamentSelector(k = 2L))#setupSimpleSelector())
control = registerMatingSelector(control, operator.fun = setupSimpleSelector())
control = registerLogger(control, logger = setupECRDefaultLogger(
  log.stats = list("min", "max", "mean"),#, "hv" = list(fun = computeDominatedHypervolume, pars = list(ref.point = rep(11, 2L)))),
  log.pop = TRUE, init.size = 10000L)
)

MAX.GENS = 2000L
MU = 100L
LAMBDA = 100L
n.evals = MU
st = system.time({
  population = initPopulation(mu = MU, control = control)
  fitness = evaluateFitness(population, control)
  # init logger
  control$logger$before()

  repeat {
  #for (gen in 1:MAX.GENS) {
    # generate offspring
    offspring = generateOffspring(control, population, fitness, lambda = LAMBDA, p.recomb = 0.7, p.mut = 0.3)
    fitness.offspring = evaluateFitness(offspring, control)

    #sel = replaceMuPlusLambda(control, population, offspring, fitness, fitness.offspring)
    sel = replaceMuCommaLambda(control, population, offspring, fitness, fitness.offspring, n.elite = 10L)

    population = sel$population
    fitness = sel$fitness

    # do some logging
    control$logger$step(control$logger, population, fitness, n.evals = LAMBDA)
    stop.obj = doTerminate(control$logger, stop.conds = list(
      stopOnEvals(104092033),
      stopOnIters(MAX.GENS)))
    if (length(stop.obj) > 0L) {
      break
    }
  }
})
print(st)

log = control$logger$env$stats
log = log[1:MAX.GENS, ]
library(reshape2)
log2 = melt(log, "gen", value.name = "value", variable.name = "stat")
pl = ggplot(log2, aes(x = gen, y = value, linetype = stat)) + geom_line()
print(pl)

stop(123)
log = control$logger$env$stats

pl = ggplot(log, aes(x = gen, y = hv)) + geom_line()
ff = as.data.frame(t(fitness))
colnames(ff) = c("f1", "f2")
pl.front = ggplot(ff, aes(x = f1, y = f2)) + geom_point()
print(grid.arrange(pl, pl.front, nrow = 1L))
print(st)

stop(123)
