library(methods)
library(devtools)
library(testthat)
library(smoof)
library(ggplot2)
library(gridExtra)
library(microbenchmark)

load_all(".")

n.dim = 30L
fitness.fun = smoof::makeZDT1Function(dimensions = n.dim)
par.set = getParamSet(fitness.fun)

control = initECRControl()
control = registerMutator(control, fun = setupGaussMutator(p = 0.3))#setupBitFlipMutator())
control = registerRecombinator(control, fun = setupCrossoverRecombinator(p = 1))
control = registerGenerator(control, fun = setupUniformGenerator(len = n.dim, lower = getLower(par.set), upper = getUpper(par.set)))#setupBinaryGenerator(len = n.dim))
control = registerSurvivalSelector(control, fun = setupNondomSelector())
control = registerObjectiveFunction(control, fun = fitness.fun, n.objectives = getNumberOfObjectives(fitness.fun))
control = registerLogger(control, logger = setupECRDefaultLogger(
  log.stats = list("mean", "sd", "hv" = list(fun = computeDominatedHypervolume, pars = list(ref.point = rep(11, 2L)))),
  log.pop = TRUE, init.size = 1000L)
)

MAX.GENS = 1000L
MU = 100L
LAMBDA = 100L
st = system.time({
  population = control$generate(size = MU)
fitness = do.call(cbind, lapply(population, control$task$fitness))

# init logger
control$logger$before()

for (gen in seq_len(MAX.GENS)) {
  # generate offspring
  offspring = lapply(population, control$mutate)
  fitness.off = do.call(cbind, lapply(offspring, control$task$fitness))

  # select next generation
  merged.pop = c(population, offspring)
  merged.fit = cbind(fitness, fitness.off)
  surv.idx = control$selectForSurvival(merged.fit, n.select = MU)
  population = merged.pop[surv.idx]
  fitness = merged.fit[, surv.idx, drop = FALSE]

  # do some logging
  #control$logger$step(control$logger, population, fitness, gen)
}
})
# log = control$logger$env$stats

# pl = ggplot(log, aes(x = gen, y = hv)) + geom_line()
# ff = as.data.frame(t(fitness))
# colnames(ff) = c("f1", "f2")
# pl.front = ggplot(ff, aes(x = f1, y = f2)) + geom_point()
# print(grid.arrange(pl, pl.front, nrow = 1L))
print(st)

stop(123)

MU = 100L
LAMBDA = 100L


stats = list("mean" = mean, "sd", "hv" = list(fun = computeDominatedHypervolume, pars = list(ref.point = c(11, 11))))

st = system.time({
# which funs are not named?
no.names = names(stats) == ""
# which funs are no characters?
no.char = !sapply(stats, is.character)
# if both is true, i.e. unnamed and no char, we cannot determine a name
stopifnot(!all(no.char & no.names))
# otherwise take chars as names ...
names(stats)[no.names] = stats[no.names]
# ... and convert names to functions
stats[no.names] = sapply(stats[no.names], get)
})
print(st)

fitness = matrix(runif(10) * 10 + runif(10), nrow = 2L)

st = system.time({
ngens = 100000L
log = makeDataFrame(ncol = length(stats), nrow = 100L, col.types = "numeric", col.names = names(stats))
for (i in 1:ngens) {
  cur.stats = lapply(stats, function(stat.fun) {
    if (is.list(stat.fun))
      return(do.call(stat.fun$fun, c(list(fitness), stat.fun$pars)))
    return(stat.fun(fitness))
  })
  if (nrow(log) < i) {
    catf("increasing log size! Doubling size: %i -> %i", nrow(log), 3 * nrow(log))
    log = rbind(log, makeDataFrame(ncol = length(stats), nrow = nrow(log) * 2, col.types = "numeric", col.names = names(log)))
  }
  log[i, ] = cur.stats
}
})
