library(devtools)

load_all(".")

set.seed(1)

# setup
n.jobs = 25L

# generate random processing times and due dates
proc.times = runif(n.jobs, 1, 20)
due.dates = proc.times + sapply(proc.times, function(i) {
  rnorm(1L, mean = 10 * i, sd = 9 * i / 3)
})
# store jobs in data.frame
jobs = data.frame(j = 1:n.jobs, p = proc.times, d = due.dates)

# We aim the minimize the makespan and the maximal completion time
fitness.fun = function(job.order) {
  sorted.jobs = jobs[job.order, ]
  c(sum(cumsum(sorted.jobs$p)), max(cumsum(sorted.jobs$p) - sorted.jobs$d))
}

# evolutionary setup
MU = 20L
LAMBDA = 10L
MAX.ITER = 2000L
ref.point = c(15000, 500L)

# initialize toolbox
control = initECRControl(fitness.fun, n.objectives = 2L, minimize = c(TRUE, TRUE))
control = registerECROperator(control, "mutate", setupScrambleMutator())
control = registerECROperator(control, "recombine", setupOXRecombinator())
control = registerECROperator(control, "selectForMating", setupSimpleSelector())
control = registerECROperator(control, "selectForSurvival", setupNondomSelector())

# initialize population of random schedules
population = genPerm(MU, n.jobs)
fitness = evaluateFitness(population, control)

# initialize logger (store HV)
log = initLogger(control,
  log.stats = list(fitness = list("HV" = list(
    fun = computeDominatedHypervolume,
    pars = list(ref.point = ref.point)))),
  init.size = MAX.ITER)
updateLogger(log, population, fitness = fitness, n.evals = MU)

# now do the evolutionary loop
for (i in seq_len(MAX.ITER)) {
  # generate offspring by recombination and mutation
  offspring = recombinate(control, population, fitness = fitness, lambda = LAMBDA, p.recomb = 0.8)
  offspring = mutate(control, offspring, p.mut = 0.3)

  # calculate costs of new schedules
  fitness.o = evaluateFitness(offspring, control)

  # apply (MU + LAMBDA) selection
  sel = replaceMuPlusLambda(control, population, offspring, fitness, fitness.o)
  population = sel$population
  fitness = sel$fitness

  # update log
  updateLogger(log, population, fitness = fitness, n.evals = LAMBDA)
}

stats = getStatistics(log)
pl.stats = plotStatistics(stats)
pl.front = plotFront(fitness, obj.names = c("SumCj", "Lmax"))
print(pl.front)
