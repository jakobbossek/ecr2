library(devtools)

load_all(".")

# AN EMOA FOR A BI-OBJECTIVE SCHEDULING PROBLEM
# =============================================
# Here we aim to approximate the Pareto optimal front of the 1||Sum C_i, L_max
# scheduling problem. A schedule is a permutation of jobs/tasks. We are searching
# for a job order which minimizes the sum of completion times Sum C_i and
# simultaneously minimizes the maximal lateness L_max = max (C_i - d_i).
# Since both objectives are conflicting we can not expect to find a single, optimal
# solution. We rather search the search space for a set of incomparable trade-off
# solutions.
#
# We do so by applying a (20 + 10)-EMOA for 2000 generations. Since we are searching
# for job orders, each individual is a permutation of the set {1, ..., n} where n
# is the number of jobs. We can thus rely on standard mutation and recombination for
# this kind of genotype: scramble mutation and ordered crossover.
# Nondominated sorting eventually followed by crowding distance computation is employed
# as the multi-criteria survival selection mechanism.

# reproducability
set.seed(1)

# setup
n.jobs = 25L

# generate random processing times and due dates
proc.times = runif(n.jobs, 1, 20)
due.dates = proc.times + sapply(proc.times, function(i) {
  rnorm(1L, mean = 10 * i, sd = 3 * i)
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
fitness = evaluateFitness(control, population)

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
  fitness.o = evaluateFitness(control, offspring)

  # apply (MU + LAMBDA) selection
  sel = replaceMuPlusLambda(control, population, offspring, fitness, fitness.o)
  population = sel$population
  fitness = sel$fitness

  # update log
  updateLogger(log, population, fitness = fitness, n.evals = LAMBDA)
}

stats = getStatistics(log)
pl.stats = plotStatistics(stats) + theme(legend.position = "top")
# ggsave("emoa_scheduling_statistics.pdf", width = 8, height = 4, plot = pl.stats)
# pl.front = plotFront(fitness, obj.names = c("SumCi", "Lmax"))
# ggsave("emoa_scheduling_front.pdf", width = 4, height = 4, plot = pl.front)
# print(pl.front)
