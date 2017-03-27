library(devtools)

load_all(".")

# OPTIMIZATION OF THE ONEMAX FUNCTION
# f(x) = sum(x) with x being a bitstring, i.e.,
# a vector of zeros and ones.
fitness.fun = function(x) sum(x)

# reproducibility
set.seed(1)

# WHITE BOX APPROACH
# ==================
# Explicit implementation of the evolutionary loop

# evolutionary setup
n.bits = 50L
MU = 25L
LAMBDA = 25L
MAX.ITER = 100L

# initialize toolbox
control = initECRControl(fitness.fun, n.objectives = 1L, minimize = FALSE)
control = registerECROperator(control, "mutate", setupBitflipMutator(p = 1 / n.bits))
control = registerECROperator(control, "recombine", setupCrossoverRecombinator())
control = registerECROperator(control, "selectForMating", setupTournamentSelector(k = 2L))
control = registerECROperator(control, "selectForSurvival", setupGreedySelector())

# initialize population of MU random bitstring
population = genBin(MU, n.bits)
fitness = evaluateFitness(control, population)

# now do the evolutionary loop
for (i in seq_len(MAX.ITER)) {
  # generate offspring by mutation only.
  offspring = generateOffspring(control, population, fitness, lambda = LAMBDA, p.recomb = 0.7, p.mut = 0.3)

  # alternatively
  offspring = recombinate(control, population, fitness, lambda = LAMBDA, p.recomb = 0.7)
  offspring = mutate(control, offspring, p.mut = 0.3)

  # even more fine-grained
  idx.mut = runif(length(offspring)) < 0.3
  if (sum(idx.mut) > 0)
    offspring[idx.mut] = lapply(offspring[idx.mut], control$mutate, list())


  # calculate costs of new schedules
  fitness.o = evaluateFitness(control, offspring)

  # apply (MU + MU) selection
  sel = replaceMuPlusLambda(control, population, offspring, fitness, fitness.o)
  population = sel$population
  fitness = sel$fitness
}

print(population[[which.max(fitness)]])
print(max(fitness))

set.seed(1)

# BLACK-BOX APPROACH
# ==================
# Now the same procedure with the ecr black-box, i.e., "plug and play"
# style.
res = ecr(fitness.fun = fitness.fun, n.objectives = 1L, minimize = FALSE,
  representation = "binary", n.bits = n.bits,
  mu = MU, lambda = LAMBDA, survival.strategy = "plus",
  mutator = setupBitflipMutator(p = 1 / n.bits),
  p.mut = 0.7, p.recomb = 0.3, terminators = list(stopOnIters(MAX.ITER)))

print(res$best.y)
print(res$best.x)
