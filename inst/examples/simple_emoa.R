library(devtools)
library(ggplot2)

load_all(".")

# Here we implement a simple multi-objective evolutionary algorithm. It generates
# an initial population and an empty Pareto archive of limited capacity. Then,
# until the generation counter reaches the generation limit, the following steps
# are performed:
# - update the archive with all non-dominated points of the newly generated offspring
#   and the existing archive. If the archive exceeds the capacity limit, remove the
#   elements with the lowest hypervolume contribution.
# - generate offspring by selecting parents from the archive via 2-tournament selection,
#   recombinating via uniform recombination and application of a Gauss mutation with a probability p < 1.

# setup
mu = 20L
max.size = 2 * 20L
max.iter = 1000L

fun = smoof::makeDTLZ1Function(dimensions = 2L, n.objectives = 2L)
lower = getLowerBoxConstraints(fun); upper = getUpperBoxConstraints(fun)
control = initECRControl(fun)
control = registerECROperator(control, "mutate", setup(mutPolynomial, lower = lower, upper = upper))
control = registerECROperator(control, "recombine", setup(recSBX, lower = lower, upper = upper))
control = registerECROperator(control, "selectForMating", selSimple)

# setup initial population
population = genReal(mu, n.dim = getNumberOfParameters(fun),
  lower = getLowerBoxConstraints(fun), upper = getUpperBoxConstraints(fun))
fitness = evaluateFitness(control, population)

# initialize Pareto archive
truncateByHVContr = function(inds, fitness, max.size, ...) {
  hvcs = computeHVContr(fitness, ...)
  hvcs.ord = order(hvcs, decreasing = TRUE)
  return(list(
    individuals = inds[hvcs.ord[seq_len(max.size)]],
    fitness = fitness[, hvcs.ord[seq_len(max.size)], drop = FALSE])
  )
}
archive = initParetoArchive(control, max.size = max.size, trunc.fun = truncateByHVContr)

for (iter in seq_len(max.iter)) {
  updateParetoArchive(archive, population, fitness)
  fitness.archive = getFront(archive)
  inds.archive = getIndividuals(archive)
  population = generateOffspring(control, inds.archive, fitness.archive, lambda = mu, p.recomb = 0, p.mut = 0.6)
  fitness = evaluateFitness(control, population)
}

pareto.front = getFront(archive)
pareto.front = as.data.frame(t(pareto.front))
names(pareto.front) = c("f1", "f2")
pl = qplot(f1, f2, data = pareto.front)
print(pl)
