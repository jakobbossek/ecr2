library(devtools)
library(BBmisc)
library(smoof)

load_all(".")

# SIMPLE-IBEA IMPLEMENTATION FOLLOWING [2]
# =========================================================
#
# [1] Zitzler E., Künzli S. (2004) Indicator-Based Selection in Multiobjective Search.
# In: Yao X. et al. (eds) Parallel Problem Solving from Nature - PPSN VIII. PPSN 2004.
# Lecture Notes in Computer Science, vol 3242. Springer, Berlin, Heidelberg
#
# [2] Zitzler E., Brockhoff D., Thiele L. (2007) The Hypervolume Indicator Revisited:
# On the Design of Pareto-compliant Indicators Via Weighted Integration. In: Obayashi S.,
# Deb K., Poloni C., Hiroyasu T., Murata T. (eds) Evolutionary Multi-Criterion Optimization.
# EMO 2007. Lecture Notes in Computer Science, vol 4403. Springer, Berlin, Heidelberg

# MOO problem
fn = smoof::makeZDT3Function(dimensions = 10L)
lower = getLowerBoxConstraints(fn)
upper = getUpperBoxConstraints(fn)
ref.point = getRefPoint(fn)

# initilaizing toolbox
control = initECRControl(fn)
control = registerECROperator(control, "mutate", mutPolynomial, lower = lower, upper = upper)
control = registerECROperator(control, "recombine", recSBX, lower = lower, upper = upper)
control = registerECROperator(control, "selectForMating", selSimple)

# setup
max.iter = 10000L
mu = 100L
lambda = 1L
indicator.fun = computeHV

# init population
population = genReal(n = mu, n.dim = getNumberOfParameters(fn),
  lower = lower, upper = upper)
fitness = evaluateFitness(control, population)

# do the evolutionary loop
for (i in seq_len(max.iter)) {
  cat(".")
  offspring = generateOffspring(control, population, fitness, lambda = lambda)

  # temporary population
  population.new = c(population, offspring)
  fitness.new = cbind(fitness, evaluateFitness(control, offspring))

  # now cut down until only mu individuals are left
  ranks = doNondominatedSorting(fitness.new)$ranks

  # get the rank / front number of the worst individual if mu individuals are selected
  max.rank = sort(ranks)[mu]

  idx.lower = which(ranks < max.rank)
  idx.equal = which(ranks == max.rank)

  n = length(idx.lower) + length(idx.equal)

  while (n > mu) {
    # now get indicator losses of each of the element on max.rank level
    fitn.equal = fitness.new[, idx.equal, drop = FALSE]
    losses = sapply(1:ncol(fitn.equal), function(i) {
      indicator.fun(fitn.equal, ref.point = matrix(ref.point, ncol = 1L)) - indicator.fun(fitn.equal[, -i, drop = FALSE], ref.point = matrix(ref.point, ncol = 1L))
    })
    idx.equal = idx.equal[-which.min(losses)]
    n = n - 1L
  }

  idx.new = c(idx.lower, idx.equal)
  population = population.new[idx.new]
  fitness = fitness.new[, idx.new, drop = FALSE]
}
plot(t(fitness), main = "Approximation of the nondominated front.")
stopf("fuuu")
