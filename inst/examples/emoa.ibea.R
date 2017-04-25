library(devtools)
library(BBmisc)
library(smoof)

load_all(".")

fn = smoof::makeDTLZ1Function(n.objectives = 2L, dimensions = 2L)
lower = getLowerBoxConstraints(fn)
upper = getUpperBoxConstraints(fn)
ref.point = getRefPoint(fn)

control = initECRControl(fn)
control = registerECROperator(control, "mutate", mutPolynomial, lower = lower, upper = upper)
control = registerECROperator(control, "recombine", recSBX, lower = lower, upper = upper)
control = registerECROperator(control, "selectForMating", selTournament)
control = registerECROperator(control, "selectForSurvival", selGreedy)

control2 = control
control2$task$n.objectives = 1L
control2$task$minimize = TRUE

max.iter = 100L
mu = 20L
tau = 0.1

population = genReal(n = mu, n.dim = getNumberOfParameters(fn),
  lower = lower, upper = upper)

dd = function(A, B) {
  if (dominates(as.numeric(A), as.numeric(B)))
    return(emoaIndHV(A, B, ref.point = ref.point))
  return(emoaIndHV(A, cbind(A, B), ref.point = ref.point))
}

fitness.fun = function(fitness) {
  n = ncol(fitness)
  fitn = numeric(n)
  for (i in seq_len(n)) {
    val = 0.0
    for (j in seq_len(n)) {
      if (i == j)
        next
      val = val - exp(-dd(fitness[, j, drop = FALSE], fitness[, i, drop = FALSE]) / tau)
    }
    fitn[i] = val
  }
  return(fitn)
}

fitness = evaluateFitness(control, population)
fitness.scalar = matrix(fitness.fun(fitness), nrow = 1L)

for (i in seq_len(max.iter)) {
  cat(".")
  print(population)
  print(fitness.scalar)
  offspring = generateOffspring(control2, population, fitness.scalar, lambda = 1L)
  print(offspring)
  population.new = c(population, offspring)
  fitness.new = cbind(fitness, evaluateFitness(control, offspring))
  fitness.new.scalar = matrix(fitness.fun(fitness.new), nrow = 1L)

  #FIXME: this is murks
  while (length(population.new) > mu) {
    min.idx = which.min(fitness.new.scalar)
    #min.fit = fitness.new[, min.idx, drop = FALSE]
    fitness.new = fitness.new[, -min.idx, drop = FALSE]
    population.new = population.new[-min.idx]
    #fitness.new.scalar = fitness.new.scalar[, -min.idx, drop = FALSE]
    fitness.new.scalar = matrix(fitness.fun(fitness.new), nrow = 1L)
    # for (i in seq_len(ncol(fitness.new.scalar))) {
    #   fitness.new.scalar[i] = fitness.new.scalar[i] + exp(-dd(min.fit, fitness.new[, i, drop = FALSE]) / tau)
    # }
  }
  population = population.new
  fitness = fitness.new
  fitness.scalar = fitness.new.scalar
}
stopf("fuuu")
