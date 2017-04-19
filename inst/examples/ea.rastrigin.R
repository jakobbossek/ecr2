library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(smoof)

load_all(".")

# EA FOR NUMERICAL OPTIMIZATION OF A STANDARD TEST-FUNCTION
# =========================================================
# The following example illustrates how to write an EA loop by hand with ecr
# helper functions. Even though doTheEvolution is quite powerful, not all tasks
# might be easily realizable.

# In this example we optimize the 2D sphere function with a custom loop.
fn = smoof::makeRastriginFunction(5L)
lower = getLowerBoxConstraints(fn)
upper = getUpperBoxConstraints(fn)

# setup EA parameters
mu = 100L
lambda = 100L
max.iter = 1000L

# Init control object / toolbox
control = initECRControl(fn)
control = registerECROperator(control, "mutate", mutUniform, lower = lower, upper = upper)
control = registerECROperator(control, "selectForMating", selRoulette)
control = registerECROperator(control, "selectForSurvival", selGreedy)

# build initial population
population = initPopulation(mu, genReal, n.dim = 5L, lower = lower, upper = upper)

# evaluate fitness function
fitness = evaluateFitness(control, population)
# here fitness is appended as an attribute to each individual
for (i in seq_along(population)) {
  attr(population[[i]], "fitness") = fitness[, i]
}

# now start the evolutionary cycle
st = system.time({
  for (iter in seq_len(max.iter)) {
    if (iter %% 10 == 0)
      catf("Generation: %i", iter)

      # then generate the offspring
      offspring = generateOffspring(control, population, fitness, lambda = lambda, p.mut = 0.2)
      fitness.o = evaluateFitness(control, offspring)
      for (i in seq_along(offspring)) {
        attr(offspring[[i]], "fitness") = fitness.o[, i]
      }

      # apply the survival selection
      # Here we want a (mu + lambda) selection
      sel = replaceMuPlusLambda(control, population, offspring)

      population = sel$population
      fitness = sel$fitness
  }
})

print(st)
min.idx = which.min(fitness)
catf("Solution x* = (%s), f(x*) = %.10f",
  collapse(population[[min.idx]]),
  fitness[, min.idx])
