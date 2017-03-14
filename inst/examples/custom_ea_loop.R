library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(smoof)

load_all(".")

# the following example illustrates how to write an EA loop by hand with ecr
# helper functions. Even though doTheEvolution is quite powerful, not all tasks
# might be easily realizable.

# In this example we optimize the 2D sphere function with a custom loop.
fn = smoof::makeRastriginFunction(5L)

# setup the control object (neccesary for custom loops as well)
mu = 100L
lambda = 100L
max.iter = 1000L

# Init control object / toolbox
# We stick to default operators.
control = initECRControlFloat(fn)
control = registerSurvivalSelector(control, setupRouletteWheelSelector())

# build initial population
population = initPopulation(mu, control)

# evaluate fitness function
fitness = evaluateFitness(population, control)
for (i in seq_along(population)) {
  attr(population[[i]], "fitness") = fitness[, i]
}

#print(population)

# now start the evolutionary cycle
st = system.time({
  for (iter in seq_len(max.iter)) {
    if (iter %% 10 == 0)
      catf("Generation: %i", iter)

      # then generate the offspring
      offspring = generateOffspring(control, population, fitness, lambda = lambda, p.mut = 0.2)
      fitness.o = evaluateFitness(offspring, control)
      for (i in seq_along(offspring)) {
        attr(offspring[[i]], "fitness") = fitness.o[, i]
      }

      #print(offspring)

      # apply the survival selection
      # Here we want a (mu + lambda) selection
      sel = replaceMuPlusLambda(control, population, offspring)#, fitness, fitness.o)

      population = sel$population
      fitness = sel$fitness
  }
})

print(st)
min.idx = which.min(fitness)
catf("Solution x* = (%s), f(x*) = %.10f",
  collapse(population[[min.idx]]),
  fitness[, min.idx])
