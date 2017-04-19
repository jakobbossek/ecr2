# ecr: Evolutionary Computation in R (2nd version)

[![CRAN Status Badge](http://www.r-pkg.org/badges/version/ecr2)](http://cran.r-project.org/web/packages/ecr2)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/ecr2)](http://cran.rstudio.com/web/packages/ecr2/index.html)
[![Build Status](https://travis-ci.org/jakobbossek/ecr2.svg?branch=master)](https://travis-ci.org/jakobbossek/ecr2)
[![Build status](https://ci.appveyor.com/api/projects/status/eu0nns2dsgocwntw/branch/master?svg=true)](https://ci.appveyor.com/project/jakobbossek/ecr2/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/jakobbossek/ecr2/badge.svg?branch=master)](https://coveralls.io/github/jakobbossek/ecr2?branch=master)

**NOTE:** The **ecr package v.2.0.0** is the official follow-up package to my package [ecr](https://github.com/jakobbossek/ecr). I was unsatisfied with some design choices and thus decided to restructure and rewrite a lot. Changes are that manifold and fundamental, that I decided to set up a new package, since most of the **ecr** functions are either deprecated, renamed, deleted or underlie substantial interface changes.

## A gentle introduction

The **ecr** package, *Evolutionary Computation in R (2nd version)*, is conceived as a "white-box" framework for single- and multi-objective optimization strongly inspired by the awesome [Evolutionary Computation (EC) framework DEAP](https://github.com/DEAP/deap) for the Python programming language. In contrast to black-box frameworks, which usually try to hide as much of internal complexity (e.g., data structures) in opaque high-level EC components, **ecr** makes the development of evolutionary algorithms (EA) - as DEAP does - transparent: the evolutionary loop is written by hand sticking to few conventions, utilizing few simple utility functions and controlling everything. We believe, that this is the most flexible way in evolutionary algorithm design. On top **ecr** ships with a black-box for *standard tasks*, e.g., optimization of a continuous function, as well. The core features of ecr are the following

* Flexible *white-box* approach to EA design and implementation.
* A lot of predefined EA operators for standard representations, i.e., permutations, binary strings and real-values vectors.
* Powerful logging mechanism.
* Possibility to use custom representations/genotypes.
* Possibility to define custom EA operators, i.e., mutation, variation and selection operators.
* Easy parallelization via [parallelMap](https://cran.r-project.org/web/packages/parallelMap/index.html)
* Black-box approach for standard tasks.
* Single- and multi-objective optimization.
* Implementations of some popular performance indicators in Evolutionary Multi-Objective Optimization (EMOA), e.g., hyper-volume-indicator, epsilon indicator as well as R1, R2 and R3 indicator.
* Predefined state-of-the-art EMOA algorithms NSGA-II, SMS-EMOA and AS-EMOA.

The best way to illustrate the process of algorithm design in **ecr** is by example. Assume we aim to find the global minimum of the highly multimodal one-dimensional Ackley-Function. The function is available in the R package [smoof](https://cran.r-project.org/web/packages/smoof/index.html) and may be initialized as follows:
```splus
library(ecr)
library(ggplot2)
library(smoof)
fn = makeAckleyFunction(1L)
autoplot(fn, show.optimum=TRUE, length.out = 1000)
```

### Writing the evolutionary loop by hand

We decide to use an evolutionary (30 + 5)-strategy, i.e., an algorithm that keeps a population of size mu = 30, in each generation creates lambda = 5 offspring by variation and selects the best mu out of mu + lambda individuals to survive. First, we define some variables.
```splus
MU = 30L; LAMBDA = 5L; MAX.ITER = 200L
lower = getLowerBoxConstraints(fn)
upper = getUpperBoxConstraints(fn)
```

In order to implement this algorithm the first step is to define a *control object*, which stores information on the objective function and a set of evolutionary operators.
```splus
control = initECRControl(fn)
control = registerECROperator(control, "mutate", mutGauss, sdev = 2, lower = lower, upper = upper)
control = registerECROperator(control, "selectForSurvival", selGreedy)
```
Here, we decide to perform mutation only. The best mu individuals (regarding fitness values) are going to be selected to build up the next generation.

Finally, the evolutionary loop is implemented. 
```splus
population = genReal(MU, getNumberOfParameters(fn), lower, upper)
fitness = evaluateFitness(control, population)
for (i in seq_len(MAX.ITER)) {
    # animate the loop
    pl = autoplot(fn, length.out = 1000L)
    df = data.frame(x = unlist(population), y = as.numeric(fitness))
    pl = pl + geom_point(data = df, mapping = aes(x = x, y = y))
    print(pl)
    Sys.sleep(0.2)
    # sample lambda individuals at random
    idx = sample(1:MU, LAMBDA)
    # generate offspring by mutation and evaluate their fitness
    offspring = mutate(control, population[idx], p.mut = 1)
    fitness.o = evaluateFitness(control, offspring)
    # now select the best out of the union of population and offspring
    sel = replaceMuPlusLambda(control, population, offspring, fitness, fitness.o)
    population = sel$population
    fitness = sel$fitness
}
print(min(fitness))
print(population[[which.min(fitness)]])
```

### Black-box approach

Since the optimization of a continuous numeric function is a standard task in EC, **ecr** ships with a black-box function `ecr(...)` which basically is a customizable wrapper around the loop above. A lot of tasks can be accomplished by utlizing this single entry point. However, often EA design requires small tweaks, changes and adaptations which are simply impossible to realize with a black box regardless of their flexebility.

The optimization of our 1D Ackley-function via `ecr(...)` (without animation) might look like this:
```splus
res = ecr(fitness.fun = fn, representation = "float",
  n.dim = getNumberOfParameters(fn), survival.strategy = "plus",
  lower = lower, upper = upper,
  mu = MU, lambda = LAMBDA,
  mutator = setup(mutGauss, sdev = 2, lower = lower, upper = upper),
  terminators = list(stopOnIters(MAX.ITER)))
print(res$best.y)
print(res$best.x)
```

## Installation Instructions

The package will be available at [CRAN](http://cran.r-project.org) soon. Install the release version via:
```splus
install.packages("ecr")
```
If you are interested in trying out and playing around with the current github developer version use the [devtools](https://github.com/hadley/devtools) package and type the following command in R:

```splus
devtools::install_github("jakobbossek/ecr2")
```

## Contact

Please address questions and missing features about the **ecr** to the author Jakob Bossek <j.bossek@gmail.com>. Found some nasty bugs? Please use the [issue tracker](https://github.com/jakobbossek/ecr2/issues) for this. Pay attention to explain the problem as good as possible. At its best you provide an example, so I can reproduce your problem quickly.



