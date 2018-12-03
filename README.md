# ecr: Evolutionary Computation in R

[![CRAN Status Badge](http://www.r-pkg.org/badges/version/ecr)](http://cran.r-project.org/web/packages/ecr)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/ecr)](http://cran.rstudio.com/web/packages/ecr/index.html)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/grand-total/ecr?color=orange)](http://cran.rstudio.com/web/packages/ecr/index.html)
[![Build Status](https://travis-ci.org/jakobbossek/ecr2.svg?branch=master)](https://travis-ci.org/jakobbossek/ecr2)
[![Build status](https://ci.appveyor.com/api/projects/status/eu0nns2dsgocwntw/branch/master?svg=true)](https://ci.appveyor.com/project/jakobbossek/ecr2/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/jakobbossek/ecr2/badge.svg?branch=master)](https://coveralls.io/github/jakobbossek/ecr2?branch=master)
[![Research software impact](http://depsy.org/api/package/cran/ecr/badge.svg)](http://depsy.org/package/r/ecr)

**NOTE:** The **ecr package v2** is the official follow-up package to my package [ecr v1](https://github.com/jakobbossek/ecr). I was unsatisfied with some design choices and thus decided to restructure and rewrite a lot. Changes are that manifold and fundamental, that I decided to set up a new repository, since most of the **ecr v1** functions are either deprecated, renamed, deleted or underlie substantial interface changes.

## News

* December 03, 2018: The ecr module on performance assessment of multi-objective stochastic algorithms is under heavy development. There will be a new release on CRAN soon.
* December 01, 2018: My paper on *Performance Assessment of Multi-Objective Evolutionary Algorithms With the R Package ecr* was accepted at the *Evolutionary Computation Software Systems (EvoSoft)* workshop hosted at the annual *Genetic and Evolutionary Computation Conference* ([GECCO'18](http://gecco-2018.sigevo.org/)).
* July 16, 2017: I gave a talk on ecr v2.0 on the annual Genetic and Evolutionary Computation Conference ([GECCO'17](http://gecco-2017.sigevo.org/)) in Berlin.
* July 10, 2017: Release of ecr v2.1.0
* April 04, 2017: My paper submission to the *Evolutionary Computation Software Systems (EvoSoft)* workshop at this years *Genetic and Evolutionary Computation Conference* ([GECCO'17](http://gecco-2017.sigevo.org/)) was accepted.

## A gentle introduction

The **ecr** package (version 2), *Evolutionary Computation in R*, is conceived as a "white-box" framework for single- and multi-objective optimization strongly inspired by the awesome [Evolutionary Computation (EC) framework DEAP](https://github.com/DEAP/deap) for the Python programming language. In contrast to black-box frameworks, which usually try to hide as much of internal complexity (e.g., data structures) in opaque high-level EC components, **ecr** makes the development of evolutionary algorithms (EA) - as DEAP does - transparent: the evolutionary loop is written by hand sticking to few conventions, utilizing few simple utility functions and controlling everything. We believe, that this is the most flexible way in evolutionary algorithm design. On top **ecr** ships with a black-box for *standard tasks*, e.g., optimization of a continuous function, as well. The core features of ecr are the following

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
```r
library(ecr)
library(ggplot2)
library(smoof)
fn = makeAckleyFunction(1L)
autoplot(fn, show.optimum=TRUE, length.out = 1000)
```

### Writing the evolutionary loop by hand

We decide to use an evolutionary (30 + 5)-strategy, i.e., an algorithm that keeps a population of size mu = 30, in each generation creates lambda = 5 offspring by variation and selects the best mu out of mu + lambda individuals to survive. First, we define some variables.
```r
MU = 30L; LAMBDA = 5L; MAX.ITER = 200L
lower = getLowerBoxConstraints(fn)
upper = getUpperBoxConstraints(fn)
```

In order to implement this algorithm the first step is to define a *control object*, which stores information on the objective function and a set of evolutionary operators.
```r
control = initECRControl(fn)
control = registerECROperator(control, "mutate", mutGauss, sdev = 2, lower = lower, upper = upper)
control = registerECROperator(control, "selectForSurvival", selGreedy)
```
Here, we decide to perform mutation only. The best mu individuals (regarding fitness values) are going to be selected to build up the next generation.

Finally, the evolutionary loop is implemented.
```r
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
```r
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
```r
install.packages("ecr")
```
If you are interested in trying out and playing around with the current github developer version use the [devtools](https://github.com/hadley/devtools) package and type the following command in R:

```r
devtools::install_github("jakobbossek/ecr2")
```

## Citation

To cite **ecr** please use:
> Bossek, J. (2017). Ecr 2.0: A Modular Framework for Evolutionary Computation
> in R. In Proceedings of the Genetic and Evolutionary Computation Conference
> (GECCO) Companion (pp. 1187–1193). Berlin, Germany:
> ACM. http://doi.org/10.1145/3067695.3082470

In case you made use of **ecr**'s performance assessment tools please cite:
> Bossek, J. (2018). Performance Assessment of Multi-objective Evolutionary
> Algorithms with the R Package Ecr. In Proceedings of the Genetic and >
> Evolutionary Computation Conference Companion (pp. 1350–1356). Kyoto, Japan:
> ACM. http://doi.org/10.1145/3205651.3208312

BibTeX entries for LaTeX users:
```
@inproceedings{Bossek2018PerformanceAssessment,
  author = {Bossek, Jakob},
  booktitle = {Proceedings of the Genetic and Evolutionary Computation Conference Companion},
  doi = {10.1145/3205651.3208312},
  pages = {1350--1356},
  publisher = {ACM},
  address = {Kyoto, Japan},
  series = {GECCO '18},
  title = {{Performance Assessment of Multi-objective Evolutionary Algorithms with the R Package Ecr}},
  url = {http://doi.acm.org/10.1145/3205651.3208312},
  year = {2018}
}
@inproceedings{B2017ecr,
  author = {Bossek, Jakob},
  booktitle = {Proceedings of the Genetic and Evolutionary Computation Conference (GECCO) Companion},
  doi = {10.1145/3067695.3082470},
  pages = {1187--1193},
  publisher = {ACM},
  series = {GECCO '18},
  address = {Berlin, Germany},
  title = {{Ecr 2.0: A Modular Framework for Evolutionary Computation in R}},
  url = {http://doi.acm.org/10.1145/3067695.3082470},
  year = {2017}
}
```

## Contact

Please address questions and missing features about the **ecr** to the author Jakob Bossek <j.bossek@gmail.com>. Found some nasty bugs? Please use the [issue tracker](https://github.com/jakobbossek/ecr2/issues) for this. Pay attention to explain the problem as good as possible. At its best you provide an example, so I can reproduce your problem quickly.



