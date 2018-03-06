# ecr 2.1.1 (upcoming)

## Fixes

* Fixed: bug in replaceMuCommaLambda if n.elite was not set by hand
* Fixed: selGreedy throws error if n.select is larger than the number of individuals passed (this led to hard-to-detect errors in (mu, lambda) strategies and white-box approach)

## New Features

* Added terminator stopOnMaxTime

# ecr 2.1.0

## New Features

* uniform crossover recombinator recUnifCrossover
* parameter log.stats for ecr function (possibility to define fitness statistics to be logged)
* logical log.pop argument for ecr function (should each population be saved in the logger?)
* possibility to store additional stuff in logger via 'extras'
* terminators stopOnEvals and stopOnOptY

# ecr 2.0.0

* First submission of ecr 2 to CRAN.
* Almost everything changed.
