# ecr: Evolutionary Computing in R

[![CRAN Status Badge](http://www.r-pkg.org/badges/version/ecr2)](http://cran.r-project.org/web/packages/ecr2)
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/ecr2)](http://cran.rstudio.com/web/packages/ecr2/index.html)
[![Build Status](https://travis-ci.org/jakobbossek/ecr2.svg?branch=master)](https://travis-ci.org/jakobbossek/ecr2)
[![Build status](https://ci.appveyor.com/api/projects/status/eu0nns2dsgocwntw/branch/master?svg=true)](https://ci.appveyor.com/project/jakobbossek/ecr2/branch/master)
[![Coverage Status](https://coveralls.io/repos/jakobbossek/ecr2/badge.svg)](https://coveralls.io/r/jakobbossek/ecr2)

The **ecr2** package is the official follow-up package to my package [ecr](https://github.com/jakobbossek/ecr). I was unsatisfied with some design choices and thus decided to restructure and rewrite a lot. Changes are that manifold and fundamental, that I decided to set up a new package, since most of the ecr function are either deprecated, renamed or underlie substantial interface changes.

## Installation Instructions

The package will be available at [CRAN](http://cran.r-project.org) soon. Install the release version via:
```splus
install.packages("ecr2")
```
If you are interested in trying out and playing around with the current github developer version use the [devtools](https://github.com/hadley/devtools) package and type the following command in R:

```splus
devtools::install_github("jakobbossek/ecr2")
```

## Contact

Please address questions and missing features about the **ecr2 package** to the author Jakob Bossek <j.bossek@gmail.com>. Found some nasty bugs? Please use the [issue tracker](https://github.com/jakobbossek/ecr2/issues) for this. Pay attention to explain the problem as good as possible. At its best you provide an example, so I can reproduce your problem quickly.



