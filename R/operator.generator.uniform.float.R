#' @title
#' Generates the uniform generator object for the initial population.
#'
#' @description
#' The returned population contains individuals which are uniformly distributed
#' within the bounds specified by the paramter set of the \pkg{smoof} objective
#' function passed to the \code{\link{doTheEvolution}} function.
#'
#' @return [\code{ecr_generator}]
#' @export
setupUniformGenerator = function() {
  generateUniformPopulation = function(size, task, control) {
    par.set = task$par.set
    if (!isNumeric(par.set)) {
      stopf("Uniform generator needs a numeric parameter set.")
    }
    if (!hasFiniteBoxConstraints(par.set)) {
      stopf("Uniform generator needs box constraints.")
    }

    lower = getLower(par.set)
    upper = getUpper(par.set)

    # create one gene
    createGene = function(constr) {
      mapply(runif, n = 1L, min = constr$low, max = constr$up)
    }
    # is one gene an individual or is a set of genes an individual
    n.params = getParamNr(par.set)
    if (n.params == 1L) {
      constraints = list(low = lower, up = upper)
      createInd = createGene
    } else {
      ids = getParamIds(par.set)
      constraints = vector("list", n.params)
      names(constraints) = ids
      for (i.param in seq(n.params)) {
        constraints[[i.param]] = list(
          low = lower[names(lower) == ids[i.param]],
          up = upper[names(upper) == ids[i.param]]
        )
      }
      # create individual from genes
      createInd = function(constr) {
        lapply(constr, createGene)
      }
    }

    population = lapply(seq(size), function(x) createInd(constraints))
    makePopulation(population)
  }

  makeGenerator(
    generator = generateUniformPopulation,
    name = "Uniform generator",
    description = "Samples uniformally distributed points in the design space.",
    supported = "float"
  )
}
