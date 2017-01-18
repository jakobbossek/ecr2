#' @title
#' Generates a generator object for the initial population.
#'
#' @description
#' The generated operator samples uniformally distributed points in the design
#' space of the target function taking care not to violate box constraints.
#'
#' @return [\code{ecr_generator}]
#' @export
setupBinaryGenerator = function() {
  generateBinaryPopulation = function(size, task, control) {
    par.set = task$par.set

    if (getParamNr(par.set) == 1L) {
      # one vector is an individual
      createInd = function(param.length) {
        sample(c(0, 1), size = param.length, replace = TRUE)
      }
    } else {
      # a list of vectors is an individual
      createInd = function(param.length) {
        lapply(param.length, function(x) sample(c(0, 1), size = x, replace = TRUE))
      }
    }

    # create population list
    population = lapply(seq(size), function(x) createInd(getParamLengths(par.set)))
    makePopulation(population)
  }

  makeGenerator(
    generator = generateBinaryPopulation,
    name = "Binary generator",
    description = "Samples uniformally distributed 0, 1 values.",
    supported = c("binary")
  )
}
