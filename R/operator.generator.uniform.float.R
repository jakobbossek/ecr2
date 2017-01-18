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
setupUniformGenerator = function(len, lower, upper) {
  assertInt(len)
  assertNumeric(lower)
  assertNumeric(upper)
  force(len)
  force(lower)
  force(upper)

  generateUniformPopulation = function(size) {
    lapply(seq_len(size), function(i) {
      ind = sapply(seq_len(len), function(j) {
        runif(1L, min = lower[j], max = upper[j])
      })
      return(ind)
    })
  }

  makeGenerator(
    generator = generateUniformPopulation,
    name = "Uniform generator",
    description = "Samples uniformally distributed points in the design space.",
    supported = "float"
  )
}
