#' @title
#' Generates the uniform generator object for the initial population.
#'
#' @description
#' The returned population contains individuals which are uniformly distributed
#' within the bounds specified.
#'
#' @param len [\code{integer(1)}]\cr
#'   Length of genotype.
#' @param lower [\code{numeric}]\cr
#'   Vector of lower box constraints for each dimension.
#'   If a single value is passed this value is taken for each dimension.
#' @param upper [\code{numeric}]\cr
#'   Vector of lower box constraints for each dimension.
#'   If a single value is passed this value is taken for each dimension.
#' @return [\code{ecr_generator}]
#' @export
setupUniformGenerator = function(len, lower, upper) {
  assertInt(len)
  assertNumeric(lower)
  assertNumeric(upper)
  if (length(lower) == 1L)
    lower = rep(lower, len)
  if (length(upper) == 1L)
    upper = rep(upper, len)
  force(len)
  force(lower)
  force(upper)

  generateUniformPopulation = function(size, par.list) {
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
