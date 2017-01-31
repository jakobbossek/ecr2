#' @title
#' Generates a generator object for the initial population.
#'
#' @description
#' The generated operator samples uniformally distributed points in the design
#' space of the target function taking care not to violate box constraints.
#'
#' @param len [\code{integer(1)}]\cr
#'   Length of genotype.
#' @return [\code{ecr_generator}]
#' @export
setupBinaryGenerator = function(len) {
  len = asInt(len)
  force(len)
  generateBinaryPopulation = function(size, par.list) {
    lapply(seq_len(size), function(i) {
      sample(c(0, 1), size = len, replace = TRUE)
    })
  }

  makeGenerator(
    generator = generateBinaryPopulation,
    name = "Binary generator",
    description = "Samples uniformally distributed 0, 1 values.",
    supported = "binary"
  )
}
