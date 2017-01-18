#' @title
#' Generates a generator object for the initial population.
#'
#' @description
#' This generator generates a population of permutations.
#'
#' @param len [\code{integer(1)}]\cr
#'   Genotype length.
#' @param set [\code{vector}]\cr
#'   Permutations will be generated from this set.
#'   Default is \code{1:len}.
#' @return [\code{ecr2_operator}]
#' @export
setupPermutationGenerator = function(len, set = seq_len(len)) {
  len = asInt(len, lower = 2L)
  if (!testVector(set))
    stopf("set needs to be a vector or list of atomic elements")
  if (!length(set) == len)
    stopf("Length needs to be equal to the set.")
  force(len)
  force(set)

  generatePermutationPopulation = function(size) {
    lapply(seq_len(size), function(i) {
      unlist(sample(set))
    })
  }

  makeGenerator(
    generator = generatePermutationPopulation,
    name = "Permutation generator",
    description = "Generates random permutations.",
    supported = "permutation"
  )
}
