#' @title
#' Generator of the indermediate recombination operator.
#'
#' @description
#' Intermediate recombination computes the component-wise mean value of the
#' \code{k} given parents. It is applicable only for float representation.
#'
#' @param k [integer(1)]\cr
#'   Number of parents required for mating. Default is \code{2}.
#' @return [\code{ecr_recombinator}]
#' @family recombinators
#' @export
setupIntermediateRecombinator = function(k = 2L) {
  assertInt(k, na.ok = FALSE, lower = 2L)

  force(k)

  recombinator = function(inds, par.list) {
    assertList(inds, len = k, any.missing = FALSE, all.missing = FALSE)
    n = length(inds[[1L]])
    child = rep(0, n)
    for (i in 1:length(inds)) {
      child = child + inds[[i]]
    }
    return(child / length(inds))
  }

  makeRecombinator(
    recombinator = recombinator,
    supported = "float",
    n.parents = k,
    n.children = 1L
  )
}
