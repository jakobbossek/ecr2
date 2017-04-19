#' @title
#' Indermediate recombinator.
#'
#' @description
#' Intermediate recombination computes the component-wise mean value of the
#' \code{k} given parents. It is applicable only for float representation.
#'
#' @param inds [\code{list}]\cr
#'   Parents, i.e., list of exactly two numeric vectors of equal length.
#' @return [\code{numeric}] Single offspring.
#' @family recombinators
#' @export
recIntermediate = makeRecombinator(
  recombinator = function(inds) {
    assertList(inds, len = 2L, any.missing = FALSE, all.missing = FALSE)
    n = length(inds[[1L]])
    child = rep(0, n)
    for (i in 1:length(inds)) {
      child = child + inds[[i]]
    }
    return(child / length(inds))
  },
  supported = "float",
  n.parents = 2,
  n.children = 1L)
