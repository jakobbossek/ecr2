#' @title
#' Generator of the \dQuote{null} recombination operator.
#'
#' @description
#' This recombinator does not perform any recombination. It simply returns the
#' first of the passed parents.
#'
#' @return [\code{ecr_recombinator}]
#' @family recombinators
#' @export
setupNullRecombinator = function() {
  recombinator = function(inds, task, control) {
    return(inds[[1L]])
  }

  makeRecombinator(
    recombinator = recombinator,
    name = "NULL recombinator",
    description = "Does not perform any recombination.",
    supported = getAvailableRepresentations(),
    n.parents = 10L,
    n.children = 1L
  )
}
