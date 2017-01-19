#' @title
#' Simple (naive) mating pool generator.
#'
#' @description
#' Just for testing. Actually does not really select, but instead returns the
#' entire population to form the mating pool.
#'
#' @return [\code{setOfIndividuals}]
#' @family selectors
#' @export
setupSimpleSelector = function() {
  selector = function(fitness, n.select) {
    return(sample(1:ncol(fitness), size = n.select, replace = TRUE))
  }
  makeSelector(
    selector = selector,
    name = "Simple selector",
    description = "Samples uniformly.",
    supported.objectives = c("single-objective", "multi-objective")
  )
}
