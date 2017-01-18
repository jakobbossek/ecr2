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
  selector = function(fitness, n.select, task, control, opt.state) {
    return(1:ncol(fitness))
  }
  makeSelector(
    selector = selector,
    name = "Simple selector",
    description = "Simply returns the entire population.",
    supported.objectives = c("single-objective", "multi-objective")
  )
}
