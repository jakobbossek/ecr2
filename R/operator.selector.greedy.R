#' @title
#' Simple \dQuote{greedy} selector.
#'
#' @description
#' Sorts the individuals according to their fitness value in increasing order
#' and selects the best ones.
#'
#' @return [\code{setOfIndividuals}]
#' @family selectors
#' @export
setupGreedySelector = function() {
  selector = function(fitness, n.select, task, control, opt.state) {
    fitness = as.numeric(fitness)
    idx = order(fitness)[seq(n.select)]
    return(idx)
  }
  makeSelector(
    selector = selector,
    name = "Greedy selector",
    description = "Return the best individuals regarding the fitness value.",
    supported.objectives = c("single-objective")
  )
}
