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
  selector = function(fitness, n.select, par.list = list()) {
    fitness = as.numeric(fitness)
    idx = order(fitness)[seq(n.select)]
    return(idx)
  }
  makeSelector(
    selector = selector,
    supported.objectives = "single-objective"
  )
}
