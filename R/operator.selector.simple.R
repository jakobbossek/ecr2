#' @title
#' Simple (naive) selector.
#'
#' @description
#' Just for testing. Actually does not really select, but instead returns a random
#' sample of \code{ncol(fitness)} indizes.
#'
#' @template arg_fitness
#' @template arg_n_select
#' @return [\code{setOfIndividuals}]
#' @family selectors
#' @export
selSimple = makeSelector(
  selector = function(fitness, n.select) {
    return(sample(1:ncol(fitness), size = n.select, replace = TRUE))
  },
  supported.objectives = c("single-objective", "multi-objective"))
