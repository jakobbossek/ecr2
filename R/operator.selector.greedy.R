#' @title
#' Simple selector.
#'
#' @description
#' Sorts the individuals according to their fitness value in increasing order
#' and selects the best ones.
#'
#' @template arg_fitness
#' @template arg_n_select
#' @return [\code{integer}] Vector of survivor indizes.
#' @family selectors
#' @export
selGreedy = makeSelector(
  selector = function(fitness, n.select) {
    if (n.select > ncol(fitness))
      stopf("selGreedy: Selection of %i individuals required, but only %i available.
        Probably, you apply a (mu, lambda) strategy with mu lambda < mu",
        n.select, ncol(fitness))
    fitness = as.numeric(fitness)
    idx = order(fitness)[seq(n.select)]
    return(idx)
  },
  supported.objectives = "single-objective")
