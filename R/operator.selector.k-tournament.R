#' @title
#' k-Tournament selector.
#'
#' @description
#' k individuals from the population are chosen randomly and the best one is
#' selected to be included into the mating pool. This process is repeated until
#' the desired number of individuals for the mating pool is reached.
#'
#' @template arg_fitness
#' @template arg_n_select
#' @param k [\code{integer(1)}]\cr
#'   Number of individuals to participate in each tournament. Default is \code{2L}.
#' @return [\code{integer}] Vector of survivor indizes.
#' @family selectors
#' @export
selTournament = makeSelector(
  selector = function(fitness, n.select, k = 3L) {
    assertInt(k, na.ok = FALSE, lower = 2L)
    fitness = as.numeric(fitness)
    pop.idx = seq_along(fitness)
    idx = integer(n.select)
    for (i in seq(n.select)) {
      # choose k individuals at random ...
      competitor.idx = sample(pop.idx, size = k, replace = TRUE)
      # ... and store the best
      idx[i] = competitor.idx[which.min(fitness[competitor.idx])]
    }
    return(idx)
  },
  supported.objectives = "single-objective")
