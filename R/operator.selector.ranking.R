#' @title
#' Rank Selection Operator
#'
#' @description
#' Rank-based selection preserves a constant selection pressure by sorting the 
#' population on the basis of fitness, and then allocating selection 
#' probabilities to individuals according to their rank, rather than according 
#' to their actual fitness values.
#' 
#' @references
#' Eiben, A. E., & Smith, J. E. (2007). Introduction to evolutionary computing. Berlin: Springer,
#' 60-61.
#' 
#' @template arg_fitness
#' @template arg_n_select
#' @param s [\code{numeric}] \cr 
#' selection pressure, the probability of an individual to be 
#' selected, usually s  (1.0 < s < 2.0)
#' @param scheme [\code{literal}] \cr 
#' mapping from rank number to selection probability, either
#' \dQuote{linear} or \dQuote{exponential}
#' @return [\code{setOfIndividuals}]
#' @family selectors
#' @export
selRanking = makeSelector(
  selector = function(fitness, n.select, s = 2, scheme = "linear") {
    
    # arguments ok?
    assertNumber(s, na.ok = FALSE, null.ok = FALSE, lower = 1, upper = 2, 
                 finite = TRUE, add = coll)
    assertChoice(scheme, c("linear","exponential"), null.ok = FALSE)
    mu = ncol(fitness)
    # rank individuals
    fitness = as.numeric(fitness)
    fitness.ordered = order(fitness,decreasing = TRUE)
    
    # calculate survival probability for each individual based on its rank
    prob = 0
    if (scheme == "linear" ){ # P(i)=((2-s)/mu)+(2i(s-1)/mu(mu-1))
      prob = sapply(fitness.ordered, function(x){
        ((2 - s) / mu) + (2 * (x - 1) * (s - 1) / (mu * (mu - 1)))
      })
    } else if (scheme == "exponential"){ # P(i)=(1-e^(-i))/c
      t = sapply(fitness.ordered, function(x){(1 - exp(-x))})
      c = sum(t)
      prob = t / c
    }
    
    idx = sample(seq_along(fitness), size = n.select, replace = TRUE, 
                 prob = prob)
    return(idx)
  },
  supported.objectives = c("single-objective")
  )
