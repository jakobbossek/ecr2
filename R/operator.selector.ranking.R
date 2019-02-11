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
#' Eiben, A. E., & Smith, J. E. (2007). Introduction to evolutionary computing.
#' Berlin: Springer.
#'
#' @template arg_fitness
#' @template arg_n_select
#' @param s [\code{numeric(1)}]\cr
#'   Selection pressure for linear ranking scheme with value range \eqn{[0,1]}.
#'   Ignored if \code{scheme} is set to \dQuote{exponential}.
#'   Default is 1.5.
#' @param scheme [\code{character(1)}]\cr
#'   Mapping from rank number to selection probability, either
#'   \dQuote{linear} or \dQuote{exponential}.
#' @return [\code{setOfIndividuals}]
#' @family selectors
#' @export
selRanking = makeSelector(
  selector = function(fitness, n.select, s = 1.5, scheme = "linear") {

    # arguments ok?
    checkmate::assertNumber(s, lower = 1, upper = 2, finite = TRUE)
    checkmate::assertChoice(scheme, c("linear","exponential"))

    mu = ncol(fitness)

    # rank individuals
    fitness = as.numeric(fitness)
    ranks = mu - rank(fitness, ties.method = "random")

    doLinearScheme = function(x, s, mu) {
      a = (2 - s) / mu
      b = (2 * (s - 1)) / (mu * (mu - 1))
      return(a + x * b)
    }

    doExponentialScheme = function(x) {
      ts = 1 - exp(-x)
      return(ts / sum(ts))
    }

    # calculate survival probability for each individual based on its rank
    probs = if (scheme == "linear") { # P(i)=((2-s)/mu)+(2i(s-1)/mu(mu-1))
      doLinearScheme(ranks, s, mu)
    } else if (scheme == "exponential"){ # P(i)=(1-e^(-i))/c
      doExponentialScheme(ranks)
    }

    # print("---")
    # print(fitness)
    # print(probs)
    # BBmisc::pause()

    idx = sample(seq_along(fitness), size = n.select, replace = TRUE, prob = probs)
    return(idx)
  },
  supported.objectives = "single-objective"
)
