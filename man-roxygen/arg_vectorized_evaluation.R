#' @param vectorized.evaluation [\code{logical(1L)}]\cr
#'   Is the fitness/objective function vectorized? I.e., does the fitness function accept
#'   a list? This allows for faster execution or parallelization by hand.
#'   If \code{TRUE} the following destinction on the type of the objective function is made:
#'   \describe{
#'     \item{Is \code{smoof_function}}{If the objective function is of type \code{smoof_function} from package \pkg{smoof}
#'     and the smoof function is vectorized, the population - which is a list internally -
#'     is reduced to a matrix and passed to the smoof function (vectorization in smoof
#'     is allowed for continuous functions only).}
#'     \item{Is not a \code{smoof_function}}{In this case the individuals of
#'     the population are passed entirely as a list to the objective function.}
#'   }
#'   Default is \code{FALSE}.
