#' @title
#' Polynomial mutation.
#'
#' @description
#' Performs an polynomial mutation as used in the SMS-EMOA algorithm.
#' Polynomial mutation tries to simulate the distribution of the offspring of 
#' binary-encoded bit flip mutations based on real-valued decision variables. 
#' Polynomial mutation favors offspring nearer to the parent. 
#' 
#' @references
#' [1] Deb, Kalyanmoy & Goyal, Mayank. (1999). A Combined Genetic Adaptive 
#' Search (GeneAS) for Engineering Design. Computer Science and Informatics. 26.
#' Retrieved from http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.27.767&rep=rep1&type=pdf
#'
#' @param ind [\code{numeric}]\cr
#'   Numeric vector / individual to mutate.
#' @param p [\code{numeric(1)}]\cr
#'   Probability of mutation for each gene of an offspring. In other words, 
#'   the probability that the value (allele) of a given gene will change.
#'   Default is 0.2
#' @param eta [\code{numeric(1)}\cr
#'   Distance parameter to control the shape of the mutation distribution. 
#'   Larger values generate offspring closer to the parents.
#'   Default is 10.
#' @param lower [\code{numeric}]\cr
#'   Vector of minimal values for each parameter of the decision space.
#'   Must have the same length as \code{ind}.
#' @param upper [\code{numeric}]\cr
#'   Vector of maximal values for each parameter of the decision space.
#'   Must have the same length as \code{ind}.
#' @return [\code{numeric}]
#' @family mutators
#' @export
mutPolynomial = makeMutator(
  mutator = function(ind, p = 0.2, eta = 10, lower, upper) {
    checkNumericMutatorArguments(ind, lower, upper, "mutPolynomial")
    checkmate::assertNumber(p, lower = 0, upper = 1, na.ok = FALSE)
    checkmate::assertNumber(eta, lower = 1, finite = TRUE, na.ok = FALSE)

    child = .Call("polynomialMutationC", ind, as.numeric(lower), as.numeric(upper), p, eta)
    return(child)
  },
  supported = "float")
