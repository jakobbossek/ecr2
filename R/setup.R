#' @title
#' Set up parameters for evolutionary operator.
#'
#' @description
#' This function builds a simple wrapper around an evolutionary operator, i.e.,
#' mutator, recombinator or selector and defines its parameters. The result is a
#' function that does not longer depend on the parameters. E.g., \code{fun = setup(mutBitflip, p = 0.3)}
#' initializes a bitflip mutator with mutation probability 0.3. Thus,
#' the following calls have the same behaviour: \code{fun(c(1, 0, 0))} and
#' \code{mutBitflip(fun(c(1, 0, 0), p = 0.3)}.
#' Basically, this type of preinitialization is only neccessary if operators
#' with additional parameters shall be initialized in order to use the black-box
#' \code{\link{ecr}}.
#'
#' @param operator [\code{ecr_operator}]\cr
#'   Evolutionary operator.
#' @param ... [any]\cr
#'   Furhter parameters for \code{operator}.
#' @return [\code{function}] Wrapper evolutionary operator with parameters \code{x} and \code{...}.
#' @examples
#' # initialize bitflip mutator with p = 0.3
#' bf = setup(mutBitflip, p = 0.3)
#' # sample binary string
#' x = sample(c(0, 1), 100, replace = TRUE)
#'
#' set.seed(1)
#' # apply preinitialized function
#' print(bf(x))
#'
#' set.seed(1)
#' # apply raw function
#' print(mutBitflip(x, p = 0.3))
#'
#' # overwrite preinitialized values with mutate
#' ctrl = initECRControl(fitness.fun = function(x) sum(x), n.objectives = 1L)
#' # here we define a mutation probability of 0.3
#' ctrl = registerECROperator(ctrl, "mutate", setup(mutBitflip, p = 0.3))
#' # here we overwrite with 1, i.e., each bit is flipped
#' print(x)
#' print(mutate(ctrl, list(x), p.mut = 1, p = 1)[[1]])
#' @export
setup = function(operator, ...) {
  assertClass(operator, "ecr_operator")
  args = list(...)
  attrs = attributes(operator)
  fn = function(x, ...) {
    do.call(operator, c(list(x), BBmisc::insert(args, list(...))))
  }
  attributes(fn) = attrs
  return(fn)
}
