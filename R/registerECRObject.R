#' @title
#' Register operators to control object.
#'
#' @description In ecr the control object stores information on the fitness
#' function and serves as a storage for evolutionary components used by your evluationary
#' algorithm. This function handles the registration process.
#'
#' @param control [\code{ecr_control}]\cr
#'   Control object.
#' @param slot [\code{character(1)}]\cr
#'   Name of the field in the control object where to store the operator.
#' @param fun [\code{function}]\cr
#'   Actual operator. In order to use the various helper functions of ecr one needs
#'   to stick to a simple convention: The first argument of \code{function} should
#'   be the individual to mutate, a list of individuals for recombination or a matrix
#'   of fitness values for recombination. If one does not want to use the corresponding
#'   helpers, e.g., \code{mutate}, the signature of the function does not matter. However,
#'   in this case you are responsable to pass arguments correctly.
#' @param ... [any]\cr
#'   Further arguments for \code{fun}. These arguments are stored in the control object
#'   and passed on to \code{fun}.
#' @return [\code{ecr_control}]
#' @export
registerECROperator = function(control, slot, fun, ...) {
  assertClass(control, "ecr_control")
  assertString(slot)
  assertFunction(fun)
  fun.pars = list(...)
  if (!is.null(control[[slot]]))
    stopf("Operator '%s' already present.", slot)
  control[[paste0(slot, ".pars")]] = fun.pars
  control[[slot]] = fun
  return(control)
}
