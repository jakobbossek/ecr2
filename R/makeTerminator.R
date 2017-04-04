#' @title
#' Generate stopping condition.
#'
#' @description Wrap a function within a stopping condition object.
#'
#' @param condition.fun [\code{function}]\cr
#'   Function which takes a logger object \code{log} (see \code{\link{initLogger}})
#'   and returns a single logical.
#' @param name [\code{character(1)}]\cr
#'   Identifier for the stopping condition.
#' @param message [\code{character(1)}]\cr
#'   Message which should be stored in the termination object, if the stopping
#'   condition is met.
#' @return [\code{ecr_terminator}]
#' @export
makeTerminator = function(condition.fun, name, message) {
  assertFunction(condition.fun, args = "log")
  assertCharacter(name, len = 1L, any.missing = FALSE)
  assertCharacter(message, len = 1L, any.missing = FALSE)

  condition.fun = setAttribute(condition.fun, "name", name)
  condition.fun = setAttribute(condition.fun, "message", message)
  condition.fun = addClasses(condition.fun, "ecr_terminator")
  return(condition.fun)
}
