#' @title Constructor for EMOA indicators.
#'
#' @description Simple wrapper for function which compute
#' performance indicators for multi-objective stochastic algorithm.
#' Basically this function appends some meta information to the
#' passed function \code{fun}.
#'
#' @param fun [\code{function(points, ...)}]\cr
#'   Function which expects a numeric matrix \dQuote{points} as first argument.
#'   Optional named arguments (often \dQuote{ref.point} for a reference
#'   point or \dQuote{ref.points} for a reference set, e.g., the true Pareto-front)
#'   are allowed. See implementations of existing indicators for examples.
#' @param minimize [\code{logical(1)}]\cr
#'   Lower values indicate better performance?
#' @param name [\code{character(1)}]\cr
#'   Short name of the indicator. Used, e.g., as column name for the indicator
#'   in the data.frame returned by \code{\link{computeIndicators}}.
#' @param latex.name [\code{character(1)}]\cr
#'   LaTeX representation of the indicator. Used in LaTeX-table output statistical
#'   tests (see \code{\link{toLatex}}).
#' @return [\code{function(points, ...)}] Argument \code{fun} with all other
#'   arguments appended.
#' @family EMOA performance assessment tools
#' @export
makeEMOAIndicator = function(
  fun,
  minimize,
  #type,
  name,
  latex.name) {
  assertFunction(fun, args = "points")
  assertFlag(minimize)
  #assertChoice(type, choices = c("binary", "unary"))
  assertString(name)
  assertString(latex.name)

  fun = BBmisc::setAttribute(fun, "minimize", minimize)
  #fun = BBmisc::setAttribute(fun, "type", type)
  fun = BBmisc::setAttribute(fun, "name", name)
  fun = BBmisc::setAttribute(fun, "latex.name", latex.name)
  fun = BBmisc::addClasses(fun, "ecr_emoa_indicator")
  return(fun)
}
