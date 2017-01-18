#' @title
#' Construct a selection operator.
#'
#' @description
#' Helper function which defines a selector method, i. e., an operator which
#' takes the population and returns a part of it for mating or survival.
#'
#' @param selector [\code{function}]\cr
#'   Actual selection operator.
#' @param name [\code{character(1)}]\cr
#'   Name of the selector.
#' @param description [\code{character(1)}]\cr
#'   Short description of how the selector works.
#' @param supported [\code{character}]\cr
#'   Vector of strings/names of supported parameter representations. For example
#'   'permutation', 'float', 'binary'.
#' @param supported.objectives [\code{character}]\cr
#'   At least one of \dQuote{single-objective} or \dQuote{multi-objective}.
#' @param supported.opt.direction [\code{character(1-2)}]\cr
#'   Does the selector work for maximization tasks xor minimization tasks or both?
#'   Default is \dQuote{\code{minimize}}, which means that the selector selects
#'   in favour of low fitness values.
#' @return [\code{ecr_selector}]
#'   Selector object.
#' @export
makeSelector = function(
  selector,
  name, description,
  supported = getAvailableRepresentations(),
  supported.objectives,
  supported.opt.direction = "minimize") {
  assertFunction(selector, args = c("fitness", "n.select", "task", "control", "opt.state"), ordered = TRUE)
  assertSubset(supported.objectives, c("single-objective", "multi-objective"))
  assertChoice(supported.opt.direction, choices = c("maximize", "minimize"))
  selector = makeOperator(selector, name, description, supported)
  selector = setAttribute(selector, "supported.objectives", supported.objectives)
  selector = setAttribute(selector, "supported.opt.direction", supported.opt.direction)
  selector = addClasses(selector, c("ecr_selector"))
  return(selector)
}
