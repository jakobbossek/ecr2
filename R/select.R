#' @title
#' Select individuals.
#'
#' @description This utility functions expect a control object, a matrix of
#' fitness values - each column containing the fitness value(s) of one individual -
#' and the number of individuals to select.
#' The corresponding selector, i.e., mating selector for \code{selectForMating}
#' or survival selector for \code{selectForSurvival} is than called internally
#' and a vector of indizes of selected individuals is returned.
#'
#' @details Both functions check the optimization directions stored in the task
#' inside the control object, i.e., whether to minimize or maximize each objective,
#' and transparently prepare/transform the \code{fitness} matrix for the selector.
#'
#' @template arg_control
#' @template arg_fitness
#' @param n.select [\code{integer(1)}]\cr
#'   Number of individuals to select.
#' @return [\code{integer}] Integer vector with the indizes of selected individuals.
#' @rdname select
#' @name select
#' @export
selectForMating = function(control, fitness, n.select) {
  assertClass(control, "ecr_control")
  assertClass(control$selectForMating, "ecr_selector")
  assertMatrix(fitness, min.rows = 1L, any.missing = FALSE, all.missing = FALSE)
  n.select = asInt(n.select, lower = 1L)
  checkIfSelectorMatchesObjectives(control$selectForMating, control, "selectForMating")
  fitness = transformFitness(fitness, control$task, control$selectForMating)
  control$selectForMating(fitness, n.select = n.select)
}

#' @rdname select
#' @export
selectForSurvival = function(control, fitness, n.select) {
  assertClass(control, "ecr_control")
  assertClass(control$selectForSurvival, "ecr_selector")
  assertMatrix(fitness, min.rows = 1L, any.missing = FALSE, all.missing = FALSE)
  n.select = asInt(n.select, lower = 1L)
  checkIfSelectorMatchesObjectives(control$selectForSurvival, control, "selectForSurvival")
  fitness = transformFitness(fitness, control$task, control$selectForSurvival)
  control$selectForSurvival(fitness, n.select = n.select)
}

checkIfSelectorMatchesObjectives = function(selector, control, type) {
  sup.obj = attr(selector, "supported.objectives")
  n.objectives = control$task$n.objectives
  if ((n.objectives == 1L & !("single-objective" %in% sup.obj)) | (n.objectives >= 2L & !("multi-objective" %in% sup.obj))) {
    BBmisc::stopf("[%s] Selector supports %s optimization, but the optimization problem has %i objective(s).",
      type, BBmisc::collapse(sup.obj, sep = " and "), n.objectives)
  }
}
