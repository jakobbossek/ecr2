#' @title
#' Creates an optimization task.
#'
#' @description
#' An optimization task consists of the fitness/objective function, the
#' number of objectives, the \dQuote{direction} of optimization, i.e.,
#' which objectives should be minimized/maximized and the names of the
#' objectives.
#'
#' @param fun [\code{function} | \code{smoof_function}]\cr
#'   Fitness/objective function.
#' @param n.objectives [\code{integer(1)}]\cr
#'   Number of objectives. This must be a positive integer value unless \code{fun}
#'   is of type \code{smoof_function}.
#' @param minimize [\code{logical}]\cr
#'   A logical vector indicating which objectives to minimize/maximize. By default
#'   all objectives are assumed to be minimized.
#' @param objective.names [\code{character}]\cr
#'   Names for the objectuves.
#'   Default is \code{NULL}. In this case the names are set to y1, ..., yn with
#'   n equal to \code{n.objectives} and simply y in the single-objective case.
#' @return [\code{ecr_optimization_task}]
#' @export
makeOptimizationTask = function(fun, n.objectives = NULL, minimize = NULL, objective.names = NULL) {
  assertFunction(fun)
  if (isSmoofFunction(fun)) {
    if (is.null(n.objectives)) {
      n.objectives = getNumberOfObjectives(fun)
    } else {
      if (n.objectives != getNumberOfObjectives(fun)) {
        stopf("Something went wrong! You passed a smoof function with %i objectives,
          but set the number of objectives in the task to %i manually.",
          getNumberOfObjectives(fun), n.objectives
        )
      }
    }
    par.set = getParamSet(fun)
    if (hasRequires(par.set) || hasForbidden(par.set)) {
      warningf("The build-in ecr operators do not care about requirements or forbidden regions,
        but the passed smoof function '%s' exhibits these.", getName(fun))
    }
  }
  assertInt(n.objectives, lower = 1L, na.ok = FALSE)

  if (is.null(objective.names)) {
    objective.names = if (n.objectives == 1L) "y" else paste0("y", seq(n.objectives))
  }

  if (is.null(minimize)) {
    if (isSmoofFunction(fun)) {
      minimize = shouldBeMinimized(fun)
    } else {
      minimize = rep(TRUE, n.objectives)
    }
  } else if (length(minimize) == 1L) {
    minimize = rep(minimize, n.objectives)
  }
  assertLogical(minimize, any.missing = FALSE)
  assertCharacter(objective.names, len = n.objectives, any.missing = FALSE, all.missing = FALSE)

  if (n.objectives != length(minimize)) {
    stopf("Number of objectives does not correspond to the length of the minimize argument.")
  }

  # if (n.objectives >= 2L && any(!minimize)) {
  #   stopf("At the moment in many-objective optimization ecr needs all objectives to be minimized,
  #     but %i objectives shall be maximized. Consider a transformation of you objective function.",
  #     sum(!minimize)
  #   )
  # }

  task = makeS3Obj(
    fitness.fun = fun,
    n.objectives = n.objectives,
    minimize = minimize,
    objective.names = objective.names,
    classes = c("ecr_optimization_task")
  )

  if (isSmoofFunction(fun)) {
    task$par.set = getParamSet(fun)
  } else {
    # dummy parameter set if the passed function if not of type smoof
    task$par.set = makeParamSet(makeCharacterParam("serialized"))
  }

  return(task)
}

#' @export
print.ecr_optimization_task = function(x, ...) {
  catf("[ecr OPTIMIZATION TASK]")
  n.obj = "Single-objective"
  if (x$n.objectives == 2L) {
    n.obj = "Bi-objective"
  } else if (x$n.objectives >= 3L) {
    n.obj = "Many-objective"
  }
  catf("Objective type: %s", n.obj)
  catf("(min: %i, max: %i)", sum(x$minimize), sum(!x$minimize))
}
