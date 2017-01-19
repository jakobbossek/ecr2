#' Initialize control object.
#'
#' @param fitness.fun [\code{function} | \code{\link[smoof]{smoof_function}}]\cr
#'   Fitness function. In case of calling \code{initECRControlFloat} with a \code{\link[smoof]{smoof_function}}
#'   parameters \code{n.objectives}, \code{n.dim} and \code{minimize} are extracted
#'   automatically and to not need to be passed by hand.
#' @param n.objectives [\code{integer(1)}]\cr
#'   Number of objectives of \code{fitness.fun}.
#' @param minimize [\code{logical(n.objectives)}]\cr
#'   Logical vector with ith entry \code{TRUE} if the ith objective of \code{fitness.fun}
#'   shall be minimized. If a single logical is passed, it is assumed to be valid
#'   for each objective.
#' @param lower [\code{integer}]\cr
#'   Lower box constraints for each decision variable. Single values are assumed to
#'   be valid for each dimension.
#' @param upper [\code{integer}]\cr
#'   Upper box constraints for each decision variable. Single values are assumed to
#'   be valid for each dimension.
#' @param n.dim [\code{integer(1)}]\cr
#'   Integer specifying the number of bits used in binary representation and the input
#'   dimension of real-valued representations.
#' @param perm [\code{integer(1)} | \code{vector}]\cr
#'   Either a single integer number. In this case the permutation is assumed to be \code{1:perm}.
#'   Alternatively, a set, i.e., a vector of elements can be passed which should form each
#'   individual.
#' @return [\code{ecr2_control}]
#' @name initECRControl
#' @rdname initECRControl
#' @export
initECRControl = function(fitness.fun, n.objectives = NULL, minimize = NULL) {
  task = makeOptimizationTask(fitness.fun, n.objectives = n.objectives, minimize = minimize)
  makeS3Obj("ecr2_control", task = task)
}

# guess type from params
# lower, upper -> float
# set = {0, 1} -> binary
# set = set of unique values -> perm
# everything missing -> custom
# if smoof fitness fun is passed, extract all stuff from it
# initControlFloat
# initControlBinary
# initControlPermutation
# initControlCustom

#' @rdname initECRControl
#' @export
initECRControlBinary = function(fitness.fun, n.bits = NULL, n.objectives = NULL, minimize = NULL) {
  control = initECRControl(fitness.fun, n.objectives = n.objectives, minimize = minimize)
  control$n.bits = asInt(n.bits, lower = 2L)
  control = addClasses(control, "ecr2_control_binary")
  return(control)
}

#' @rdname initECRControl
#' @export
initECRControlPermutation = function(fitness.fun, perm = NULL, n.objectives = NULL, minimize = NULL) {
  control = initECRControl(fitness.fun, n.objectives = n.objectives, minimize = minimize)
  if (length(perm) == 1L)
    perm = 1:perm
  assertSetEqual(perm, unique(perm))
  control$perm = perm
  control = addClasses(control, "ecr2_control_permutation")
  return(control)
}

#' @rdname initECRControl
#' @export
initECRControlFloat = function(fitness.fun, lower = NULL, upper = NULL,
  n.objectives = NULL, minimize = NULL, n.dim = NULL) {
  assertFunction(fitness.fun)

  passed.pars = list(lower = lower, upper = upper, n.objectives = n.objectives, minimize = minimize, n.dim = n.dim)
  extracted.pars = extractFunctionParameters(fitness.fun)
  final.pars = BBmisc::insert(passed.pars, extracted.pars)

  if (is.null(final.pars$lower) | is.null(final.pars$upper)) {
    stopf("You need to pass both lower and upper box constraints.")
  }
  if (is.null(final.pars$n.dim))
    stopf("You need to pass n.dim.")
  n.dim = asInt(final.pars$n.dim, lower = 1L)
  if (is.null(final.pars$n.objectives))
    stopf("You need to pass n.objectives.")
  n.objectives = asInt(final.pars$n.objectives, lower = 1L)
  minimize = final.pars$minimize
  if (is.null(minimize)) {
    minimize = rep(TRUE, n.objectives)
  }
  if (length(minimize) == 1L & n.objectives != 1L) {
    minimize = rep(minimize, n.objectives)
  }
  lower = final.pars$lower
  if (length(lower) != n.dim) {
    if (length(lower) != 1L) {
      stopf("Lower box constraints need to have length 1 or equal to n.dim.")
    }
    lower = rep(lower, n.dim)
  }
  upper = final.pars$upper
  if (length(upper) != n.dim) {
    if (length(upper) != 1L) {
      stopf("Upper box constraings need to have length 1 or equal to n.dim.")
    }
    upper = rep(upper, n.dim)
  }
  assertNumeric(lower, len = n.dim, any.missing = FALSE, all.missing = FALSE)
  assertNumeric(upper, len = n.dim, any.missing = FALSE, all.missing = FALSE)
  control = initECRControl(fitness.fun, n.objectives = n.objectives, minimize = minimize)
  control$n.dim = n.dim
  # we unname here since named vectors cause pmin/pmax to be much slower!!!
  # but we need this a lot in real-valued optimization
  control$lower = unname(lower)
  control$upper = unname(upper)
  control = addClasses(control, "ecr2_control_float")
  return(control)
}

#FIXME: converter soobench -> params
#FIXME: converter moobench -> params

extractFunctionParameters = function(fun) {
  UseMethod("extractFunctionParameters")
}

extractFunctionParameters.smoof_function = function(fun) {
  par.set = getParamSet(fun)
  return(list(
    n.objectives = getNumberOfObjectives(fun),
    n.dim = getNumberOfParameters(fun),
    minimize = shouldBeMinimized(fun),
    upper = getUpper(par.set),
    lower = getLower(par.set))
  )
}

extractFunctionParameters.function = function(fun) {
  return(list())
}

extractFunctionParameters.smoof_wrapped_function = function(fun) {
  extractFunctionParameters(getWrappedFunction(fun))
}
