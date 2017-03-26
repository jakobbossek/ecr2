#' @export
getSize = function(x) {
  UseMethod("getSize")
}

#' @export
getSize.ecr2_pareto_archive = function(x) {
  return(x$env$size)
}

#' @export
getIndividuals = function(x) {
  UseMethod("getIndividuals")
}

#' @export
getIndividuals.ecr2_pareto_archive = function(x) {
  return(x$env$individuals)
}

#' @export
getFront = function(x) {
  UseMethod("getFront")
}

#' @export
getFront.ecr2_pareto_archive = function(x) {
  return(x$env$fitness)
}
