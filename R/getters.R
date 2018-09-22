#' @title
#' Get size of Pareto-archive.
#'
#' @description Returns the number of stored individuals in Pareto archive.
#'
#' @param x [\code{ecr_pareto_archive}]\cr
#'   Pareto archive.
#' @return [\code{integer(1)}]
#' @family ParetoArchive
#' @export
getSize = function(x) {
  UseMethod("getSize")
}

#' @export
getSize.ecr_pareto_archive = function(x) {
  return(x$env$size)
}

#' @title Extract individuals from Pareto archive.
#'
#' @description Get the non-dominated individuals logged in the Pareto archive.
#'
#' @inheritParams getSize
#' @return [\code{list}]
#' @family ParetoArchive
#' @export
getIndividuals = function(x) {
  UseMethod("getIndividuals")
}

#' @export
getIndividuals.ecr_pareto_archive = function(x) {
  return(x$env$individuals)
}

#' @title
#' Extract fitness values from Pareto archive.
#'
#' @description Get all non-dominated points in objective space, i.e., an (m x n)
#' matrix of fitness with m being the number of objectives and n being the number
#' of non-dominated points in the Pareto archive.
#'
#' @inheritParams getSize
#' @return [\code{matrix}]
#' @export
getFront = function(x) {
  UseMethod("getFront")
}

#' @export
getFront.ecr_pareto_archive = function(x) {
  return(x$env$fitness)
}

#' @export
getFront.ecr_multi_objective_result = function(x) {
  return(x$pareto.front)
}

