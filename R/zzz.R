#' @import BBmisc
#' @import smoof
#' @import ggplot2
#' @import checkmate
#' @import parallelMap
#' @import reshape2
#' @import ParamHelpers
#' @importFrom stats median rnorm runif
#' @importFrom utils tail
#' @useDynLib ecr
NULL

.onAttach = function(libname, pkgname) {
  parallelRegisterLevels(package = "ecr", levels = c("evaluateFitness", "generateOffspring"))
}
