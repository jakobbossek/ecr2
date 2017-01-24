#' @import ParamHelpers
#' @import BBmisc
#' @import smoof
#' @import ggplot2
#' @import checkmate
#' @import parallelMap
#' @import reshape2
#' @import gridExtra
#' @importFrom stats median rnorm runif
#' @importFrom utils tail
#' @useDynLib ecr2
NULL

.onAttach = function(libname, pkgname) {
  parallelRegisterLevels(package = "ecr2", levels = c("ecr2.evaluateFitness", "ecr2.generateOffspring"))
}
