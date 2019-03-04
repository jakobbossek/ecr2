#' @import BBmisc
#' @import smoof
#' @import ggplot2
#' @import checkmate
#' @import parallelMap
#' @import reshape2
#' @import ParamHelpers
#' @import kableExtra
#' @importFrom stats median rnorm runif dist formula sd wilcox.test
#' @importFrom utils tail head
#' @importFrom knitr kable
#' @importFrom lazyeval interp
#' @useDynLib ecr, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

.onAttach = function(libname, pkgname) {
  parallelMap::parallelRegisterLevels(package = "ecr", 
                                      levels = c("evaluateFitness", 
                                                 "generateOffspring", 
                                                 "computeDominanceRanking", 
                                                 "computeIndicators"))
}
