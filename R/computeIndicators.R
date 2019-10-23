#' @title Computation of EMOA performance indicators.
#'
#' @description Given a data.frame of Pareto-front approximations for different
#' sets of problems, algorithms and replications, the function computes sets
#' of unary and binary EMOA performance indicators.
#' This function makes use of \code{\link[parallelMap]{parallelMap}} to
#' parallelize the computation of indicators.
#'
#' @references
#' [1] Knowles, J., Thiele, L., & Zitzler, E. (2006). A Tutorial on the Performance Assessment
#' of Stochastic Multiobjective Optimizers. Retrieved from https://sop.tik.ee.ethz.ch/KTZ2005a.pdf
#' [2] Knowles, J., & Corne, D. (2002). On Metrics for Comparing Non-Dominated Sets.
#' In Proceedings of the 2002 Congress on Evolutionary Computation Conference (CEC02)
#' (pp. 711–716). Honolulu, HI, USA: Institute of Electrical and Electronics Engineers.
#' [3] Okabe, T., Yaochu, Y., & Sendhoff, B. (2003). A Critical Survey of Performance
#' Indices for Multi-Objective Optimisation. In Proceedings of the 2003 Congress on Evolutionary
#' Computation Conference (CEC03) (pp. 878–885). Canberra, ACT, Australia: IEEE.
#'
#' @param df [\code{data.frame}]\cr
#'   Data frame with columns \code{obj.cols}, \dQuote{prob}, \dQuote{algorithm}
#'   and \dQuote{repl}.
#' @param obj.cols [\code{character(>= 2)}]\cr
#'   Column names of the objective functions.
#'   Default is \code{c("f1", "f2")}, i.e., the bi-objective case is assumed.
#' @param unary.inds [\code{list}]\cr
#'   Named list of unary indicators which shall be calculated.
#'   Each component must be another list with mandatory argument \code{fun} (the
#'   function which calculates the indicator) and optional argument \code{pars} (a named
#'   list of parameters for \code{fun}). Function \code{fun} must have the
#'   signiture \dQuote{function(points, arg1, ..., argk, ...)}.
#'   The arguments \dQuote{points} and \dQuote{...} are mandatory, the remaining are
#'   optional.
#'   The names of the components on the first level are used for the column names
#'   of the output data.frame.
#'   Default is \code{list(HV = list(fun = computeHV))}, i.e., the dominated
#'   Hypervolume indicator.
#' @param binary.inds [\code{list}]\cr
#'   Named list of binary indicators which shall be applied for each algorithm
#'   combination. Parameter \code{binary.inds} needs the same structure as \code{unary.inds}.
#'   However, the function signature of \code{fun} is slighly different:
#'   \dQuote{function(points1, points2, arg1, ..., argk, ...)}.
#'   See function \code{\link{emoaIndEps}} for an example.
#'   Default is \code{list(EPS = list(fun = emoaIndEps))}.
#' @param normalize [\code{logical(1)}]\cr
#'   Normalize approximation sets to \eqn{[0, 1]^p} where \eqn{p} is the number of
#'   objectives? Normalization is done on the union of all approximation sets for each
#'   problem.
#'   Default is \code{FALSE}.
#' @param offset [\code{numeric(1)}]\cr
#'   Offset added to reference point estimations.
#'   Default is 0.
#' @param ref.points [\code{list}]\cr
#'   Named list of numeric vectors (the reference points). The names must be the
#'   unique problem names in \code{df$prob} or a subset of these.
#'   If \code{NULL} (the default), reference points are estimated from the
#'   approximation sets for each problem.
#' @param ref.sets [\code{list}]\cr
#'   Named list matrizes (the reference sets). The names must be the
#'   unique problem names in \code{df$prob} or a subset of these.
#'   If \code{NULL} (the default), reference points are estimated from the
#'   approximation sets for each problem.
#' @return [\code{list}] List with components \dQuote{unary} (data frame of
#'   unary indicators), \dQuote{binary} (list of matrizes of binary indicators),
#'   \dQuote{ref.points} (list of reference points used) and \dQuote{ref.sets}
#'   (reference sets used).
#' @export
computeIndicators = function(df,
  obj.cols = c("f1", "f2"),
  unary.inds = NULL, binary.inds = NULL,
  normalize = FALSE,
  offset = 0,
  ref.points = NULL,
  ref.sets = NULL
  ) {

  assertDataFrame(df)
  assertFlag(normalize)
  cnames = colnames(df)
  # note: repl is added later
  required.names = c(cnames, c("prob", "algorithm"))
  assertSubset(obj.cols, cnames)

  # get some meta data
  algos   = unique(df$algorithm)
  probs   = unique(df$prob)

  n.algos = length(algos)
  n.probs = length(probs)
  n.obj   = length(obj.cols)

  # normalize approximation sets
  if (normalize)
    df = ecr::normalize(df, obj.cols = obj.cols)

  # check list of unary indicators
  if (is.null(unary.inds))
    unary.inds = list(HV = list(fun = emoaIndHV))

  # check list of binary indicators
  # if (is.null(binary.inds))
  #   binary.inds = list(EPS = list(fun = emoaIndEps))

  # check reference points
  if (!is.null(ref.points)) {
    probs.yes.ref.point = names(ref.points)
    if (!all(probs.yes.ref.point %in% probs))
      stopf("computeIndicators: reference point for probs '%s' passed, but these are not listed in
        df$prob.", collapse(setdiff(probs.yes.ref.point, probs), sep = ", "))
    if (length(probs.yes.ref.point) > unique(probs.yes.ref.point))
      stopf("computeIndicators: there must be a single reference point for each prob.")

    # some reference points missing -> estimate from data
    if (length(probs.yes.ref.point) < n.probs) {
      probs.no.ref.point = setdiff(probs, probs.yes.ref.point)
      df.tmp = df[df$prob %in% probs.no.ref.point, , drop = FALSE]
      ref.points.missing = approximateRefPoints(df.tmp, obj.cols, as.df = FALSE)
      ref.points = c(ref.points, ref.points.missing)
    }
  } else {
    ref.points = approximateRefPoints(df, obj.cols, offset = offset, as.df = FALSE)
  }

  ref.points.length = sapply(ref.points, length)
  if (any(ref.points.length != n.obj))
    stopf("computeIndicators: considering %i objectives, but reference point %s do not match in length.",
      n.obj, collapse(which(ref.points.length != n.obj), sep = ", "))

  if (is.null(ref.sets))
    ref.sets = approximateRefSets(df, obj.cols, as.df = FALSE)

  grid = expand.grid(algorithm = algos, prob = probs)
  df$prob = as.factor(df$prob)

  if (is.null(df$repl)) {
    warningf("No repl column. Assuming one replication per prob-algorithm combination.")
    df$repl = 1L
  }

  # unary indicators
  unary.inds.names = sapply(unary.inds, function(x) attr(x$fun, "name"))
  names(unary.inds) = unary.inds.names

  computeUnaryIndicators = function(x) {
    # all EMOA indicators expect a matrix of type n.obj x n.points
    approx = t(x[, obj.cols, drop = FALSE])
    mode(approx) = "double"

    res = list(
      algorithm = as.character(x$algorithm[1L]),
      prob      = as.character(x$prob[1L]),
      repl      = as.integer(x$repl[1L])
    )

    for (unary.ind.name in unary.inds.names) {
      ind.fun = unary.inds[[unary.ind.name]][["fun"]]
      ind.args = BBmisc::coalesce(unary.inds[[unary.ind.name]][["pars"]], list())
      ind.args = BBmisc::insert(list(approx,
                                     ref.point = ref.points[[as.character(x$prob[1L])]],
                                     ref.points = ref.sets[[as.character(x$prob[1L])]]),
                                ind.args)
      #print(ind.args)
      res[[unary.ind.name]] = if (!is.null(ind.args$ref.point) & !is.null(ind.args$ref.points))
        do.call(ind.fun, ind.args) else NA
      #print(res[[unary.ind.name]])
    }

    res = as.data.frame(res)
    return(res)
  }
  # split by algorithm x prob x repl combination
  listOfDfs = split(df, list(as.character(df$algorithm), as.character(df$prob), as.integer(df$repl)))
  # compute Indicators
  unary.indicators = parallelLapply(listOfDfs, computeUnaryIndicators, level="ecr.computeIndicators")
  unary.indicators = do.call(rbind, unary.indicators)

  # binary indicators
  binary.inds.names = names(binary.inds)
  binary.indicators = list()

  for (binary.ind.name in binary.inds.names) {
    for (prob in probs) {
      prob.ind = list()
      # filter data
      df.prob = df[which(df[["prob"]] == prob), , drop = FALSE]
      # empty indicators matrix
      ind.mat = matrix(NA, nrow = n.algos, ncol = n.algos)
      colnames(ind.mat) = rownames(ind.mat) = algos
      for (i in 1:n.algos) {
        for (j in 1:n.algos) {
          approx.i = t(df.prob[which(df.prob[["algorithm"]] == algos[i]), obj.cols])
          approx.j = t(df.prob[which(df.prob[["algorithm"]] == algos[j]), obj.cols])

          # call indicator
          ind.fun = binary.inds[[binary.ind.name]][["fun"]]
          ind.args = BBmisc::coalesce(binary.inds[[binary.ind.name]][["pars"]], list())
          ind.args = BBmisc::insert(list(approx.i, approx.j), ind.args)
          ind.mat[i, j] = do.call(ind.fun, ind.args)
        }
      }
      prob.ind[[prob]] = ind.mat
      binary.indicators[[binary.ind.name]] = c(binary.indicators[[binary.ind.name]], prob.ind)
    }
  }
  return(list(
    unary = BBmisc::setAttribute(unary.indicators, "unary.inds", unary.inds),
    binary = binary.indicators,
    ref.points = ref.points,
    ref.sets = ref.sets
  ))
}
