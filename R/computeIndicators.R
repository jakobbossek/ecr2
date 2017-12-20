#' @title Computation of EMOA performance indicators.
#'
#' @description Given a data.frame of Pareto-front approximations for different
#' sets of problems, algorithms and replications, the function computes sets
#' of unary and binary EMOA performance indicators.
#'
#' @param df [\code{data.frame}]\cr
#'   Data frame with columns \code{obj.cols}, \dQuote{prob}, \dQuote{algorithm}
#'   and \dQuote{repl}.
#' @param obj.cols [\code{character(>= 2)}]\cr
#'   Column names of the objective functions.
#'   Default is \code{c("f1", "f2")}, i.e., the bi-objective case is assumed.
#' @param offset [\code{numeric(1)}]\cr
#'   Offset added to reference point estimations.
#'   Default is 0.
#' @param ref.points [\code{list}]\cr
#'   Named list of numeric vectors (the reference points). The names must be the
#'   unique problem names in \code{df$prob} or a subset of these.
#'   If \code{NULL} (the default), reference points are estimated from the
#'   approximation sets for each problem.
#' @return [\code{list}] List with components \dQuote{unary} (data frame of
#'   unary indicators) and \dQuote{binary} (list of matrizes of binary indicators).
#' @export
#FIXME: allow to pass list of ref.points, list of ref.sets or better a data.frame
# of \code{df} structure?
#FIXME: imagine something like AS-EMOA, where we want each one approx set for each prob
computeIndicators = function(df, obj.cols = c("f1", "f2"), offset = 0, ref.points = NULL) {
  assertDataFrame(df)

  approximateRefPoints = function(df, obj.cols, as.df = FALSE) {
    # split by prob(lem)
    ref.points = by(df, list(df$prob), function(x) {
      # get data points
      pf.approx = x[, obj.cols, drop = FALSE]
      # compute reference point
      ref.point = apply(pf.approx, 2L, max) + offset
      # return a list with component prob
      res = list()
      res[[x$prob[1L]]] = ref.point
      return(res)
    })
    # drop "by" class and attributes
    attr(ref.points, "call") = NULL
    ref.points = unclass(ref.points)
    # eventually convert to data.frame
    if (as.df) {
      probs = names(ref.points)
      ref.points = as.data.frame(do.call(rbind, unname(ref.points)))
      ref.points$prob = probs
    }
    return(ref.points)
  }

  # get some meta data
  algos = unique(df$algorithm)
  probs = unique(df$prob)

  n.algos = length(algos)
  n.probs = length(probs)
  n.obj   = length(obj.cols)

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
    ref.points = approximateRefPoints(df, obj.cols, as.df = FALSE)
  }

  ref.points.length = sapply(ref.points, length)
  if (any(ref.points.length != n.obj))
    stopf("computeIndicators: considering %i objectives, but reference point %s do not match in length.",
      n.obj, collapse(which(ref.point.length != n.obj), sep = ", "))

  grid = expand.grid(algorithm = algos, prob = probs)
  df$prob = as.factor(df$prob)

  if (is.null(df$repl)) {
    warningf("No repl column. Assuming one replication per prob-algorithm combination.")
    df$repl = 1L
  }

  print(ref.points)

  # unary indicators
  unary.inds = by(df,
    list(df$algorithm, df$prob, df$repl),
    function(x) {
      approx = x[, obj.cols, drop = FALSE]
      data.frame(
        algorithm = x$algorithm[1L],
        prob      = x$prob[1L],
        repl      = x$repl[1L],
        HV        = ecr::computeHV(t(approx), ref.point = as.numeric(ref.points[[x$prob[1L]]])),
        NDISTINCT = nrow(approx[!duplicated(approx), , drop = FALSE]),
        N         = nrow(approx)
        #DELTA = emoaIndDelta(approx[!duplicated(approx), , drop = FALSE])
      )
    })

  unary.inds = do.call(rbind, unary.inds)

  # binary indicators
  #FIXME: generalize for multiple indicators
  #FIXME: use outer()
  eps.inds = list()
  for (prob in probs) {
    df.prob = df[which(df[["prob"]] == prob), , drop = FALSE]
    eps.mat = matrix(NA, nrow = n.algos, ncol = n.algos)
    colnames(eps.mat) = rownames(eps.mat) = algos
    for (i in 1:n.algos) {
      for (j in 1:n.algos) {
        pfaA = df.prob[which(df.prob[["algorithm"]] == algos[i]), obj.cols]
        pfaB = df.prob[which(df.prob[["algorithm"]] == algos[j]), obj.cols]
        eps.mat[i, j] = ecr::emoaIndEps(t(pfaA), t(pfaB))
      }
    }
    eps.inds[[prob]] = eps.mat
  }

  return(list(
    unary = unary.inds,
    binary = list(epsilon = eps.inds)
  ))
}

# plotIndicatorDistribution = function(inds) {
#   df = reshape2::melt(inds, id.vars = c("algorithm", "prob", "repl"), value.name = "Value", variable.name = "Measure")
#   print(head(df))
#   pl = ggplot2::ggplot(df, ggplot2::aes_string(x = "algorithm", y = "Value"))
#   pl = pl + ggplot2::geom_boxplot(ggplot2::aes_string(fill = "Measure"))
#   pl = pl + ggplot2::facet_grid(Measure ~ prob)

#   # pl = ggplot(df, aes_string(x = "prob", y = "Value"))
#   # pl = pl + geom_boxplot(aes_string(fill = "algo"))
#   # #pl = pl + facet_wrap("Measure")
#   # pl = pl + facet_grid(algo ~ .)
#   pl = pl + ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
#   pl = pl + ggplot2::scale_y_log10()
#   pl = pl + viridis::scale_fill_viridis(discrete = TRUE)
#   return(pl)
# }
