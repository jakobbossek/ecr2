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
#' @return [\code{list}] List with components \dQuote{unary} (data frame of
#'   unary indicators) and \dQuote{binary} (list of matrizes of binary indicators).
#' @export
#FIXME: ignores prob column
#FIXME: allow to pass list of ref.points, list of ref.sets or better a data.frame
# of \code{df} structure?
#FIXME: imagine something like AS-EMOA, where we want each one approx set for each prob
computeIndicators = function(df, obj.cols = c("f1", "f2"), offset = 10) {
  assertDataFrame(df)

  # ref point approx
  #FIXME: generalize to problem of arbitrary many probs
  pareto.approx = df[, obj.cols, drop = FALSE]
  ref.point = apply(pareto.approx, 2L, max)
  ref.point = ref.point + offset

  algos = unique(df[["algorithm"]])
  probs = unique(df[["prob"]])
  n.algos = length(algos)

  grid = expand.grid(algorithm = algos, prob = probs)
  df$prob = as.factor(df$prob)

  if (is.null(df$repl))
    df$repl = 1L

  #FIMXE: generalize to obj.cols
  ref.points = plyr::ddply(df, "prob", dplyr::summarize, f1 = max(f1), f2 = max(f2))
  # was für ne scheiße ist das hier?!
  # ref.points = plyr::ddply(mcMST, "prob",
  #   function(x) {
  #     data.frame(f1 = max(x[, obj.cols[1L]]), f2 = x[, obj.cols[2L]])
  #   }
  # )
  colnames(ref.points) = c("prob", obj.cols)

  print(ref.points)

  # unary indicators
  unary.inds = by(df,
    list(df$algorithm, df$prob, df$repl),
    function(x) {
      approx = x[, obj.cols, drop = FALSE]
      data.frame(algo = x[["algorithm"]][1L],
        prob = x[["prob"]][1L],
        repl = x[["repl"]][1L],
        HV = ecr::computeHV(t(approx), ref.point = as.numeric(ref.points[which(ref.points$prob == x[1L, "prob"]), obj.cols])),
        NDISTINCT = nrow(approx[!duplicated(approx), , drop = FALSE]),
        N = nrow(approx)
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

plotIndicatorDistribution = function(inds) {
  df = reshape2::melt(inds, id.vars = c("algo", "prob", "repl"), value.name = "Value", variable.name = "Measure")
  print(head(df))
  pl = ggplot2::ggplot(df, ggplot2::aes_string(x = "algo", y = "Value"))
  pl = pl + ggplot2::geom_boxplot(ggplot2::aes_string(fill = "Measure"))
  pl = pl + ggplot2::facet_grid(Measure ~ prob)

  # pl = ggplot(df, aes_string(x = "prob", y = "Value"))
  # pl = pl + geom_boxplot(aes_string(fill = "algo"))
  # #pl = pl + facet_wrap("Measure")
  # pl = pl + facet_grid(algo ~ .)
  pl = pl + ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
  pl = pl + ggplot2::scale_y_log10()
  pl = pl + viridis::scale_fill_viridis(discrete = TRUE)
  return(pl)
}
