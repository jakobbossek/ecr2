#' @title Plot distributions of EMOA indicators.
#'
#' @description Visualizes of empirical distributions of unary EMOA indicator
#' based on the results of \code{\link{compareApproximations}}.
#'
#' @param inds [\code{data.frame}]\cr
#'   Data frame with columns \dQuote{algorithm}, \dQuote{prob}, \dQuote{repl} and
#'   one additional column per EMOA indicator.
#' @param plot.type [\code{character(1)}]\cr
#'   Either \dQuote{boxplot} (the default) for boxplots or \dQuote{violin} for
#'   violin plots.
#' @return [\code{\link[ggplot2]{ggplot}}]
plotIndicatorDistribution = function(inds) {
  df = reshape2::melt(inds, id.vars = c("algorithm", "prob", "repl"), value.name = "Value", variable.name = "Measure")
  print(head(df))
  pl = ggplot2::ggplot(df, ggplot2::aes_string(x = "algorithm", y = "Value"))
  pl = pl + ggplot2::geom_boxplot()#ggplot2::aes_string(fill = "Measure"))
  pl = pl + ggplot2::facet_grid(Measure ~ prob, scale = "free_y")

  # pl = ggplot(df, aes_string(x = "prob", y = "Value"))
  # pl = pl + geom_boxplot(aes_string(fill = "algorithm"))
  # #pl = pl + facet_wrap("Measure")
  # pl = pl + facet_grid(algorithm ~ .)
  pl = pl + ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
  pl = pl + ggplot2::scale_y_log10()
  pl = pl + viridis::scale_fill_viridis(discrete = TRUE)
  return(pl)
}
