#' @title Plot distributions of EMOA indicators.
#'
#' @description Visualizes of empirical distributions of unary EMOA indicator
#' based on the results of \code{\link{computeIndicators}}.
#'
#' @param inds [\code{data.frame}]\cr
#'   Data frame with columns \dQuote{algorithm}, \dQuote{prob}, \dQuote{repl} and
#'   one additional column per EMOA indicator.
#' @param plot.type [\code{character(1)}]\cr
#'   Either \dQuote{boxplot} (the default) for boxplots or \dQuote{violin} for
#'   violin plots.
#' @return [\code{\link[ggplot2]{ggplot}}]
plotIndicatorDistribution = function(inds, plot.type = "boxplot", facet.type = "grid", facet.args = list(), logscale = character()) {
  assertDataFrame(inds)
  assertChoice(plot.type, choices = c("boxplot", "violinplot"))
  assertChoice(facet.type, choices = c("grid", "wrap"))
  assertList(facet.args)

  df = reshape2::melt(inds, id.vars = c("algorithm", "prob", "repl"), value.name = "Value", variable.name = "Measure")
  #print(head(df))

  #assertSubset(logscale, choices = unique(df$Measure))
  if (length(logscale) > 0L)
    df[df$Measure %in% logscale, "Value"] = log(df[df$Measure %in% logscale, "Value"])
  pl = ggplot2::ggplot(df, ggplot2::aes_string(x = "algorithm", y = "Value"))
  plot.fun = if (plot.type == "boxplot") ggplot2::geom_boxplot else ggplot2::geom_violin
  pl = pl + plot.fun()#ggplot2::aes_string(fill = "Measure"))
  default.facet.args = list(facets = formula(Measure ~ prob), scales = "free_y")
  facet.args = BBmisc::insert(default.facet.args, facet.args)
  facet.fun = if (facet.type == "grid") ggplot2::facet_grid else ggplot2::facet_wrap
  pl = pl + do.call(facet.fun, facet.args)

  # pl = ggplot(df, aes_string(x = "prob", y = "Value"))
  # pl = pl + geom_boxplot(aes_string(fill = "algorithm"))
  # #pl = pl + facet_wrap("Measure")
  # pl = pl + facet_grid(algorithm ~ .)
  pl = pl + ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
  pl = pl + ggplot2::scale_y_log10()
  pl = pl + viridis::scale_fill_viridis(discrete = TRUE)
  return(pl)
}
