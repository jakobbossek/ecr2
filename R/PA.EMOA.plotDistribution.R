#' @title Plot distribution of EMOA indicators.
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
#' @param fill [\code{character(1)}]\cr
#'   Variable used to fill boxplots.
#'   Default is \dQuote{algorithm}.
#' @param facet.type [\code{character(1)}]\cr
#'   Which faceting method to use? Pass \dQuote{wrap} for \code{\link[ggplot2]{facet_wrap}}
#'   or \dQuote{grid} for \code{\link[ggplot2]{facet_grid}}.
#'   Default is \dQuote{wrap}.
#' @param facet.args [\code{list}]\cr
#'   Named list of arguments passed down to \code{\link[ggplot2]{facet_wrap}} or
#'   \code{\link[ggplot2]{facet_grid}} respectively (depends on \code{facet.type}).
#'   E.g., \code{nrow} to change layout.
#'   Default is the empty list. In this case data is grouped by problem and indicator.
#' @param logscale [\code{character}]\cr
#'   Vector of indicator names which should be log-transformed prior to
#'   visualization.
#'   Default is the empty character vector.
#' @return [\code{\link[ggplot2]{ggplot}}]
#' @family EMOA performance assessment tools
#' @export
plotDistribution = function(inds,
  plot.type = "boxplot",
  fill = "algorithm",
  facet.type = "grid", facet.args = list(), logscale = character()) {
  assertDataFrame(inds)
  #assertChoice(plot.type, choices = c("boxplot", "violinplot"))
  assertChoice(facet.type, choices = c("grid", "wrap"))
  assertList(facet.args)

  df = reshape2::melt(inds, id.vars = c("algorithm", "prob", "repl"), value.name = "Value", variable.name = "Measure")
  #print(head(df))

  #assertSubset(logscale, choices = unique(df$Measure))
  if (length(logscale) > 0L)
    df[df$Measure %in% logscale, "Value"] = log(df[df$Measure %in% logscale, "Value"])
  pl = ggplot2::ggplot(df, ggplot2::aes_string(x = "algorithm", y = "Value", fill = fill))
  if ("violinplot" %in% plot.type)
    pl = pl + ggplot2::geom_violin(trim = FALSE)
  if ("boxplot" %in% plot.type)
    pl = pl + ggplot2::geom_boxplot()
  default.facet.args = list(facets = formula(Measure ~ prob), scales = "free_y")
  facet.args = BBmisc::insert(default.facet.args, facet.args)
  facet.fun = if (facet.type == "grid") ggplot2::facet_grid else ggplot2::facet_wrap
  pl = pl + do.call(facet.fun, facet.args)

  # pl = ggplot(df, aes_string(x = "prob", y = "Value"))
  # pl = pl + geom_boxplot(aes_string(fill = "algorithm"))
  # #pl = pl + facet_wrap("Measure")
  # pl = pl + facet_grid(algorithm ~ .)
  pl = pl + ggplot2::theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top")
  #pl = pl + ggplot2::scale_y_log10()
  pl = pl + viridis::scale_fill_viridis(discrete = TRUE)#, end = 0.8)
  return(pl)
}
