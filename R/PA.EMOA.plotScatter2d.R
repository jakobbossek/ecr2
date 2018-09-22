#' @title Visualize bi-objective Pareto-front approximations.
#'
#' @description Given a data frame with the results of (multiple) runs of (multiple)
#' different multi-objective optimization algorithms on (multiple) problem instances
#' the function generates \code{\link[ggplot2]{ggplot}} plots of the obtained
#' Pareto-front approximations.
#'
#' @note At the moment only approximations of bi-objective functions are supported.
#'
#' @param df [\code{data.frame}]\cr
#'   Data.frame with columns at least \code{obj.cols}, \dQuote{prob} and \dQuote{algorithm}.
#' @param obj.cols [\code{character(>= 2)}]\cr
#'   Column names of the objective functions.
#'   Default is \code{c("f1", "f2")}.
#' @param highlight.algos [\code{character(1)}]\cr
#'   Name of algorithm to highlight exclusively. Useful to highlight, e.g., the
#'   true Pareto-optimal front (if known) or some reference set.
#'   Default is \code{NULL}, i.e., unknown.
#' @param offset.highlighted [\code{numeric(1)}]\cr
#'   Numeric offset used to shift set (see \code{highlight.algos})
#'   which should be highlighted.
#'   Even though this produces objective vectors it
#'   may be used to make visible reference sets which otherwise would
#'   be hidden by overlap of multiple other approximation sets.
#' @param shape [\code{character(1)}]\cr
#'   Name of column which shall be used to define shape of points.
#'   Default is \dQuote{algorithm}.
#' @param colour [\code{character(1)}]\cr
#'   Name of column which shall be used to define colour of points.
#'   Default is \code{NULL}, i.e., coloring is deactivated.
#' @param title [\code{character(1)}]\cr
#'   Plot title.
#' @param subtitle [\code{character(1)}]\cr
#'   Plot subtitle.
#' @param facet.type [\code{character(1)}]\cr
#'   Which faceting method to use? Pass \dQuote{wrap} for \code{\link[ggplot2]{facet_wrap}}
#'   or \dQuote{grid} for \code{\link[ggplot2]{facet_grid}}.
#'   Default is \dQuote{wrap}.
#' @param facet.args [\code{list}]\cr
#'   Named list of arguments passed down to \code{\link[ggplot2]{facet_wrap}} or
#'   \code{\link[ggplot2]{facet_grid}} respectively (depends on \code{facet.type}).
#'   E.g., \code{nrow} to change layout.
#'   Default is the empty list. In this case data is grouped by problem.
#' @return [\code{\link[ggplot2]{ggplot}}] A ggplot object.
#' @family EMOA performance assessment tools
#' @examples
#' \dontrun{
#' # load examplary data
#' data(mcMST)
#' print(head(mcMST))
#'
#' # no customization; use the defaults
#' pl = plotFronts(mcMST)
#'
#' # algo PRIM is obtained by weighted sum scalarization
#' # Since the front is (mainly) convex we highlight these solutions
#' pl = plotFronts(mcMST, highlight.algos = "PRIM")
#'
#' # customize layout
#' pl = plotFronts(mcMST, title = "Pareto-approximations",
#'   subtitle = "based on different mcMST algorithms.", facet.args = list(nrow = 2))
#' }
#' @export
#FIXME: allow to work if there is no prob column
#FIXME: allow to pass column numbers of obj.cols
plotScatter2d = function(df,
  obj.cols = c("f1", "f2"),
  shape = "algorithm",
  colour = NULL,
  highlight.algos = NULL,
  offset.highlighted = 0,
  title = NULL, subtitle = NULL,
  facet.type = "wrap", facet.args = list()) {
  assertDataFrame(df, min.rows = 2L, min.cols = 2L)
  assertCharacter(obj.cols, min.len = 2L)
  assertNumber(offset.highlighted, lower = 0, finite = TRUE)
  assertChoice(facet.type, choices = c("wrap", "grid"))
  assertList(facet.args)

  # sanity check columns containing objective values
  if (!all(obj.cols %in% colnames(df)))
    stopf("obj.cols needs to contain valid column names.")

  df.obj = df[, obj.cols, drop = FALSE]
  obj.cols.numeric = sapply(df.obj, is.numeric)
  if (!all(obj.cols.numeric))
    stopf("Only numeric values allowed in obj.cols, but column(s) '%s' %s not numeric!",
      collapse(obj.cols[which(!obj.cols.numeric)], ifelse(sum(!obj.cols.numeric) > 1L, "are", "is")))

  # insert dummy values if missing
  if (is.null(df$algorithm))
    df$algorithm = "Algorithm"
  if (is.null(df$prob))
    df$prob = "Problem"
  if (is.null(df$repl))
    df$repl = as.factor(1L)

  df$repl = as.factor(df$repl)

  assertChoice(shape, choices = setdiff(colnames(df), obj.cols))
  assertChoice(colour, choices = setdiff(colnames(df), obj.cols), null.ok = TRUE)

  # get algorithm names
  algos = unique(df$algorithm)
  probs = unique(df$prob)

  # get number of problems and algorithms
  n.algos = length(algos)
  n.probs = length(probs)

  if (!is.null(highlight.algos)) {
    assertChoice(highlight.algos, choices = algos)
  }

  assertString(title, null.ok = TRUE)
  assertString(subtitle, null.ok = TRUE)

  pl = ggplot2::ggplot(mapping = ggplot2::aes_string(x = obj.cols[1L], y = obj.cols[2L]))

  data = df
  data.highlight = NULL

  if (!is.null(highlight.algos)) {
    data = df[df$algorithm != highlight.algos, , drop = FALSE]
    data.highlight = df[df$algorithm == highlight.algos, , drop = FALSE]
    data.highlight[, obj.cols] = data.highlight[, obj.cols] - offset.highlighted
  }

  if (!is.null(data.highlight)) {
    pl = pl + ggplot2::geom_step(
      data = data.highlight,
      alpha = 0.3)
    pl = pl + ggplot2::geom_line(
      data = data.highlight,
      alpha = 0.3)
    # pl = pl + ggplot2::geom_point(
    #   data = data.highlight,
    #   size = 3.5,
    #   shape = 1,
    #   alpha = 0.8,
    #   colour = "tomato")
  }
  pl = pl + ggplot2::geom_point(
    data = data,
    mapping = ggplot2::aes_string(shape = shape, colour = colour),
    alpha = 0.5)
  pl = pl + scale_shape_manual(values = 1:nlevels(as.factor(data[[shape]])))
  if (n.probs > 1L) {
    # how to group stuff
    group.by = if (facet.type == "wrap") formula( ~ prob) else formula(. ~ prob)
    default.facet.args = list(facets = group.by, scale = "free")
    facet.args = BBmisc::insert(default.facet.args, facet.args)
    if (facet.type == "wrap")
      pl = pl + do.call(ggplot2::facet_wrap, facet.args)
    else
      pl = pl + do.call(ggplot2::facet_grid, facet.args)
  }
  pl = pl + ggplot2::labs(
    title = title,
    subtitle = subtitle,
    shape = "Algorithm",
    colour = "Algorithm"
  )
  pl = pl + ggplot2::theme(
    legend.position = "bottom",
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )
  if (!is.null(colour))
    pl = pl + viridis::scale_colour_viridis(discrete = TRUE)

  return(pl)
}

#FIXME: docs
convertToGG = function(x, value.name = "value") {
  rns = rownames(x)
  cns = colnames(x)
  x = as.data.frame(x)
  rownames(x) = rns
  colnames(x) = cns
  x = as.matrix(x)
  x = reshape2::melt(x, value.name = value.name)
  return(x)
}
