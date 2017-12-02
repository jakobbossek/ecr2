#' @title Visualize Pareto-front approximations.
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
#' @param shape [\code{character(1)}]\cr
#'   Name of column which shall be used to define shape of points.
#'   Default is \dQuote{algorithm}.
#' @param colour [\code{character(1)}]\cr
#'   Name of column which shall be used to define colour of points.
#'   Default is \code{NULL}, i.e., colouring is deactivated.
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
#' pl = plotFronts(mcMST, title = "Pareto-approximations", subtitle = "based on different mcMST algorithms.", facet.args = list(nrow = 2))
#' }
#' @export
#'
plotFronts = function(df, obj.cols = c("f1", "f2"), shape = "algorithm", colour = NULL, highlight.algos = NULL, title = NULL, subtitle = NULL, facet.type = "wrap", facet.args = list(),
  highlight.dom.area = FALSE, ref.point = NULL) {
  assertDataFrame(df, min.rows = 2L, min.cols = 4L)
  assertCharacter(obj.cols, min.len = 2L)
  assertChoice(facet.type, choices = c("wrap", "grid"))
  assertChoice(shape, choices = setdiff(colnames(df), obj.cols))
  assertChoice(colour, choices = setdiff(colnames(df), obj.cols), null.ok = TRUE)
  assertList(facet.args)
  assertFlag(highlight.dom.area)
  assertNumeric(ref.point, len = 2L, null.ok = TRUE, any.missing = FALSE, all.missing = FALSE)

  if (!all(obj.cols %in% colnames(df)))
    stopf("obj.cols needs to contain valid column names.")

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

  BBmisc::requirePackages("ggplot2", why = "ecr::plotFronts")
  pl = ggplot(mapping = aes_string(x = obj.cols[1L], y = obj.cols[2L]))

  # experimental
  if (highlight.dom.area) {
    offset = 10
    if (is.null(ref.point))
      ref.point = approximateNadirPoint(t(df[, obj.cols, drop = FALSE])) + offset
    data.rects = df
    data.rects$r1 = ref.point[1L]
    data.rects$r2 = ref.point[2L]
    pl = pl + geom_rect(
      data = data.rects,
      mapping = aes_string(
        xmin = obj.cols[1L], xmax = "r1",
        ymin = obj.cols[2L], ymax = "r2"
      ),
      fill = "black",
      alpha = 0.01
    )
  }

  data = df
  data.highlight = NULL

  if (!is.null(highlight.algos)) {
    data = df[df$algorithm != highlight.algos, , drop = FALSE]
    data.highlight = df[df$algorithm == highlight.algos, , drop = FALSE]
  }

  if (!is.null(data.highlight)) {
    pl = pl + geom_step(
      data = data.highlight,
      alpha = 0.3)
    pl = pl + geom_line(
      data = data.highlight,
      alpha = 0.3)
    pl = pl + geom_point(
      data = data.highlight,
      size = 3.5,
      shape = 1,
      alpha = 0.8,
      colour = "tomato")
  }
  pl = pl + geom_point(
    data = data,
    mapping = aes_string(shape = shape, colour = colour),
    alpha = 0.5)
  if (n.probs > 1L) {
    # how to group stuff
    group.by = ". ~ prob"
    default.facet.args = list(facets = group.by, scale = "free")
    facet.args = BBmisc::insert(default.facet.args, facet.args)
    if (facet.type == "wrap")
      pl = pl + do.call("facet_wrap", facet.args)
    else
      pl = pl + do.call("facet_grid", facet.args)
  }
  pl = pl + labs(
    title = title,
    subtitle = subtitle,
    shape = "Algorithm",
    colour = "Algorithm"
  )
  pl = pl + theme(legend.position = "bottom")

  return(pl)
}

#FIXME: docs
toGG = function(x, value.name = "value") {
  rns = rownames(x)
  cns = colnames(x)
  x = as.data.frame(x)
  rownames(x) = rns
  colnames(x) = cns
  x = as.matrix(x)
  x = reshape2::melt(x, value.name = value.name)
  return(x)
}

#FIXME: docs
plotHeatmap = function(x, value.name = "Value") {
  # just transform to long format if matrix provided
  if (is.matrix(x))
    ggdf = toGG(x, value.name = value.name)
  # otherwise assume a named list (named by problem)
  else if (is.list(x))
    ggdf = do.call(rbind, lapply(names(x), function(prob) {
      tmp = x[[prob]]
      tmp = toGG(tmp, value.name = value.name)
      tmp$prob = prob
      return(tmp)
    }))

  # plot heatmap
  pl = ggplot(ggdf, aes(x = Var1, y = Var2))
  pl = pl + geom_tile(aes_string(fill = value.name), color = "white", size = 0.1)
  pl = pl + geom_text(aes(label = round(Value, 1)), color = "white", size = 1.4)
  pl = pl + coord_equal()

  # split if multiple problems available
  if (!is.null(ggdf$prob))
    pl = pl + facet_wrap(~ prob, nrow = 1L)#, scales = "free")

  # default layout
  requirePackages(c("viridis", "ggthemes"), why = "ecr::plotHeatmap")
  pl = pl + scale_fill_viridis()
  pl = pl + theme(axis.ticks = element_blank())
  pl = pl + theme(axis.text = element_text(size=7))
  #pl = pl + theme_tufte(base_family = "Helvetica")
  pl = pl + theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top")
  pl = pl + xlab("") + ylab("")
  return(pl)
}
