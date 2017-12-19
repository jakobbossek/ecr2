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
#' pl = plotFronts(mcMST, title = "Pareto-approximations",
#'   subtitle = "based on different mcMST algorithms.", facet.args = list(nrow = 2))
#' }
#' @export
#FIXME: allow to work if there is no prob column
#FIXME: allow to pass column numbers of obj.cols
plotFronts = function(df, obj.cols = c("f1", "f2"), shape = "algorithm", colour = NULL, highlight.algos = NULL, title = NULL, subtitle = NULL, facet.type = "wrap", facet.args = list()) {
  assertDataFrame(df, min.rows = 2L, min.cols = 4L)
  assertCharacter(obj.cols, min.len = 2L)
  assertChoice(facet.type, choices = c("wrap", "grid"))
  assertChoice(shape, choices = setdiff(colnames(df), obj.cols))
  assertChoice(colour, choices = setdiff(colnames(df), obj.cols), null.ok = TRUE)
  assertList(facet.args)

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
  pl = ggplot2::ggplot(mapping = ggplot2::aes_string(x = obj.cols[1L], y = obj.cols[2L]))

  data = df
  data.highlight = NULL

  if (!is.null(highlight.algos)) {
    data = df[df$algorithm != highlight.algos, , drop = FALSE]
    data.highlight = df[df$algorithm == highlight.algos, , drop = FALSE]
  }

  if (!is.null(data.highlight)) {
    pl = pl + ggplot2::geom_step(
      data = data.highlight,
      alpha = 0.3)
    pl = pl + ggplot2::geom_line(
      data = data.highlight,
      alpha = 0.3)
    pl = pl + ggplot2::geom_point(
      data = data.highlight,
      size = 3.5,
      shape = 1,
      alpha = 0.8,
      colour = "tomato")
  }
  pl = pl + ggplot2::geom_point(
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

#' @title Visualize three-objective Pareto-front approximations.
#'
#' @description Given a data frame with the results of (multiple) runs of (multiple)
#' different three-objective optimization algorithms on (multiple) problem instances
#' the function generates 3D scatterplots of the obtained Pareto-front approximations.
#'
#' @param df [\code{data.frame}]\cr
#'   Data.frame with columns at least \code{obj.cols}, \dQuote{prob} and \dQuote{algorithm}.
#' @param obj.cols [\code{character(>= 3)}]\cr
#'   Column names of the objective functions.
#'   Default is \code{c("f1", "f2", "f3")}.
#' @param max.in.col [\code{integer(1)}]\cr
#'   Maximum number of plots to be displayed side by side.
#'   Default is 4.
#' @param ... [any]\cr
#'   Furhter parameters passed down to plot function.
#' @return Nothing
#' @export
plotFronts3d = function(df, obj.cols = c("f1", "f2", "f3"), max.in.col = 4L, driver = "scatterplot3d", ...) {
  assertDataFrame(df, min.rows = 2L, min.cols = 5L)
  assertCharacter(obj.cols, min.len = 2L)
  assertChoice(driver, c("scatterplot3d", "plot3D", "plotly"))
  max.in.col = asInt(max.in.col, lower = 1L, upper = 10L)

  if (!all(obj.cols %in% colnames(df)))
    stopf("obj.cols needs to contain valid column names.")

  # get algorithm and problem names
  algos = factor(unique(df$algorithm))
  probs = factor(unique(df$prob))

  # how many algorithms/problems are we faced with?
  n.algos = nlevels(algos)
  n.probs = nlevels(probs)

  # points types and colours
  #FIXME: I assume that we need to do this for each driver in another way
  pchs = 1:16
  cols = viridis::viridis_pal()(n.probs)

  n.rows = ceiling(n.probs / max.in.col)
  opar = graphics::par(mfrow = c(n.rows, n.probs))
  on.exit(par(opar))

  #FIXME: enable driver
  for (prob in probs) {
    df2 = df[df$prob == prob, , drop = FALSE]
    prob.enc = as.integer(as.factor(df2$algorithm))
    pchs2 = pchs[prob.enc]
    cols2 = cols[prob.enc]

    if (driver == "scatterplot3d") {
      scatterplot3d::scatterplot3d(x = as.matrix(df2[, obj.cols]),
        pch = pchs2, color = cols2, main = prob, ...)
      graphics::legend("topright", legend = algos, bg = "white", col = cols[as.integer(algos)],
        pch = pchs[as.integer(algos)])
    } else if (driver == "plot3D") {
      x = df2[, obj.cols[1L]]
      y = df2[, obj.cols[2L]]
      z = df2[, obj.cols[3L]]
      objs = list(x = x, y = y, z = z)
      args.default = list(
        colvar = prob.enc, col = cols,
        pch = pchs2,
        ticktype = "detailed",
        colkey = list(
          at = 1:n.algos,
          labels = algos
        ),
        bty = "b2")
      args = c(objs, BBmisc::insert(args.default, list(...)))
      do.call(plot3D::scatter3D, args)
    }
  }
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

#' @title Plot heatmap.
#'
#' @description Given a matrix or list of matrizes \code{x} this function
#' visualizes each matrix with a heatmap.
#'
#' @param x [\code{matrix} | \code{list[matrix]}]\cr
#'   Either a matrix or a list of matrizes.
#' @param value.name [\code{character(1)}]\cr
#'   Name for the values represented by the matrix.
#'   Internally, the matrix is transformed into a \code{data.frame}
#'   via \code{\link[reshape2]{melt}} in order to obtain a format
#'   which may be processed by \code{\link[ggplot2]{ggplot}} easily.
#'   Default is \dQuote{Value}.
#' @param show.values [\code{logical(1L)}]\cr
#'   Should the values be printed within the heatmap cells?
#'   Default is \code{FALSE}.
#' @return [\code{\link[ggplot2]{ggplot}}] ggplot object.
#' @examples
#' # simulate two (correlation) matrizes
#' x = matrix(runif(100), ncol = 10)
#' y = matrix(runif(100), ncol = 10)
#' \dontrun{
#' pl = plotHeatmap(x)
#' pl = plotHeatmap(list(x, y), value.name = "Correlation")
#' pl = plotHeatmap(list(MatrixX = x, MatrixY = y), value.name = "Correlation")
#' }
#' @export
plotHeatmap = function(x, value.name = "Value", show.values = FALSE) {
  assertMatrix(x, mode = "numeric")
  assertFlag(show.values)
  assertString(value.name)

  # just transform to long format if matrix provided
  ggdf = NULL
  if (is.matrix(x)) {
    ggdf = convertToGG(x, value.name = value.name)
  }
  # otherwise assume a named list (named by problem)
  else if (is.list(x)) {
    ns = names(x)
    if (is.null(ns))
      ns = as.character(1:length(x))
    if (any(ns == ""))
      stopf("ecr::plotHeatmap: Either all elements of x or none must be named.")
    ggdf = do.call(rbind, lapply(1:length(x), function(i) {
      tmp = x[[i]]
      tmp = convertToGG(tmp, value.name = value.name)
      tmp$prob = ns[i]
      return(tmp)
    }))
  }

  # plot heatmap
  pl = ggplot2::ggplot(ggdf, ggplot2::aes_string(x = "Var1", y = "Var2"))
  pl = pl + ggplot2::geom_tile(ggplot2::aes_string(fill = value.name), color = "white", size = 0.1)

  # workaround to get rounded values
  if (show.values) {
    ggdf2 = ggdf
    ggdf2[[value.name]] = round(ggdf2[[value.name]], 1L)
    pl = pl + ggplot2::geom_text(data = ggdf2, ggplot2::aes_string(label = value.name), color = "white", size = 1.4)
  }

  pl = pl + ggplot2::coord_equal()

  # split if multiple problems available
  if (!is.null(ggdf$prob))
    pl = pl + ggplot2::facet_wrap(~ prob, nrow = 1L)#, scales = "free")

  # default layout
  pl = pl + viridis::scale_fill_viridis()
  pl = pl + ggplot2::theme(axis.ticks = element_blank())
  pl = pl + ggplot2::theme(axis.text = element_text(size=7))
  pl = pl + ggplot2::theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top")
  pl = pl + ggplot2::xlab("") + ggplot2::ylab("")
  return(pl)
}

#' @title Plot distributions of EMOA indicators.
#'
#' @description Visualizes of empirical distributions of unary EMOA indicator
#' based on the results of \code{\link{compareApproximations}}.
#'
#' @param inds [\code{data.frame}]\cr
#'   Data frame with columns \dQuote{algo}, \dQuote{prob}, \dQuote{repl} and
#'   one additional column per EMOA indicator.
#' @param plot.type [\code{character(1)}]\cr
#'   Either \dQuote{boxplot} (the default) for boxplots or \dQuote{violin} for
#'   violin plots.
#' @return [\code{\link[ggplot2]{ggplot}}]
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
