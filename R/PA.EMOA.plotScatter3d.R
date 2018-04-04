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
#' @param max.in.row [\code{integer(1)}]\cr
#'   Maximum number of plots to be displayed side by side in a row.
#'   Default is 4.
#' @param package [\code{character(1L)}]\cr
#'   Which package to use for 3d scatterplot generation?
#'   Possible choices are \dQuote{scatterplot3d}, \dQuote{plot3D}, \dQuote{plot3Drgl}
#'   or \dQuote{plotly}.
#'   Default is \dQuote{scatterplot3d}.
#' @param ... [any]\cr
#'   Further arguments passed down to scatterplot function.
#' @return Nothing
#' @family EMOA performance assessment tools
#' @export
plotScatter3d = function(df, obj.cols = c("f1", "f2", "f3"), max.in.row = 4L, package = "scatterplot3d", ...) {
  assertDataFrame(df, min.rows = 2L, min.cols = 3L)
  assertCharacter(obj.cols, min.len = 2L)
  assertChoice(package, c("scatterplot3d", "plot3D", "plot3Drgl", "plotly"))
  max.in.row = asInt(max.in.row, lower = 1L, upper = 10L)

  if (!all(obj.cols %in% colnames(df)))
    stopf("obj.cols needs to contain valid column names.")

  if (is.null(df$algorithm))
    df$algorithm = "Algorithm"
  if (is.null(df$prob))
    df$prob = "Problem"
  if (is.null(df$repl))
    df$repl = as.factor(1L)

  df$repl = as.factor(df$repl)

  # get algorithm and problem names
  algos = factor(unique(df$algorithm))
  probs = factor(unique(df$prob))

  # how many algorithms/problems are we faced with?
  n.algos = nlevels(algos)
  n.probs = nlevels(probs)

  # if (n.probs > 1L & package == "plot3Drgl")
  #   stopf("plotFronts3d: package 'plot3Drgl only appicable if only one problem is passed.")

  # points types and colours
  pchs = 1:16
  cols = viridis::viridis_pal()(n.algos)

  if (n.probs > 1L) {
    n.rows = ceiling(n.probs / max.in.row)
    opar = graphics::par(mfrow = c(n.rows, max.in.row))
    on.exit(graphics::par(opar))
  }

  if (package %in% c("scatterplot3d", "plot3D", "plot3Drgl")) {
    for (prob in probs) {
      df2 = df[df$prob == prob, , drop = FALSE]
      prob.enc = as.integer(as.factor(df2$algorithm))
      pchs2 = pchs[prob.enc]
      cols2 = cols[prob.enc]

      if (package == "scatterplot3d") {
        objs = list(x = as.matrix(df2[, obj.cols]))
        args.default = list(
          pch = pchs2, color = cols2,
          main = prob)
        args = c(objs, BBmisc::insert(args.default, list(...)))
        do.call(scatterplot3d::scatterplot3d, args)
        graphics::legend("topright", legend = algos, bg = "white", col = cols[as.integer(algos)],
          pch = pchs[as.integer(algos)])
      } else if (package %in% c("plot3D", "plot3Drgl")) {
        x = df2[, obj.cols[1L]]
        y = df2[, obj.cols[2L]]
        z = df2[, obj.cols[3L]]
        objs = list(x = x, y = y, z = z)
        args.default = list(
          colvar = prob.enc, col = cols,
          pch = pchs2,
          ticktype = "detailed", # we want to see axis ticks
          colkey = list( # labeled legend
            at = 1:n.algos,
            labels = algos
          ),
          bty = "g", #"b2", # layout of the box
          main = prob,
          xlab = obj.cols[1L],
          ylab = obj.cols[2L],
          zlab = obj.cols[3L],
          clab = "Algorithm" # label for legend
        )
        args = c(objs, BBmisc::insert(args.default, list(...)))
        do.call(plot3D::scatter3D, args)
        if (package == "plot3Drgl")
          plot3Drgl::plotrgl()
      } else {
        stopf("plotScatter3d: this should not happen. Please contact the package author.")
      }
    }
  } # endif (package %in% ...)
  if (package == "plotly") {
    i = 1
    plot.list = lapply(probs, function(prob) {
      df2 = df[df$prob == prob, , drop = FALSE]
      prob.enc = as.integer(as.factor(df2$algorithm))

      x = df2[[obj.cols[1L]]]
      y = df2[[obj.cols[2L]]]
      z = df2[[obj.cols[3L]]]

      pl = plotly::plot_ly(df2, x = x, y = y, z = z,
        color = df2$algorithm, colors = cols, scene = paste0("scene", i))
      i <<- i + 1L

      pl = plotly::add_markers(pl)
      pl = plotly::layout(pl, title = prob)
      return(pl)
    })

    plot.list$nrows = n.rows
    p = do.call(plotly::subplot, plot.list)

    scene.options = list()
    for (i in 1:n.probs) {
      scene.options[[paste0("scene", i)]] = list(aspectmode = "cube")
    }
    p = do.call(plotly::layout, c(list(p = p), scene.options))
    print(p)
  }
}
