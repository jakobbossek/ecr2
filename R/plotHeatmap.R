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
  if (is.list(x))
    assertList(x, types = "matrix", any.missing = FALSE, all.missing = FALSE)
  else
    assertMatrix(x, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
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
  pl = pl + ggplot2::theme(
    axis.ticks = element_blank(),
    axis.text = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top")
  pl = pl + ggplot2::xlab("") + ggplot2::ylab("")
  return(pl)
}
