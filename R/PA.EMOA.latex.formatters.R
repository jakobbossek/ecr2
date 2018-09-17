#' @title Formatter for table cells of LaTeX tables.
#'
#' @description This formatter function should be applied to
#' tables where each table cell contains a \eqn{p}-value of
#' a statistical significance test. See \code{\link{toLatex}}
#' for an application.
#'
#' @param cell [any]\cr
#'   Cell value. In the majority of cases this will be a numeric value.
#' @param alpha [\code{numeric(1)}]\cr
#'   Significance level of underlying statistical test.
#'   Default is 0.05.
#' @return Formatted output.
#' @family EMOA performance assessment tools
#' @export
niceCellFormater = function(cell, alpha = 0.05) {
  if (is.na(cell))
    return ("-")
  else if (as.numeric(cell) > alpha)
    return (sprintf("$> %.2f$", alpha))
  else
    return (sprintf("$\\mathbf{%s}$", toScientificLaTeXNotation(cell)))
}
