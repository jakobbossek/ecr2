#' @title Explode/implode data frame column(s).
#'
#' @description Given a data frame and a column name, function
#' \code{explode} splits the content of a column by a specified
#' delimter (thus exploded) into multiple columns. Function \code{implode}
#' does vice versa, i.e., given a non-empty set of column names or
#' number, the function glues together the columns. Hence, functions
#' \code{explode} and \code{implode} are kind of inverse to each other.
#'
#' @param df [\code{data.frame}]\cr
#'   Data frame.
#' @param col [\code{character(1)}]\cr
#'   Name of column which should be exploded.
#' @param cols [\code{character(1)}]\cr
#'   Names of columns (or column number) which should be imploded.
#' @param by [\code{character(1)}]\cr
#'   Delimeter used to split cell entries (for \code{explode}) or
#'   glue them together (for \code{implode}).
#' @param keep [\code{logical(1)}]\cr
#'   Should exploded or imploded source column be kept?
#'   Default is \code{FALSE}.
#' @param col.names [\code{character}]\cr
#'   Names of new columns.
#'   Default is \dQuote{\code{col}.1}, ..., \dQuote{\code{col}.k}, where
#'   k is the number of elements each cell in column \code{col} is split into.
#' @param col.name [\code{character(1)}]\cr
#'   Name of new column.
#' @return [\code{data.frame}] Modified data frame.
#' @examples
#' df = data.frame(x = 1:3, y = c("a.c", "a.b", "a.c"))
#' df.ex = explode(df, col = "y", col.names = c("y1", "y2"))
#' df.im = implode(df.ex, cols = c("y1", "y2"), by = "---", col.name = "y", keep = TRUE)
#' @name explode
#' @rdname explode
#' @export
explode = function(df, col, by = ".", keep = FALSE, col.names = NULL) {
  assertDataFrame(df, min.cols = 1L, min.rows = 1L)
  assertChoice(col, choices = colnames(df))
  assertString(by)
  assertFlag(keep)

  # create dummy "column"
  to.explode = as.character(df[[col]])

  col.exploded = strsplit(to.explode, split = by, fixed = TRUE)
  lengths = sapply(col.exploded, length)

  if (length(unique(lengths)) != 1L)
    stopf("explode: not all exploded cells have the same length")

  if (is.null(col.names))
    col.names = paste0(col, 1:lengths[1L])

  if (length(col.names) != lengths[1L])
    stopf("explode: col.names must contain as many elements as exploded cells.")
  assertCharacter(col.names, len = lengths[1L], any.missing = FALSE)

  df.new.cols = do.call(rbind, col.exploded)
  colnames(df.new.cols) = col.names

  if (length(intersect(colnames(df), col.names)) > 0L)
    stopf("explode: existing and new column names must not intersect.")

  df = cbind(df, df.new.cols)

  if (!keep)
    df[[col]] = NULL

  return(df)
}

#' @rdname explode
#' @export
implode = function(df, cols, by = ".", keep = FALSE, col.name) {
  assertDataFrame(df, min.rows = 1L, min.cols = 2L)
  # cols can be either all numeric (indizes) or character (column names)
  if (is.numeric(cols)) {
    cols = as.integer(cols)
    assertInteger(cols, min.len = 2L, max.len = ncol(df), lower = 1L, upper = ncol(df), any.missing = FALSE)
  } else if (is.character(cols)) {
    assertSubset(cols, choices = colnames(df))
  }
  sel.df = df[cols]

  # generate imploded column
  imploded = apply(sel.df, 1L, BBmisc::collapse, sep = by)
  imploded = data.frame(x = imploded)
  colnames(imploded) = col.name

  # and append
  df = cbind(df, imploded)

  if (!keep)
    df[cols] = NULL

  return(df)
}
