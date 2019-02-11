#' @title Explode/implode data frame column(s).
#'
#' @description Given a data frame and a column name, function
#' \code{explode} splits the content of a column by a specified
#' delimiter (thus exploded) into multiple columns. Function \code{implode}
#' does vice versa, i.e., given a non-empty set of column names or
#' numbers, the function glues together the columns. Hence, functions
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
  df.new.cols = as.data.frame(df.new.cols, stringsAsFactors = FALSE)
  colnames(df.new.cols) = col.names

  # if (length(intersect(colnames(df), col.names)) > 0L)
  #   stopf("explode: existing and new column names must not intersect.")

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
  #sel.df = vapply(sel.df, format, FUN.VALUE = character(nrow(sel.df)), trim = TRUE)
  imploded = apply(sel.df, 1L, BBmisc::collapse, sep = by)
  imploded = data.frame(x = imploded, stringsAsFactors = FALSE)
  colnames(imploded) = col.name

  # and append
  df = cbind(df, imploded)

  if (!keep)
    df[cols] = NULL

  return(df)
}

#' @title
#' Assign group membership based on another group membership.
#'
#' @description
#' Given a data frame and a grouping column of type factor or character this function
#' generates a new grouping column which groups the groups.
#'
#' @param df [\code{data.frame}]\cr
#'   Data frame.
#' @param col [\code{character(1)}]\cr
#'   Column name of group variable.
#' @param categories [\code{list}]\cr
#'   Named list. Names indicate the name of the category while the values are character vectors
#'   of values within the range of the \code{col} column.
#' @param cat.col [\code{character(1)}]\cr
#'   Column name for categorization.
#' @param keep [\code{logical(1)}]\cr
#'   Keep the source column \code{col}?
#'   Default is \code{TRUE}.
#' @param overwrite [\code{logical(1)}]\cr
#'   If \code{TRUE}, \code{cat.col} is set to \code{col}.
#' @return [\code{data.frame}]
#' df = data.frame(
#'   group = c("A1", "A1", "A2", "A2", "B1", "B2"),
#'   perf = runif(6),
#'   stringsAsFactors = FALSE)
#' df2 = categorize(df, col = "group", categories = list(A = c("A1", "A2"), B = c("B1", "B2")), cat.col = "group2")
#' @export
categorize = function(df, col, categories, cat.col, keep = TRUE, overwrite = FALSE) {
  assertDataFrame(df, min.rows = 1L, min.cols = 1L)
  assertChoice(col, choices = colnames(df))
  assertFlag(overwrite)
  if (!overwrite) {
    assertString(cat.col)
    if (cat.col %in% colnames(df))
      BBmisc::stopf("Column for categorization must not exist in data frame df if overwrite = FALSE.")
  } else {
    cat.col = col
  }

  assertList(categories, min.len = 1L, types = "character")
  assertFlag(keep)

  values = df[[col]]
  unique.values = unique(values)

  if (!is.character(values) & !is.factor(values))
    BBmisc::stopf("Can only categorize factor or character columns.")

  cat.names = names(categories)
  cat.values = unname(do.call(c, categories))

  # check if there are factors which are in neither category
  not.in.category = setdiff(unique.values, cat.values)
  if (length(not.in.category) > 0L) {
    # if such elements exist, make another "rest" category
    cat.names = c(cat.names, "rest")
    cat.values = unique.values
    categories[["rest"]] = not.in.category
  }

  # now do the categorization
  df[[cat.col]] = sapply(values, function(value) {
    idx = sapply(categories, function(category) {
      return(value %in% category)
    })
    return(cat.names[idx])
  })

  if (!keep & !overwrite)
    df[col] = NULL

  return(df)
}


#' @title Grouping helpers
#'
#' @description
#' Consider a data frame with results of multi-objective stochastic optimizers on
#' a set of problems from different categories/groups (say indicated by column \dQuote{group}).
#' Occasionally, it is useful to unite the results of several groups into a meta-group.
#' The function \code{addUnionGroup} aids in generation of such a meta-group while
#' function \code{addAllGroup} is a wrapper around the former which generates a
#' union of all groups.
#'
#' @param df [\code{data.frame}]\cr
#'   Data frame.
#' @param col [\code{character(1)}]\cr
#'   Column name of group-column.
#' @param group [\code{character(1)}]\cr
#'   Name for new group.
#' @param values [\code{character(1)}]\cr
#'   Subset of values within the value range of column \code{col}.
#' @return [\code{data.frame}] Modified data frame.
#' @examples
#' df = data.frame(
#'   group = c("A1", "A1", "A2", "A2", "B"),
#'   perf = runif(5),
#'   stringsAsFactors = FALSE)
#'
#' df2 = addUnionGroup(df, col = "group", group = "A", values = c("A1", "A2"))
#' df3 = addAllGroup(df, col = "group", group = "ALL")
#' @name addUnionGroup
#' @rdname addUnionGroup
#' @export
addUnionGroup = function(df, col, group, values) {
  assertDataFrame(df, min.cols = 1L, min.rows = 2L)
  assertChoice(col, choices = colnames(df))
  assertString(group)

  df.values = df[[col]]

  tmp = df[df.values %in% values, , drop = FALSE]
  tmp[[col]] = group

  return(rbind(df, tmp))
}

#' @rdname addUnionGroup
#' @export
addAllGroup = function(df, col, group = "all") {
  addUnionGroup(df, col, group, values = unique(as.character(df[[col]])))
}
