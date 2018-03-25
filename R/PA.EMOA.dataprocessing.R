#FIXME: test
#FIMXE: docs
#FIXME: col.names must not be already in df
explode = function(df, col, by = ".", keep = FALSE, col.names = NULL) {
  assertDataFrame(df, min.cols = 1L, min.rows = 1L)
  assertChoice(col, choices = colnames(df))
  assertString(by)
  assertFlag(keep)

  if (is.factor(df[[col]]))
    as.character(df[[col]])

  #FIXME: ordering may get lost
  #FIXME: maybe better simply move this to tail of function and do not rely on on.exit
  if (keep)
    on.exit(as.factor(df[[col]]))

  col.exploded = strsplit(df[[col]], split = by, fixed = TRUE)
  lengths = sapply(col.exploded, length)

  if (length(unique(lengths)) != 1L)
    stopf("explodeBy: not all exploded cells have the same length")

  if (length(col.names) != lengths[1L])
    stopf("explodeBy: col.names must contain as many elements as exploded cells.")

  df.new.cols = do.call(rbind, col.exploded)
  colnames(df.new.cols) = col.names

  if (!keep)
    df[[col]] = NULL

  df = cbind(df, df.new.cols)
  return(df)
}

#FIXME: check what happens if keep = FALSE and all cols are imploded
#FIXME: test
implode = function(df, cols, sep = ".", keep = FALSE, col.name) {
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
  imploded = apply(sel.df, 1L, BBmisc::collapse, sep = sep)
  imploded = data.frame(x = imploded)
  colnames(imploded) = col.name

  # and append
  if (!keep)
    df[cols] = NULL
  df = cbind(df, imploded)
  return(df)
}
