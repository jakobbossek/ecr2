context("PA data preprocessing")

test_that("Column explosion and implosion", {
  n.rows = 7L
  df = data.frame(
    c1 = c("a", "a", "b", "c", "b", "c", "a"),
    c2 = c("x", "y", "x", "y", "x", "x", "y"),
    i1 = sample(1:12, size = n.rows, replace = TRUE),
    n1 = runif(n.rows),
    n2 = rnorm(n.rows),
    stringsAsFactors = FALSE)

  # now glue stuff together
  glue.cols = c("c1", "c2", "i1")
  sep = "---"
  glue1 = ecr::implode(df, cols = glue.cols, by = sep, keep = FALSE, col.name = "glued")
  # check if glued cols are dropped
  expect_true(length(intersect(glue.cols, colnames(glue1))) == 0)
  expect_character(glue1$glued, pattern = ".*---.*---.*")

  # now inverse operation and check for equality
  df2 = ecr::explode(glue1, col = "glued", by = sep, col.names = glue.cols)
  df2$i1 = as.numeric(df2$i1)
  expect_true(all(df$c1 == df2$c1))
  expect_true(all(df$c2 == df2$c2))
  expect_true(all(df$i1 == df2$i1))
})

test_that("Grouping by character/factor levels", {
  n.rows = 7L
  df = data.frame(
    c1 = c("a", "a", "b", "c", "b", "c", "a"),
    c2 = c("x", "y", "x", "y", "x", "x", "y"),
    i1 = sample(1:12, size = n.rows, replace = TRUE),
    n1 = runif(n.rows),
    n2 = rnorm(n.rows),
    stringsAsFactors = FALSE)

  df2 = addUnionGroup(df, col = "c1", group = "group", values = c("a", "b"))
  expect_true(nrow(df2[df2$c1 == "group", , drop = FALSE]) == nrow(df[df$c1 %in% c("a", "b"), , drop = FALSE]))

  df2 = addAllGroup(df, col = "c1", group = "group")
  expect_true(nrow(df2) == 2 * nrow(df))

  df3 = categorize(df, col = "c1", categories = list("X" = c("a", "c")), cat.col = "c3")
  expect_true(all(df3$c3 %in% c("X", "rest")))
  expect_equal(ncol(df3), ncol(df) + 1L)
})
