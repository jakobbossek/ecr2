context("test logger")

test_that("logger keeps track the right way in single-objective case", {
  # dummy control
  control = initECRControlBinary(function(x) sum(x), minimize = TRUE, n.objectives = 1L,
    n.bits = 10L)

  evals.per.iter = 10L
  n.iters = 20L
  log.stats = c("min", "median", "ncol", "myRange" = function(x) max(x) - min(x))

  log = initLogger(control,
    log.stats = log.stats,
    log.pop = TRUE,
    init.size = 10L)

  for (i in seq.int(n.iters)) {
    population = replicate(sample(c(0, 1), 10L, replace = TRUE), n = evals.per.iter, simplify = FALSE)
    fitness = matrix(sapply(population, sum), nrow = 1L)
    updateLogger(log, population, fitness, n.evals = evals.per.iter)
  }

  # now check that stuff
  stats = getLoggedStats(log)
  pops  = getLoggedPopulations(log)

  # check that stats is a data.frame
  assertDataFrame(stats, nrows = n.iters, ncols = length(log.stats) + 1L,
    any.missing = FALSE, all.missing = FALSE)

  # check for logged stats
  expected.stats = c("gen", "min", "median", "ncol", "myRange")
  expect_set_equal(colnames(stats), expected.stats)

  # check that all are numeric
  for (stat in expected.stats) {
    expect_numeric(stats[[stat]], info = sprintf("Doh! Stat %s is not numeric: %s",
      stat, collapse(stats[[stat]])))
  }

  expect_length(pops, n.iters)
  expect_true(!all(sapply(pops, is.null)))
})
