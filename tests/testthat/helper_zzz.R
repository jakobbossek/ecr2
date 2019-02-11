set.seed(1)

ONEMIN = function(x) {
  sum(x)
}

ONEMAX = function(x) {
  length(x) - sum(x)
}

makeOneMinFunction = function(dimensions) {
  assertInteger(dimensions, len = 1L, lower = 2L, upper = 100L)
  smoof::makeSingleObjectiveFunction(
    fn = function(x) {
      sum(x)
    },
    par.set = makeParamSet(
      makeIntegerVectorParam("x", lower = 0L, upper = 1L, len = dimensions)
    ),
    name = "OneMin"
  )
}

expect_is_pareto_approximation = function(pf, n.obj, algo, prob, algo.pars) {
  info.suffix = sprintf("Algo '%s' failed on problem '%s' with params '%s'",
    algo, prob, printList(algo.pars))
  expect_equal(nrow(pf), length(which.nondominated(t(pf))), info = paste0(info.suffix, "Not all returned points are nondominated."))
  expect_data_frame(pf, any.missing = FALSE, all.missing = FALSE, ncols = n.obj, types = "numeric", info = paste0(info.suffix, "Not all returned points are numeric."))
  expect_equal(ncol(pf), n.obj, info = paste0(info.suffix, "Number of columns is not equal to the number of objectives."))
}

#FIXME: is something like this in BBmisc? Needed that multiple times now.
printList = function(l) {
  ns = names(l)
  pairs = sapply(ns, function(n) {
    paste0(n, l[[n]], sep = "=")
  })
  collapse(pairs, ", ")
}
