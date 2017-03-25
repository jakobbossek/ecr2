context("Evolutionary Multi-Objective Algorithms")

test_that("preimplemented EMOAs work well", {

  #FIXME: is something like this in BBmisc? Needed that multiple times now.
  printList = function(l) {
    ns = names(l)
    pairs = sapply(ns, function(n) {
      paste0(n, l[[n]], sep = "=")
    })
    collapse(pairs, ", ")
  }

  expect_is_pareto_approximation = function(pf, n.obj, algo, prob, algo.pars) {
    info.suffix = sprintf("Algo '%s' failed on problem '%s' with params '%s'",
      algo, prob, printList(algo.pars))
    expect_equal(nrow(pf), length(which.nondominated(t(pf))), info = paste0(info.suffix, "Not all returned points are nondominated."))
    expect_data_frame(pf, any.missing = FALSE, all.missing = FALSE, ncols = n.obj, types = "numeric", info = paste0(info.suffix, "Not all returned points are numeric."))
    expect_equal(ncol(pf), n.obj, info = paste0(info.suffix, "Number of columns is not equal to the number of objectives."))
  }

  fns = list(
    zdt1 = smoof::makeZDT1Function(dimensions = 2L),
    zdt2 = smoof::makeZDT2Function(dimensions = 3L)
  )
  max.evals = 200L

  # test NSGA-II
  for (mu in c(5, 10, 15)) {
    for (fn in names(fns)) {
      res = nsga2(
        fitness.fun = fns[[fn]],
        n.population = mu,
        n.offspring = 5L,
        lower = getLowerBoxConstraints(fns[[fn]]),
        upper = getUpperBoxConstraints(fns[[fn]]),
        terminators = list(stopOnEvals(max.evals))
      )
      expect_is_pareto_approximation(res$pareto.front, getNumberOfObjectives(fns[[fn]]), "nsga2", fn,
        list(mu = mu, lambda = 5L, max.evals = max.evals)
      )
    }
  }

  # test SMS-EMOA
  for (mu in c(5, 10, 15)) {
    for (fn in names(fns)) {
      res = smsemoa(
        fitness.fun = fns[[fn]],
        n.population = mu,
        n.offspring = 5L,
        ref.point = rep(11, getNumberOfObjectives(fns[[fn]])),
        lower = getLowerBoxConstraints(fns[[fn]]),
        upper = getUpperBoxConstraints(fns[[fn]]),
        terminators = list(stopOnEvals(max.evals))
      )
      expect_is_pareto_approximation(res$pareto.front, getNumberOfObjectives(fns[[fn]]), "smsemoa", fn,
        list(mu = mu, lambda = 5L, max.evals = max.evals)
      )
    }
  }

  # test Aspiration-Set EMOA (AS-EMOA)
  fitness.fun = makeZDT3Function(dimensions = 2L)
  for (mu in c(10, 20)) {
    aspiration.set = matrix(
      c(0.2, 0.25,
        0.21, 0.2,
        0.18, 0.4), ncol = 3L, byrow = FALSE
    )
    res = asemoa(fitness.fun, mu = mu, aspiration.set = aspiration.set,
      lower = getLowerBoxConstraints(fitness.fun), upper = getUpperBoxConstraints(fitness.fun))
    expect_is_pareto_approximation(res$pareto.front, 2L, "asemoa", "ZDT3",
      list(mu = mu, n.archive = ncol(aspiration.set)))
  }
})
