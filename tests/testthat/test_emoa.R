context("Evolutionary Multi-Objective Algorithms")

test_that("preimplemented EMOAs work well", {

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
        mu = mu,
        lambda = 5L,
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
  fitness.fun = makeZDT2Function(dimensions = 2L)
  for (mu in c(50)) {
    aspiration.set = matrix(
      c(0.2, 0.75,
        0.37, 0.6,
        0.5, 0.5), ncol = 3L, byrow = FALSE
    )
    res = asemoa(fitness.fun, mu = mu, aspiration.set = aspiration.set,
      lower = getLowerBoxConstraints(fitness.fun), upper = getUpperBoxConstraints(fitness.fun),
      terminators = list(stopOnIters(250L)))
    expect_is_pareto_approximation(res$pareto.front, 2L, "asemoa", "ZDT2",
      list(mu = mu, n.archive = ncol(aspiration.set)))
  }
})
