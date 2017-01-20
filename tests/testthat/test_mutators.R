context("mutation operators (mutators)")

test_that("mutation operators working on permutation genes create valid offspring", {
  # defs
  s = 1:10L
  n.reps = 5L

  # gather all mutators for permutation representation
  available.mutators = c(
    setupSwapMutator, setupInversionMutator,
    setupInsertionMutator, setupScrambleMutator
  )

  test.ind = letters[3:13]

  # check validity of produced output for each permutation-based mutator
  for (mutatorGenerator in available.mutators) {
    mutate = mutatorGenerator() # no mutation control parameters to check here
    expect_true(isEcrOperator(mutate))
    expect_output(print(mutate), regexp = "Name")
    for (i in seq(n.reps)) {
      child = mutate(test.ind, NULL)
      expect_true(setequal(test.ind, child), info = sprintf("Mutator '%s' did not produce a valid
        permutation! Input: (%s), Output: (%s)", getOperatorName(mutate),
        collapse(test.ind), collapse(child)))
    }
  }
})

test_that("mutation operators working on real-numbered representation create valid offspring", {
  # defs
  n.reps = 5L

  available.mutators = c(setupUniformMutator, setupGaussMutator)

  for (mutatorGenerator in available.mutators) {
    mutate = mutatorGenerator()
    test.ind = runif(5L)
    params = list(lower = rep(0, 5), upper = rep(1, 5))
    for (i in seq(n.reps)) {
      child = mutate(test.ind, params)
      expect_true(all(child >= 0 && child <= 1), info = sprintf("Mutator '%s' did not stick to the
        box constraints! Input: (%s), Output: (%s)", getOperatorName(mutate), collapse(test.ind), collapse(child)))
    }
  }

})

test_that("mutation operators working on binary representation create valid offspring", {
  n.reps = 5L

  available.mutators = c(setupBitflipMutator)

  for (mutatorGenerator in available.mutators) {
    mutate = mutatorGenerator()
    test.ind = sample(c(0, 1), 10L, replace = TRUE)
    for (i in seq(n.reps)) {
      child = mutate(test.ind, params)
      expect_true(all(child %in% c(0, 1)), info = sprintf("Mutator '%s' did not produce {0,1}* string! Input: (%s), Output: (%s)",
        getOperatorName(mutate), collapse(test.ind), collapse(child)))
    }
  }
})
