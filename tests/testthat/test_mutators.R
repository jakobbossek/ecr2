context("mutation operators (mutators)")

test_that("mutation operators working on permutation genes create valid offspring", {
  # defs
  s = 1:10L
  n.reps = 5L

  # gather all mutators for permutation representation
  available.mutators = c(
    mutSwap, mutInversion, mutInsertion, mutScramble, mutJump
  )

  test.ind = letters[3:13]

  # check validity of produced output for each permutation-based mutator
  for (mutate in available.mutators) {
    for (i in seq(n.reps)) {
      child = mutate(test.ind)
      expect_true(setequal(test.ind, child), info = sprintf("Mutator '%s' did not produce a valid
        permutation! Input: (%s), Output: (%s)", "mutate",
        collapse(test.ind), collapse(child)))
    }
  }
})

test_that("mutation operators working on real-numbered representation create valid offspring", {
  # defs
  n.reps = 5L

  available.mutators = c(ecr::setup(mutGauss, lower = rep(0, 5), upper = rep(1, 5)),
    ecr::setup(mutUniform, lower = rep(0, 5), upper = rep(1, 5)))

  for (mutate in available.mutators) {
    test.ind = runif(5L)
    for (i in seq(n.reps)) {
      child = mutate(test.ind)
      expect_true(all((child >= 0) & (child <= 1)), info = sprintf("Mutator '%s' did not stick to the
        box constraints! Input: (%s), Output: (%s)", "mutate", collapse(test.ind), collapse(child)))
    }
  }

})

test_that("mutation operators working on binary representation create valid offspring", {
  n.reps = 5L

  available.mutators = c(mutBitflip)

  for (mutate in available.mutators) {
    test.ind = sample(c(0, 1), 10L, replace = TRUE)
    for (i in seq(n.reps)) {
      child = mutate(test.ind)
      expect_true(all(child %in% c(0, 1)), info = sprintf("Mutator '%s' did not produce {0,1}* string! Input: (%s), Output: (%s)",
        "mutate", collapse(test.ind), collapse(child)))
    }
  }
})
