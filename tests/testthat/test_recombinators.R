context("recombination operators (recombinators)")

test_that("recombinators for permutations work as expected", {
  # defs
  n = 10L # permutation length
  n.reps = 5L # number of repetitions
  n.bits = 10L
  n.dim = 10L
  lower = rep(-1, 10)
  upper = rep(0, 10)
  perm.set = LETTERS[1:10] # sequence we expect offspring to be a permutation of
  params = list(lower = lower, upper = upper) # needed for some recombinators

  getBinaryParents = function(n.parents = 2L) {
    replicate(n.parents, sample(c(0, 1), n.bits, TRUE), simplify = FALSE)
  }

  getPermutationParents = function(n.parents = 2L) {
    replicate(n.parents, sample(perm.set), simplify = FALSE)
  }

  getRealValuedParents = function(n.parents = 2L) {
    fn = setupUniformGenerator(n.dim, lower, upper)
    fn(n.parents)
  }

  parent.mapper = list("binary" = getBinaryParents, "permutation" = getPermutationParents,
    "float" = getRealValuedParents)

  # check validity of produced output for each permutation-based recombinator
  available.recombinators = c(setupPMXRecombinator, setupOXRecombinator,
    setupCrossoverRecombinator, setupSBXRecombinator, setupIntermediateRecombinator)

  for (recombinatorGenerator in available.recombinators) {
    recombine = recombinatorGenerator()
    representations = getSupportedRepresentations(recombine)

    # check type
    expect_true(isEcrOperator(recombine))

    # check output
    expect_output(print(recombine), regexp = "Name")

    for (representation in representations) {
      parentMaker = parent.mapper[[representation]]

      # generate samples
      for (i in seq(n.reps)) {
        parents = parentMaker(n.parents = 2L)
        children = recombine(parents, params)
        # check that the child is actually a permutation
        for (j in seq(length(children))) {
          if (representation == "float") {
            expect_true(all(children[[j]] >= lower & children[[j]] <= upper), info = sprintf(
            "%i-th offspring ('%s') is not in bounds for operator '%s'.",
            j,
            collapse(children[[j]]),
            getOperatorName(recombine)))
          } else if (representation == "binary") {
            expect_true(setequal(c(0, 1), children[[j]]), info = sprintf(
            "%i-th offspring ('%s') is not a {0,1}* sequence for operator '%s'",
            j,
            collapse(children[[j]]),
            getOperatorName(recombine)))
          } else if (representation == "permutation") {
            expect_true(setequal(perm.set, children[[j]]), info = sprintf(
            "%i-th offspring ('%s') is not a permutation of sequence (%s) for operator '%s'",
            j,
            collapse(children[[j]]),
            collapse(perm.set),
            getOperatorName(recombine)))
          }
        }
      }
    }
  }
})
