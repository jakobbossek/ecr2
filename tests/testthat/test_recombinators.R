context("recombination operators (recombinators)")

test_that("recombinators for permutations work as expected", {
  # defs
  n = 10L # permutation length
  n.reps = 5L # number of repetitions
  n.bits = 10L
  n.dim = 10L
  lower = rep(-1, 10)
  upper = rep(0, 10)
  perm.set = 10L # sequence we expect offspring to be a permutation of
  params = list(lower = lower, upper = upper) # needed for some recombinators

  getBinaryParents = function(n.parents = 2L) {
    genBin(n.parents, n.bits)
  }

  getPermutationParents = function(n.parents = 2L) {
    genPerm(n.parents, perm.set)
  }

  getRealValuedParents = function(n.parents = 2L) {
    genReal(n.parents, n.dim, lower, upper)
  }

  parent.mapper = list("binary" = getBinaryParents, "permutation" = getPermutationParents,
    "float" = getRealValuedParents)

  # check validity of produced output for each permutation-based recombinator
  available.recombinators = c(setupPMXRecombinator, setupOXRecombinator,
    setupCrossoverRecombinator, setupIntermediateRecombinator) # setupSBXRecombinator

  for (recombinatorGenerator in available.recombinators) {
    recombine = recombinatorGenerator()
    representations = getSupportedRepresentations(recombine)

    # check type
    expect_true(isEcrOperator(recombine))

    # check output
    expect_output(print(recombine), regexp = "ECR2 OPERATOR")

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
            "%i-th offspring ('%s') is not in bounds for operator.",
            j,
            collapse(children[[j]])))
          } else if (representation == "binary") {
            expect_true(setequal(c(0, 1), children[[j]]), info = sprintf(
            "%i-th offspring ('%s') is not a {0,1}* sequence for operator.",
            j,
            collapse(children[[j]])))
          } else if (representation == "permutation") {
            expect_true(setequal(seq_len(perm.set), children[[j]]), info = sprintf(
            "%i-th offspring ('%s') is not a permutation of sequence (%s) for operator.",
            j,
            collapse(children[[j]]),
            collapse(perm.set)))
          }
        }
      }
    }
  }
})
