context("selection operators (selector)")
test_that("selection operators create valid selections for single-objective
          optimizations", {
  fitness.fun = function(x)sum(x)
  #variables
  MU = 25; LAMBDA = 30; N.BITS = 50; n.select = 10
  population = genBin(MU, N.BITS)

  # setup control
  ctrl = initECRControl(fitness.fun, n.objectives = 1L, minimize = FALSE)
  fitness = evaluateFitness(ctrl, population)

  for (selector in c(selGreedy, selSimple, selTournament, selRoulette, selRanking)){
    sel  = selector(fitness, n.select)
    expect_integer(sel)
    expect_length(sel, n.select)
  }
})

# test_that("selection operators create valid selections for multi-objective
#           optimizations", {

#             fitness.fun = function(x) {c(min(dist(x)),max(sum(x)))}
#             #variables
#             MU = 25; LAMBDA = 30; N = 20; n.select = 10
#             population = gen(matrix(runif(N*2),ncol = 2), MU)

#             # setup control
#             ctrl = initECRControl(fitness.fun, n.objectives = 2L, minimize = FALSE)
#             fitness = evaluateFitness(ctrl, population)

#             for (selector in c(setup(selDomHV,ref.point = c(N*2.5,N*2.5)),selNondom)){
#               sel  = selector(fitness, n.select)
#               print(sel)
#               expect_integer(sel)
#               #expect_length(sel, n.select) temporary disabled due to issue #106
#             }



#           })
