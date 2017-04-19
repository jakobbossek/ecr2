library(methods)
library(testthat)
library(devtools)
library(BBmisc)
library(ggplot2)

load_all(".")

# EA FOR THE GENERATION OF A POINT CLOUD MAXIMIZING THE MINIMAL
# DISTANCE BETWEEN POINTS (RESEMBLES A MAXIMIN-LATIN-HYPERCUBE-SAMPLING)
# ======================================================================
# This example illustrates custom representations. Here an individual is
# a (N x 2) matrix.

# reproducability
set.seed(1)

# defs
N = 30L

# we aim to maximize the minimal distance
fitness.fun = function(x) {
  min(dist(x))
}

# "Point-Replacement mutation"
# Replace each points with a low probability with a random point
# in [0,1] x [0,1]
mutPointReplace = makeMutator(
  mutator = function(ind) {
    idx = which(runif(nrow(ind)) < 0.1)
    ind[idx, ] = matrix(runif(2 * length(idx)), ncol = 2)
    return(ind)
  },
  supported = "custom"
)

MU = 50L
LAMBDA = 50L
N.ELITE = 10L
MAX.ITER = 1000L

control = initECRControl(fitness.fun, n.objectives = 1L, minimize = FALSE)
control = registerECROperator(control, "mutate", mutPointReplace)
control = registerECROperator(control, "selectForSurvival", selGreedy)

population = gen(matrix(runif(N * 2L), ncol = 2L), MU)
fitness = evaluateFitness(control, population)

log = initLogger(control, init.size = MAX.ITER + 1L)
updateLogger(log, population, fitness, n.evals = LAMBDA)

for (i in seq_len(MAX.ITER)) {
  offspring = population
  offspring = mutate(control, offspring, p.mut = 0.8)
  fitness.o = evaluateFitness(control, offspring)

  sel = replaceMuCommaLambda(control, population, offspring,
    fitness, fitness.o, n.elite = N.ELITE)
  population = sel$population
  fitness = sel$fitness
  updateLogger(log, population, fitness, n.evals = LAMBDA)
}

pl = plotStatistics(log)
pl = pl + theme(legend.position = "top")
print(pl)
ggsave("ea_lhs_statistics.pdf", width = 6, height = 3)

lhs.des =  population[[which.max(fitness)]]
unif.des = matrix(runif(N * 2L), ncol = 2L)

des = rbind(unif.des, lhs.des)
des = as.data.frame(des)
names(des) = c("x1", "x2")
des$Type = rep(c("UNIFORM", "MAXIMIN-LHS"), each = N)

pl = ggplot(des, aes(x = x1, y = x2)) + geom_point() + facet_grid(. ~ Type)
pl = pl + xlab("") + ylab("")
print(pl)
ggsave("designs.pdf", width = 6, height = 3)



res = ecr(fitness.fun = fitness.fun, n.objectives = 1L, minimize = FALSE,
  representation = "custom",
  mu = MU, lambda = LAMBDA, survival.strategy = "plus",
  initial.solutions = gen(matrix(runif(N * 2L), ncol = 2L), MU),
  mutator = mutPointReplace,
  terminators = list(stopOnIters(MAX.ITER)))

plot(res$best.x[[1L]])
