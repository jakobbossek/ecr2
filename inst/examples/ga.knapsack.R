library(plyr)
library(reshape2)

load_all(".")

# reproducability
set.seed(1)

# SOLVING THE 0-1-KNAPSACK PROBLEM WITH A GENETIC ALGORITHM
# =========================================================
# Given a knapsack with a weight capacity limit and a set of n objects with each
# a weight and a value, we aim to fill the knapsack with objects in a way that
# the capacity limit is respected and the sum of values is maximized.

# knapsack with 10 objects A, B, ..., J with weight w_i and value v_i
ks = data.frame(
  object = LETTERS[1:10],
  weight = c(10, 11, 4, 2, 2, 9, 3, 5, 15, 4),
  value  = c(5, 10, 1, 6, 3, 5, 3, 19, 23, 1)
)

# knapsack capacity
ks.limit = 25L

# objective function to be maximized, i.e., total value of bagged objects,
# under the restriction of the total weight being lower than the capacity.
fitness.fun = function(x) {
  val = sum(x * ks$value)
  weight = sum(x * ks$weight)
  if (weight > ks.limit)
    return(0) # penalty for limit violation
  return(val)
}

# use "natural" binary representation x = (x_1, ..., x_n) with x_i = 1 means,
# that object i is bagged and x_i = 0 otherwise.
res = ecr(fitness.fun, n.objectives = 1L, minimize = FALSE,
  representation = "binary", n.bits = nrow(ks),
  mu = 25L, lambda = 10L, survival.strategy = "plus",
  terminators = list(stopOnIters(100L)))

# extract EA knapsack solution
print(res$best.y)
print(res$best.x)
print(ks[as.logical(res$best.x[[1L]]), ])
