library(plyr)
library(reshape2)

set.seed(1)

load_all(".")

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
    return(0)
  return(-val)
}

# use "natural" binary representation x = (x_1, ..., x_n) with x_i = 1 means,
# that object i is bagged and x_i = 0 otherwise.
res = ecr(fitness.fun, n.objectives = 1L,
  mu = 25L, lambda = 10L, survival.strategy = "plus",
  representation = "binary", n.bits = nrow(ks),
  terminators = list(stopOnIters(100L)))

# extract EA knapsack solution
print(-res$best.y)
print(res$best.x)
print(ks[as.logical(res$best.x[[1L]]), ])

# plot optimization path
# ...
