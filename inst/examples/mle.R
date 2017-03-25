library(devtools)

load_all(".")

# reproducibility
set.seed(1)

lambda = 5
x = rexp(100L, rate = lambda)

fitness.fun = function(lambda) {
  val = sum(dexp(x, rate = lambda, log = TRUE))
  return(-val)
}

res = ecr(fitness.fun = fitness.fun, representation = "float",
  n.objectives = 1L, n.dim = 1L, lower = 0.0001, upper = 10,
  mu = 10L, lambda = 10L,
  mutator = setupGaussMutator(lower = 0.0001, upper = 10),
  survival.strategy = "comma", n.elite = 4L,
  terminators = list(stopOnIters(100L)))

#pdf(file = "ml_estimator.pdf", width = 8, height = 5)
hist(x, freq = FALSE)
curve(dexp(x, lambda), add = TRUE, lty = 3)
curve(dexp(x, res$best.x[[1L]]), add = TRUE, lty = 2, col = "red")
legend(x = 0.8, y = 3,
  legend = c(expression(lambda), expression(lambda[ML])),
  lty = c(3,2), col = c("black", "red")
)
#dev.off()

#library(ggplot2)
#pl = qplot(x, binwidth = 0.2)
