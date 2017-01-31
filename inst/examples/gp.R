library(ecr)
library(BBmisc)
library(devtools)
library(rpn)

load_all()

set.seed(123)

# polynomial we aim to approximate
target.fun = function(x) {
  #x^4 - x^3 + 2 * x^2 + 5
  x^4 + x^3 - 2 * x^2 + x + 10
}

lower = -5
upper = 5

# We do regression here. Thus we need a set of points and its values (x_i, y_i),
# i = 1, ..., n. Here we generate an initial design by creating a equidistant
# sequence of x values with subsequent noise addition.
xs = jitter(seq(lower + 0.2, upper - 0.2, by = 1), amount = 0.2)
design = data.frame(x = xs, y = target.fun(xs))
print(design)

# The objective function expects an individual, lower and upper bounds of the
# interval we aim to find a nice approximation in and the design itself.
# The objective function computes the sum of squared distances between the
# true function values f(x_i) and the model approximations m(x_i).
obj.fun = function(obj, lower.bounds, upper.bounds, design) {
  approx.fun = function(x) {
    # call rpn interpreter to get infix notation
    infix = rpn(obj, eval = FALSE)$infix
    eval(parse(text = infix))
  }

  # residual sum of squares
  rss = sum((design$y - sapply(design$x, function(x) {
    approx.fun(x)
  }))^2)
  return(rss)
}

# Recursively generated a random expression in reverse polish notation.
makeRandomExpression = function(depth = 1L) {
  nonterm = c("+", "-", "*")
  if (runif(1) < 0.3 || depth == 4) {
    ex = c(sample(c("x", as.character(round(runif(1L, 0, 10)))), 1L))
  } else {
    op = sample(nonterm, 1L)
    ex = c("(", makeRandomExpression(depth + 1L), makeRandomExpression(depth + 1L), op, ")")
  }
  return(ex)
}

# Initializes a population by generating random reverse polish notation expressions.
generator = makeGenerator(
  generator = function(size, par.list) {
    lapply(1:size, function(x) makeRandomExpression())
  },
  name = "RPN creator",
  description = "Randomly generates RPN expressions.",
  supported = "custom"
)

# Generates a mutator, which randomly selects non-terminal elements and replaces
# them with random reverse polish notation expressions.
mutator = makeMutator(
  mutator = function(ind, par.list) {
    if (runif(1L) < 1) {
      # sample until we find a (non-)terminal element and skip all comma symbols
      poss = which(!(ind %in% c("(", ")")))
      n = length(ind)
      pos = sample(poss, 1L)
      el = ind[pos]
      #catf("Replacing element at pos %i: %s", pos, el)
      if (el %nin% c("+", "-", "*", "%")) {
        left = if (pos == 1) c() else ind[1:(pos - 1)]
        right = if (pos == n) c() else ind[(pos + 1):n]
      } else if (el %in% c("+", "-", "*", "%")) {
        # replace binary operator (op X Y)
        # I.e. search for second bracket
        pos.brr = pos + 1L
        pos2 = pos.brr
        open.brackets = 1L
        while (open.brackets > 0L) {
          #catf("Pos %i of %i (ob: %i)", pos2, n, open.brackets)
          pos2 = pos2 - 1L
          if (ind[pos2] == "(") {
            open.brackets = open.brackets - 1L
          } else if (ind[pos2] == ")") {
            open.brackets = open.brackets + 1L
          }
        }
        pos.brl = pos2
        right = if (pos.brr == n) c() else ind[(pos.brr + 1):n]
        left = if (pos.brl == 1) c() else ind[1:(pos.brl - 1)]
      }
      ind = c(left, makeRandomExpression(), right)
    }
    return(ind)
  },
  supported = "custom",
  name = "Expression Mutation",
  description = "Randomly replace "
)

# run the GP
set.seed(123)
res = ecr(fitness.fun = obj.fun, n.objectives = 1L,
  representation = "custom",
  mu = 25L, lambda = 25L,
  survival.strategy = "comma", n.elite = 1L,
  mutator = mutator, generator = generator,
  survival.selector = setupGreedySelector(),
  terminators = list(stopOnIters(500L)),
  lower.bounds = lower, upper.bounds = upper, design = design
)

# Get the infix notation of the best parameter found.
print(rpn(res$best.x[[1L]]))

approx.fun = function(x) {
  sapply(x, function(y) rpn(res$best.x[[1L]], vars = list(x = y))$value)
}

# visualize true function and approximation
pdf("symbolic_regression.pdf", width = 8, height = 5)
curve(target.fun(x), lower, upper, ylim = c(-100, 1000))
curve(approx.fun(x), lower, upper, add = TRUE, col = "blue")
points(design$x, design$y, col = "black")
legend(x = -4.8, y = 1000,
  legend = c(expression(f(x)), expression(hat(f)(x))),
  col = c("black", "blue"), lty = c(1, 1)
)
dev.off()

