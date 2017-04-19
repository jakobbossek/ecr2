library(BBmisc)
library(devtools)
library(rpn)

load_all(".")

# Simple EA for R. Dawkin’s "METHINKS IT IS LIKE A WEASEL“ problem.
# See http://rosettacode.org/wiki/Evolutionary_algorithm for details.

# reproducibility
set.seed(1)

# optimal value. I.e., the target string.
target = unlist(strsplit("METHINKS IT IS LIKE A WEASEL", ""))

# character set the EA works on
char.set = c(LETTERS, " ")

# no smoof function since we use a custom representation here
# ("string functions" not a default genotype)
fitness.fun = function(string, target) {
  sum(string != target) / length(target)
}

# helper function to create a mutator for strings, i.e., character vectors.
# It works by selecting genes, i.e., string positions, with a low probability p,
# and replacing these with random values from the char.set.
mutString = makeMutator(
  mutator = function(ind, p) {
    # determine which genes to mutate
    idx = which(runif(length(ind)) < p)
    n.mut = length(idx)
    # perform mutation
    if (n.mut > 0)
      ind[idx] = sample(char.set, n.mut, replace = TRUE)
    return(ind)
  },
  supported = "custom" # non-standard representation
)

# here we generate the initial population by hand (no generator object)
initial.solutions = list(sample(char.set, length(target), replace = TRUE))

res = ecr(fitness.fun, n.objectives = 1L, minimize = TRUE,
  representation = "custom",
  mu = 1L, lambda = 100L,
  mutator = setup(mutString, p = 0.05),
  initial.solutions = initial.solutions,
  terminators = list(stopOnIters(200L)),
  # further params for the fitness function
  target = target)


print("Best parameter found:")
print(paste(res$best.x[[1L]], collapse = ""))
