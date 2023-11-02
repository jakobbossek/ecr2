library(devtools)

load_all(".")

# SMS-EMOA(s) EXAMPLES
# ===
# [BNE07] Reference. Beume, Nicola, Boris Naujoks and M. Emmerich. SMS-EMOA: Multiobjective selection
#'based on dominated hypervolume.” European Journal of Operational Research. 181 (2007): 1653-1669.
#
# NOTE: the parent selection is always random selection as this is not
# specified in the above paper. If you want something with more selection pressure
# you need to implement it yourself.

# 2-D Test function
fn = smoof::makeDTLZ2Function(dimensions = 2L, n.objectives = 2L)

# extract necessary variables
lower = smoof::getLowerBoxConstraints(fn)
upper = smoof::getUpperBoxConstraints(fn)
ref.point = smoof::getRefPoint(fn)

# ALGORITHM 1 FROM [BNE07] with survival strategy from Algorithm 2
# ===
# This is also implemented in ecr::smsemoa, but I suggest to use it this way
# since it is more flexible
tt = system.time({
res = ecr(fitness.fun = fn,
    mu = 50L, lambda = 1L, representation = "float", survival.strategy = "plus",
    parent.selector = selSimple,
    mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = lower, upper = upper),
    recombinator = setup(recSBX, eta = 15, p = 0.7, lower = lower, upper = upper),
    terminators = list(stopOnIters(100000L)),
    survival.selector = setup(selDomHV, ref.point = ref.point))
})
plot(res$pareto.front)



# ALGORITHM 1 FROM [BNE07] with survival strategy Algorithm 3
# ===
# Essentially the same code, but the survival selection operator works slightly
# different (see Algorithm 3 in [BNE07]).
tt = system.time({
res = ecr(fitness.fun = fn,
    mu = 50L, lambda = 1L, representation = "float", survival.strategy = "plus",
    parent.selector = selSimple,

    mutator = setup(mutPolynomial, eta = 25, p = 0.2, lower = lower, upper = upper),
    recombinator = setup(recSBX, eta = 15, p = 0.7, lower = lower, upper = upper),
    terminators = list(stopOnIters(100000L)),
    #survival.selector = selNondom)
    survival.selector = setup(selDomNumberPlusHV, ref.point = ref.point)) # here is the difference :)
})
plot(res$pareto.front)

# approx = t(as.matrix(res$pareto.front))
# size = ncol(approx) - 1L
# R = 100000

# tt = system.time({
# res = replicate(R, selDomNumberPlusHV(approx, size, ref.point = c(100,100)))
# })
