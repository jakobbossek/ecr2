#FIXME: make test parameter
#FIXME: not all relevant tests share the same signature my.test(x, y, alternative)$p.value
# Document that this signature is needed.
#FIXME: results object should have attributes at least alpha, used test etc.
#FIXME: pairwise.wilcox.test
#FIXME: add docs
applyStatisticalTests = function(inds, ind.names, alpha = 0.05) {
  assertSubset(ind.names, choices = colnames(inds))
  assertNumber(alpha, lower = 0.0000001, upper = 1)

  probs = unique(inds$prob)
  algos = unique(inds$algorithm)

  n.probs = length(probs)
  n.algos = length(algos)

  res = list()
  for (prob in probs) {
    res[[prob]] = list()
    for (ind.name in ind.names) {
      p.mat = matrix(NA, nrow = n.algos, ncol = n.algos)
      rownames(p.mat) = colnames(p.mat) = algos
      for (i in 1:n.algos) {
        for (j in 1:n.algos) {
          ind.algo.i = inds[inds$prob == prob & inds$algorithm == algos[i], ind.name]
          ind.algo.j = inds[inds$prob == prob & inds$algorithm == algos[j], ind.name]
          p.mat[i, j] = wilcox.test(x = ind.algo.i, y = ind.algo.j, alternative = "greater")$p.value
        }
      }
      diag(p.mat) = NA
      res[[prob]][[ind.name]] = p.mat
    }
  }

  res = BBmisc::setAttribute(res, "alpha", alpha)
  return(res)
}
