applyStatisticalTests = function(inds, ind.names) {
  assertSubset(ind.names, choices = colnames(inds))

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
  return(res)
}



