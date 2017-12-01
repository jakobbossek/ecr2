#FIXME: ignores prob column
compareApproximations = function(df, obj.cols = c("f1", "f2"), algo.col = "algorithm", offset = 10) {
  assertDataFrame(df)

  # ref point approx
  pareto.approx = df[, obj.cols, drop = FALSE]
  ref.point = apply(pareto.approx, 2L, max)
  ref.point = ref.point + offset

  algos = unique(df[[algo.col]])
  n.algos = length(algos)

  # unary indicators
  unary.inds = do.call(rbind, lapply(algos, function(algo) {
    pfa = df[which(df[[algo.col]] == algo), obj.cols]
    data.frame(
      algorithm = algo,
      HV = ecr::computeHV(t(pfa), ref.point = ref.point),
      NND = sum(ecr::nondominated(t(pfa))),
      DISTINCT = sum(ecr::nondominated(t(pfa[!duplicated(pfa), , drop = FALSE])))
    )
  }))

  #algo.combn = combn(unique(df[[algo.col]]), 2L)
  eps.mat = matrix(NA, nrow = n.algos, ncol = n.algos)
  colnames(eps.mat) = rownames(eps.mat) = algos
  for (i in 1:n.algos) {
    for (j in 1:n.algos) {
      pfaA = df[which(df[[algo.col]] == algos[i]), obj.cols]
      pfaB = df[which(df[[algo.col]] == algos[j]), obj.cols]
      eps.mat[i, j] = ecr::emoaIndEps(t(pfaA), t(pfaB))
    }
  }

  return(list(
    unary = unary.inds,
    binary = list(epsilon = eps.mat)
  ))
}
