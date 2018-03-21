rbind.ecr_result_compilation = function(x) {
  catf("==============================")
  stats = lapply(x, getStatistics)
  runs = length(stats)
  gens = sapply(stats, nrow)
  print(runs)
  print(gens)

  res = do.call(rbind, stats)
  res$repl = rep(seq_len(runs), gens)
  return(res)
}

toGG.ecr_result_compilation = function(x, drop.stats = character(0L)) {
  stats = lapply(x, toGG, drop.stats = drop.stats)
  nruns = length(stats)
  gg = lapply(seq_len(nruns), function(run) {
    stats[[run]]$repl = run
    return(stats[[run]])
  })
  do.call(rbind, gg)
}

plotStatistics.ecr_result_compilation = function(x, drop.stats = character(0L)) {
  plotStatistics(toGG(x, drop.stats = drop.stats))
}
