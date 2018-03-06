applyStatisticalTest = function(df, indicator) {
  assertDataFrame(df)

  res = by(df, list(df$prob), function(x) {
    ind.vals = x[, c("algorithm", indicator), drop = FALSE]
    #print(ind.vals)
    kruskal.test(x = ind.vals[[indicator]], g = ind.vals$algorithm)$p.value
  })
  res = unclass(res)
  attributes(res) = NULL
  print(res)

}
