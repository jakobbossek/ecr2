summarize = function(df, sum.funs, ...) {
  assertDataFrame(df)
  for (sum.fun in sum.funs) {
    assertFunction(sum.fun)
  }


}
