set.seed(123)

makeOneMinFunction = function(dimensions) {
  assertInteger(dimensions, len = 1L, lower = 2L, upper = 100L)
  smoof::makeSingleObjectiveFunction(
    fn = function(x) {
      sum(x)
    },
    par.set = makeParamSet(
      makeIntegerVectorParam("x", lower = 0L, upper = 1L, len = dimensions)
    ),
    name = "OneMin"
  )
}
