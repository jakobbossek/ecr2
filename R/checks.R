checkNumericMutatorArguments = function(ind, lower, upper, op.name) {
  checkmate::assertNumeric(ind, min.len = 1L, any.missing = FALSE, all.missing = FALSE)
  n = length(ind)
  checkmate::assertNumeric(lower, any.missing = FALSE, all.missing = FALSE)
  checkmate::assertNumeric(upper, any.missing = FALSE, all.missing = FALSE)
  if ((length(lower) != n) | (length(upper) != n))
    BBmisc::stopf("[ecr::%s] Arguments lower and upper need to have as many components as the individual.", op.name)
}
