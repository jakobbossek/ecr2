
makePopulation = function(inds, fitness = NULL, ...) {
  assertList(inds)
  n = length(inds)
  if (!is.null(fitness))
    assertMatrix(fitness, ncols = n, min.rows = 1L, any.missing = FALSE, all.missing = FALSE)

  pop = BBmisc::makeS3Obj("ecr_population",
    inds = inds,
    fitness = fitness
  )

  additional = list(...)
  if (length(additional) > 0L) {
    # check all elements to be vectors or lists of length n
    for (elem in additional) {
      if (is.list(elem)) {
        assertList(elem, len = n)
      }
      # FIXME: go on here
    }
  }
  return(pop)
}

print.ecr_population = function(x) {
  catf("%i individuals.", length(x))
  if (!is.null(x$fitness)) {
    catf("With fitness values.")
  }
}

"[.ecr_population" = function(x, i, ...) {
  return(x$population[[i]])
}
