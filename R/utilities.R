# @title
#  Assert same dimensions.
#
# @description
#   We frequently need to check whether the dimension of an approximation set is
#   equal to the dimension of the ideal point and/or nadir point. This does exactly
#   that assertion for an arbitrary number of arguments.
#
# @param ... [any]
#   Vectors or matrizes.
# @return Nothing. Stops if not all dimensions are equal.
assertSameDimensions = function(...) {
  xs = list(...)
  dims = sapply(xs, function(x) {
    # we store vectors one per column
    return(if (is.matrix(x)) nrow(x) else length(x))
  })
  if (!hasAllEqualElements(dims)) {
    stopf("All point sets and points need to be of the same dimension.")
  }
}

# @title
#   Check whether all elements of an vector are equal.
#
# @param x [vector]
#   Input vector.
# @return [logical(1)]
hasAllEqualElements = function(x) {
  return(length(unique(x)) == 1L)
}

#' @title
#' Wrap the individuals constructed by a recombination operator.
#'
#' @description
#' Should be used if the recombinator returns multiple children.
#'
#' @param ... [any]\cr
#'   Individuals.
#' @return [\code{list}] List of individuals.
#' @export
wrapChildren = function(...) {
  children = list(...)
  if (length(children) > 1L) {
    # if multiple children were generated, indicate that with an attribute
    # and return a list of children
    children = setAttribute(children, "multiple", TRUE)
    return(children)
  }
  # otherwise simply return the first (and single) child
  # This branch here is a fallback if somebody makes use of 'wrapChildren'
  # in a recombinator which returns just a single child.
  return(children[[1L]])
}

# @title
# Add some properties to matrix of fitness values.
#
# @param fitness [matrix]
#   Fitness matrix.
# @param control [ecr_control]
#   Control object.
# @return [ecr_fitness_matrix]
makeFitnessMatrix = function(fitness, control) {
  fitness = BBmisc::addClasses(fitness, "ecr_fitness_matrix")
  fitness = BBmisc::setAttribute(fitness, "minimize", control$task$minimize)
  return(fitness)
}
