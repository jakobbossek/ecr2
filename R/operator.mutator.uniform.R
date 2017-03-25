#' @title
#' Generator of the Uniform mutation operator.
#'
#' @description
#' This mutation operator works on real-valued genotypes only. It selects a position
#' in the solution vector at random and replaced it with a uniformally chosen value
#' within the box constraints of the corresponding parameter.
#' This mutator may proof beneficial in early stages of the optimization process,
#' since it distributes points widely within the box constraints and thus may
#' hinder premature convergence. However, in later stages - when fine tuning is
#' necessary, this feature is disadvantegous.
#'
#' @param lower [\code{numeric}]\cr
#'   Vector of minimal values for each parameter of the decision space.
#' @param upper [\code{numeric}]\cr
#'   Vector of maximal values for each parameter of the decision space.
#' @return [\code{ecr2_mutator}]
#' @family mutators
#' @export
setupUniformMutator = function(lower, upper) {
  assertNumeric(lower, any.missing = FALSE, all.missing = FALSE)
  assertNumeric(lower, any.missing = FALSE, all.missing = FALSE)
  if (length(lower) != length(upper)) {
    stopf("Uniform mutator: length of lower and upper bounds need to be equal!")
  }
  force(lower)
  force(upper)

  mutator = function(ind, par.list) {
    n = length(ind)
    idx = sample(n, size = 1L)
    ind[idx] = runif(1L, min = lower[idx], max = upper[idx])
    return(ind)
  }

  makeMutator(
    mutator = mutator,
    name = "Uniform mutator",
    description = "Replaces a randomly chosen parameter with a random value within the bounds",
    supported = "float"
  )
}
