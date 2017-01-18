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
#' @return [\code{ecr_mutator}]
#' @family mutators
#' @export
setupUniformMutator = function() {
  mutator = function(ind, task, control) {
    n.params = length(ind)
    idx = sample(n.params, size = 1L)
    ind[idx] = runif(1L, min = getLower(task$par.set)[idx], max = getUpper(task$par.set)[idx])
    return(ind)
  }

  makeMutator(
    mutator = mutator,
    name = "Uniform mutator",
    description = "Replaces a randomly chosen parameter with a random value within the bounds",
    supported = "float"
  )
}
