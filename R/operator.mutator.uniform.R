#' @title
#' Uniform mutator.
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
#' @param ind [\code{numeric}]\cr
#'   Numeric vector / individual to mutate.
#' @param lower [\code{numeric}]\cr
#'   Vector of minimal values for each parameter of the decision space.
#' @param upper [\code{numeric}]\cr
#'   Vector of maximal values for each parameter of the decision space.
#' @return [\code{numeric}]
#' @family mutators
#' @export
mutUniform = makeMutator(
  mutator = function(ind, lower, upper) {
    checkNumericMutatorArguments(ind, lower, upper, "mutUniform")
    idx = sample(1:length(ind), size = 1L)
    ind[idx] = runif(1L, min = lower[idx], max = upper[idx])
    return(ind)
  },
  supported = "float")
