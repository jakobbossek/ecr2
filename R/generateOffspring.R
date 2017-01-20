#' @title
#' Creates list of offspring individuals
#'
#' @description
#' Given and optimization state and a mating pool of individuals this function
#' generates offspring individuals based on the parameters specified in the
#' control object.
#'
#' @template arg_control
#' @param population [\code{list}]\cr
#'   Current population, i.e., list of individuals.
#' @param fitness [\code{matrix}]\cr
#'   Matrix of fitness values with one column per individual of \code{individuals}.
#' @param lambda [\code{integer(1)}]\cr
#'   Number of offspring individuals to generate.
#' @param p.recomb [\code{numeric(1)}]\cr
#'   Probability of two parents to perform crossover.
#' @param p.mut [\code{numeric(1)}]\cr
#'   Probability to apply mutation to a child.
#' @param recomb.pars [\code{list}]\cr
#'   Optional list of further arguments passed down to recombinator.
#'   Default is the empty list.
#' @param mut.pars [\code{list}]\cr
#'   Optional list of further arguments passed down to mutator.
#'   Default is the empty list.
#' @return [\code{list}] Offspring.
#' @export
generateOffspring = function(control, population, fitness, lambda, p.recomb = 0.7, p.mut = 0.1) {
  selectorFun = coalesce(control$selectForMating, setupSimpleSelector())
  mutatorFun = control$mutate
  recombinatorFun = control$recombine

  if (is.null(mutatorFun) & is.null(recombinatorFun))
    stopf("At least a mutator or recombinator needs to be available.")

  # determine how many elements need to be chosen by parentSelector
  # if no recombinator exists we select simply lambda elements
  n.mating = lambda
  n.parents = 1L
  if (!is.null(recombinatorFun)) {
    n.children = getNumberOfChildren(recombinatorFun)
    n.parents = getNumberOfParentsNeededForMating(recombinatorFun)
    n.mating = floor(lambda * n.parents / n.children)
    if (n.mating == 1L)
      n.mating = n.parents
  }
  # create mating pool. This a a matrix, where each row contains the indizes of
  # a set of >= 2 parents
  mating.idx = matrix(selectorFun(fitness, n.select = n.mating), ncol = n.parents)

  # now perform recombination
  if (is.null(recombinatorFun)) {
    offspring = population[as.integer(mating.idx)]
  } else {
    offspring = apply(mating.idx, 1L, function(parents.idx) {
      parents = population[parents.idx]
      children = if (runif(1L) < p.recomb & !is.null(recombinatorFun)) {
        tmp = recombinatorFun(parents, control$params)
        if (hasAttributes(tmp, "multiple")) tmp else list(tmp)
      } else {
        parents
      }
      children
    })
    # unfortunately we need to "unwrap" one listing layer here
    offspring = unlist(offspring, recursive = FALSE)
  }

  # now eventually apply mutation
  if (!is.null(mutatorFun)) {
    offspring = lapply(offspring, function(child) {
      if (runif(1L) < p.mut)
        return(mutatorFun(child, control$params))
      return(child)
    })
  }

  # if n.children is odd/even and lambda is even/odd we need to remove some children
  if (length(offspring) > lambda) {
    offspring = offspring[-sample(1:length(offspring), length(offspring) - lambda)]
  }

  return(offspring)
}
