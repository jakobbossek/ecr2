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
#' @template arg_lambda
#' @template arg_p_recomb
#' @template arg_p_mut
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
    n.mating = ceiling(lambda * n.parents / n.children)
    if (n.mating == 1L)
      n.mating = n.parents
    # if number of offspring is odd and number of mating
    if (n.mating %% n.parents != 0L)
      n.mating = n.mating + (n.mating %% n.parents)
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
    do.mutate = runif(length(offspring)) < p.mut
    if (sum(do.mutate) > 0) {
      offspring[do.mutate] = lapply(offspring[do.mutate], mutatorFun, control$params)
    }
  }

  # if n.children is odd/even and lambda is even/odd we need to remove some children
  if (length(offspring) > lambda) {
    offspring = offspring[-sample(1:length(offspring), length(offspring) - lambda)]
  }

  return(offspring)
}

#' @title Mutate helper.
#'
#' @description Receives a list of individuals and a control object and applies
#' the mutation operator stored in control to each individual with a certain
#' probability.
#'
#' @param x [\code{ecr2_control} | \code{ecr2_mutator}]\cr
#'   Either a mutator or a control object. If a control object is passed
#'   the functions is internally called on the mutator in \code{control$mutate}.
#'   In this case the \code{control$params} list is passed down via \code{par.list}.
#' @param inds [\code{list}]\cr
#'   List of individuals.
#' @template arg_p_mut
#' @param par.list [\code{list}]\cr
#'   Named list of parameters passed down to mutator.
#'   Default is the empty list.
#' @param ... [any]\cr
#'   Further parameters passed down to mutators.
#'   There parameters will overwrite parameters in \code{par.list}.
#' @return [\code{list}] Mutated \code{inds} list.
#' @export
#' @examples
#' # generate list of bistrings and apply mutation
#' inds = replicate(5, sample(c(0, 1), 10, replace = TRUE), simplify = FALSE)
#' mutator = setupBitflipMutator(p = 0.1)
#' mut.inds = mutate(mutator, inds, p.mut = 0.4)
mutate = function(x, inds, p.mut, par.list = list(), ...) {
  UseMethod("mutate")
}

#' @export
mutate.ecr2_mutator = function(x, inds, p.mut, par.list = list(), ...) {
  assertNumber(p.mut, lower = 0, upper = 1)
  assertList(inds)
  assertList(par.list)
  par.list = BBmisc::insert(par.list, list(...))
  mutatorFun = x
  do.mutate = runif(length(inds)) < p.mut
  if (any(do.mutate > 0)) {
    inds[do.mutate] = lapply(inds[do.mutate], mutatorFun, par.list)
  }
  return(inds)
}

#' @export
mutate.ecr2_control = function(x, inds, p.mut = 0.3, par.list = list(), ...) {
  if (!is.null(x$mutate)) {
    par.list = BBmisc::insert(x$params, par.list)
    return(mutate(x$mutate, inds, p.mut, par.list, ...))
  }
  return(inds)
}
