#' @title Helper functions for offspring generation
#'
#' @description
#' Function \code{mutate} expects a list of individuals and a mutation operator.
#' It then mutates each individual with a certain probability. Function \code{recombinate}
#' expects a recombination operator, a list of individuals as well as their fitness
#' matrix and creates \code{lambda} offspring individuals by recombining parents
#' from \code{inds}. Which parents take place in the parent selection depends on
#' the \code{parent.selector}.
#' Finally, function \code{generateOffspring} is a wrapper for both \code{recombinate}
#' and \code{mutate}. Both functions are applied subsequently to generate new individuals
#' by variation and mutation.
#'
#' @template arg_control
#' @param inds [\code{list}]\cr
#'   List of individuals.
#' @param x [\code{ecr2_control} | \code{ecr2_recombinator} | \code{ecr2_mutator}]\cr
#'   Either the control object or a recombinator for \code{recombinate} and a
#'   mutator for \code{mutate} respectively.
#' @param fitness [\code{matrix}]\cr
#'   Matrix of fitness values with one column per individual of \code{inds}.
#' @template arg_lambda
#' @template arg_p_recomb
#' @template arg_p_mut
#' @template arg_parent_selector
#' @template arg_par_list
#' @param ... [any]\cr
#'   Furhter arguments passed down to recombinator/mutator.
#'   There parameters will overwrite parameters in \code{par.list}.
#' @return [\code{list}] List of individuals.
#' @rdname generateOffspring
#' @name generateOffspring
#' @export
#' @examples
#' # generate list of bistrings and apply mutation
#' inds = replicate(5, sample(c(0, 1), 10, replace = TRUE), simplify = FALSE)
#' mutator = setupBitflipMutator(p = 0.1)
#' mut.inds = mutate(mutator, inds, p.mut = 0.4)
generateOffspring = function(control, inds, fitness, lambda, p.recomb = 0.7, p.mut = 0.1) {

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
    offspring = inds[as.integer(mating.idx)]
  } else {
    offspring = apply(mating.idx, 1L, function(parents.idx) {
      parents = inds[parents.idx]
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

#' @rdname generateOffspring
#' @export
mutate = function(x, inds, p.mut, par.list = list(), ...) {
  UseMethod("mutate")
}

#' @rdname generateOffspring
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

#' @rdname generateOffspring
#' @export
mutate.ecr2_control = function(x, inds, p.mut = 0.3, par.list = list(), ...) {
  if (!is.null(x$mutate)) {
    par.list = BBmisc::insert(x$params, par.list)
    return(mutate(x$mutate, inds, p.mut, par.list, ...))
  }
  return(inds)
}

#' @rdname generateOffspring
#' @export
recombinate = function(x, inds, fitness, lambda = length(inds), p.recomb = 0.7, parent.selector = NULL, par.list = list(), ...) {
  UseMethod("recombinate")
}

#' @rdname generateOffspring
#' @export
recombinate.ecr2_recombinator = function(x, inds, fitness, lambda = length(inds), p.recomb = 0.7, parent.selector = NULL, par.list = list(), ...) {
  if (is.null(parent.selector))
    stopf("recombinate: parent.selector needed!")
  selectorFun = parent.selector
  recombinatorFun = x

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
    offspring = inds[as.integer(mating.idx)]
  } else {
    offspring = apply(mating.idx, 1L, function(parents.idx) {
      parents = inds[parents.idx]
      children = if (runif(1L) < p.recomb & !is.null(recombinatorFun)) {
        tmp = recombinatorFun(parents, par.list)
        if (hasAttributes(tmp, "multiple")) tmp else list(tmp)
      } else {
        parents
      }
      children
    })
    # unfortunately we need to "unwrap" one listing layer here
    offspring = unlist(offspring, recursive = FALSE)
  }

  # if n.children is odd/even and lambda is even/odd we need to remove some children
  if (length(offspring) > lambda) {
    offspring = offspring[-sample(1:length(offspring), length(offspring) - lambda)]
  }

  return(offspring)
}

#' @rdname generateOffspring
#' @export
recombinate.ecr2_control = function(x, inds, fitness, lambda = length(inds), p.recomb = 0.7, parent.selector = NULL, par.list = list(), ...) {
  parent.selector = coalesce(parent.selector, x$selectForMating)
  if (!is.null(x$recombinate)) {
    par.list = BBmisc::insert(x$params, par.list)
    return(recombinate(x$recombinate, inds, fitness, lambda, p.recomb,
      par.list, parent.selector = parent.selector, ...))
  }
  return(inds)
}
