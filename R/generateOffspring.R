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
#' @template arg_fitness
#' @template arg_lambda
#' @template arg_p_recomb
#' @template arg_p_mut
#' @template arg_par_list
#' @param ... [any]\cr
#'   Furhter arguments passed down to recombinator/mutator.
#'   There parameters will overwrite parameters in \code{par.list}.
#' @return [\code{list}] List of individuals.
#' @rdname generateOffspring
#' @name generateOffspring
#' @export
# @examples
# # init the control object and register mutation operator
# fn = function(x) sum(x)
# control = initECRControlBinary(fn, n.objectives = 1L, minimize = TRUE, n.bits = 10)
# control = registerMutator(control, setupBitflipMutator(p = 0.1))
# # generate list of bistrings/individuals by hand
# inds = replicate(5, sample(c(0, 1), 10, replace = TRUE), simplify = FALSE)
# # finally apply mutation
# mut.inds = mutate(control, inds, p.mut = 0.4)
generateOffspring = function(control, inds, fitness, lambda, p.recomb = 0.7, p.mut = 0.1) {

  #FIXME: maybe better drop this function!
  if (is.null(control$mutate) & is.null(control$recombinate))
    stopf("generateOffspring: At least a mutator or recombinator needs to be available.")

  offspring = if (!is.null(control$recombine)) {
    recombinate(control, inds, fitness, lambda, p.recomb = p.recomb)
  } else {
    inds
  }
  offspring = mutate(control, offspring, p.mut = p.mut)

  return(offspring)
}

#' @rdname generateOffspring
#' @export
mutate = function(control, inds, p.mut = 0.1, par.list = list(), ...) {
  assertClass(control, "ecr2_control")
  mutatorFun = control$mutate
  mutator.pars = control[["mutate.pars"]]
  if (is.null(mutatorFun) | is.null(mutator.pars))
    stopf("mutate: no mutation operator or mutation parameters missing. Did you register a
      mutator via registerECROperator?")
  assertClass(mutatorFun, "ecr2_mutator")
  assertList(mutator.pars)
  assertNumber(p.mut, lower = 0, upper = 1)
  assertList(inds)
  assertList(par.list)
  # append parameters
  par.list = BBmisc::insert(control$params, par.list)
  par.list = BBmisc::insert(par.list, mutator.pars)
  par.list = BBmisc::insert(par.list, list(...))
  do.mutate = runif(length(inds)) < p.mut
  if (any(do.mutate > 0)) {
    inds[do.mutate] = lapply(inds[do.mutate], mutatorFun, par.list)
  }
  return(inds)
}

#' @rdname generateOffspring
#' @export
recombinate = function(control, inds, fitness, lambda = length(inds), p.recomb = 0.7, par.list = list(), ...) {
  assertClass(control, "ecr2_control")
  assertList(inds)
  assertMatrix(fitness, ncols = length(inds), min.rows = 1L, any.missing = FALSE, all.missing = FALSE)
  lambda = asInt(lambda, lower = 1L)
  assertNumber(p.recomb, lower = 0, upper = 1L)
  assertList(par.list)

  recombinatorFun = control$recombine
  recombinator.pars = control[["recombine.pars"]]
  if (is.null(recombinatorFun) | is.null(recombinator.pars))
    stopf("recombinate: no recombination operator or recombination parameters missing. Did you register a
      recombinator via registerECROperator?")

  assertClass(recombinatorFun, "ecr2_recombinator")
  assertList(recombinator.pars)

  assertClass(control$selectForMating, "ecr2_selector")

  # append parameters
  par.list = BBmisc::insert(control$params, par.list)
  par.list = BBmisc::insert(par.list, recombinator.pars)
  par.list = BBmisc::insert(par.list, list(...))

  #FIXME: eventually drop this in order to come up with a simpler interface
  #FIXME: why all the recombinator checks? If none is passed we cannot recombine!
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
  mating.idx = matrix(selectForMating(control, fitness, n.select = n.mating), ncol = n.parents)
  # catf("n.fitness: %i", ncol(fitness))
  # catf("n.individuals: %i", length(inds))
  # catf("max.idx: %i", max(mating.idx))
  # catf("n.mating: %i", n.mating)
  # catf("n.parents: %i", n.parents)

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
