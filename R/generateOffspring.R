#' @title
#' Helper functions for offspring generation
#'
#' @description
#' Function \code{mutate} expects a control object, a list of individuals, and a mutation
#' probability. The mutation operator registered in the control object is then applied
#' with the given probability to each individual.
#' Function \code{recombinate} expects a control object, a list of individuals as well as
#' their fitness matrix and creates \code{lambda} offspring individuals by recombining parents
#' from \code{inds}. Which parents take place in the parent selection depends on
#' the \code{parent.selector} registered in the control object.
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
#' @param slot [\code{character(1)}]\cr
#'   The slot of the control object which contains the registered operator to use.
#'   Default is \dQuote{mutate} for \code{mutate} and \dQuote{recombine} for \code{recombinate}.
#'   In most cases there is no need to change this. However, it might be useful if you make use
#'   of different mutation operators registerted, e.g., in the slots \dQuote{mutate1} and \dQuote{mutate2}.
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
# control = initECRControl(fn, n.objectives = 1L, minimize = TRUE)
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
    # otherwise simply select individuals without recombination
    mating.idx = getMatingPool(control, inds, fitness, lambda = lambda)
    offspring = inds[as.integer(mating.idx)]
  }
  offspring = mutate(control, offspring, p.mut = p.mut)

  return(offspring)
}

#' @rdname generateOffspring
#' @export
mutate = function(control, inds, p.mut = 0.1, slot = "mutate", ...) {
  assertClass(control, "ecr_control")
  assertString(slot)
  mutatorFun = control[[slot]]
  mutator.pars = control[[paste0(slot, ".pars")]]
  if (is.null(mutatorFun) | is.null(mutator.pars))
    stopf("mutate: no mutation found in control slot '%s' or mutation parameters missing. Did you register a
      mutator via registerECROperator?", slot)
  assertFunction(mutatorFun)
  assertList(mutator.pars)
  assertNumber(p.mut, lower = 0, upper = 1)
  assertList(inds)
  # append parameters
  par.list = BBmisc::insert(control$params, mutator.pars)
  par.list = BBmisc::insert(par.list, list(...))

  do.mutate = runif(length(inds)) < p.mut
  # print("Mutating: %i", sum(do.mutate))
  # print(match.call())
  if (any(do.mutate > 0)) {
    inds[do.mutate] = lapply(inds[do.mutate], function(x) {
  # print(do.mutate)
  # print(c(list(x), par.list))

      do.call(mutatorFun, c(list(x), par.list))
    })
  }
  return(inds)
}

#' @rdname generateOffspring
#' @export
recombinate = function(control, inds, fitness, lambda = length(inds), p.recomb = 0.7, slot = "recombine", ...) {
  assertClass(control, "ecr_control")
  assertString(slot)
  assertList(inds)
  assertMatrix(fitness, ncols = length(inds), min.rows = 1L, any.missing = FALSE, all.missing = FALSE)
  lambda = asInt(lambda, lower = 1L)
  assertNumber(p.recomb, lower = 0, upper = 1L)

  recombinatorFun = control[[slot]]
  recombinator.pars = control[[paste0(slot, ".pars")]]
  if (is.null(recombinatorFun) | is.null(recombinator.pars))
    stopf("recombinate: no recombination found in control slot '%s' or recombination parameters missing.
      Did you register a recombinator via registerECROperator?", slot)

  assertFunction(recombinatorFun)
  assertList(recombinator.pars)

  # append parameters
  par.list = BBmisc::insert(control$params, recombinator.pars)
  par.list = BBmisc::insert(par.list, list(...))

  mating.idx = getMatingPool(control, inds, fitness, lambda = lambda, slot = slot)

  # now perform recombination
  offspring = apply(mating.idx, 1L, function(parents.idx) {
    parents = inds[parents.idx]
    children = if (runif(1L) < p.recomb & !is.null(recombinatorFun)) {
      tmp = do.call(recombinatorFun, c(list(parents), par.list))
      if (hasAttributes(tmp, "multiple")) tmp else list(tmp)
    } else {
      parents
    }
    children
  })
  # unfortunately we need to "unwrap" one listing layer here
  offspring = unlist(offspring, recursive = FALSE)

  # if n.children is odd/even and lambda is even/odd we need to remove some children
  if (length(offspring) > lambda) {
    offspring = offspring[-sample(1:length(offspring), length(offspring) - lambda)]
  }

  return(offspring)
}


# @title
# Generate mating pool.
#
# @description Determine how many individuals are needed to create lambda offspring
# and produce a (k x lambda) matrix with k being the number of individuals needed for
# potential recombination.
#
# @param control [ecr_control]
#   Control object.
# @param fitness [matrix]
#   Matrix of fitness values.
# @param lambda [integer(1)]
#   Number of offspring to generate.
# @param slot [character(1)]
#   Slot in control for recombinator.
# @return [matrix]
# @param
#FIXME: actually this is not the mating pool, but the matrix of individuals for mating.
#FIXME: export
getMatingPool = function(control, inds, fitness, lambda = length(inds), slot = "recombine") {
  assertFunction(control$selectForMating)
  recombinatorFun = control[[slot]]

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
  return(mating.idx)
}
