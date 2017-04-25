#' @title
#' Dominated Hypervolume selector.
#'
#' @description
#' Performs nondominated sorting and drops the individual from the last front
#' with minimal hypervolume contribution.
#'
#' @template arg_fitness
#' @template arg_n_select
#' @param ref.point [\code{numeric}]\cr
#'   Reference point for hypervolume computation.
#' @return [\code{integer}] Vector of survivor indizes.
#' @family selectors
#' @export
selDomHV = makeSelector(
  selector = function(fitness, n.select, ref.point) {
    assertNumeric(ref.point)
    all.idx = 1:ncol(fitness)

    # do non-dominated sorting
    nds.res = doNondominatedSorting(fitness)
    ranks = nds.res$ranks
    idx.max = which(ranks == max(ranks))

    # there is exactly one individual that is "maximally" dominated
    if (length(idx.max) == 1L) {
      return(setdiff(all.idx, idx.max))
    }

    # compute exclusive hypervolume contributions and remove the one with the smallest
    hvctrbs = computeHVContr(fitness[, idx.max, drop = FALSE], ref.point = ref.point)
    die.idx = idx.max[getMinIndex(hvctrbs)]

    return(setdiff(all.idx, die.idx))
  },
  supported.objectives = "multi-objective")
