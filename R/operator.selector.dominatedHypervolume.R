#' @title
#' Dominated hypervolume selector.
#'
#' @description
#' Performs nondominated sorting and drops the individual from the last front
#' with minimal hypervolume contribution.
#'
#' @return [\code{setOfIndividuals}]
#' @family selectors
#' @export
setupDominatedHypervolumeSelector = function() {
  selector = function(fitness, n.select, task, control, opt.state) {
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
    hvctrbs = computeHypervolumeContribution(fitness[, idx.max, drop = FALSE], ref.point = control$ref.point)
    die.idx = idx.max[getMinIndex(hvctrbs)]

    return(setdiff(all.idx, die.idx))
  }
  makeSelector(
    selector = selector,
    name = "Hypervolume contribution selector",
    description = "description",
    supported.objectives = "multi-objective"
  )
}
