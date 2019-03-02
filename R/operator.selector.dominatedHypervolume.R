#' @title
#' Dominated Hypervolume selector.
#'
#' @description
#' Performs non-dominated sorting and drops the individual from the last front
#' with minimal hypervolume contribution. This selector is the basis of the
#' S-Metric Selection Evolutionary Multi-Objective Algorithm, termed SMS-EMOA
#' (see \code{\link{smsemoa}}).
#'
#' @note Note that the current implementation expects \code{n.select = ncol(fitness) - 1}
#' and the selection process quits with an error message if \code{n.select} is greater
#' than 1.
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
    checkmate::assertMatrix(fitness, mode = "numeric", min.rows = 2L, min.cols = 2L, any.missing = FALSE, all.missing = FALSE)
    checkmate::assertNumeric(ref.point, any.missing = FALSE, all.missing = FALSE)

    n = ncol(fitness)
    if (n.select != (n - 1)) {
      BBmisc::stopf("[ecr::selDomHV] Note that selDomHV always drops a single individual
        and hence (ncol(fitness) - 1) individuals are selected. Your choice of
        n.select (= %i) is not allowed since (ncol(fitness) - 1) equals %i. Check your setup!", n.select, (n - 1L))
    }

    all.idx = seq_len(n)

    # do non-dominated sorting
    ranks = doNondominatedSorting(fitness)$ranks
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
