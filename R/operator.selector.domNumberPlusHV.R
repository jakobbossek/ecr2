#' @title
#' Modified dominated Hypervolume selector.
#'
#' @description
#' Alternative SMS-EMOA survival selection as proposed in Algorithm 3 of [1]. Performs non-dominated
#' sorting first. If the number of non-domination levels is at least two the algorithm
#' drops the individual with the highest number of dominating points (ties are
#' broken at random) from the last layer. If there is just one non-domination layer,
#' i.e., all points are non-domianted the method drops the individual with minimal
#' hypervolume contribution. This selector is the basis of the
#' S-Metric Selection Evolutionary Multi-Objective Algorithm, termed SMS-EMOA
#' (see \code{\link{smsemoa}}).
#'
#' @references
#' [1] Beume, Nicola, Boris Naujoks and M. Emmerich. SMS-EMOA: Multiobjective selection
#' based on dominated hypervolume.” European Journal of Operational Research. 181 (2007): 1653-1669.
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
selDomNumberPlusHV = makeSelector(
  selector = function(fitness, n.select, ref.point) {
    # checkmate::assertMatrix(fitness, mode = "numeric", min.rows = 2L, min.cols = 2L, any.missing = FALSE, all.missing = FALSE)
    # checkmate::assertNumeric(ref.point, any.missing = FALSE, all.missing = FALSE)

    n = ncol(fitness)
    if (n.select != (n - 1)) {
      BBmisc::stopf("[ecr::selDomNumberPlusHV] Note that selDomNumberPlusHV always drops a single individual
        and hence (ncol(fitness) - 1) individuals are selected. Your choice of
        n.select (= %i) is not allowed since (ncol(fitness) - 1) equals %i. Check your setup!", n.select, (n - 1L))
    }

    all.idx = seq_len(n)

    # do non-dominated sorting
    nds.res = doNondominatedSorting(fitness)
    ranks = nds.res$ranks
    dom.counter = nds.res$dom.counter

    # get the maximum rank/level
    max.rank = max(ranks)

    if (max.rank > 1) {
      # If there are at least two ranks/levels use the domination number to drop individuals:
      # Get IDs of individuals on last level
      idx.max.rank = which(ranks == max.rank)

        # there is exactly one individual that is "maximally" dominated
      if (length(idx.max.rank) == 1L) {
        return(setdiff(all.idx, idx.max.rank))
      }

      # get IDs with maximum number of dominating points
      max.rank.domcounts  = dom.counter[idx.max.rank]
      max.rank.domcounts.ids = idx.max.rank[max.rank.domcounts == max(max.rank.domcounts)]
      if (length(max.rank.domcounts.ids) > 1L) {
        max.rank.domcounts.ids = sample(max.rank.domcounts.ids, size = 1L)
      }
      return(setdiff(all.idx, max.rank.domcounts.ids))
    }

    # compute exclusive hypervolume contributions and remove the one with the smallest
    hvctrbs = computeHVContr(fitness, ref.point = ref.point)
    die.idx = getMinIndex(hvctrbs)

    return(setdiff(all.idx, die.idx))
  },
  supported.objectives = "multi-objective")
