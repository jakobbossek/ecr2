#' Filter approximation sets by duplicate objective vectors.
#'
#' @note Note that this may be misleading if there can be solutions with identical
#' objective function values but different values in decision space.
#'
#' @param x [\code{object}]\cr
#'  Object of type data frame (objectives column-wise), matrix (objectives row-wise),
#'  \code{\link[=ecr_result]{ecr_multi_objective_result}} or \code{list} (with components \dQuote{pareto.front})
#'  and \dQuote{pareto.set}.
#' @param ... [any]\cr
#'  Not used at the moment
#' @return [\code{object}] Modified input \code{x}.
#' @name filterDuplicated
#' @rdname filterDuplicated
#' @export
filterDuplicated = function(x, ...) {
  UseMethod("filterDuplicated")
}

#' @rdname filterDuplicated
#' @export
filterDuplicated.data.frame = function(x, ...) {
  is.dup = duplicated(x)
  return(x[!is.dup, , drop = FALSE])
}

#' @rdname filterDuplicated
#' @export
filterDuplicated.matrix = function(x, ...) {
  is.dup = duplicated(t(x))
  return(x[, !is.dup, drop = FALSE])
}

#' @rdname filterDuplicated
#' @export
filterDuplicated.ecr_multi_objective_result = function(x, ...) {
  is.dup = duplicated(x$pareto.front)
  x$pareto.front = x$pareto.front[!is.dup, , drop = FALSE]
  x$pareto.set = x$pareto.set[!is.dup]
  return(x)
}

#' @rdname filterDuplicated
#' @export
filterDuplicated.list = function(x, ...) {
  if (!containsNames(x, c("pareto.front", "pareto.set")))
    BBmisc::stopf("[filterDuplicated] List needs components pareto.{front,set}.")
  filterDuplicated.ecr_multi_objective_result(x)
}

#' Sort Pareto-front approximation by objective.
#'
#' @param x [\code{object}]\cr
#'   Object of type data frame (objectives column-wise), matrix (objectives row-wise),
#'   \code{\link[=ecr_result]{ecr_multi_objective_result}} or \code{list} (with components \dQuote{pareto.front})
#'   and \dQuote{pareto.set}.
#' @param obj [\code{integer(1) | character(1)}]\cr
#'   Either the row/column number to sort by or the column name, e.g., for data frames.
#' @param ... [any]\cr
#'   Further arguments passed down to \code{\link[base]{order}}.
#' @return Modified object.
#' @name sortByObjective
#' @rdname sortByObjective
#' @export
sortByObjective = function(x, obj = 1L, ...) {
  UseMethod("sortByObjective")
}

#' @rdname sortByObjective
#' @export
sortByObjective.data.frame = function(x, obj = 1L, ...) {
  ord = order(x[[obj]], ...)
  return(x[ord, , drop = FALSE])
}

#' @rdname sortByObjective
#' @export
sortByObjective.matrix = function(x, obj = 1L, ...) {
  ord = order(x[obj, ], ...)
  return(x[, ord, drop = FALSE])
}

#' @rdname sortByObjective
#' @export
sortByObjective.ecr_multi_objective_result = function(x, obj = 1L, ...) {
  ord = order(x$pareto.front[[obj]], ...)
  x$pareto.front = x$pareto.front[ord, , drop = FALSE]
  x$pareto.set   = x$pareto.set[ord]
  return(x)
}

#' @rdname sortByObjective
#' @export
sortByObjective.list = function(x, obj = 1L, ...) {
  if (!containsNames(x, c("pareto.front", "pareto.set")))
    BBmisc::stopf("[sortByObjective] List needs components pareto.{front,set}.")
  sortByObjective.ecr_multi_objective_result(x)
}

#' Combine multiple data frames into a single data.frame.
#'
#' @param res [\code{list}]\cr
#'   List of data frames or other lists which contain a data frame as one of the
#'   components which is selected by \code{what}. If \code{res} is a named list
#'   those names are used to fill the group column. Otherwise the names are 1 to
#'   \code{length(res)} by default.
#' @param what [\code{character(1)}]\cr
#'   Which component of each list element in \code{res} to choose. Set this to
#'   \code{NULL}, if \code{res} is not complex, i.e., is not a list of lists.
#' @param group.col.name [\code{character(1)}]\cr
#'   Name for the grouping column.
#' @export
reduceToSingleDataFrame = function(res = list(), what = NULL, group.col.name) {
  checkmate::assertList(res)
  #res = BBmisc::insert(res, list(...))
  names = names(res)
  resdf = do.call(rbind, lapply(1:length(res), function(i) {
    tmp = res[[i]]
    if (!is.null(what))
      tmp = tmp[[what]]
    tmp[[group.col.name]] = if (!is.null(names)) {
      if (names[i] == "") {
        i
      } else {
        names[i]
      }
    } else {
      i
    }
    print(tmp)
    return(tmp)
  }))
  resdf[[group.col.name]] = as.factor(resdf[[group.col.name]])
  return(resdf)
}

#' Convert matrix to Pareto front data frame.
#'
#' Inside ecr EMOA algorithms the fitness is maintained in an \eqn{(o, n)} matrix
#' where \eqn{o} is the number of objectives and \eqn{n} is the number of individuals.
#' This function basically transposes such a matrix and converts it into a data frame.
#'
#' @param x [\code{matrix}]\cr
#'   Matrix.
#' @param filter.dups [\code{logical(1)}]\cr
#'   Shall duplicates be removed?
#'   Default is \code{FALSE}.
#' @return [\code{data.frame}]
#' @export
toParetoDf = function(x, filter.dups = FALSE) {
  checkmate::assertMatrix(x, mode = "numeric", min.cols = 1L, min.rows = 1L)
  df = as.data.frame(t(x))
  colnames(df) = paste0("y", seq_len(ncol(df)))
  if (filter.dups)
    df = filterDuplicated(df)
  return(df)
}

containsNames = function(x, names) {
  x.names = names(x)
  if (is.null(x.names))
    return(FALSE)
  return(all(names %in% x.names))
}
