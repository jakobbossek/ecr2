#' Filter approximation sets by duplicate objective vectors.
#'
#' @note Note that this may be misleading if there can be solutions with identical
#' objective function values but different values in decision space.
#'
#' @export
filterDuplicated = function(res) {
  is.dup = duplicated(res$pareto.front)
  res$pareto.front = res$pareto.front[!is.dup, , drop = FALSE]
  res$pareto.set = res$pareto.set[!is.dup]
  return(res)
}

#' Combine multiple Pareto-front approximations into a single data.frame.
#' @export
reduceToSingleDataFrame = function(res = list(), group.col.name) {
  checkmate::assertList(res)
  #res = BBmisc::insert(res, list(...))
  names = names(res)
  print(names)
  pf = do.call(rbind, lapply(1:length(res), function(i) {
    tmp = res[[i]]$pareto.front
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
  pf[[group.col.name]] = as.character(pf[[group.col.name]])
  return(pf)
}
