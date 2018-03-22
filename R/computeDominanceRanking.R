computeDominanceRanking = function(df, obj.cols) {
  assertDataframe(df)
  assertCharacter(obj.cols, min.len = 2L)
  # here we duplicate prob col, since application of by() function make these unavailable
  df$prob2 = df$prob

  warningf("Note, that this may take a long time if the number of problems,
    algorithms and/or replications is high.")

  # now split by problem
  res = by(tt, list(tt$prob2),
  function(x) {
    # get grid the ugly way: now we need to iterate over the rows
    grid = dplyr::group_by(x, prob, algorithm, repl)
    grid = dplyr::summarize(grid, rg = NA)
    grid = ungroup(grid)

    n = nrow(grid)
    for (i in 1:n) {
      # Rank is rg(C_i) = 1 + |{C_j in C | C_j <= C_i}| (see Knowles et al.)
      rg = 1L
      for (j in 1:n) {
        cat(".")
        if (i == j)
          next
        approx.a = x[x$algorithm == as.character(grid[i, "algorithm"]) & x$repl == as.integer(grid[i, "repl"]), obj.cols, drop = FALSE]
        approx.b = x[x$algorithm == as.character(grid[j, "algorithm"]) & x$repl == as.integer(grid[j, "repl"]), obj.cols, drop = FALSE]
        rg = rg + as.integer(setDominates(t(approx.b), t(approx.a)))
      }
      grid[i, "rg"] = rg
    }
    return(grid)
  })
  do.call(rbind, res)
}
