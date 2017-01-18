# @title
# Fitness transformation / scaling.
#
# @description
# Some selectors support maximization only, e.g., roulette wheel selector, or
# minimization (most others). This function computes a factor from {-1, 1} for
# each objective to match supported selector optimization directions and
# the actual objectives of the task.
#
# @param fitness [matrix]
#   Matrix of fitness values with the fitness vector of individual i in the i-th
#   column.
# @param task [ecr_optimization_task]
#   Optimization task.
# @param control [ecr_control]
#   Control object.
# @return [matrix] Transformed / scaled fitness matrix.
transformFitness = function(fitness, task, selector) {
  # logical vector of opt directions
  task.dir = task$minimize
  # "vectorize" character indicating supported opt direction by selector
  sup.dir = rep(attr(selector, "supported.opt.direction"), task$n.objectives)
  # "logicalize" selector opt direction
  sup.dir = (sup.dir == "minimize")

  fn.scale = ifelse(xor(task.dir, sup.dir), -1, 1)

  # build transformation matrix
  fn.scale = if (task$n.objectives == 1L) {
    #FIXME: R BUG?!?!
    # diag(ifelse(xor(task.dir, sup.dir), -1, 1)) breaks with message
    # Fehler in diag(ifelse(xor(task.dir, sup.dir), -1, 1)) : ung"ultiger 'nrow' Wert (< 0)
    # if n.objectives is 1! -.-
    # Weird R bug??? diag(1) works!
    as.matrix(fn.scale)
  } else {
    diag(fn.scale)
  }
  # transform fitness
  return(fn.scale %*% fitness)
}
