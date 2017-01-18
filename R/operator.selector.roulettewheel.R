#' @title
#' Generator for Roulette-Wheel-Selection (fitness-proportional selection).
#'
#' @description
#' Selects genes for the mating pool. The chance of an individual to get selected
#' is proportional to its fitness, i.e., better individuals get a higher chance
#' to take part in the reproduction process. Low-fitness individuals however,
#' have a positive fitness as well.
#'
#' @details
#' Fitness proportional selection can be naturally applied to single objective
#' maximization problems. However, negative fitness values can are problematic.
#' The Roulette-Wheel selector thus works with the following heuristic: if
#' negative values occur, the negative of the smallest fitness value is added
#' to each fitness value. In this case to avoid the smallest shifted fitness
#' value to be zero and thus have a zero probability of being selected an additional
#' positive constant \code{offset} is added (see parameters).
#'
#' @param offset [\code{numeric(1)}]\cr
#'   In case of negative fitness values all values are shifted towards positive
#'   values by adding the negative of the minimal fitness value. However, in this
#'   case the minimal fitness value after the shifting process is zero. The
#'   \code{offset} is a positive numeric value which is added additionally to each
#'   shifted fitness value. This way even the individual with the smallest fitness
#'   value has a positive porbability to be selected.
#'   Default is \code{0.1}.
#' @return [\code{setOfIndividuals}]
#' @family selectors
#' @export
setupRouletteWheelSelector = function(offset = 0.1) {
  assertNumber(offset, na.ok = FALSE, lower = 0, finite = TRUE)

  force(offset)

  selector = function(fitness, n.select, task, control, opt.state) {
    fitness = as.numeric(fitness)
    # shift negative values
    if (any(fitness <= 0L)) {
      fitness = fitness + abs(min(fitness)) + offset
    }
    #FIXME: this selector supports maximization only at the moment
    #fitness = 1 / fitness
    prob = fitness / sum(fitness)
    idx = sample(seq_along(fitness), size = n.select, replace = TRUE, prob = prob)
    return(idx)
  }
  makeSelector(
    selector = selector,
    name = "Roulette-Wheel selector",
    description = "Selects individuals in a fitness-proportional fashion.",
    supported.objectives = c("single-objective"),
    supported.opt.direction = "maximize"
  )
}
