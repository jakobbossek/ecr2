#' @title
#' Construct a generator.
#'
#' @description
#' Helper function which constructs a generator, i. e., a function which generates
#' an initial population.
#
#' @param generator [\code{function}]\cr
#'   Actual generator function. Should expect the size of the population \code{size}
#'   as a first and a ecr control object \code{control} as the second argument.
#' @param name [\code{character(1)}]\cr
#'   Name of the generator.
#' @param description [\code{character(1)}]\cr
#'   Short description of how the generator works.
#' @param params [\code{list}]\cr
#'   Named list of the parameters the generator has been initialized with.
#'   Default is the empty list.
#' @param supported [\code{character}]\cr
#'   Vector of strings/names of supported parameter representations, i.e.,
#'   'permutation', 'float', 'binary' or 'custom'.
#' @return [\code{ecr_generator}]
#'   Generator object.
#' @export
makeGenerator = function(
  generator, name, description,
  supported = getAvailableRepresentations(),
  params = list()) {
  assertFunction(generator, args = c("size", "task", "control"), ordered = TRUE)
  generator = makeOperator(generator, name, description, supported, params)
  generator = addClasses(generator, c("ecr_generator"))
  return(generator)
}
