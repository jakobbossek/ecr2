#' @title
#' Construct evolutionary operator.
#'
#' @description
#' Helper function which constructs an evolutionary operator.
#'
#' @note
#' In general you will not need this function, but rather one of its
#' deriviatives like \code{\link{makeMutator}} or \code{\link{makeSelector}}.
#'
#' @param operator [\code{function}]\cr
#'   Actual mutation operator.
#' @param name [\code{character(1)}]\cr
#'   Name of the operator.
#' @param description [\code{character(1)}]\cr
#'   Short description of how the mutator works.
#'   Default is \code{NULL} which means no description at all.
#' @param supported [\code{character}]\cr
#'   Vector of names of supported parameter representations. Possible choices:
#'   \dQuote{permutation}, \dQuote{float}, \dQuote{binary} or \dQuote{custom}.
#' @param params [\code{list}]\cr
#'   Named list of the parameters the operator has been initialized with.
#'   Default is the empty list.
#' @return [\code{ecr_operator}] Operator object.
#' @export
makeOperator = function(
  operator,
  name,
  description = NULL,
  supported = getAvailableRepresentations(),
  params = list()) {
  assertFunction(operator)
  assertString(name)
  if (!is.null(description)) {
    assertString(description)
  }
  assertSubset(supported, choices = getAvailableRepresentations(), empty.ok = FALSE)
  assertList(params, unique = TRUE, any.missing = FALSE, all.missing = FALSE)

  operator = setAttribute(operator, "name", name)
  operator = setAttribute(operator, "description", coalesce(description, "-"))
  operator = setAttribute(operator, "supported", supported)
  operator = setAttribute(operator, "params", params)

  operator = addClasses(operator, c("ecr_operator"))
  return(operator)
}

#' @title
#' Get name of a operator.
#'
#' @description
#' Returns the name of the passed operator.
#'
#' @param operator [\code{ecr_operator}]\cr
#'   Operator object.
#' @return [\code{character(1)}]
#'   Name of the operator.
#' @export
getOperatorName = function(operator) {
  UseMethod("getOperatorName")
}

#' @export
getOperatorName.ecr_operator = function(operator) {
  attr(operator, "name")
}

#' @title
#' Check if given function is an ecr operator.
#'
#' @description
#' Checks if the passed object is of type \code{ecr_operator}.
#'
#' @param obj [any]\cr
#'   Arbitrary R object to check.
#' @return [\code{logical(1)}]
#' @export
isEcrOperator = function(obj) {
  return(inherits(obj, "ecr_operator"))
}

#' @export
print.ecr_operator = function(x, ...) {
  catf("Name: %s", getOperatorName(x))
  catf("Description: %s", attr(x, "description"))
  catf("Supported representations: %s", collapse(getSupportedRepresentations(x)))
  catf("Parameters: %s", getParametersAsString(getOperatorParameters(x)))
}

#' @export
print.ecr_recombinator = function(x, ...) {
  print.ecr_operator(x)
  catf("Number of returned children: %i", attr(x, "n.parents"))
}

#' @export
print.ecr_selector = function(x, ...) {
  print.ecr_operator(x)
  catf("Supported #objectives: %s", attr(x, "supported.objectives"))
}

#' @title
#' Get supported representations.
#'
#' @description
#' Returns the character vector of representation which the operator supports.
#'
#'
#' @param operator [\code{ecr_operator}]\cr
#'   Operator object.
#' @return [\code{character}]
#'   Vector of representation types.
#' @export
getSupportedRepresentations = function(operator) {
  UseMethod("getSupportedRepresentations")
}

#' @export
getSupportedRepresentations.ecr_operator = function(operator) {
  attr(operator, "supported")
}

#' @title
#' Check if ecr operator supports given representation.
#'
#' @description
#' Check if the given operator supportds a certain representation, e.g., \dQuote{float}.
#'
#' @param operator [\code{ecr_operator}]\cr
#'   Object of type \code{ecr_operator}.
#' @param representation [\code{character(1)}]\cr
#'   Representation as a string.
#' @return [\code{logical(1)}]
#'   \code{TRUE}, if operator supports the representation type.
#' @export
is.supported = function(operator, representation) {
  UseMethod("is.supported")
}

#' @export
is.supported.ecr_operator = function(operator, representation) {
  return (representation %in% getSupportedRepresentations(operator))
}

#' @title
#' Get the operator's parameters.
#'
#' @description
#' Return a named list of parameters the operator was initialized with.
#'
#' @param operator [\code{ecr_operator}]\cr
#'   Operator object.
#' @return [\code{list}]
#'   Named list of parameters.
#' @export
getOperatorParameters = function(operator) {
  UseMethod("getOperatorParameters")
}

#' @export
getOperatorParameters.ecr_operator = function(operator) {
  attr(operator, "params")
}
