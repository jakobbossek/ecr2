#' @title
#' Initialize a log object.
#'
#' @description Logging is a central aspect of each EA. Besides the final solution(s)
#' especially in research often we need to keep track of different aspects of the
#' evolutionary process, e.g., fitness statistics. The logger of ecr keeps
#' track of different user-defined statistics and the population.
#' It may also be used to check stopping conditions (see \code{makeECRTerminator}). Most
#' important this logger is used internally by the \code{\link{ecr}} black-box interface.
#'
#' @note Statistics are logged in a \code{data.frame}.
#'
#' @template arg_control
#' @param log.stats [\code{list}]\cr
#'   List of lists for statistic computation on attributes of the individuals
#'   of the population. Each entry should be named by the attribute it should be
#'   based on, e.g., fitness, and should contain a list of R functions as a
#'   character string or a a list with elements \code{fun} for the function, and \code{pars} for additional
#'   parameters which shall be passed to the corresponding function.
#'   Each function is required to return a scalar numeric value.
#'   By default the minimum, mean and maximum of the fitness values is computed.
#'   Since fitness statistics are the most important ones these do not have to
#'   be stored as attributes, but can be passed as a matrix to the update function.
#' @param log.extras [\code{character}]\cr
#'   Possibility to instruct the logger to store additional
#'   scalar values in each generation. Named character vector where the names
#'   indicate the value to store and the value indicates the corresponding data types.
#'   Currently we support all atomic modes of \code{\link[base]{vector}} expect \dQuote{factor}
#'   and \dQuote{raw}.
#' @template arg_logpop
#' @param init.size [\code{integer(1)}]\cr
#'   Initial number of rows of the slot of the logger, where the fitness
#'   statistics are stored. The size of the statistics log is doubled each time an
#'   overflow occurs.
#'   Default is 1000.
#' @return [\code{ecr_logger}]
#'   An S3 object of class \code{ecr_logger} with the following components:
#'   \describe{
#'     \item{log.stats}{The \code{log.stats} list.}
#'     \item{log.pop}{The \code{log.pop} parameter.}
#'     \item{init.size}{Initial size of the log.}
#'     \item{env}{The actual log. This is an R environment which ensures, that
#'     in-place modification is possible.}
#'   }
#' @family logging
#' @examples
#' control = initECRControl(function(x) sum(x), minimize = TRUE,
#'   n.objectives = 1L)
#' control = registerECROperator(control, "mutate", mutBitflip, p = 0.1)
#' control = registerECROperator(control, "selectForMating", selTournament, k = 2)
#' control = registerECROperator(control, "selectForSurvival", selGreedy)
#'
#' log = initLogger(control,
#'   log.stats = list(
#'     fitness = list("mean", "myRange" = function(x) max(x) - min(x)),
#'     age = list("min", "max")
#'   ), log.pop = TRUE, init.size = 1000L)
#'
#'  # simply pass stuff down to control object constructor
#' population = initPopulation(mu = 10L, genBin, n.dim = 10L)
#' fitness = evaluateFitness(control, population)
#'
#' # append fitness to individuals and init age
#' for (i in seq_along(population)) {
#'   attr(population[[i]], "fitness") = fitness[, i]
#'   attr(population[[i]], "age") = 1L
#' }
#'
#' for (iter in seq_len(10)) {
#'   # generate offspring
#'   offspring = generateOffspring(control, population, fitness, lambda = 5)
#'   fitness.offspring = evaluateFitness(control, offspring)
#'
#'   # update age of population
#'   for (i in seq_along(population)) {
#'     attr(population[[i]], "age") = attr(population[[i]], "age") + 1L
#'   }
#'
#'   # set offspring attributes
#'   for (i in seq_along(offspring)) {
#'     attr(offspring[[i]], "fitness") = fitness.offspring[, i]
#'     # update age
#'     attr(offspring[[i]], "age") = 1L
#'   }
#'
#'   sel = replaceMuPlusLambda(control, population, offspring)
#'
#'   population = sel$population
#'   fitness = sel$fitness
#'
#'   # do some logging
#'   updateLogger(log, population, n.evals = 5)
#' }
#' head(getStatistics(log))
#' @export
initLogger = function(
  control,
  log.stats = list(fitness = list("min", "mean", "max")),
  log.extras = NULL,
  log.pop = FALSE, init.size = 1000L) {

  assertClass(control, "ecr_control")
  assertList(log.stats, names = "unique")
  assertFlag(log.pop)
  init.size = asInt(init.size, lower = 10L)

  # init names
  n.stat.cats = length(log.stats)
  stat.cat.names = names(log.stats)

  stats = lapply(1:length(log.stats), function(i) {
    the.stats = log.stats[[i]]
    nm = stat.cat.names[i]
    tmp = ensureNamedStats(the.stats)
    names(tmp) = paste(nm, names(tmp), sep = ".")
    tmp
  })
  names(stats) = names(log.stats)

  n.stats = sum(sapply(stats, length))
  stat.names = unname(do.call(c, lapply(stats, names)))
  stat.types = rep("numeric", n.stats)

  #
  n.extras = 0
  extra.names = character(0L)
  extra.types = character(0L)
  if (!is.null(log.extras)) {
    assertCharacter(log.extras, names = "unique", any.missing = FALSE, all.missing = FALSE)
    assertSubset(log.extras, choices = c("logical", "integer", "double", "numeric", "complex", "character"))
    extra.names = names(log.extras)
    extra.types = unname(log.extras)
    n.extras = length(log.extras)
  }

  env = new.env()
  env$stats = BBmisc::makeDataFrame(
    ncol = 1L + n.stats + n.extras, nrow = init.size,
    col.types = c("integer", stat.types, extra.types),
    col.names = c("gen", stat.names, extra.names))

  env$stats = addClasses(env$stats, "ecr_statistics")
  env$cur.line = 1L
  env$time.started = Sys.time()
  env$n.evals = 0L
  env$n.gens = 0L
  env$task = control$task
  env$log.pop = log.pop
  env$extra.names = extra.names

  if (log.pop) {
    env$pop = vector("list", length = init.size)
  }

  makeS3Obj("ecr_logger",
    log.stats = stats,
    log.pop = log.pop,
    init.size = init.size,
    env = env)
}

#' @title Update the log.
#'
#' @description This function modifies the log in-place, i.e., without making
#' copies.
#'
#' @param log [\code{ecr_logger}]\cr
#'   The log generated by \code{initLogger}.
#' @param population [\code{list}]\cr
#'   List of individuals.
#' @param fitness [\code{matrix}]\cr
#'   Optional matrix of fitness values (each column contains the fitness value(s) for
#'   one individual) of \code{population}. If no matrix is passed and the log shall
#'   store information of the fitness, each individual needs to have an attribute fitness.
#' @param n.evals [\code{integer(1)}]\cr
#'   Number of fitness function evaluations performed in the last generation.
#' @param extras [\code{list}]\cr
#'   Optional named list of additional scalar values to log.
#'   See \code{log.extras} argument of \code{\link{initLogger}} for details.
#' @param ... [any]\cr
#'   Furhter arguments. Not used at the moment.
#' @family logging
#' @export
updateLogger = function(log, population, fitness = NULL, n.evals, extras = NULL, ...) {
  # basic stuff
  log$env$time.passed = Sys.time() - log$env$time.started
  log$env$n.gens = log$env$n.gens + 1L
  log$env$n.evals = log$env$n.evals + n.evals

  if (is.null(fitness))
    fitness = do.call(cbind, lapply(population, function(ind) attr(ind, "fitness")))

  # keep track of best individual
  n.objectives = nrow(fitness)
  if (n.objectives == 1L) {
    if (is.null(log$env$best.y)) {
      log$env$best.x = NA
      log$env$best.y = if (log$env$task$minimize) Inf else -Inf
    }
    if (log$env$task$minimize) {
      cur.best.idx = which.min(as.numeric(fitness))
      cur.best.y = fitness[, cur.best.idx]
      if (log$env$best.y > cur.best.y) {
        log$env$best.y = cur.best.y
        log$env$best.x = population[cur.best.idx]
      }
    } else {
      cur.best.idx = which.max(as.numeric(fitness))
      cur.best.y = fitness[, cur.best.idx]
      if (log$env$best.y < cur.best.y) {
        log$env$best.y = cur.best.y
        log$env$best.x = population[cur.best.idx]
      }
    }
  }

  # eventually make log bigger (exponential growing)
  #FIXME: make growing fun a parameter
  n.log = nrow(log$env$stats)

  getTypes = function(obj) {
    unname(sapply(obj, typeof))
  }

  # grow memory
  if (n.log < log$env$cur.line) {
    #catf("increasing log size! Doubling size: %i -> %i", n.log, 2 * n.log)
    log$env$stats = rbind(log$env$stats,
      makeDataFrame(ncol = ncol(log$env$stats),
      nrow = n.log * 2,
      col.types = getTypes(log$env$stats),
      col.names = names(log$env$stats)))

    if (log$env$log.pop) {
      log$env$pop = c(log$env$pop, vector("list", length = n.log * 2))
    }
  }

  #catf("log size: %i, Gen: %i", nrow(log$env$stats), log$env$n.gens)
  # compute stats for current population
  stat.types = names(log$log.stats)

  cur.stats = lapply(stat.types, function(stat.type) {
    stat.funs = log$log.stats[[stat.type]]
    # get the corresponding information from the population
    objs = if (stat.type == "fitness") fitness else
      do.call(cbind, lapply(population, function(ind) attr(ind, stat.type)))
    #print(objs)
    lapply(stat.funs, function(stat.fun) {
      if (is.list(stat.fun))
        return(do.call(stat.fun$fun, c(list(objs), stat.fun$pars)))
      return(stat.fun(objs))
    })
  })
  cur.stats = unlist(cur.stats)

  if (length(log$env$extra.names) > 0L) {
    extra.names = log$env$extra.names
    if (is.null(extras)) {
      extras = BBmisc::namedList(init = NA, names = extra.names)
    }
    assertList(extras, names = "unique", any.missing = TRUE, all.missing = TRUE)
    assertSubset(names(extras), extra.names)
  }

  #print(cur.stats)
  #stop("hooray!!")

  # cur.stats = lapply(log$log.stats, function(stat.fun) {
  #   if (is.list(stat.fun))
  #     return(do.call(stat.fun$fun, c(list(fitness), stat.fun$pars)))
  #   return(stat.fun(fitness))
  # })

  log$env$stats[log$env$cur.line, ] = c(list(gen = log$env$n.gens), cur.stats, extras)

  # store population if requested
  if (log$env$log.pop) {
    log$env$pop[[log$env$cur.line]] = list(population = population, fitness = fitness)
  }
  log$env$cur.line = log$env$cur.line + 1L
}

# Helper function to ensure proper naming of the log.stats list passed
# to the logger.
ensureNamedStats = function(stats) {
  no.names = names(stats) == ""
  if (length(no.names) == 0L)
    no.names = rep(TRUE, length(stats))
  # which funs are no characters?
  no.char = !sapply(stats, is.character)
  # if both is true, i.e. unnamed and no char, we cannot determine a name
  if (all(no.char & no.names))
    stopf("log.stats needs to be a list of function names as strings or named lists.")
  # otherwise take chars as names ...
  names(stats)[no.names] = stats[no.names]
  # ... and convert names to functions
  stats[no.names] = sapply(stats[no.names], get)
  return(stats)
}

#' @title Access the logged statistics.
#'
#' @description Simple getter for the logged fitness statistics.
#'
#' @param log [\code{ecr_logger}]\cr
#'   The log generated by \code{initLogger}.
#' @param trim [\code{logical(1)}]\cr
#'   Should unused lines in the logged be cut off?
#'   Usually one wants this behaviour.
#'   Thus the default is \code{TRUE}.
#' @return [\code{data.frame}] Logged statistics.
#' @family logging
#' @export
getStatistics = function(log, trim = TRUE) {
  assertClass(log, "ecr_logger")
  assertFlag(trim)
  stats = log$env$stats
  if (trim & (log$env$cur.line < nrow(stats))) {
    stats = stats[seq.int(log$env$cur.line - 1L), , drop = FALSE]
  }
  return(stats)
}

#' @title Access to logged populations.
#'
#' @description Simple getter for the logged populations.
#'
#' @details This functions throws an error if the logger was initialized with
#' \code{log.pop = FALSE} (see \code{initLogger}).
#'
#' @param log [\code{ecr_logger}]\cr
#'   The log generated by \code{initLogger}.
#' @param trim [\code{logical(1)}]\cr
#'   Should unused lines in the logged be cut off?
#'   Usually one wants this behaviour.
#'   Thus the default is \code{TRUE}.
#' @return [\code{list}] List of populations.
#' @family logging
#' @export
getPopulations = function(log, trim = TRUE) {
  assertClass(log, "ecr_logger")
  assertFlag(trim)
  if (!log$log.pop)
    stopf("Log did not store populations.")

  pops = log$env$pop
  if (is.null(pops))
    stopf("This should not happen. Populations should be saved, but not found.")

  if (trim & (log$env$cur.line < length(pops))) {
    pops = pops[seq.int(log$env$cur.line - 1L)]
  }
  return(pops)
}

#' @title Transform to long format.
#'
#' @description Transform the data.frame of logged statistics from wide to
#' \pkg{ggplot2}-friendly long format.
#'
#' @param x [\code{ecr_statistics} | \code{ecr_logger}]\cr
#'   Logger object or statistics data frame from logger object.
#' @param drop.stats [\code{character}]\cr
#'   Names of logged statistics to be dropped.
#'   Default is the empty character, i.e., not to drop any stats.
#' @return [\code{data.frame}]
#' @export
toGG = function(x, drop.stats = character(0L)) {
  UseMethod("toGG")
}

#' @export
toGG.ecr_statistics = function(x, drop.stats = character(0L)) {
  assertCharacter(drop.stats)
  x = dropStatistics(x, drop.stats)
  BBmisc::requirePackages("reshape2", why = "ecr::toGG")
  melt(x, "gen", value.name = "value", variable.name = "stat")
}

#' @export
toGG.ecr_logger = function(x, drop.stats = character(0L)) {
  toGG(getStatistics(x), drop.stats = drop.stats)
}

#' @title Generate line plot of logged statistics.
#'
#' @description Expects a data.frame of logged statistics, e.g., extracted from
#' a logger object by calling \code{\link{getStatistics}}, and generates a basic
#' line plot. The plot is generated with the \pkg{ggplot2} package and the ggplot
#' object is returned.
#'
#' @inheritParams toGG
#' @export
plotStatistics = function(x, drop.stats = character(0L)) {
  UseMethod("plotStatistics")
}

#' @export
plotStatistics.ecr_statistics = function(x, drop.stats = character(0L)) {
  stats = toGG(x, drop.stats = drop.stats)
  plotStatistics(stats)
}

#' @export
plotStatistics.data.frame = function(x, drop.stats = character(0L)) {
  x = dropStatistics(x, drop.stats)
  if (!(isSubset(c("gen", "value", "stat"), colnames(x))))
    stopf("plotStatistics: Data frame needs at least the columns 'gen', 'value' and 'stat'.")
  requirePackages("ggplot2", why = "ecr::plotStatistics")
  pl = ggplot(x, aes_string(x = "gen", y = "value", linetype = "stat")) + geom_line()
  return(pl)
}

#' @export
plotStatistics.ecr_logger = function(x, drop.stats = character(0L)) {
  return(plotStatistics(getStatistics(x), drop.stats = drop.stats))
}

# Helper to drop some stats from statistics data frame.
dropStatistics = function(x, drop.stats = character(0L)) {
  stat.names = colnames(x)
  if ("gen" %in% drop.stats)
    stopf("Name 'gen' cannot be deleted from stats.")
  if (length(drop.stats) > 0L)
    x = x[, -which(stat.names %in% drop.stats)]
  return(x)
}
