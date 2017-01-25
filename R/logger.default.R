setupECRDefaultLogger = function(step = 1L, log.stats = list("min", "mean", "max"),
  log.pop = FALSE, init.size = 1000L) {
  step = asInt(step)
  assertList(log.stats)
  assertFlag(log.pop)
  init.size = asInt(init.size)

  stats = ensureNamedStats(log.stats)

  force(step)
  force(stats)
  force(log.pop)
  force(init.size)

  monitor = makeECRMonitor(
    before = function(log, population, fitness, n.evals, ...) {
      catf("Initializing logger!")
      # nothing to do
    },
    step = function(log, population, fitness, n.evals, ...) {
      log$env$time.passed = Sys.time() - log$env$time.started
      log$env$n.gens = log$env$n.gens + 1L
      log$env$n.evals = log$env$n.evals + n.evals

      # keep track of best individual
      n.objectives = nrow(fitness)
      if (n.objectives == 1L) {
        #FIXME: maximization?
        if (is.null(log$env$best.y)) {
          log$env$best.x = NA
          log$env$best.y = Inf
        }
        cur.best.idx = which.min(as.numeric(fitness))
        cur.best.y = fitness[, cur.best.idx]
        if (log$env$best.y > cur.best.y) {
          log$env$best.y = cur.best.y
          log$env$best.x = population[cur.best.idx]
        }
      }

      if ((log$env$n.gens %% step) == 0L) {
        #catf("log size: %i, Gen: %i", nrow(log$env$stats), log$env$n.gens)
        # compute stats for current population
        cur.stats = lapply(stats, function(stat.fun) {
        if (is.list(stat.fun))
          return(do.call(stat.fun$fun, c(list(fitness), stat.fun$pars)))
        return(stat.fun(fitness))
        })
        # eventually make log bigger (exponential growing)
        #FIXME: make growing fun a parameter
        n.log = nrow(log$env$stats)
        if (n.log < log$env$cur.line) {
          catf("increasing log size! Doubling size: %i -> %i", n.log, 2 * n.log)
          log$env$stats = rbind(log$env$stats, makeDataFrame(ncol = length(stats) + 1L, nrow = n.log * 2, col.types = "numeric", col.names = names(log$env$stats)))
        }
        log$env$stats[log$env$cur.line, ] = c(list(gen = log$env$n.gens), cur.stats)
        log$env$cur.line = log$env$cur.line + 1L
      }
    },
    after = function(log, population, fitness, n.evals, ...) {
      # nothing to do here
      #FIXME: maybe delete empty, preallocated rows
    }
  )
  #FIXME: col.types need to be specifiable? by user
  monitor$env = new.env()
  monitor$env$stats = BBmisc::makeDataFrame(ncol = length(stats) + 1L, nrow = init.size,
    col.types = "numeric", col.names = c("gen", names(stats)))
  monitor$env$cur.line = 1L
  monitor$env$time.started = Sys.time()
  monitor$env$n.evals = 0L
  monitor$env$n.gens = 0L
  return(monitor)
}

#shouldStop.ecr_monitor = function(monitor, stop.conds)

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
