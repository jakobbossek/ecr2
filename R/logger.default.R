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
    before = function(log, population, fitness, gen, ...) {
      catf("Initializing logger!")
      # nothing to do
    },
    step = function(log, population, fitness, gen, ...) {
      # catf("gen: %i, step: %i\n", gen, step)
      # print(log)
      if ((gen %% step) == 0L) {
        # compute stats for current population
        cur.stats = lapply(stats, function(stat.fun) {
        if (is.list(stat.fun))
          return(do.call(stat.fun$fun, c(list(fitness), stat.fun$pars)))
        return(stat.fun(fitness))
        })
        # evantually make log bigger (exponential growing)
        #FIXME: make growing fun a parameter
        n.log = nrow(log$env$stats)
        if (n.log < log$env$cur.line) {
          catf("increasing log size! Doubling size: %i -> %i", n.log, 3 * n.log)
          log$env$stats = rbind(log$env$stats, makeDataFrame(ncol = length(stats) + 1L, nrow = n.log * 2, col.types = "numeric", col.names = names(log$env$stats)))
        }
        log$env$stats[log$env$cur.line, ] = c(list(gen = gen), cur.stats)
        log$env$cur.line = log$env$cur.line + 1L
      }
    },
    after = function(log, population, fitness, gen, ...) {
      # nothing to do here
      #FIXME: maybe delete empty, preallocated rows
    }
  )
  #FIXME: col.types need to be specifiable? by user
  monitor$env = new.env()
  monitor$env$stats = BBmisc::makeDataFrame(ncol = length(stats) + 1L, nrow = init.size,
    col.types = "numeric", col.names = c("gen", names(stats)))
  monitor$env$cur.line = 1L
  return(monitor)
}

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
