# @title
# Check termination conditions.
#
# @description
# Helper function which checks whether some stopping conditions
# are met and returns a termination object.
#
# @param logger [\code{ecr_monitor}]
#   Monitoring object.
# @param control [\code{ecr_control}]
#   Control object.
# @return [stopObject]
doTerminate = function(log, stop.conds) {
  stop.object = list()
  # if we have not specified any stopping conditions always return the empty object
  if (!length(stop.conds)) {
    return(stop.object)
  }

  # otherwise iterate over stopping conditions and check
  for (stop.conds in stop.conds) {
    shouldStop = stop.conds(log = log)
    if (shouldStop) {
      stop.object$name = attr(stop.conds, "name")
      stop.object$message = attr(stop.conds, "message")
      break
    }
  }
  return(stop.object)
}
