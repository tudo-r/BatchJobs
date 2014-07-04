# is a worker busy, see rules below
getWorkerSchedulerStatus = function(worker) {
  # we have already used up our maximal load on this node
  if (worker$status$n.jobs >= worker$max.jobs)
    return("J")
  # should not have too much load average
  if (worker$status$load[1L] > worker$max.load)
    return("L")
  # there are already ncpus expensive R jobs running on the node
  if (worker$status$n.rprocs.50 >= worker$ncpus)
    return("R")
  # should not have too many R sessions open
  if(worker$status$n.rprocs >= 3 * worker$ncpus)
    return("r")
  # else all clear, submit the job!
  return("A")
}

# update status of worker IN PLACE
updateWorker = function(worker, file.dir, tdiff) {
  time = now()
  if (worker$available == "A" || time - worker$last.update >= tdiff) {
    worker$last.update = time
    worker$status = getWorkerStatus(worker, file.dir)
    worker$available = getWorkerSchedulerStatus(worker)
  }
}

# find worker via isBusyWorker and update workers while looking
# workers with a low load are more likely to be selected when there are
# multiple workers available
findWorker = function(workers, file.dir, tdiff) {
  lapply(workers, updateWorker, file.dir = file.dir, tdiff = tdiff)
  rload = vnapply(workers, function(w) w$status$load / w$ncpus)
  Find(function(w) w$available=="A", sample(workers, prob = 1 / (rload + 0.1)), nomatch = NULL)
}
