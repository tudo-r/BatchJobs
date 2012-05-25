# is a worker busy, see rules below
getWorkerSchedulerStatus = function(worker) {
  # we have already used up our maximal load on this node
  if (!worker$updated)
    "U"
  else if (worker$status$n.jobs >= worker$max.jobs)
    "J"
  # should not have too much load average
  else if (worker$status$load[1L] > worker$max.load)
    "L"
  # there are already ncpus expensive R jobs running on the node
  else if (worker$status$n.rprocs.50 >= worker$ncpus)
    "R"
  # should not have too many R sessions open
  else if(worker$status$n.rprocs > 3 * worker$ncpus)
    "r"
  # else all clear, submit the job!
  else
    "A"
}

# update status of worker IN PLACE
updateWorker = function(worker, file.dir, tdiff) {        
  time = as.integer(Sys.time())
  if (time - worker$last.update > tdiff) {
    worker$updated = TRUE
    worker$last.update = time
    worker$status = getWorkerStatus(worker, file.dir)
  } else {
    worker$updated = FALSE
  }
}

# find worker via isBusyWorker and update workers while looking
findWorker = function(workers, file.dir, tdiff) {
  lapply(workers, updateWorker, tdiff=tdiff)
  Find(function(w) getWorkerSchedulerStatus(w)=="A", workers, nomatch=NULL)
}
