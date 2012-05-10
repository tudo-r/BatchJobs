getWorkerNumberOfCPUs.WorkerLinux = function(worker) {
  as.integer(onWorkerLinux(worker, "number-of-cpus"))
}

getWorkerStatus.WorkerLinux = function(worker, file.dir) {
  res = onWorkerLinux(worker, "status", file.dir)
  res = as.list(as.numeric(strsplit(res, " +")[[1L]]))
  names(res) = c("load", "n.rprocs", "n.rprocs.50", "n.jobs")
  return(res)
}

startWorkerJob.WorkerLinux = function(worker, rfile, outfile) {
  onWorkerLinux(worker, "start-job", c(worker$rhome, rfile, outfile))
}

killWorkerJob.WorkerLinux = function(worker, pid) {
  onWorkerLinux(worker, "kill-job", pid)
}

listWorkerJobs.WorkerLinux = function(worker, file.dir) {
  res = onWorkerLinux(worker, "list-jobs", file.dir)
  gsub("^\\s+|\\s+$", "", res)
}

