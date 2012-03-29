# implementation of interface for local and remote linux workers
# delegate to onWorkerLinux

# FIXME rename helper commands / unify
getWorkerNumberOfCPUs.WorkerLinux = function(worker) {
  onWorkerLinux(worker, "number-of-cpus")
}

getWorkerStatus.WorkerLinux = function(worker, file.dir) {
  onWorkerLinux(worker, "worker-status", file.dir)
}

startWorkerJob.WorkerLinux = function(worker, work.dir, rfile, outfile) {
  onWorkerLinux(worker, "start-job", c(work.dir, rfile, outfile))
}

killWorkerJob.WorkerLinux = function(worker, pid) {
  onWorkerLinux(worker, "kill-job", pid)
}

# FIXME: use job dir
listWorkerJobs.WorkerLinux = function(worker) {
  onWorkerLinux(worker, "list-jobs")
}


