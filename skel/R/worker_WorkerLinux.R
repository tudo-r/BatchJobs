# implementation of interface for local and remote linux workers
# delegate to onWorkerLinux

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

listWorkerJobs.WorkerLinux = function(worker, file.dir) {
  onWorkerLinux(worker, "list-jobs")
}


