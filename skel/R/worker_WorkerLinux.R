# implementation of interface for local and remote linux workers
# delegate to onWorkerLinux

convertLinuxWorkerStatus = function(res) {
  res = str_trim(res)
  load = res[1]
  load = as.numeric(str_split(str_split(load, "load average: ")[[1]][2], ", ")[[1]][1])
  rprocs = res[-1]
  rprocs = str_split_fixed(rprocs, " +", 3)
  rprocs = as.data.frame(rprocs, stringsAsFactors=FALSE)
  rprocs[,2]=as.numeric(rprocs[,2])
  list(load = load, rprocs = rprocs)
}

getLinuxWorkerRJobs = function(rcmds, file.dir) {
  which(str_detect(rcmds, pattern=paste(file.dir, "jobs", sep="/")))
}

getWorkerNumberOfCPUs.WorkerLinux = function(worker) {
  as.integer(onWorkerLinux(worker, "number-of-cpus"))
}

getWorkerStatus.WorkerLinux = function(worker, file.dir) {
  res = onWorkerLinux(worker, "uptime-and-rprocs")
  res = convertLinuxWorkerStatus(res)
  list(
    load = res$load,
    n.rprocs = nrow(res$rprocs),
    n.rprocs.50 = sum(res$rprocs[,2] >= 50),
    n.jobs = length(getLinuxWorkerRJobs(res$rprocs[,3], file.dir))
  )
}

startWorkerJob.WorkerLinux = function(worker, rfile, outfile) {
  onWorkerLinux(worker, "start-job", c(worker$rhome, rfile, outfile))
}

killWorkerJob.WorkerLinux = function(worker, pid) {
  onWorkerLinux(worker, "kill-job", pid)
}

listWorkerJobs.WorkerLinux = function(worker, file.dir) {
  res = onWorkerLinux(worker, "uptime-and-rprocs")
  rprocs = convertLinuxWorkerStatus(res)$rprocs
  rprocs[getLinuxWorkerRJobs(rprocs[,3], file.dir),1]
}

