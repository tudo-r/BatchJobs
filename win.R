#FIXME 
# - language dependent german counter names?
# - criterion to disallow submission?
# - getstatus? probably not possible

### interface ###
cfSubmitJob = function(conf, job.name, rscript, log.file, out.dir, resources, ...) {
  resources = cfCheckResources(resources)
  d1 = getRSlavesList()
  cmd = sprintf("R CMD BATCH --vanilla %s %s",
    rscript, log.file)
  system(cmd, intern=FALSE, wait=FALSE)
  # find out which R process was added
  d2 = getRSlavesList()
  batch.job.id = setdiff(d2$pid, d1$pid) 
  list(status=0, batch.job.id=batch.job.id, msg=as.character(NA))
}

cfKillJob = function(conf, batch.job.id) {
  cmd = sprintf("taskkill /PID %i /F /T", batch.job.id)
  system(cmd, intern=FALSE, wait=TRUE)
}

cfCheckResources = function(r) {
  if (length(r) > 0)
    stop("Resources cannot be used with these clusterFunctions!")
}

### helpers ###

getTaskList = function () {
  data = system("tasklist /FO CSV /NH", intern=TRUE)
  data = str_replace_all(data, "\"", "")
  data = strsplit(data, ",")
  data = as.data.frame(do.call(rbind, data)[,1:2], stringsAsFactors=FALSE)
  colnames(data) = c("name", "pid")
  data$pid = as.integer(data$pid)
  data
}


getRSlavesList = function () {
  data = getTaskList()
  subset(data, name=="R.exe")
}

getCPULoad = function() {
  s = system("typeperf \"\\Prozessor(_Total)\\Prozessorzeit (%)\" -sc 1", intern=TRUE)[3]
  s = str_replace_all(s, "\"", "")
  s = strsplit(s, ",")[[1]][2]
  as.numeric(s)
}


