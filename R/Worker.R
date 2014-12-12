# ******************** Constructors ********************

# Abstract base class constructor for general workers.
#
# @param nodename [\code{character(1)}]\cr
#   Host name of node.
# @param rhome [\code{character(1)}]\cr
#  Path to R installation on worker.
#  \dQuote{} means R installation on the PATH is used.
# @param r.options [\code{character}]
#   Options for R and Rscript, one option per element of the vector,
#   a la \dQuote{--vanilla}.
# @param script [\code{character(1)}]\cr
#   Path to helper script on worker.
#   Default means to call \code{\link{findHelperScriptLinux}}.
# @param ncpus [\code{integers(1)}]\cr
#   Number of VPUs of worker.
#   Default means to query the worker via \code{\link{getWorkerNumberOfCPUs}}.
# @param max.jobs [\code{integer(1)}]\cr
#   Maximal number of jobs that can run concurrently for the current registry.
#   Default is \code{ncpus}.
# @param max.load [\code{numeric(1)}]\cr
#   Load average (of the last 5 min) at which the worker is considered occupied,
#   so that no job can be submitted.
#   Default is \code{ncpus-1}.
# @param nice [\code{integer(1)}]\cr
#   Process priority to run R with set via nice. Integers between -20 and 19 are allowed.
#   If missing, processes are not nice'd and the system default applies (usually 0).
# @param classes [\code{character}]\cr
#   Extra classes, more specific than dQuote{Worker}.
#   Will be added to the class attribute of the object.
# @return [\code{\link{Worker}}].
makeWorker = function(ssh, nodename, rhome, r.options = c("--no-save", "--no-restore", "--no-init-file", "--no-site-file"),
  script, ncpus, max.jobs, max.load, nice, classes) {
  assertFlag(ssh)
  assertString(nodename)
  assertString(rhome)
  assertCharacter(r.options, any.missing = FALSE)
  if (missing(script)) {
    # FIXME: dont use linux specific in base class
    script = findHelperScriptLinux(rhome, r.options, ssh, nodename)
  } else {
    assertString(script)
  }

  # construct object partially so we can query ncpus
  w = as.environment(list(
    ssh = ssh,
    nodename = nodename,
    rhome = rhome,
    r.options = r.options,
    script = script,
    last.update = -Inf,
    available = "A", # worker is available, we can submit, set in update loop in scheduleWorkerJobs.R
    status = NULL))
  class(w) = c(classes, "Worker")

  if (missing(ncpus)) {
    ncpus = getWorkerNumberOfCPUs(w)
    messagef("Setting for worker %s: ncpus=%i", w$nodename, ncpus)
  } else {
    ncpus = asCount(ncpus)
  }
  if (missing(max.jobs)) {
    max.jobs = ncpus
  } else {
    max.jobs = asCount(max.jobs)
  }
  if (missing(max.load)) {
    max.load = ncpus-1L
  } else {
    assertNumber(max.load, lower = 0)
  }
  if (missing(nice)) {
    nice = ""
  } else {
    nice = asInt(nice, lower = -20, upper = 19)
  }

  w$ncpus = ncpus
  w$max.jobs = max.jobs
  w$max.load = max.load
  w$nice = nice
  return(w)
}

# ******************** Interface definition ********************

# Return number of cores on worker.
# @param worker [\code{\link{Worker}}].
#   Worker.
# @return [\code{integer(1)}].
getWorkerNumberOfCPUs = function(worker) {
  UseMethod("getWorkerNumberOfCPUs")
}

# Return 4 numbers to describe worker status.
# - load average of last 1 min, as given by e.g. uptime
# - number of R processes by _all_ users
# - number of R processes by _all_ users which have a load of >= 50%
# - number of R processes by current user which match $FILEDIR/jobs in the cmd call of R
# @param worker [\code{\link{Worker}}].
#   Worker.
# @param file.dir [\code{character(1)}}].
#   File dir of registry.
# @return [named \code{list} of \code{numeric(1)}].
getWorkerStatus = function(worker, file.dir) {
  UseMethod("getWorkerStatus")
}

# Start a job on worker, probably with R CMD BATCH.
# @param worker [\code{\link{Worker}}].
#   Worker.
# @param rfile [\code{character(1)}].
#   Path to R file to execute.
# @param outfile [\code{character(1)}].
#   Path to log file for R process.
# @return [\code{character(1)}]. Relevant process id.
startWorkerJob = function(worker, rfile, outfile) {
  UseMethod("startWorkerJob")
}

# Kill a job on worker. Really do it.
# @param worker [\code{\link{Worker}}].
#   Worker.
# @param pid [\code{character(1)}].
#   Process id from DB/batch.job.id to kill.
# @return Nothing.
killWorkerJob = function(worker, pid) {
  UseMethod("killWorkerJob")
}

# List all jobs on worker belonging to the current registry.
# @param worker [\code{\link{Worker}}].
#   Worker.
# @param file.dir [\code{character(1)}}].
#   File dir of registry.
# @return [\code{character}]. Vector of process ids.
listWorkerJobs = function(worker, file.dir) {
  UseMethod("listWorkerJobs")
}
