\name{makeSSHWorker}
\alias{makeSSHWorker}
\alias{SSHWorker}
\title{Create SSH worker for SSH cluster functions.}
\usage{
  makeSSHWorker(nodename, rhome = "", ncpus, max.jobs,
    max.load, nice,
    r.options = c("--no-save", "--no-restore", "--no-init-file", "--no-site-file"),
    script)
}
\arguments{
  \item{nodename}{[\code{character(1)}]\cr Host name of
  node.}

  \item{rhome}{[\code{character(1)}]\cr Path to R
  installation on worker.  \dQuote{} means R installation
  on the PATH is used, of course this implies that it must
  be on the PATH (also for non-interactive shells)!
  Default is \dQuote{}.}

  \item{ncpus}{[\code{integers(1)}]\cr Number of VPUs of
  worker.  Default means to query the worker via
  \dQuote{/proc/cpuinfo}.}

  \item{max.jobs}{[\code{integer(1)}]\cr Maximal number of
  jobs that can run concurrently for the current registry.
  Default is \code{ncpus}.}

  \item{max.load}{[\code{numeric(1)}]\cr Load average (of
  the last 5 min) at which the worker is considered
  occupied, so that no job can be submitted.  Default is
  \code{ncpus-1}.}

  \item{nice}{[\code{integer(1)}]\cr Process priority to
  run R with set via nice. Integers between -20 and 19 are
  allowed.  If missing, processes are not nice'd and the
  system default applies (usually 0).  Default is no
  niceing.}

  \item{r.options}{[\code{character}] Options for R and
  Rscript, one option per element of the vector, a la
  \dQuote{--vanilla}.  Default is \code{c("--no-save",
  "--no-restore", "--no-init-file", "--no-site-file")}.}

  \item{script}{[\code{character(1)}]\cr Path to helper
  bash script which interacts with the worker.  You really
  should not have to touch this, as this would imply that
  we have screwed up and published an incompatible version
  for your system.  This option is only provided as a last
  resort for very experienced hackers.  Note that the path
  has to be absolute.  This is what is done in the package:
  \url{http://code.google.com/p/batchjobs/source/browse/trunk/BatchJobs/skel/inst/bin/linux-helper}
  Default means to take it from package directory.}
}
\value{
  [\code{\link{SSHWorker}}].
}
\description{
  Create SSH worker for SSH cluster functions.
}

