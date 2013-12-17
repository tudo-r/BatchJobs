\name{getJobs}
\alias{getJobs}
\title{Get jobs from registry by id.}
\usage{
getJobs(reg, ids, load.fun = FALSE, check.ids = TRUE)
}
\arguments{
  \item{reg}{[\code{\link{Registry}}]\cr Registry.}

  \item{ids}{[\code{integer}]\cr Ids of jobs.  Default is
  all jobs.}

  \item{load.fun}{[\code{logical(1)}]\cr Load job function
  from disk?  Default is \code{FALSE}.}

  \item{check.ids}{[\code{logical(1)}]\cr Check the job
  ids?  Default is \code{TRUE}.}
}
\value{
[list of \code{\link{Job}}].
}
\description{
Get jobs from registry by id.
}

