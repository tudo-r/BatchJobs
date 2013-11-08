\name{getJob}
\alias{getJob}
\title{Get job from registry by id.}
\usage{
  getJob(reg, id, load.fun = FALSE, check.id = TRUE)
}
\arguments{
  \item{reg}{[\code{\link{Registry}}]\cr Registry.}

  \item{id}{[\code{integer(1)}]\cr Id of job.}

  \item{load.fun}{[\code{logical(1)}]\cr Load job function
  from disk?  Default is \code{FALSE}.}

  \item{check.id}{[\code{logical(1)}]\cr Check the job id?
  Default is \code{TRUE}.}
}
\value{
  [\code{\link{Job}}].
}
\description{
  Get job from registry by id.
}

