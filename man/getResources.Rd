\name{getResources}
\alias{getResources}
\title{Function to get job resources in job function.}
\usage{
  getResources()
}
\value{
  [\code{list}].
}
\description{
  Return the list passed to \code{\link{submitJobs}}, e.g.
  nodes, walltime, etc.
}
\details{
  Can only be called in job function during job execution
  on slave.
}

