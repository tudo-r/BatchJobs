\name{makeClusterFunctionsLocal}
\alias{makeClusterFunctionsLocal}
\title{Create cluster functions for sequential execution on local host.}
\usage{
makeClusterFunctionsLocal()
}
\value{
[\code{\link{ClusterFunctions}}].
}
\description{
All jobs executed under these cluster functions are
executed sequentially, but in an independent, new R
session. That is, \code{submitJob} does not return until
the job has finished. The main use of this
\code{ClusterFunctions} implementation is to test and debug
programs on a local computer.
}
\details{
Listing jobs returns an empty vector (as no jobs can be
running when you call this) and \code{killJob} returns at
once (for the same reason).
}

