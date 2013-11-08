\name{testJob}
\alias{testJob}
\title{Tests a job by running it with Rscript in a new process.}
\usage{
  testJob(reg, id, resources = list())
}
\arguments{
  \item{reg}{[\code{\link{Registry}}]\cr Registry.}

  \item{id}{[\code{integer(1)}]\cr Id of job to test.
  Default is first job id of registry.}

  \item{resources}{[\code{list}]\cr Usually not needed,
  unless you call the function \code{\link{getResources}}
  in your job.  See \code{\link{submitJobs}}.  Default is
  empty list.}
}
\value{
  [any]. Result of job. If the job did not complete because
  of an error, NULL is returned.
}
\description{
  Useful for debugging. Note that neither the registry,
  database or file directory are changed.
}
\examples{
reg <- makeRegistry(id="BatchJobsExample", file.dir=tempfile(), seed=123)
f <- function(x) if (x==1) stop("oops") else x
batchMap(reg, f, 1:2)
testJob(reg, 1)
testJob(reg, 2)
}
\seealso{
  \code{\link{reduceResults}}
}

