\name{makeClusterFunctionsLSF}
\alias{makeClusterFunctionsLSF}
\title{Create cluster functions for LSF systems.}
\usage{
makeClusterFunctionsLSF(template.file)
}
\arguments{
  \item{template.file}{[\code{character(1)}]\cr Path to a
  brew template file that is used for the job file.}
}
\value{
[\code{\link{ClusterFunctions}}].
}
\description{
Job files are created based on the brew template
\code{template.file}. This file is processed with brew and
then submitted to the queue using the \code{bsub} command.
Jobs are killed using the \code{bkill} command and the list
of running jobs is retrieved using \code{bjobs -u $USER
-w}. The user must have the appropriate privileges to
submit, delete and list jobs on the cluster (this is
usually the case).
}
\details{
The template file can access all arguments passed to the
\code{submitJob} function, see here
\code{\link{ClusterFunctions}}. It is the template file's
job to choose a queue for the job and handle the desired
resource allocations. Examples can be found on
\url{https://github.com/tudo-r/BatchJobs/tree/master/examples/cfLSF}.
}

