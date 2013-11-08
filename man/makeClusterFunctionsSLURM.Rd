\name{makeClusterFunctionsSLURM}
\alias{makeClusterFunctionsSLURM}
\title{Create cluster functions for SLURM-based systems.}
\usage{
  makeClusterFunctionsSLURM(template.file)
}
\arguments{
  \item{template.file}{[\code{character(1)}]\cr Path to a
  brew template file that is used for the SLURM job file.}
}
\value{
  [\code{\link{ClusterFunctions}}].
}
\description{
  Job files are created based on the brew template
  \code{template.file}. This file is processed with brew
  and then submitted to the queue using the \code{sbatch}
  command. Jobs are killed using the \code{scancel} command
  and the list of running jobs is retrieved using
  \code{squeue}. The user must have the appropriate
  privileges to submit, delete and list jobs on the cluster
  (this is usually the case).
}
\details{
  The template file can access all arguments passed to the
  \code{submitJob} function, see here
  \code{\link{ClusterFunctions}}. It is the template file's
  job to choose a queue for the job and handle the desired
  resource allocations. A simple example is provided here
  \url{http://code.google.com/p/batchjobs/source/browse/trunk/BatchJobs/examples/cfSLURM/simple.tmpl}
  and a more complex one here
  \url{http://code.google.com/p/batchjobs/source/browse/trunk/BatchJobs/examples/cfSLURM/dortmund_fk_statistik.tmpl}
  in the package repository on its homepage.
}

