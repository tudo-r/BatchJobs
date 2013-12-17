\name{loadConfig}
\alias{loadConfig}
\title{Load a specific configuration file.}
\usage{
loadConfig(conffile = ".BatchJobs.R")
}
\arguments{
  \item{conffile}{[\code{character(1)}]\cr Location of the
  configuration file to load.  Default is
  \dQuote{.BatchJobs.conf} in the current working
  directory.}
}
\value{
Invisibly returns a list of configuration settings.
}
\description{
Load a specific configuration file.
}
\seealso{
\code{\link{getConfig}}, \code{\link{setConfig}}
}

