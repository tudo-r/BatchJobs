\name{setConfig}
\alias{setConfig}
\title{Set and overwrite configuration settings}
\usage{
setConfig(conf = list(), ...)
}
\arguments{
  \item{conf}{[\code{Config} or \code{list}]\cr List of
  configuration parameters as returned by
  \code{\link{loadConfig}} or \code{\link{getConfig}}.}

  \item{...}{[\code{ANY}]\cr Named configuration
  parameters. Overwrites parameters in \code{conf}, if
  provided.}
}
\value{
Invisibly returns a list of configuration settings.
}
\description{
Set and overwrite configuration settings
}
\seealso{
\code{\link{getConfig}}, \code{\link{loadConfig}}
}

