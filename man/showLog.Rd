\name{showLog}
\alias{showLog}
\title{Display the contents of a log file.}
\usage{
  showLog(reg, id, pager = getOption("pager"))
}
\arguments{
  \item{reg}{[\code{\link{Registry}}]\cr Registry.}

  \item{id}{[\code{integer(1)}]\cr Id of selected job.
  Default is first id in registry.}

  \item{pager}{[\code{any}]\cr Pager to use to display the
  log. Defaults to \code{getOption("pager")}.  This option
  is passed to \code{file.show} and is highly OS dependant
  and GUI dependant.  If either R's pager or the
  environment variable \dQuote{PAGER} is set to
  \dQuote{less} or \dQuote{vim}, the correct part of the
  log file will be shown.  Otherwise you find information
  about the correct part in the beginning of the displayed
  file.}
}
\value{
  [\code{character(1)}]. Invisibly returns path to log
  file.
}
\description{
  Display the contents of a log file, useful in case of
  errors.
}
\details{
  Note this rare special case: When you use chunking,
  submit some jobs, some jobs fail, then you resubmit these
  jobs again in different chunks, the log files will
  contain the log of the old, failed job as well.
  \code{showLog} tries to jump to the correct part of the
  new log file with a supported pager.
}
\seealso{
  \code{\link{grepLogs}}, \code{\link{getErrorMessages}}
}
