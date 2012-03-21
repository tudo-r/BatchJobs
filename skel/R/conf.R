#' Loads a configuration file with cluster specific options.
#' See \code{\link{setBatchJobsConf}} for available options.
#' @param conffile [\code{character}]\cr
#'   Configuration file. Defaults to '~/.BatchJobs.R'.
#' @return [\code{logical}]. TRUE if file found, FALSE otherwise.
#' @export
useBatchJobsConf = function(conffile) {
  template = function() {
    paste("Format must be like this:\n",
          "  cluster.functions = <ClusterFunctions object>",
          "  mail.start = 'none' | 'all' | 'first' | 'last' | 'first+last'",
          "  mail.done = 'none' | 'all' | 'first' | 'last' | 'first+last'",
          "  mail.error = 'none' | 'all' | 'first' | 'last' | 'first+last'",
          "  mail.from = <sender address>",
          "  mail.to = <recipient address>",
          "  mail.control = <control object for sendmail package>\n\n",
          "Using default configuration.",
          sep = "\n")
  }

  if(missing(conffile)) {
    possible.conffiles = c(suppressWarnings(normalizePath(".BatchJobs.R")),
                           path.expand("~/.BatchJobs.R"),
                           file.path(.path.package("BatchJobs"), "etc", ".BatchJobs.R"))
    conffile = head(Filter(file.exists, possible.conffiles), 1L)
  }

  if(length(conffile) == 0L || !file.exists(conffile)) {
    message("Configuration file does not exist: '", conffile, "'")
    message(template())
    useDefaultBatchJobsConf()
  } else {
    message("Sourcing configuration file: ", conffile)
    ee = new.env()
    x = try(
      sys.source(conffile, envir=ee)
    )
    if (is.error(x)) {
      message("There was an error in sourcing your configuration file!")
      message(template())
      useDefaultBatchJobsConf()
    } else {
      conf = as.list(ee)
      checkConf(conf)
      do.call(setBatchJobsConf, conf)
    }
    invisible(TRUE)
  }
}

#' Set configuration options of package. Only provided parameters get changed.
#' The mailer settings mean: 
#' \dQuote{none} = do not mail for any job, \dQuote{all} = mail for all jobs,
#' \dQuote{first} = mail for first job, \dQuote{last} = mail for last job, 
#' \dQuote{first+last} = mail for first and last job.
#' The default settings are: \code{\link{makeClusterFunctionsInteractive}}, no mails. 
#' 
#' @title Set configuration options.
#' @param cluster.functions [\code{\link{ClusterFunctions}}]\cr
#'   Cluster functions to use. 
#' @param mail.start [\code{character(1)}]\cr
#'   For which jobs should mails be sent when the job is started?
#'   One of \dQuote{none}, \dQuote{all}, \dQuote{first}, \dQuote{last}, \dQuote{first+last}. 
#'   Default is \dQuote{none}.
#' @param mail.done [\code{character(1)}]\cr
#'   For which jobs should mails be sent when the job is done?
#'   One of \dQuote{none}, \dQuote{all}, \dQuote{first}, \dQuote{last}, \dQuote{first+last}. 
#'   Default is \dQuote{none}.
#' @param mail.error [\code{character(1)}]\cr
#'   For which jobs should mails be sent when the job generates an error?
#'   One of \dQuote{none}, \dQuote{all}, \dQuote{first}, \dQuote{last}, \dQuote{first+last}. 
#'   Default is \dQuote{none}.
#' @param mail.from [\code{character(1)}]\cr
#'   Sender adress of status notification mails.
#' @param mail.to [\code{character(1)}]\cr
#'   Recipient adress of status notification mails.
#' @param mail.control [\code{list}]\cr
#'   Control object for \code{\link[sendmailR]{sendmail}}.
#' @return Nothing.
#' @export
setBatchJobsConf = function(cluster.functions, mail.start, mail.done, mail.error, mail.from, 
                            mail.to, mail.control) {
  conf = getBatchJobsConf()
  if (!missing(cluster.functions)) {
    checkArg(cluster.functions, cl = "ClusterFunctions", na.ok = FALSE)
    conf$cluster.functions = cluster.functions
  }

  if (!missing(mail.start)) {
    checkArg(mail.start, choices = c("none", "first", "last", "first+last", "all"))
    conf$mail.start = mail.start
  } else {
    conf$mail.start = "none"
  }
  if (!missing(mail.done)) {
    checkArg(mail.done, choices = c("none", "first", "last", "first+last", "all"))
    conf$mail.done = mail.done
  } else {
    conf$mail.done = "none"
  }
  if (!missing(mail.error)) {
    checkArg(mail.error, choices = c("none", "first", "last", "first+last", "all"))
    conf$mail.error = mail.error
  } else {
    conf$mail.error = "none"
  }
  if (!missing(mail.from)) {
    checkArg(mail.from, cl = "character", len = 1L, na.ok = FALSE)
    conf$mail.from = mail.from
  }
  if (!missing(mail.to)) {
    checkArg(mail.to, cl = "character", len = 1L, na.ok = FALSE)
    conf$mail.to = mail.to
  }
  if (!missing(mail.control)) {
    checkArg(mail.control, cl = "list")
    conf$mail.control = mail.control
  } else {
    conf$mail.control = list()
  }
  conf$db.driver = "SQLite"
  conf$db.options = list()
  invisible(NULL)
}


useDefaultBatchJobsConf = function() {
  setBatchJobsConf(
    cluster.functions = makeClusterFunctionsInteractive(),
    mail.start = "none",
    mail.done = "none",
    mail.error = "none",
    mail.from = "",
    mail.to = "",
    mail.control = list()
  )
}

# loads conf into namespace on slave
loadConf = function(reg) {
  fn = getConfFilePath(reg)
  message("Loading conf: ", fn)
  ee = new.env()
  load(fn, envir=ee)
  ns = ls(ee$conf)
  # assign all stuff to conf in namespace
  conf = getBatchJobsConf()
  lapply(ns, function(x) assign(x, ee$conf[[x]], envir=conf))
  return(NULL)
}

getBatchJobsConf = function() {
  get(".BatchJobs.conf", envir=getNamespace("BatchJobs"))
}

saveConf = function(reg) {
  fn = getConfFilePath(reg)
  message("Saving conf: ", fn)
  conf = getBatchJobsConf()
  save(file=fn, conf)
}

checkConf = function(conf) {
  ns = names(conf)
  ns2 = c("cluster.functions", "mail.start", "mail.done", "mail.error", 
    "mail.from", "mail.to", "mail.control", "db.driver", "db.options")
  if (!all(ns %in% ns2))
    stop("You are only allowed to define the following R variables in your config file:\n",
      collapse(ns2, sep=", "))
}

getClusterFunctions = function(conf) {
  conf$cluster.functions
}