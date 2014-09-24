#' Cluster functions helper: Read in your brew template file.
#'
#' @description
#' This function is only intended for use in your own cluster functions implementation.
#'
#' Simply reads your template and returns it as a character vector.
#' If you do this in the constructor of your cluster functions once, you can avoid this
#' repeated file access later on.
#'
#' @param template.file [\code{character(1)}]\cr
#'   File path.
#' @return [\code{character}].
#' @export
cfReadBrewTemplate = function(template.file) {
  assertFile(template.file, "r")
  tmpl = readLines(template.file)
  if (length(tmpl) == 0L)
    stopf("Error reading template '%s' or empty template", template.file)
  collapse(tmpl, "\n")
}

#' Cluster functions helper: Brew your template into a job description file.
#'
#' @description
#' This function is only intended for use in your own cluster functions implementation.
#'
#' Calls brew silently on your template, any error will lead to an exception.
#' If debug mode is turned on in the configuration, the file is stored at the same place as the
#' corresponding R script in the \dQuote{jobs}-subdir of your files directory,
#' otherwise in the temp dir via \code{\link{tempfile}}.
#'
#' @param conf [\code{environment}]\cr
#'   BatchJobs configuration.
#' @param template [\code{character(1)}]\cr
#'   Job desfription template as a char vecrtor,
#'   possibly read in via \code{\link{cfReadBrewTemplate}}.
#' @param rscript [\code{character(1)}]\cr
#'   File path to you your corresponding R script for the job.
#' @param extension [\code{character(1)}]\cr
#'   Extension for the job description file, e.g. \dQuote{pbs}.
#' @return [\code{character(1)}]. File path of result.
#' @export
cfBrewTemplate = function(conf, template, rscript, extension) {
  assertEnvironment(conf)
  assertString(template)
  assertString(rscript)
  assertString(extension)
  if (conf$debug) {
    # if debug, place in jobs dir
    outfile = sub("\\.R$", sprintf(".%s", extension), rscript)
  } else {
    outfile = tempfile("template")
  }
  pf = parent.frame()
  old = getOption("show.error.messages")
  on.exit(options(show.error.messages = old))
  options(show.error.messages = FALSE)
  z = suppressAll(try(brew(text = template, output = outfile, envir = pf), silent = TRUE))
  if (is.error(z))
    stopf("Error brewing template: %s", as.character(z))
  waitForFiles(outfile, conf$fs.timeout)
  return(outfile)
}

#' Cluster functions helper: Handle an unknown error during job submission.
#'
#' @description
#' This function is only intended for use in your own cluster functions implementation.
#'
#' Simply constructs a \code{\link{SubmitJobResult}} object with status code 101,
#' NA as batch job id and an informative error message containing the output of the OS command in \code{output}.
#'
#' @param cmd [\code{character(1)}]\cr
#'   OS command used to submit the job, e.g. qsub.
#' @param exit.code [\code{integer(1)}]\cr
#'   Exit code of the OS command, should not be 0.
#' @param output [\code{character}]\cr
#'   Output of the OS command, hopefully an informative error message.
#'   If these are mutiple lines in a vector, they are automatically pasted together.
#' @return [\code{\link{SubmitJobResult}}].
#' @export
cfHandleUnknownSubmitError = function(cmd, exit.code, output) {
  assertString(cmd)
  exit.code = asInt(exit.code)
  assertCharacter(output, any.missing = FALSE)
  msg = sprintf("%s produced exit code %i; output %s",
    cmd, exit.code, collapse(output, sep = "\n"))
  makeSubmitJobResult(status = 101L, batch.job.id = NA_character_, msg = msg)
}

#' Cluster functions helper: Kill a batch job via OS command
#'
#' @description
#' This function is only intended for use in your own cluster functions implementation.
#'
#' Calls the OS command to kill a job via \code{system} like this: \dQuote{cmd batch.job.id}.
#' If the command returns an exit code > 0, the command is repeated
#' after a 1 second sleep \code{max.tries-1} times.
#' If the command failed in all tries, an exception is generated.
#'
#' @param cmd [\code{character(1)}]\cr
#'   OS command, e.g. \dQuote{qdel}.
#' @param batch.job.id [\code{character(1)}]\cr
#'   Id of the batch job on the batch system.
#' @param max.tries [\code{integer(1)}]\cr
#'   Number of total times to try execute the OS command in cases of failures.
#'   Default is \code{3}.
#' @return Nothing.
#' @export
cfKillBatchJob = function(cmd, batch.job.id, max.tries = 3L) {
  assertString(cmd)
  assertString(batch.job.id)
  max.tries = asCount(max.tries)
  assertCount(max.tries)

  for (tmp in seq_len(max.tries)) {
    res = runOSCommandLinux(cmd, batch.job.id, stop.on.exit.code = FALSE)
    if (res$exit.code == 0L)
      return()
    Sys.sleep(1)
  }
  stopf("Really tried to kill job, but could not do it. batch job id is %s.\nMessage: %s",
        batch.job.id, collapse(res$output, sep = "\n"))
}
