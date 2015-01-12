#' @title Submit jobs or chunks of jobs to batch system via cluster function.
#'
#' @description
#' If the internal submit cluster function completes successfully, the \code{retries}
#' counter is set back to 0 and the next job or chunk is submitted.
#' If the internal submit cluster function returns a fatal error, the submit process
#' is completely stopped and an exception is thrown.
#' If the internal submit cluster function returns a temporary error, the submit process
#' waits for a certain time, which is determined by calling the user-defined
#' \code{wait}-function with the current \code{retries} counter, the counter is
#' increased by 1 and the same job is submitted again. If \code{max.retries} is
#' reached the function simply terminates.
#'
#' Potential temporary submit warnings and errors are logged inside your file
#' directory in the file \dQuote{submit.log}.
#' To keep track you can use \code{tail -f [file.dir]/submit.log} in another
#' terminal.
#'
#'
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Vector for job id or list of vectors of chunked job ids.
#'   Only corresponding jobs are submitted. Chunked jobs will get executed
#'   sequentially as a single job for the scheduler.
#'   Default is all jobs which were not yet submitted to the batch system.
#' @param resources [\code{list}]\cr
#'   Required resources for all batch jobs. The elements of this list
#'   (e.g. something like \dQuote{walltime} or \dQuote{nodes} are defined by your template job file.
#'   Defaults can be specified in your config file.
#'   Default is empty list.
#' @param wait [\code{function(retries)}]\cr
#'   Function that defines how many seconds should be waited in case of a temporary error.
#'   Default is exponential back-off with \code{10*2^retries}.
#' @param max.retries [\code{integer(1)}]\cr
#'   Number of times to submit one job again in case of a temporary error
#'   (like filled queues). Each time \code{wait} is called to wait a certain
#'   number of seconds.
#'   Default is 10 times.
#' @param chunks.as.arrayjobs [\code{logical(1)}]\cr
#'   If ids are passed as a list of chunked job ids, execute jobs in a chunk
#'   as array jobs. Note that your scheduler and your template must be adjusted to
#'   use this option. Default is \code{FALSE}.
#' @param job.delay [\code{function(n, i)} or \code{logical(1)}]\cr
#'   Function that defines how many seconds a job should be delayed before it starts.
#'   This is an expert option and only necessary to change when you want submit
#'   extremely many jobs. We then delay the jobs a bit to write the submit messages as
#'   early as possible to avoid writer starvation.
#'   \code{n} is the number of jobs and \code{i} the number of the ith job.
#'   The default function used with \code{job.delay} set to \code{TRUE} is no delay for
#'   100 jobs or less and otherwise \code{runif(1, 0.1*n, 0.2*n)}.
#'   If set to \code{FALSE} (the default) delaying jobs is disabled.
#' @template arg_progressbar
#' @return [\code{integer}]. Vector of submitted job ids.
#' @export
#' @examples
#' reg = makeRegistry(id = "BatchJobsExample", file.dir = tempfile(), seed = 123)
#' f = function(x) x^2
#' batchMap(reg, f, 1:10)
#' submitJobs(reg)
#' waitForJobs(reg)
#'
#' # Submit the 10 jobs again, now randomized into 2 chunks:
#' chunked = chunk(getJobIds(reg), n.chunks = 2, shuffle = TRUE)
#' submitJobs(reg, chunked)
submitJobs = function(reg, ids, resources = list(), wait, max.retries = 10L, chunks.as.arrayjobs = FALSE,
  job.delay = FALSE, progressbar = TRUE) {
  ### helper function to calculate the delay
  getDelays = function(cf, job.delay, n) {
    if (is.logical(job.delay)) {
      if (job.delay && n > 100L && cf$name %nin% c("Interactive", "Multicore", "SSH")) {
        return(runif(n, n*0.1, n*0.2))
      }
      return(delays = rep.int(0, n))
    }
    vnapply(seq_along(ids), job.delay, n = n)
  }

  ### argument checks on registry and ids
  checkRegistry(reg)
  syncRegistry(reg)
  if (missing(ids)) {
    ids = dbFindSubmitted(reg, negate = TRUE)
    if (length(ids) == 0L) {
      info("All jobs submitted, nothing to do!")
      return(invisible(integer(0L)))
    }
  } else {
    if (is.list(ids)) {
      ids = lapply(ids, checkIds, reg = reg, check.present = FALSE)
      dbCheckJobIds(reg, unlist(ids))
    } else if(is.numeric(ids)) {
      ids = checkIds(reg, ids)
    } else {
      stop("Parameter 'ids' must be a integer vector of job ids or a list of chunked job ids (list of integer vectors)!")
    }
  }

  ### initialization of some helping vars
  conf = getBatchJobsConf()
  cf = getClusterFunctions(conf)
  limit.concurrent.jobs = is.finite(conf$max.concurrent.jobs)
  n = length(ids)

  ### argument checks for other parameters
  assertList(resources)
  resources = resrc(resources)

  if (missing(wait))
    wait = function(retries) 10 * 2^retries
  else
    assertFunction(wait, "retries")
  if (is.finite(max.retries))
    max.retries = asCount(max.retries)
  assertFlag(chunks.as.arrayjobs)
  if (chunks.as.arrayjobs && is.na(cf$getArrayEnvirName())) {
    warningf("Cluster functions '%s' do not support array jobs, falling back on chunks", cf$name)
    chunks.as.arrayjobs = FALSE
  }
  assert(checkFlag(job.delay), checkFunction(job.delay, c("n", "i")))
  assertFlag(progressbar)


  if (!is.null(cf$listJobs)) {
    ### check for running jobs
    ids.intersect = intersect(unlist(ids), dbFindOnSystem(reg, unlist(ids)))
    if (length(ids.intersect) > 0L) {
      stopf("Some of the jobs you submitted are already present on the batch system! E.g. id=%i.",
            ids.intersect[1L])
    }
  }

  if (limit.concurrent.jobs && (cf$name %in% c("Interactive", "Local", "Multicore", "SSH") || is.null(cf$listJobs))) {
    warning("Option 'max.concurrent.jobs' is enabled, but your cluster functions implementation does not support the listing of system jobs.\n",
            "Option disabled, sleeping 5 seconds for safety reasons.")
    limit.concurrent.jobs = FALSE
    Sys.sleep(5)
  }

  ### quick sanity check
  if (n > 5000L) {
    warningf(collapse(c("You are about to submit %i jobs.",
                        "Consider chunking them to avoid heavy load on the scheduler.",
                        "Sleeping 5 seconds for safety reasons."), sep = "\n"), n)
    Sys.sleep(5)
  }


  ### save config, start the work
  saveConf(reg)
  is.chunked = is.list(ids)
  info("Submitting %i chunks / %i jobs.", n, if(is.chunked) sum(viapply(ids, length)) else n)
  info("Cluster functions: %s.", cf$name)
  info("Auto-mailer settings: start=%s, done=%s, error=%s.", conf$mail.start, conf$mail.done, conf$mail.error)


  # use staged queries on master if fs.timeout is set
  # -> this way we are relatively sure that db transactions are performed in the intended order
  fs.timeout = conf$fs.timeout
  staged = conf$staged.queries && !is.na(fs.timeout)
  interrupted = FALSE

  submit.msgs = buffer(type = "list", capacity = 1000L, value = dbSendMessages,
                       reg = reg, max.retries = 10000L, sleep = function(r) 5,
                       staged = staged, fs.timeout = fs.timeout)

  logger = makeSimpleFileLogger(file.path(reg$file.dir, "submit.log"), touch = FALSE, keep = 1L)

  ### set on exit handler to avoid inconsistencies caused by user interrupts
  on.exit({
    # we need the second case for errors in brew (e.g. resources)
    if(interrupted && exists("batch.result", inherits = FALSE)) {
      submit.msgs$push(dbMakeMessageSubmitted(reg, id, time = submit.time,
        batch.job.id = batch.result$batch.job.id, first.job.in.chunk.id = if(is.chunked) id1 else NULL,
        resources.timestamp = resources.timestamp))
    }
    # send remaining msgs now
    info("Sending %i submit messages...\nMight take some time, do not interrupt this!", submit.msgs$pos())
    submit.msgs$clear()

    # message the existance of the log file
    if (logger$getSize())
      messagef("%i temporary submit errors logged to file '%s'.\nFirst message: %s",
               logger$getSize(), logger$getLogfile(), logger$getMessages(1L))
  })

  ### write R scripts
  info("Writing %i R scripts...", n)
  resources.timestamp = saveResources(reg, resources)
  rscripts = writeRscripts(reg, cf, ids, chunks.as.arrayjobs, resources.timestamp, disable.mail = FALSE,
    delays = getDelays(cf, job.delay, n))
  waitForFiles(rscripts, timeout = fs.timeout)

  ### reset status of jobs: delete errors, done, ...
  dbSendMessage(reg, dbMakeMessageKilled(reg, unlist(ids), type = "first"), staged = staged, fs.timeout = fs.timeout)

  ### initialize progress bar
  bar = getProgressBar(progressbar, max = n, label = "SubmitJobs")
  bar$set()

  tryCatch({
    for (i in seq_along(ids)) {
      id = ids[[i]]
      id1 = id[1L]
      retries = 0L

      repeat { # max.retries may be Inf
        if (limit.concurrent.jobs && length(cf$listJobs(conf, reg)) >= conf$max.concurrent.jobs) {
          # emulate a temporary erroneous batch result
          batch.result = makeSubmitJobResult(status = 10L, batch.job.id = NA_character_, "Max concurrent jobs exhausted")
        } else {
          # try to submit the job
          interrupted = TRUE
          submit.time = now()
          batch.result = cf$submitJob(
            conf = conf,
            reg = reg,
            job.name = sprintf("%s-%i", reg$id, id1),
            rscript = rscripts[i],
            log.file = getLogFilePath(reg, id1),
            job.dir = getJobDirs(reg, id1),
            resources = resources,
            arrayjobs = if(chunks.as.arrayjobs) length(id) else 1L
          )
        }

        ### validate status returned from cluster functions
        if (batch.result$status == 0L) {
          submit.msgs$push(dbMakeMessageSubmitted(reg, id, time = submit.time,
              batch.job.id = batch.result$batch.job.id, first.job.in.chunk.id = if(is.chunked) id1 else NULL,
              resources.timestamp = resources.timestamp))
          interrupted = FALSE
          bar$inc(1L)
          break
        }

        ### submitJob was not successful, handle the return status
        interrupted = FALSE

        if (batch.result$status > 0L && batch.result$status <= 100L) {
          if (is.finite(max.retries) && retries > max.retries)
            stopf("Retried already %i times to submit. Aborting.", max.retries)

          # temp error, wait and increase retries, then submit again in next iteration
          Sys.sleep(wait(retries))

          # log message to file
          logger$log(batch.result$msg)

          retries = retries + 1L
        } else if (batch.result$status > 100L && batch.result$status <= 200L) {
          # fatal error, abort at once
          stopf("Fatal error occured: %i. %s", batch.result$status, batch.result$msg)
        } else {
          # illeagal status code
          stopf("Illegal status code %s returned from cluster functions!", batch.result$status)
        }
      }
    }
  }, error = bar$error)

  ### return ids (on.exit handler kicks now in to submit the remaining messages)
  return(invisible(ids))
}
