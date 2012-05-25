#' Submit jobs or chunks of jobs to batch system via cluster function.
#'
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
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Vector for job id or list of vectors of chunked job ids.
#'   Only corresponding jobs are submitted. Chunked jobs will get executed
#'   sequentially as a single job for the scheduler.
#'   Default is all jobs which were not yet submitted to the batch system.
#' @param resources [\code{list}]\cr
#'   Required resources for all batch jobs.
#'   Default is empty list.
#' @param wait [\code{function(retries)}]\cr
#'   Function that defines how many seconds should be waited in case of a temporary error.
#'   Default is exponential back-off with \code{10*2^retries}.
#' @param max.retries [\code{integer(1)}]\cr
#'   Number of times to submit one job again in case of a temporary error
#'   (like filled queues). Each time \code{wait} is called to wait a certain
#'   number of seconds.
#'   Default is 10 times.
#' @param job.delay [\code{function(n, i)}]\cr
#'   Function that defines how many seconds a job should be delayed before it starts.
#'   This is an expert option and only necessary to change, when you want submit
#'   extremely many jobs. We then delay the jobs a bit to write the submit messages as
#'   early as possible to avoid writer starvation.
#'   \code{n} is the number of jobs and \code{i} the number of
#'   the ith job.
#'   The default is no delay for less than 100 jobs and otherwise
#'   \code{runif(1, 0.1*n, 0.2*n)}.
#' @return Vector of submitted job ids.
#' @export
#' @examples
#' reg <- makeRegistry(id="BatchJobsExample", file.dir=tempfile(), seed=123)
#' f <- function(x) x^2
#' batchMap(reg, f, 1:10)
#' submitJobs(reg)
submitJobs = function(reg, ids, resources=list(), wait, max.retries=10L, job.delay) {

  checkArg(reg, cl="Registry")
  if (missing(ids)) {
    ids = dbGetNotSubmitted(reg)
    if (length(ids) == 0L) {
      message("All jobs submitted, nothing to do!")
      return(invisible(NULL))
    }
  } else {
    if (is.list(ids)) {
      ids = lapply(ids, convertIntegers)
      checkListElementClass(ids, "integer")
      if(any(is.na(unlist(ids))))
        stop("Chunks must not contain NAs!")
    } else if(is.numeric(ids)) {
      ids = convertInteger(ids)
      checkArg(ids, "integer", na.ok=FALSE)
    } else {
      stop("ids must be a integer vector of job ids or a list of chunked job ids (list of integer vectors)!")
    }
    checkIdsPresent(reg, unlist(ids))
  }
  checkArg(resources, "list")
  if(!isProperlyNamed(resources))
    stop("'resources' must be all be uniquely named!")

  if (missing(wait))
    wait = function(retries) 10 * 2^retries # ^ always converts to double
  else
    checkArg(wait, formals="retries")

  if(!is.infinite(max.retries)) {
    max.retries = convertInteger(max.retries)
    checkArg(max.retries, "integer", len=1L, na.ok=FALSE)
  }

  if (missing(job.delay)) {
    job.delay = function(n, i)
      if (n > 100L) runif(1L, n*0.1, n*0.2) else 0
  } else {
    checkArg(job.delay, formals=c("n", "i"))
  }

  if (!is.null(getListJobs())) {
    ids.intersect = intersect(unlist(ids), findOnSystem(reg))
    if (length(ids.intersect) > 0L) {
      stopf("Some of the jobs you submitted are already present on the batch system! E.g. id=%i.",
        ids.intersect[1L])
    }
  }
  saveConf(reg)

  is.chunks = is.list(ids)
  conf = getBatchJobsConf()
  cf = getClusterFunctions(conf)
  messagef("Submitting %i chunks / %i jobs.",
    length(ids), if(is.chunks) sum(vapply(ids, length, integer(1L))) else length(ids))
  messagef("Cluster functions: %s.", cf$name)
  messagef("Auto-mailer settings: start=%s, done=%s, error=%s.",
    conf$mail.start, conf$mail.done, conf$mail.error)

  interrupted = FALSE
  submit.msgs = buffer("list", 1000L, dbFlushMessages,
                       reg=reg, max.retries=10000L, sleep=function(r) 5)

  # set on exit handler to avoid inconsistencies caused by user interrupts
  on.exit({
    # we need the second case for errors in brew (e.g. resources)
    if(interrupted && exists("batch.result", inherits=FALSE)) {
      submit.msgs$push(dbMakeMessageSubmitted(reg, id, time=submit.time,
        batch.job.id=batch.result$batch.job.id, first.job.in.chunk.id = if(is.chunks) id1 else NULL))
    }
    # if we have remaining messages send them now
    messagef("Sending %i submit messages...\nMight take some time, do not interrupt this!",
      submit.msgs$size())
    submit.msgs$clear()
  })

  # write R scripts before so we save some time in the important loop
  messagef("Writing %i R scripts...", length(ids))
  delays = vapply(seq_along(ids), job.delay, numeric(1L), n=length(ids))
  writeRscripts(reg, ids, disable.mail=FALSE, delays=delays,
               interactive.test = !is.null(conf$interactive))

  bar = makeProgressBar(max=length(ids), label="submitJobs               ")
  bar$set()

  tryCatch({
    for (id in ids) {
      id1 = id[1L]
      # we use no for loop here to allow infinite retries
      retries = 0L
      repeat {
        # try to submit the job
        submit.time = as.integer(Sys.time())
        interrupted = TRUE
        batch.result = cf$submitJob(conf=conf, reg=reg,
                                    job.name=sprintf("%s-%i", reg$id, id1),
                                    rscript=getRScriptFilePath(reg, id1),
                                    log.file=getLogFilePath(reg, id1),
                                    job.dir=getJobDirs(reg, id1),
                                    resources=resources)

        # validate status returned from cluster functions
        if (batch.result$status == 0L) {
          submit.msgs$push(dbMakeMessageSubmitted(reg, id, time=submit.time,
            batch.job.id=batch.result$batch.job.id, first.job.in.chunk.id = if(is.chunks) id1 else NULL))
          interrupted = FALSE
          bar$inc(1L)
          break
        }

        # submitJob was not successful, therefore we don't care for interrupts any more
        interrupted = FALSE

        if (batch.result$status > 0L && batch.result$status <= 100L) {
          # if temp error, wait and increase retries, then submit again
          sleep.secs = wait(retries)

          retries = retries + 1L
          if (retries > max.retries)
            stopf("Retried already %i times to submit. Aborting.", retries)

          bar$inc(msg=sprintf("Status: %i, zzz=%.1fs.", batch.result$status, sleep.secs))
          #warningf("Id: %i. Temporary error: %s. Retries: %i. Sleep: %.1fs.", id1, batch.result$msg, retries, sleep.secs)
          Sys.sleep(sleep.secs)
          next
        }

        if (batch.result$status > 100L && batch.result$status <= 200L) {
          # fatal error, abort at once
          message("Fatal error occured: ", batch.result$status)
          message("Fatal error msg: ", batch.result$msg)
          stop("Fatal error occured: ", batch.result$status)
        }

        stopf("Illegal status code %s returned from cluster functions!", batch.result$status)
      }
    }
  }, error=bar$error)
  return(invisible(ids))
}
