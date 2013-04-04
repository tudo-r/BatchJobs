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
#' @param job.delay [\code{function(n, i)} or \code{logical(1)}]\cr
#'   Function that defines how many seconds a job should be delayed before it starts.
#'   This is an expert option and only necessary to change when you want submit
#'   extremely many jobs. We then delay the jobs a bit to write the submit messages as
#'   early as possible to avoid writer starvation.
#'   \code{n} is the number of jobs and \code{i} the number of the ith job.
#'   The default function used with \code{job.delay} set to \code{TRUE} is no delay for
#'   100 jobs or less and otherwise \code{runif(1, 0.1*n, 0.2*n)}.
#'   If set to \code{FALSE} (the default) delaying jobs is disabled.
#' @return Vector of submitted job ids.
#' @export
#' @examples
#' reg <- makeRegistry(id="BatchJobsExample", file.dir=tempfile(), seed=123)
#' f <- function(x) x^2
#' batchMap(reg, f, 1:10)
#' submitJobs(reg)
#'
#' # Submit the 10 jobs again, now randomized into 2 chunks:
#' chunked = chunk(getJobIds(reg), n.chunks=2, shuffle=TRUE)
#' submitJobs(reg, chunked)
submitJobs = function(reg, ids, resources=list(), wait, max.retries=10L, job.delay=FALSE) {
  ### helper function to calculate the delay
  getDelays = function(cf, job.delay, n) {
    if (is.logical(job.delay)) {
      if (job.delay && n > 100L && cf$name %nin% c("Interactive", "Multicore", "SSH")) {
        return(runif(n, n*0.1, n*0.2))
      }
      return(delays = rep.int(0, n))
    }
    vapply(seq_along(ids), job.delay, numeric(1L), n=n)
  }

  ### argument checks on registry and ids
  checkArg(reg, cl="Registry")
  syncRegistry(reg)
  if (missing(ids)) {
    ids = dbFindSubmitted(reg, negate=TRUE)
    if (length(ids) == 0L) {
      message("All jobs submitted, nothing to do!")
      return(invisible(NULL))
    }
  } else {
    if (is.list(ids)) {
      ids = lapply(ids, checkIds, reg=reg, check.present=FALSE)
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
  checkArg(resources, "list")
  resources = resrc(resources)

  if (missing(wait))
    wait = function(retries) 10 * 2^retries
  else
    checkArg(wait, formals="retries")

  if (is.logical(job.delay)) {
    checkArg(job.delay, "logical", len=1L, na.ok=FALSE)
  } else {
    checkArg(job.delay, formals=c("n", "i"))
  }

  if (is.finite(max.retries)) {
    max.retries = convertInteger(max.retries)
    checkArg(max.retries, "integer", len=1L, na.ok=FALSE)
  }

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
    warningf(collapse(c("You are about to submit '%i' jobs.",
                        "Consider chunking them to avoid heavy load on the scheduler.",
                        "Sleeping 5 seconds for safety reasons."), sep = "\n"), n)
    Sys.sleep(5)
  }


  ### save config, start the work
  saveConf(reg)
  is.chunked = is.list(ids)
  messagef("Submitting %i chunks / %i jobs.", n, if(is.chunked) sum(vapply(ids, length, integer(1L))) else n)
  messagef("Cluster functions: %s.", cf$name)
  messagef("Auto-mailer settings: start=%s, done=%s, error=%s.", conf$mail.start, conf$mail.done, conf$mail.error)

  interrupted = FALSE
  submit.msgs = buffer("list", 1000L, dbSendMessages,
                       reg=reg, max.retries=10000L, sleep=function(r) 5,
                       staged=useStagedQueries())

  ### set on exit handler to avoid inconsistencies caused by user interrupts
  on.exit({
    # we need the second case for errors in brew (e.g. resources)
    if(interrupted && exists("batch.result", inherits=FALSE)) {
      submit.msgs$push(dbMakeMessageSubmitted(reg, id, time=now(),
        batch.job.id=batch.result$batch.job.id, first.job.in.chunk.id = if(is.chunked) id1 else NULL,
        resources.timestamp=resources.timestamp))
    }
    # send remaining msgs now
    messagef("Sending %i submit messages...\nMight take some time, do not interrupt this!", submit.msgs$pos())
    submit.msgs$clear()
  })

  ### write R scripts
  messagef("Writing %i R scripts...", n)
  resources.timestamp = saveResources(reg, resources)
  writeRscripts(reg, ids, resources.timestamp, disable.mail=FALSE, delays=getDelays(cf, job.delay, n),
               interactive.test = !is.null(conf$interactive))

  ### reset status of jobs: delete errors, done, ...
  dbSendMessage(reg, dbMakeMessageKilled(reg, unlist(ids)), staged=FALSE)


  ### initialize progress bar
  bar = makeProgressBar(max=n, label="submitJobs               ")
  bar$set()

  tryCatch({
    for (id in ids) {
      id1 = id[1L]
      retries = 0L

      repeat { # max.retires max be Inf
        if (limit.concurrent.jobs && length(cf$listJobs(conf, reg)) >= conf$max.concurrent.jobs) {
          # emulate a temporary erroneous batch result
          batch.result = makeSubmitJobResult(status=10L, batch.job.id=NA_character_, "Max concurrent jobs exhausted")
        } else {
          # try to submit the job
          interrupted = TRUE
          batch.result = cf$submitJob(conf=conf, reg=reg,
                                      job.name=sprintf("%s-%i", reg$id, id1),
                                      rscript=getRScriptFilePath(reg, id1),
                                      log.file=getLogFilePath(reg, id1),
                                      job.dir=getJobDirs(reg, id1),
                                      resources=resources)
        }

        ### validate status returned from cluster functions
        if (batch.result$status == 0L) {
          submit.msgs$push(dbMakeMessageSubmitted(reg, id, time=now(),
                                                  batch.job.id=batch.result$batch.job.id, first.job.in.chunk.id = if(is.chunked) id1 else NULL,
                                                  resources.timestamp=resources.timestamp))
          interrupted = FALSE
          bar$inc(1L)
          break
        }

        ### submitJob was not successful, handle the return status
        interrupted = FALSE

        if (batch.result$status > 0L && batch.result$status <= 100L) {
          # temp error, wait and increase retries, then submit again in next iteration
          sleep.secs = wait(retries)

          retries = retries + 1L
          if (retries > max.retries)
            stopf("Retried already %i times to submit. Aborting.", retries)


          # FIXME we could use the sleep here for synchronization
          bar$inc(msg=sprintf("Status: %i, zzz=%.1fs", batch.result$status, sleep.secs))
          # FIXME: the next lines are an ugly hack and should be moved to bbmisc
          Sys.sleep(sleep.secs/2)
          pbw = getOption("BBmisc.ProgressBar.width", getOption("width"))
          labw = environment(bar$set)$label.width
          lab = sprintf(sprintf("%%%is", labw),
            sprintf("Status: %i, zzz=%.1fs", batch.result$status, sleep.secs))
          cat(paste(rep.int("\b \b", pbw-5), collapse=""))
          bmrmsg = batch.result$msg
          msgline = sprintf("%s msg=%s", lab, bmrmsg)
          cat(msgline)
          Sys.sleep(sleep.secs/2)
          cat(paste(rep.int("\b \b", nchar(msgline)), collapse=""))
        } else if (batch.result$status > 100L && batch.result$status <= 200L) {
          # fatal error, abort at once
          stopf("Fatal error occured: %i. %s", batch.result$status, batch.result$msg)
        } else {
          # illeagal status code
          stopf("Illegal status code %s returned from cluster functions!", batch.result$status)
        }
      }
    }
  }, error=bar$error)

  ### return ids (on.exit handler kicks now in to submit the remaining messages)
  return(invisible(ids))
}
