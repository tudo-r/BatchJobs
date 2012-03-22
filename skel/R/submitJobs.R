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
#'   Default is all jobs where results are missing.
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
#' @return Nothing.
#' @export
#' @examples
#' reg <- makeRegistry(id="BatchJobsExample", file.dir=tempfile(), seed=123)
#' f <- function(x) x^2
#' batchMap(reg, f, 1:10)
#' submitJobs(reg)
submitJobs = function(reg, ids, resources=list(), wait, max.retries=10L) {
  checkArg(reg, cl="Registry")
  if (missing(ids)) {
    ids = dbGetMissingResults(reg)
    if (length(ids) == 0L)
      stop("All jobs finished, nothing to do!")
  } else {
    if (!is.list(ids) && !is.numeric(ids))
      stop("ids must be a integer vector of job ids or a list of chunked job ids (list of integer vectors)!")        
    if (is.list(ids)) {
      ids = lapply(ids, convertIntegers)
      checkListElementClass(ids, "integer")
    } else {
      ids = convertInteger(ids)
      checkArg(ids, "integer", na.ok=FALSE)
    }
    # must check length for list and vector!
    if (length(ids) == 0L)
      stop("ids must have non-zero length!")
    checkIds(reg, unlist(ids))
  }
  checkArg(resources, "list")
  if (missing(wait))
    wait = function(retries) 10L * 2L^retries
  else
    checkArg(wait, formals="retries")    
  max.retries = convertInteger(max.retries)
  checkArg(max.retries, "integer", len=1, na.ok=FALSE)   
  if (!is.null(getListJobs())) {
    ids.present = findOnSystem(reg)
    ids.intersect = intersect(unlist(ids), ids.present) 
    if (length(ids.intersect) > 0) {
      stopf("Some of the jobs you submitted are already present on the batch system! E.g. id=%i.", 
        ids.intersect[1])
    }
  }
  saveConf(reg)
  submitJobsInternal(reg, ids, resources, wait, max.retries)
}

# does the real work, is called by submitJobs and submitJobsAndWait
submitJobsInternal = function(reg, ids, resources, wait, max.retries) {
  is.chunks = is.list(ids)
  # for chunks we take the first id of the last chunk as "last" job, as first is stored in chunk
  # results and we store the log file under that name, etc
  first = if(is.chunks) head(unlist(head(ids, 1L)), 1L) else head(ids, 1L)
  last = if(is.chunks) head(unlist(tail(ids, 1L)), 1L) else tail(ids, 1L)
  conf = getBatchJobsConf()
  cf = getClusterFunctions(conf)
  messagef("Submitting %i chunks / %i jobs.",
    length(ids), sum(sapply(ids, length)))
  messagef("Cluster functions: %s.", cf$name)
  messagef("Auto-mailer settings: start=%s, done=%s, error=%s.",
    conf$mail.start, conf$mail.done, conf$mail.error)
  bar = makeProgressBar(max=length(ids), label="submitJobs             ")
  bar(0)
  for (i in seq_along(ids)) {
    id = ids[[i]]
    id1 = id[1]

    fn.rscript = getRScriptFilePath(reg, id1)
    writeRscript(fn.rscript, reg$file.dir, id, reg$multiple.result.files,
      disable.mail=FALSE, first, last, interactive.test = !is.null(conf$interactive))
    fn.log = getLogFilePath(reg, id1)
    jd = getJobDir(reg, id1)
    job.name = paste(reg$id, id1, sep="-")
    retries = 0L
    while (TRUE) {
      if (retries > max.retries) {
        # reset everything to NULL in DB for this job
        dbSendMessage(reg, dbMakeMessageKilled(reg, ids))
        stop("Retried already ", retries, " times in submit.")
      }
      # only send submitted msg on first try
      if(retries == 0L) {
        dbSendMessage(reg, dbMakeMessageSubmitted(reg, id,
          time=as.integer(Sys.time()),
          first.job.in.chunk.id = if(is.chunks) id1 else NULL
        ))
      }
      sent.bji = FALSE
      batch.result = cf$submitJob(reg, job.name, fn.rscript,
                                  fn.log, jd, resources)
      # make sure to send bji even if user intterupted submitJobs
      exit.pars = list(id=id, batch.result=batch.result)
      on.exit({
        if (exit.pars$batch.result$status == 0L && !sent.bji) {
          messagef("Interrupted. Setting last batch.job.id in DB.")
          exit.pars$reg=reg
          msg = dbMakeMessageSetBatchJobId(reg, exit.pars$id, 
            batch.job.id=exit.pars$batch.result$batch.job.id)
          dbSendMessage(reg, msg)
        }
      })                             
      if (batch.result$status == 0L) {
        # if success send batch.job.id to db and do serve next id
        dbSendMessage(reg, dbMakeMessageSetBatchJobId(reg, id, batch.job.id=batch.result$batch.job.id))
        sent.bji = TRUE         
        bar(i)
        break
      } else if (batch.result$status >= 1L && batch.result$status <= 100L) {
        # if temp error, wait and increase retries, then submit again
        sleep.secs = wait(retries)
        bar(i-1, msg=sprintf("Status: %i, zzz=%is.", batch.result$status, sleep.secs))
        warningf("Submit iteration: %i. Temporary error: %s. Retries: %i. Sleep: %i.", i, batch.result$msg, retries, sleep.secs)
        Sys.sleep(sleep.secs)
        retries = retries + 1L
      } else if (batch.result$status >= 101L && batch.result$status <= 200L) {
        # fatal error, abort at once
        # reset everything to NULL in DB for this job
        dbSendMessage(reg, dbMakeMessageKilled(reg, ids))
        message("Fatal error occured: ", batch.result$status)
        message("Fatal error msg: ", batch.result$msg)
        stop("Fatal error occured: ", batch.result$status)
      }
    }
  }
}
