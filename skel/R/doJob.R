doJob = function(reg, ids, multiple.result.files, disable.mail, first, last) {
  messagef("%s: Starting job on slave.", as.character(Sys.time()))
  reqJobPacks(reg)
  loadConf(reg)
  conf = getBatchJobsConf()

  # we need to see all warnings immediatly
  if (conf$cluster.functions$name %nin% c("Interactive", "Testing"))
    options(warning.length=8170L, warn=1L)

  messagef("Auto-mailer settings: start=%s, done=%s, error=%s.",
    conf$mail.start, conf$mail.done, conf$mail.error)
  wd = mySetWd(reg)
  if (length(ids) == 1L) {
    res = doSingleJob(reg, ids, multiple.result.files, disable.mail, first, last)
  } else {
    res = doChunk(reg, ids, multiple.result.files, disable.mail, first, last)
  }
  myResetWd(wd)
  messagef("%s: Job on slave is finished.", as.character(Sys.time()))
  messagef("Memory usage according to gc:")
  print(gc())
  return(res)
}

doSingleJob = function(reg, id, multiple.result.files, disable.mail, first, last) {
  dbSendMessage(reg, dbMakeMessageStarted(reg, id))
  job = getJob(reg, id, load.fun=TRUE, check.id=FALSE)
  sendMail(reg, job, result.str, "", disable.mail, condition = "start", first, last)

  result = executeOneJob(reg, job, multiple.result.files)
  if (is.error(result)) {
    errmsg = as.character(result)
    message("Error occurred: ", errmsg)
    dbSendMessage(reg, dbMakeMessageError(reg, job$id, err.msg=errmsg))
    result.str = errmsg
  } else {
    saveSingleResult(reg, job, result, multiple.result.files)
    dbSendMessage(reg, dbMakeMessageDone(reg, id))
    result.str = calcResultString(result)
  }
  sendMail(reg, job, result.str, "", disable.mail, condition=ifelse(is.error(result), "error", "done"),
    first, last)
  return(result)
}


doChunk = function(reg, ids, multiple.result.files, disable.mail, first, last) {

  doChunkFlush = function(reg, msg.buf, wait.flush) {
    cur.time = as.integer(Sys.time())
    if (cur.time - last.flush >= wait.flush) {
      ok = dbFlushMessages(reg, msg.buf)
      last.flush <<- as.integer(Sys.time())
      messagef("%s: Flushing %i msgs, succeeded: %s.",
        as.character(Sys.time()), length(msg.buf), ok)
      if (ok)
        return(character(0L))
    }
    return(msg.buf)
  }

  jobs = getJobs(reg, ids, load.fun=TRUE, check.ids=FALSE)
  result.strs = character(length(jobs))
  error = logical(length(jobs))
  # set all jobs to started with too early time
  dbSendMessage(reg, dbMakeMessageStarted(reg, ids))
  msg.buf = character(0L)
  # now collect messages in a buffer to reduce overhead
  # wait somewhere between 5-10 mins at least to flush them
  wait.flush = round(runif(1L, 300, 600))
  messagef("%s: Waiting %i secs between msg flushes.",
    as.character(Sys.time()), wait.flush)
  last.flush = as.integer(Sys.time())
  sendMail(reg, jobs, result.strs, "", disable.mail, condition="start", first, last)
  for (i in seq_along(jobs)) {
    job = jobs[[i]]
    msg.buf[length(msg.buf)+1L] = dbMakeMessageStarted(reg, job$id)
    result = executeOneJob(reg, job, multiple.result.files)
    if (is.error(result)) {
      errmsg = as.character(result)
      message("Error occurred: ", errmsg)
      msg.buf[length(msg.buf)+1L] = dbMakeMessageError(reg, job$id, err.msg=errmsg)
      result.strs[i] = errmsg
      error[i] = TRUE
    } else {
      saveSingleResult(reg, job, result, multiple.result.files)
      msg.buf[length(msg.buf)+1L] = dbMakeMessageDone(reg, job$id)
      result.strs[i] = calcResultString(result)
    }
    # if some minutes have passed since last flush, we can do it now
    msg.buf = doChunkFlush(reg, msg.buf, wait.flush)
  }

  # try to flush the remaining msgs at the end
  for(tries in seq_len(10L)) {
    msg.buf = doChunkFlush(reg, msg.buf, 0L)
    if (length(msg.buf) == 0L) break
    # wait between 1-2 min to try to flush last stuff
    wait.flush = round(runif(1L, 60, 2*60))
    messagef("%s: Waiting to flush msgs in final loop: %i secs.",
      as.character(Sys.time()), wait.flush)
    Sys.sleep(wait.flush)
  }
  mail.extra.msg = ""
  # we could not flush all messages
  if (length(msg.buf) > 0L) {
    flush.warn = paste("Some DB messages could not be flushed.",
      "This indicates some DB problem or too much communication with the DB.",
      "Everything should still be ok, you only might have to resubmit some jobs as they are not recorded as 'done'."
    )
    warningf(flush.warn)
    mail.extra.msg = flush.warn
  }
  # send mail for whole chunk
  # if one of the jobs had an error, treat the whole chunk as erroneous in mail
  sendMail(reg, jobs, result.strs, mail.extra.msg, disable.mail, condition=ifelse(any(error), "error", "done"),
    first, last)
  return(NULL)
}

# simple runs one job, without saving or message passing
executeOneJob = function(reg, job, multiple.result.files) {
  messagef("########## Executing jid=%s ##########", job$id)
  messagef("Timestamp: %s" , as.character(Sys.time()))
  print(job)
  # initialize RNG if not already happened
  # save current seed
  # use seed from job
  # on exit, reset to previous seed
  message("Setting seed: ", job$seed)
  seed = seeder(reg, job$seed)
  on.exit(seed$reset())

  result = try(applyJobFunction(reg, job), silent=TRUE)
  print(str(result, max.level=1L, list.len=5L))

  if (multiple.result.files) {
    if (!is.list(result)) {
      result = structure("multiple.result.files is TRUE, but your algorithm did not return a list!",
        class="try-error")
    } else if (!isProperlyNamed(result)) {
      result = structure("multiple.result.files is TRUE, but some the returned lists is not fully, distinctly named!",
        class="try-error")
    }
  }
  return(result)
}

saveSingleResult = function(reg, job, result, multiple.result.files) {
  if (multiple.result.files) {
    nresult = names(result)
    for (i in seq_along(result)) {
      fn.res = getResultFilePath(reg, job$id, nresult[i])
      message("Writing result file: ", fn.res)
      save2(file=fn.res, result = result[[i]])
    }
  } else {
    fn.res = getResultFilePath(reg, job$id)
    message("Writing result file: ", fn.res)
    save(file=fn.res, result)
  }
}

mySetWd = function(reg) {
  wd = getwd()
  message("Setting work dir: ", reg$work.dir)
  setwd(reg$work.dir)
  wd
}

myResetWd = function(wd) {
  message("Setting work back to: ", wd)
  setwd(wd)
}

calcResultString = function(result) {
  paste(capture.output(str(result)), collapse = "\n")
}

reqJobPacks = function(reg) {
  messagef("Requiring packages: [%s]", collapse(names(reg$packages)))
  for (p in names(reg$packages)) {
    if (!require(p, character.only = TRUE))
      stopf("Could not load required package '%s' on node '%s'!", p, Sys.info()["nodename"])
  }
}

