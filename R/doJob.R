doJob = function(reg, ids, multiple.result.files, disable.mail, first, last, array.id) {
  messagef("%s: Starting job on node %s.", Sys.time(), Sys.info()["nodename"])
  loadConf(reg)
  conf = getBatchJobsConf()
  # we need to see all warnings immediatly
  if (conf$cluster.functions$name != "Testing") {
      options(warning.length=8170L, warn=1L+conf$raise.warnings)
  }

  messagef("Auto-mailer settings: start=%s, done=%s, error=%s.",
    conf$mail.start, conf$mail.done, conf$mail.error)
  wd = switchWd(reg)
  on.exit({
    wd$reset()
    messagef("Memory usage according to gc:")
    print(gc())
  })

  if (!is.na(array.id)) {
    # FIXME better send error to database here, we don't see those errors on the master :(
    array.id = convertInteger(array.id)
    messagef("Processing array id %s", array.id)
    checkArg(array.id, "integer", len=1L, lower=1L, upper=length(ids), na.ok=FALSE)
    ids = ids[array.id]
  }

  if (length(ids) == 1L) {
    res = doSingleJob(reg, conf, ids, multiple.result.files, disable.mail, first, last)
  } else {
    res = doChunk(reg, conf, ids, multiple.result.files, disable.mail, first, last)
  }
  messagef("%s: Job on slave is finished.", Sys.time())
  return(res)
}

doSingleJob = function(reg, conf, id, multiple.result.files, disable.mail, first, last) {
  staged = conf$staged.queries
  dbSendMessage(reg, dbMakeMessageStarted(reg, id), staged = staged)
  job = getJob(reg, id, load.fun=TRUE, check.id=FALSE)
  sendMail(reg, job, result.str, "", disable.mail, condition = "start", first, last)

  result = executeOneJob(reg, job, multiple.result.files)
  error = is.error(result)
  if (error) {
    errmsg = as.character(result)
    message("Error occurred: ", errmsg)
    dbSendMessage(reg, dbMakeMessageError(reg, job$id, err.msg=errmsg), staged = staged)
    result.str = errmsg
  } else {
    saveSingleResult(reg, job, result, multiple.result.files)
    dbSendMessage(reg, dbMakeMessageDone(reg, id), staged = staged)
    result.str = calcResultString(result)
  }
  sendMail(reg, job, result.str, "", disable.mail, condition=ifelse(error, "error", "done"), first, last)
  return(result)
}


doChunk = function(reg, conf, ids, multiple.result.files, disable.mail, first, last) {
  # define and preallocate vars
  staged = conf$staged.queries
  jobs = getJobs(reg, ids, load.fun=TRUE, check.ids=FALSE)
  result.strs = character(length(jobs))
  error = logical(length(jobs))
  msg.buf = buffer("list", 2L * length(jobs) + 1L, TRUE)
  wait.flush = round(runif(1L, 300, 600))
  last.flush = now()
  mail.extra.msg = ""

  # send started message
  dbSendMessage(reg, dbMakeMessageStarted(reg, ids), staged = staged)

  # notify status
  messagef("%s: Waiting %i secs between msg flushes.", Sys.time(), wait.flush)
  sendMail(reg, jobs, result.strs, "", disable.mail, condition="start", first, last)

  for (i in seq_along(jobs)) {
    job = jobs[[i]]
    msg.buf$push(dbMakeMessageStarted(reg, job$id))
    result = executeOneJob(reg, job, multiple.result.files)

    if (is.error(result)) {
      result.strs[i] = as.character(result)
      msg.buf$push(dbMakeMessageError(reg, job$id, err.msg=result.strs[i]))
      message("Error occurred: ", result.strs[i])
      error[i] = TRUE
    } else {
      result.strs[i] = calcResultString(result)
      msg.buf$push(dbMakeMessageDone(reg, job$id))
      saveSingleResult(reg, job, result, multiple.result.files)
    }

    # if some minutes have passed since last flush, we can do it now
    cur.time = now()
    if (cur.time - last.flush >= wait.flush && dbSendMessages(reg, msg.buf$get(), staged = staged)) {
      last.flush = cur.time
      msg.buf$clear()
    }
  }

  # try to flush the remaining msgs at the end
  for (i in seq_len(10L)) {
    if (dbSendMessages(reg, msg.buf$get(), staged = staged)) {
      msg.buf$clear()
      break
    }
    wait.flush = round(runif(1L, 60, 2*60))
    messagef("%s: Waiting to flush msgs in final loop: %i secs.", Sys.time(), wait.flush)
    Sys.sleep(wait.flush)
  }

  # check if there are still remaining messages
  if (!msg.buf$empty()) {
    mail.extra.msg = paste("Some DB messages could not be flushed.",
                           "This indicates some DB problem or too much communication with the DB.",
                           "Everything should still be ok, you only might have to resubmit some jobs as they are not recorded as 'done'.",
                           sep = "\n")
    warningf(mail.extra.msg)
  }

  # send mail for whole chunk
  # if one of the jobs had an error, treat the whole chunk as erroneous in mail
  sendMail(reg, jobs, result.strs, mail.extra.msg, disable.mail, condition=ifelse(any(error), "error", "done"), first, last)
  return(NULL)
}

# simple runs one job, without saving or message passing
executeOneJob = function(reg, job, multiple.result.files) {
  messagef("########## Executing jid=%s ##########", job$id)
  messagef("Timestamp: %s" , Sys.time())
  print(job)
  # initialize RNG if not already happened
  # save current seed
  # use seed from job
  # on exit, reset to previous seed
  message("Setting seed: ", job$seed)
  seed = seeder(reg, job$seed)
  on.exit(seed$reset())

  result = try(applyJobFunction(reg, job), silent=TRUE)
  catf("Result:")
  print(str(result, max.level=1L, list.len=5L))

  if (multiple.result.files) {
    if (!is.list(result)) {
      result = setClasses("multiple.result.files is TRUE, but your algorithm did not return a list!", "try-error")
    } else if (!isProperlyNamed(result)) {
      result = setClasses("multiple.result.files is TRUE, but some the returned lists is not fully, distinctly named!", "try-error")
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

calcResultString = function(result) {
  paste0(capture.output(str(result)), collapse = "\n")
}
