sendMail = function(reg, job, result.str, extra.msg="",
  disable.mail, condition, first, last, conf) {

  if (disable.mail)
    return(invisible(NULL))
  conf = getBatchJobsConf()
  ischunk = !is(job, "Job")
  firstjob = if(ischunk) job[[1L]] else job
  # should we mail
  mail.conds = list(start=conf$mail.start, done=conf$mail.done, error=conf$mail.error)
  mail.cond = mail.conds[[condition]]
  if (mail.cond == "all" ||
    (mail.cond %in% c("first", "first+last") && firstjob$id == first) ||
    (mail.cond %in% c("last", "first+last") && firstjob$id == last)) {

    myformat = function(title, lines) {
      width = 76L
      str.top = paste("###", title, collapse(rep("#", width - 5L - nchar(title), ""), ""), sep = " ")
      str.bot = collapse(rep("#", width), "")
      paste(str.top,
        collapse(strwrap(lines, width = width - 2L, prefix = "# ", exdent = 2L), "\n"),
        str.bot, "\n", sep = "\n")
    }

    if (ischunk) {
      ids = extractSubList(job, "id")
      pars = vapply(job, function(j) listToShortString(j$par), character(1L))
    } else {
      ids = job$id
      pars = capture.output(print(firstjob))
    }

    cstr = switch(condition,
      "start" = "started",
      "done"  = "finished",
      "error" = "terminated with exception")
    subj = sprintf("[%s]: %s %s has %s", reg$id, ifelse(ischunk, "Chunk", "Job"), firstjob$id, cstr)
    msg = paste(myformat("Ids", ids), myformat("Job Info", pars), sep = "")

    # append result information
    if (condition != "start") {
      if (extra.msg != "")
        msg = paste(msg, myformat("Message", extra.msg), sep = "")
      msg = paste(msg, myformat("Results", result.str), sep = "")
      # we cannot not list the jobs while on the slave in showStatus
      if(firstjob$id == last)
        msg = paste(msg, myformat("Status", capture.output(showStatus(reg, run.and.exp=FALSE))), sep = "")
    }
    # if a mail problem occurs, we only warn but do not terminate
    ok = try (
      sendmail(conf$mail.from, conf$mail.to, subj, msg, control=conf$mail.control)
    )
    if (is.error(ok)) {
      warningf("Could not send mail!\nFrom: %s\nTo: %s\nControl: %s\nError message: %s",
        conf$mail.from, conf$mail.to, listToShortString(conf$mail.control), as.character(ok))
    }
  }
  invisible(NULL)
}
