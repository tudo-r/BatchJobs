sendMail = function(reg, ids, result.str, extra.msg = "",
  disable.mail, condition, first, last, conf) {

  if (disable.mail)
    return(invisible(NULL))
  conf = getBatchJobsConf()
  ischunk = (length(ids) > 1L)
  first.id = if(ischunk) ids[[1L]] else ids
  # should we mail
  mail.conds = list(start = conf$mail.start, done = conf$mail.done, error = conf$mail.error)
  mail.cond = mail.conds[[condition]]
  if (mail.cond == "all" ||
    (mail.cond %in% c("first", "first+last") && first.id == first) ||
    (mail.cond %in% c("last", "first+last") && first.id == last)) {

    myformat = function(title, lines) {
      collapse(c(sprintf("### %s", title), lines, "", ""), "\n")
    }

    cstr = switch(condition,
      "start" = "started",
      "done"  = "finished",
      "error" = "terminated with exception")
    subj = sprintf("[%s]: %s %s has %s", reg$id, ifelse(ischunk, "Chunk with first job ", "Job"), first.id, cstr)
    msg = myformat("Ids", ids)

    # append result and status information
    if (condition != "start") {
      if (extra.msg != "")
        msg = paste0(msg, myformat("Message", extra.msg))
      msg = paste0(msg, myformat("Results", result.str))
      if(first.id == last)
        msg = paste0(msg, myformat("Status", capture.output(showStatus(reg, run.and.exp = FALSE))))
    }

    # if a mail problem occurs, we only warn but do not terminate
    ok = try (sendmail(conf$mail.from, conf$mail.to, subj, msg, control = conf$mail.control))
    if (is.error(ok)) {
      warningf("Could not send mail to signal condition '%s'!\nFrom: %s\nTo: %s\nControl: %s\nError message: %s",
        condition, conf$mail.from, conf$mail.to, convertToShortString(conf$mail.control), as.character(ok))
    } else {
      messagef("Mail signaling condition '%s' send to %s", condition, conf$mail.to)
    }
  }
  invisible(NULL)
}
