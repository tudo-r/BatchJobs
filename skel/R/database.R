############################################
### Common database functions
############################################
dbGetConnection = function(drv, ...) {
  # method dispatch tp support different DBMS
  UseMethod("dbGetConnection")
}

dbGetConnection.SQLiteDriver = function(drv, reg, flags="ro", ...) {
  flags = switch(flags, "ro" = SQLITE_RO, "rw" = SQLITE_RW, "rwc" = SQLITE_RWC)
  opts = list(dbname = file.path(reg$file.dir, "BatchJobs.db"),
              flags = flags, drv = drv)
  do.call(dbConnect, args=c(reg$db.options, opts))
}

dbConnectToJobsDB = function(reg, flags="ro") {
  drv = dbDriver(reg$db.driver)
  dbGetConnection(drv, reg, flags)
}

dbDoQueries = function(reg, queries, flags="ro") {
  for (i in seq_len(100L)) {
    con = dbConnectToJobsDB(reg, flags)
    ok = try ({
      dbBeginTransaction(con)
      ress = lapply(queries, dbGetQuery, con=con)
    }, silent = TRUE)
    if (!is.error(ok)) { 
      dbCommit(con)
      dbDisconnect(con)
      return(ress)
    } else {
      ok = as.character(ok)
      dbRollback(con)
      dbDisconnect(con)
      if(grepl("lock", ok, ignore.case=TRUE)) {
        Sys.sleep(runif(1L, min=1, max=2))
      } else {
        stopf("Error in dbDoQueries. Displaying only 1st query. %s (%s)", ok, queries[1])
      }
    }
  }
  stop("dbDoQueries: max retries reached, database is still locked!")
}

dbDoQuery = function(reg, query, flags="ro") {
  for (i in seq_len(100L)) {
    con = dbConnectToJobsDB(reg, flags)
    res = try(dbGetQuery(con, query), silent=TRUE)
    dbDisconnect(con)
    if (! is.error(res)) 
      return(res)
    
    res = as.character(res)
    if(grepl("lock", res, ignore.case=TRUE)) {
      Sys.sleep(runif(1L, min=1, max=2))
    } else {
      stopf("Error in dbDoQuery. %s (%s)", res, query)
    }
  }
  stop("dbDoQuery: max retries reached, database is still locked!")
}


dbAddData = function(reg, tab, data) {
  query = sprintf("INSERT INTO %s_%s (%s) VALUES(%s)", reg$id, tab,
                  collapse(colnames(data)), collapse(rep("?", ncol(data))))
  con = dbConnectToJobsDB(reg, flags="rw")
  on.exit(dbDisconnect(con))
  dbBeginTransaction(con)
  ok = try(dbGetPreparedQuery(con, query, bind.data = data))
  if(is.error(ok)) {
    dbRollback(con)
    stopf("Error in dbAddData: %s", as.character(ok))
  }
  
  dbCommit(con)
  as.integer(dbGetQuery(con, "SELECT total_changes()"))
}

############################################
### CREATE
############################################
#' ONLY FOR INTERNAL USAGE.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @return Nothing.
#' @export
dbCreateJobDefTable = function(reg) {
  UseMethod("dbCreateJobDefTable")
}

#' @S3method dbCreateJobDefTable Registry
dbCreateJobDefTable.Registry = function(reg) {
  message("Initializing job definition table...")

  query = sprintf("CREATE TABLE %s_job_def (job_def_id INTEGER PRIMARY KEY, fun_id TEXT, pars TEXT)", reg$id)
  dbDoQuery(reg, query, flags="rwc")
  dbCreateExpandedJobsView(reg)
}

dbCreateJobStatusTable = function(reg, extra.cols="", constraints="") {
  message("Initializing job status table...")
 
  query = sprintf(paste("CREATE TABLE %s_job_status (job_id INTEGER PRIMARY KEY, job_def_id INTEGER,",
                   "first_job_in_chunk_id INTEGER, seed INTEGER, submitted INTEGER,",
                   "started INTEGER, batch_job_id TEXT, node TEXT, r_pid INTEGER,",
                   "done INTEGER, error TEXT %s %s)"), reg$id, extra.cols, constraints)
  dbDoQuery(reg, query, flags="rwc")

  query = sprintf("CREATE INDEX job_def_id ON %s_job_status(job_def_id)", reg$id)
  dbDoQuery(reg, query, flags="rw")
  
  return(invisible(TRUE))
}


dbCreateExpandedJobsView = function(reg) {
  query = sprintf("CREATE VIEW %1$s_expanded_jobs AS SELECT * FROM %1$s_job_status LEFT JOIN %1$s_job_def USING(job_def_id)", reg$id)
  dbDoQuery(reg, query, flags="rw")
}


############################################
### SELECT
############################################

#' ONLY FOR INTERNAL USAGE.
#' @param reg [\code{\link{Registry}}]\cr
#'   Registry.
#' @param ids [\code{integer}]\cr
#'   Ids of selected jobs.
#' @return [list of \code{\link{Job}}]. Retrieved jobs from DB.
#' @export
dbGetJobs = function(reg, ids) {
  UseMethod("dbGetJobs")
}

# note that this does not load the job function from disk to increase speed
#' @method dbGetJobs Registry
#' @S3method dbGetJobs Registry
dbGetJobs.Registry = function(reg, ids) {
  query = sprintf("SELECT job_id, fun_id, pars, seed FROM %s_expanded_jobs", reg$id)
  if (missing(ids)) {
    tab = dbDoQuery(reg, query)
  } else {
    query = sprintf("%s WHERE job_id IN ('%s')", query, collapse(ids, sep="','"))
    tab = dbDoQuery(reg, query)
    if(nrow(tab) == 0L) 
      stopf("No jobs found for ids: %s", collapse(ids))
    tab = tab[match(ids, tab$job_id),, drop=FALSE]
  }
  lapply(seq_len(nrow(tab)), function(i) {
    makeJob(id=tab$job_id[i], fun.id=tab$fun_id[i], fun=NULL,
            pars=unserialize(charToRaw(tab$pars[i])), seed=tab$seed[i])
  })
}

dbGetExpandedJobsTable = function(reg, ids, columns) {
  if (missing(columns))
    columns2 = "*"
  else
    columns2 = collapse(columns)

  query = sprintf("SELECT %s FROM %s_expanded_jobs", columns2, reg$id)
  if (missing(ids)) {
    tab = dbDoQuery(reg, query)
  } else {
    query = sprintf("%s WHERE job_id IN ('%s')", query, collapse(ids, sep="','"))
    tab = dbDoQuery(reg, query)
    tab = tab[match(ids, tab$job_id),, drop=FALSE]
  }
  if (missing(columns) || "job_id" %in% columns)
    rownames(tab) = tab$job_id
  tab
}

dbGetJobStatusTable = function(reg, ids, convert.dates=TRUE) {
  query = sprintf("SELECT * FROM %s_job_status", reg$id)
  if (missing(ids)) {
    tab = dbDoQuery(reg, query)
  } else {
    query = sprintf("%s WHERE job_id IN ('%s')", query, collapse(ids, sep="','"))
    tab = dbDoQuery(reg, query)
    tab = tab[match(ids, tab$job_id),, drop=FALSE]
  }

  if (convert.dates) {
    tab$submitted = dbConvertNumericToPOSIXct(tab$submitted)
    tab$started = dbConvertNumericToPOSIXct(tab$started)
    tab$done = dbConvertNumericToPOSIXct(tab$done)
  }

  rownames(tab) = tab$job_id
  tab
}


dbGetJobCount = function(reg) {
  query = sprintf("SELECT COUNT(*) AS count FROM %s_job_status", reg$id)
  as.integer(dbDoQuery(reg, query)$count)
}

dbGetJobIds = function(reg) {
  query = sprintf("SELECT job_id FROM %s_job_status", reg$id)
  as.integer(dbDoQuery(reg, query)$job_id)
}

dbGetJobIdsIfAllDone = function(reg) {
  query = sprintf("SELECT job_id, done FROM %s_job_status", reg$id)
  res = dbDoQuery(reg, query)
  if (all(! is.na(res$done)))
      return(res$job_id)
  stop("Not all jobs finished (yet)!")
}

dbGetLastAddedIds = function(reg, tab, id.col, n) {
  query = sprintf("SELECT %s AS id_col FROM %s_%s ORDER BY %s DESC LIMIT %i",
                  id.col, reg$id, tab, id.col, n)
  rev(dbDoQuery(reg, query)$id_col)
}

dbGetDone = function(reg) {
  query = sprintf("SELECT job_id FROM %s_job_status WHERE done IS NOT NULL", reg$id)
  dbDoQuery(reg, query)$job_id
}

dbGetMissingResults = function(reg) {
  query = sprintf("SELECT job_id FROM %s_job_status WHERE done IS NULL", reg$id)
  dbDoQuery(reg, query)$job_id
}

dbGetErrors = function(reg) {
  query = sprintf("SELECT job_id FROM %s_job_status WHERE error IS NOT NULL", reg$id)
  dbDoQuery(reg, query)$job_id
}

dbGetSubmitted = function(reg) {
  query = sprintf("SELECT job_id FROM %s_job_status WHERE submitted IS NOT NULL", reg$id)
  dbDoQuery(reg, query)$job_id
}

dbGetStarted = function(reg) {
  query = sprintf("SELECT job_id FROM %s_job_status WHERE started IS NOT NULL", reg$id)
  dbDoQuery(reg, query)$job_id
}

dbGetJobIdsFromBatchJobIds = function(reg, batch.job.ids, clause="") {
  query = sprintf("SELECT job_id FROM %s_job_status WHERE batch_job_id IN ('%s')",
                  reg$id, collapse(batch.job.ids, sep="','"), clause)
  if (clause != "")
    query = paste(query, sprintf("AND %s", clause))
  dbDoQuery(reg, query)$job_id
}

dbGetExpiredJobs = function(reg, batch.job.ids) {
  # started, not terminated, not running
  query = sprintf("SELECT job_id FROM %s_job_status WHERE started IS NOT NULL AND done IS NULL AND error is NULL AND
      batch_job_id NOT IN ('%s')", reg$id, collapse(batch.job.ids, sep="','"))
  dbDoQuery(reg, query)$job_id
}

dbGetSubmittedAndNotTerminatedJobs = function(reg) {
  # we cannot check if job is running in DB, do this later
  query = sprintf("SELECT job_id FROM %s_job_status WHERE submitted IS NOT NULL AND done IS NULL AND error is NULL", reg$id)
  dbDoQuery(reg, query)$job_id
}

dbGetMaxOfColumn = function(reg, tab, column, default) {
  query = sprintf("SELECT COALESCE(MAX(%s), %i) AS x FROM %s_%s", column, default, reg$id, tab)
  dbDoQuery(reg, query)$x
}

dbGetMaxSeed = function(reg, default) {
  dbGetMaxOfColumn(reg, "job_status", "seed", default)
}

dbGetFirstJobInChunkIds = function(reg, ids){
  query = sprintf("SELECT job_id, first_job_in_chunk_id FROM %s_job_status WHERE job_id IN ('%s')", reg$id, collapse(ids, sep="','"))
  first = dbDoQuery(reg, query)
  first[match(ids, first$job_id), "first_job_in_chunk_id"]
}

dbGetJobTimes = function(reg, ids){
  query = sprintf("SELECT started,done FROM %s_job_status WHERE job_id IN ('%s')", reg$id, collapse(ids, sep="','"))
  dbDoQuery(reg, query)
}

############################################
### DELETE
############################################
dbRemoveJobs = function(reg, ids) {
  query = sprintf("DELETE FROM %s_job_status WHERE job_id IN ('%s')", reg$id, collapse(ids, sep="','"))
  dbDoQuery(reg, query, flags="rw")
  query = sprintf("DELETE FROM %1$s_job_def WHERE job_def_id NOT IN (SELECT DISTINCT job_def_id FROM %1$s_job_status)", reg$id)
  dbDoQuery(reg, query, flags="rw")
  return(invisible(TRUE))
}

############################################
### UPDATE
############################################
dbSendMessage = function(reg, msg) {
  dbDoQuery(reg, msg, flags="rw")
}

dbMakeMessageSubmitted = function(reg, job.ids, time, first.job.in.chunk.id=NULL) {
  if(is.null(first.job.in.chunk.id))
    first.job.in.chunk.id = "NULL"
  updates = sprintf("first_job_in_chunk_id=%s, submitted=%i, started=NULL, batch_job_id=NULL, node=NULL, r_pid=NULL, done=NULL, error=NULL",
                    first.job.in.chunk.id, time)
  sprintf("UPDATE %s_job_status SET %s WHERE job_id in ('%s')", reg$id, updates, collapse(job.ids, sep="','"))
}

dbMakeMessageSetBatchJobId = function(reg, job.ids, batch.job.id) {
  updates = sprintf("batch_job_id='%s'", batch.job.id)
  sprintf("UPDATE %s_job_status SET %s WHERE job_id in ('%s')", reg$id, updates, collapse(job.ids, sep="','"))
}

dbMakeMessageStarted = function(reg, job.ids, time) {
  node = gsub("'", "''", Sys.info()["nodename"], fixed=TRUE)
  updates = sprintf("started=%i, node='%s', r_pid=%i, error=NULL, done=NULL", time, node, Sys.getpid())
  sprintf("UPDATE %s_job_status SET %s WHERE job_id in ('%s')", reg$id, updates, collapse(job.ids, sep="','"))
}

dbMakeMessageError = function(reg, job.ids, err.msg) {
  err.msg = gsub("'", "''", err.msg, fixed=TRUE)
  updates = sprintf("error='%s', done=NULL", err.msg)
  sprintf("UPDATE %s_job_status SET %s WHERE job_id in ('%s')", reg$id, updates, collapse(job.ids, sep="','"))
}

dbMakeMessageDone = function(reg, job.ids, time) {
  updates = sprintf("done=%i, error=NULL", time)
  sprintf("UPDATE %s_job_status SET %s WHERE job_id in ('%s')", reg$id, updates, collapse(job.ids, sep="','"))
}

dbMakeMessageKilled = function(reg, job.ids) {
  updates = "submitted=NULL, started=NULL, batch_job_id=NULL, node=NULL, r_pid=NULL, done=NULL, error=NULL"
  sprintf("UPDATE %s_job_status SET %s WHERE job_id in ('%s')", reg$id, updates, collapse(job.ids, sep="','"))
}

dbConvertNumericToPOSIXct = function(x) {
  now = Sys.time()
  as.POSIXct(x, origin=now - as.integer(now))
}


############################################
### INSERT
############################################
dbAddJobs = function(reg, jobs, ...) {
  fun.ids = extractSubList(jobs, "fun.id") 
  seeds = extractSubList(jobs, "seed")
  pars = sapply(jobs, function(j) rawToChar(serialize(j$pars, connection=NULL, ascii=TRUE)))

  n = dbAddData(reg, "job_def", data = data.frame(fun_id=fun.ids, pars=pars))
  job.def.ids = dbGetLastAddedIds(reg, "job_def", "job_def_id", n)
  n = dbAddData(reg, "job_status", data=data.frame(job_def_id=job.def.ids, seed=seeds))
  job.ids = dbGetLastAddedIds(reg, "job_status", "job_id", n)

  list(job.ids=job.ids, job.def.ids=job.def.ids)
}


# flushes messages en block. 
dbFlushMessages = function(reg, msgs) {
  ok = try(dbDoQueries(reg, msgs, flags="rw"))
  if (is.error(ok)) {
    ok = as.character(ok)
    if (ok == "dbDoQueries: max retries reached, database is still locked!") {
      return(FALSE)
    } else {
      #throw exception again
      stop(ok)
    }
  }
  return(TRUE)
}
