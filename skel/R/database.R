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

dbDoQueries = function(reg, queries, flags="ro", max.retries=200L, sleep=function(r) 1.025^r) {
  for (i in seq_len(max.retries)) {
    con = dbConnectToJobsDB(reg, flags)
    ok = try ({
      dbBeginTransaction(con)
      ress = lapply(queries, dbGetQuery, con=con)
    }, silent = TRUE)
    if (!is.error(ok)) {
      # this can fail because DB is locked
      ok2 = dbCommit(con)
      if (ok2) {
        dbDisconnect(con)
        return(ress)
      } else {
        dbRollback(con)
        dbDisconnect(con)
      }
    } else {
      ok = as.character(ok)
      dbRollback(con)
      dbDisconnect(con)
      if(!grepl("lock", tolower(ok), fixed=TRUE)) {
        stopf("Error in dbDoQueries. Displaying only 1st query. %s (%s)", ok, queries[1L])
      }
    }
    # if we reach this here, DB was locked
    Sys.sleep(runif(1L, min=1, max=sleep(i)))
  }
  stopf("dbDoQueries: max retries (%i) reached, database is still locked!", max.retries)
}

dbDoQuery = function(reg, query, flags="ro", max.retries=200L, sleep=function(r) 1.025^r) {
  for (i in seq_len(max.retries)) {
    con = dbConnectToJobsDB(reg, flags)
    res = try(dbGetQuery(con, query), silent=TRUE)
    dbDisconnect(con)
    if (! is.error(res))
      return(res)
    res = as.character(res)
    if(grepl("lock", tolower(res), fixed=TRUE)) {
      Sys.sleep(runif(1L, min=1, max=sleep(i)))
    } else {
      print(res)
      print(query)
      stopf("Error in dbDoQuery. %s (%s)", res, query)
    }
  }
  stopf("dbDoQuery: max retries (%i) reached, database is still locked!", max.retries)
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

dbSelectWithIds = function(reg, query, ids, where=TRUE, limit=-1L) {
  if(!missing(ids))
    query = sprintf("%s %s job_id IN (%s)", query, ifelse(where, "WHERE", "AND"), collapse(ids))
  query = sprintf("%s LIMIT %i", query, limit)
  res = dbDoQuery(reg, query, flags="ro")
  if(!missing(ids))
    # order result same as ids
    res[na.omit(match(ids, res$job_id)),, drop=FALSE]
  else
    res
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
  #message("Initializing job definition table...")

  query = sprintf("CREATE TABLE %s_job_def (job_def_id INTEGER PRIMARY KEY, fun_id TEXT, pars TEXT)", reg$id)
  dbDoQuery(reg, query, flags="rwc")
  dbCreateExpandedJobsView(reg)
}

dbCreateJobStatusTable = function(reg, extra.cols="", constraints="") {
  #message("Initializing job status table...")

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
  tab = dbSelectWithIds(reg, query, ids)
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
  tab = dbSelectWithIds(reg, query, ids)
  if (missing(columns) || "job_id" %in% columns)
    rownames(tab) = tab$job_id
  tab
}

dbGetJobStatusTable = function(reg, ids, convert.dates=TRUE) {
  query = sprintf("SELECT * FROM %s_job_status", reg$id)
  tab = dbSelectWithIds(reg, query, ids)
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
  dbDoQuery(reg, query)$count
}

dbGetJobId = function(reg) {
  query = sprintf("SELECT job_id FROM %s_job_status LIMIT 1", reg$id)
  as.integer(dbDoQuery(reg, query)$job_id)
}

dbGetJobIds = function(reg) {
  query = sprintf("SELECT job_id FROM %s_job_status", reg$id)
  dbDoQuery(reg, query)$job_id
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

dbGetDone = function(reg, ids) {
  query = sprintf("SELECT job_id FROM %s_job_status WHERE done IS NOT NULL", reg$id)
  dbSelectWithIds(reg, query, ids, where=FALSE)$job_id
}

dbGetMissingResults = function(reg, ids) {
  query = sprintf("SELECT job_id FROM %s_job_status WHERE done IS NULL", reg$id)
  dbSelectWithIds(reg, query, ids, where=FALSE)$job_id
}

dbGetErrors = function(reg, ids) {
  query = sprintf("SELECT job_id FROM %s_job_status WHERE error IS NOT NULL", reg$id)
  dbSelectWithIds(reg, query, where=FALSE)$job_id
}

dbGetSubmitted = function(reg, ids) {
  query = sprintf("SELECT job_id FROM %s_job_status WHERE submitted IS NOT NULL", reg$id)
  dbSelectWithIds(reg, query, ids, where=FALSE)$job_id
}

dbGetNotSubmitted = function(reg, ids) {
  query = sprintf("SELECT job_id FROM %s_job_status WHERE submitted IS NULL", reg$id)
  dbSelectWithIds(reg, query, ids, where=FALSE)$job_id
}

dbGetStarted = function(reg, ids) {
  query = sprintf("SELECT job_id FROM %s_job_status WHERE started IS NOT NULL", reg$id)
  dbSelectWithIds(reg, query, ids, where=FALSE)$job_id
}

dbGetJobIdsFromBatchJobIds = function(reg, batch.job.ids, ids, clause="") {
  query = sprintf("SELECT job_id FROM %s_job_status WHERE batch_job_id IN ('%s')",
                  reg$id, collapse(batch.job.ids, sep="','"), clause)
  if (clause != "")
    query = sprintf("%s AND %s", query, clause)
  dbSelectWithIds(reg, query, ids, where=FALSE)$job_id
}

dbGetExpiredJobs = function(reg, batch.job.ids, ids) {
  # started, not terminated, not running
  query = sprintf("SELECT job_id FROM %s_job_status WHERE started IS NOT NULL AND done IS NULL AND error is NULL AND
      batch_job_id NOT IN ('%s')", reg$id, collapse(batch.job.ids, sep="','"))
  dbSelectWithIds(reg, query, ids, where=FALSE)$job_id
}

dbGetSubmittedAndNotTerminatedJobs = function(reg, ids) {
  # we cannot check if job is running in DB, do this later
  query = sprintf("SELECT job_id FROM %s_job_status WHERE submitted IS NOT NULL AND done IS NULL AND error is NULL", reg$id)
  dbSelectWithIds(reg, query, ids, where=FALSE)$job_id
}

dbGetMaxOfColumn = function(reg, tab, column, default) {
  query = sprintf("SELECT MAX(%s) AS x FROM %s_%s", column, reg$id, tab)
  res = dbDoQuery(reg, query)$x
  if(is.na(res))
    return(default)
  return(res)
}

dbGetMaxSeed = function(reg, default) {
  dbGetMaxOfColumn(reg, "job_status", "seed", default)
}

dbGetFirstJobInChunkIds = function(reg, ids){
  query = sprintf("SELECT job_id, first_job_in_chunk_id FROM %s_job_status", reg$id)
  dbSelectWithIds(reg, query, ids)$first_job_in_chunk_id
}

dbGetJobTimes = function(reg, ids){
  query = sprintf("SELECT job_id, done-started AS time FROM %s_job_status", reg$id)
  # incorrect type for empty id vec possible
  tab = dbSelectWithIds(reg, query, ids)
  if (!is.integer(tab$time))
    tab$time = as.integer(tab$time)
  tab
}


dbGetStats = function(reg, ids, running=FALSE, expired=FALSE) {
  q.r = q.e = "NULL"

  if(running || expired) {
    fun = getListJobs("Cannot find running or expired jobs")
    batch.job.ids = fun(getBatchJobsConf(), reg)

    if(running)
      q.r = sprintf("SUM(started IS NOT NULL AND batch_job_id IN ('%s'))", collapse(batch.job.ids, sep="','"))
    if(expired)
      q.e = sprintf("SUM(started IS NOT NULL AND done IS NULL AND error IS NULL AND batch_job_id NOT IN ('%s'))", collapse(batch.job.ids, sep="','"))
  }

  query = sprintf(paste(
    "SELECT COUNT(job_id) AS n,",
    "COUNT(submitted) AS submitted,",
    "COUNT(started) AS started,",
    "COUNT(done) AS done,",
    "COUNT(error) AS error,",
    "%s AS running,",
    "%s AS expired,",
    "MIN(done - started) AS t_min,",
    "AVG(done - started) AS t_avg,",
    "MAX(done - started) AS t_max",
    "FROM %s_job_status"), q.r, q.e, reg$id)

  if(!missing(ids))
    query = sprintf("%s WHERE job_id IN (%s)", query, collapse(ids))

  df = dbDoQuery(reg, query)

  # Convert to correct type. Null has no type and casts don't work properly with RSQLite
  doubles = c("t_min", "t_avg", "t_max")
  lapply(df, function(x) if(x %in% doubles) as.double(x) else as.integer(x))
}

############################################
### DELETE
############################################
dbRemoveJobs = function(reg, ids) {
  query = sprintf("DELETE FROM %s_job_status WHERE job_id IN (%s)", reg$id, collapse(ids))
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

dbMakeMessageSubmitted = function(reg, job.ids, time=as.integer(Sys.time()),
                                  batch.job.id, first.job.in.chunk.id=NULL) {
  if(is.null(first.job.in.chunk.id))
    first.job.in.chunk.id = "NULL"
  updates = sprintf("first_job_in_chunk_id=%s, submitted=%i, batch_job_id='%s'",
                    first.job.in.chunk.id, time, batch.job.id)
  sprintf("UPDATE %s_job_status SET %s WHERE job_id in (%s)", reg$id, updates, collapse(job.ids))
}

dbMakeMessageStarted = function(reg, job.ids, time=as.integer(Sys.time())) {
  node = gsub("'", "''", Sys.info()["nodename"], fixed=TRUE)
  updates = sprintf("started=%i, node='%s', r_pid=%i, error=NULL, done=NULL", time, node, Sys.getpid())
  sprintf("UPDATE %s_job_status SET %s WHERE job_id in (%s)", reg$id, updates, collapse(job.ids))
}

dbMakeMessageError = function(reg, job.ids, err.msg) {
  err.msg = gsub("'", "''", err.msg, fixed=TRUE)
  updates = sprintf("error='%s', done=NULL", err.msg)
  sprintf("UPDATE %s_job_status SET %s WHERE job_id in (%s)", reg$id, updates, collapse(job.ids))
}

dbMakeMessageDone = function(reg, job.ids, time=as.integer(Sys.time())) {
  updates = sprintf("done=%i, error=NULL", time)
  sprintf("UPDATE %s_job_status SET %s WHERE job_id in (%s)", reg$id, updates, collapse(job.ids))
}

dbMakeMessageKilled = function(reg, job.ids) {
  updates = "submitted=NULL, started=NULL, batch_job_id=NULL, node=NULL, r_pid=NULL, done=NULL, error=NULL"
  sprintf("UPDATE %s_job_status SET %s WHERE job_id in (%s)", reg$id, updates, collapse(job.ids))
}

dbConvertNumericToPOSIXct = function(x) {
  now = Sys.time()
  as.POSIXct(x, origin=now - as.integer(now))
}

dbSetJobFunction = function(reg, ids, fun.id) {
  query = sprintf("UPDATE %1$s_job_def SET fun_id = '%2$s' WHERE job_def_id IN (SELECT job_def_id FROM %1$s_job_status WHERE job_id IN (%3$s))", reg$id, fun.id, collapse(ids))
  dbDoQuery(reg, query, flags="rw")
}

############################################
### INSERT
############################################
dbAddJobs = function(reg, jobs, ...) {
  fun.ids = extractSubList(jobs, "fun.id")
  seeds = extractSubList(jobs, "seed")
  pars = vapply(jobs, function(j) rawToChar(serialize(j$pars, connection=NULL, ascii=TRUE)), character(1L))

  n = dbAddData(reg, "job_def", data = data.frame(fun_id=fun.ids, pars=pars))
  job.def.ids = dbGetLastAddedIds(reg, "job_def", "job_def_id", n)
  n = dbAddData(reg, "job_status", data=data.frame(job_def_id=job.def.ids, seed=seeds))
  job.ids = dbGetLastAddedIds(reg, "job_status", "job_id", n)

  list(job.ids=job.ids, job.def.ids=job.def.ids)
}


# flushes messages en block.
dbFlushMessages = function(reg, msgs, max.retries=200L, sleep=function(r) 1.025^r) {
  ok = try(dbDoQueries(reg, msgs, flags="rw", max.retries, sleep))
  if (is.error(ok)) {
    ok = as.character(ok)
    if (ok == "dbDoQueries: max retries reached, database is still locked!") {
      return(FALSE)
    } else {
      #throw exception again
      stopf("Error in dbFlushMessages: %s", ok)
    }
  }
  return(TRUE)
}

