library("BatchJobs")

f <- function(data) {
  if (data > 4) {
    Sys.sleep(100*data)
  }
  -data
}

## Create simple registry:
reg <- makeRegistry(id="minimal", file.dir="minimal")
batchMap(reg, f, 1:20)

## Submit jobs:
submitJobs(reg)

## Give jobs a chance to register as started and then show the job status:
Sys.sleep(5) 
showStatus(reg)

## Wait for half the jobs to finish and kill the rest
Sys.sleep(4) 
ids <- getJobIds(reg)
killJobs(reg, ids)
showStatus(reg)

## Collect (partial) results:
res <- reduceResults(reg, fun=function(aggr, job, res) c(aggr, res))
print(res)
