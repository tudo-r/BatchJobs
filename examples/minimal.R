library("BatchJobs")

f <- function(data) {
  if (data > 4) {
    Sys.sleep(100*data)
  }
  -data
}

## Create simple registry:
registry <- makeRegistry(id="minimal", file.dir="minimal")
batchMap(registry, f, 1:20)

## Submit jobs:
submitJobs(registry)

## Give jobs a chance to register as started and then show the queue
## status:
Sys.sleep(1) 
showStatus(registry)

## Wait for half the jobs to finish and kill the rest
Sys.sleep(4) 
ids <- getJobIds(registry)
killJobs(registry, ids) ##
showStatus(registry)

## Collect (partial) results:
res <- reduceResults(registry, fun=function(aggr, job, res) c(aggr, res),
                     init=c())
print(res)
