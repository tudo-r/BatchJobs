library("BatchJobs")

f = function(data) {
  Sys.sleep(1)
  -data
}

registry <- makeRegistry(id="minimal", file.dir="minimal")
batchMap(registry, f, 1:10)
submitJobs(registry)

showStatus(registry)
Sys.sleep(10)

res <- reduceResults(registry, fun=function(aggr, job, res) c(aggr, res),
                     init=c())
print(res)
