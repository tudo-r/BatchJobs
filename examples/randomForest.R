library(BatchJobs)

# do a 10-fold-CV for a random forest on iris in parallel
test = split(sample(1:nrow(iris)), rep(1:5, each=30))
train = lapply(test, function(x) setdiff(1:150, x))

f = function(train, test) {
  Sys.sleep(10)
  library(randomForest)
  train = iris[train, ]
  test = iris[test, ]
  m = randomForest(Species~., data=train)
  p = predict(m, newdata=test)
  as.character(p)
}

reg = makeRegistry(id="randomforest_cv", file.dir="example_files")
batchMap(reg, f, train, test)
submitJobs(reg)

#pred = reduceResults(reg, fun=function(a,j,r) c(a, r), init=c())
#print(mean(pred != iris$Species[unlist(test)]))
