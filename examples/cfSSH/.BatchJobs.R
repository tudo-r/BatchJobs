cluster.functions = makeClusterFunctionsSSH(list(
  list(nodename="compute1", ncpus=6, rhome="/opt/R/R-current/"),
  list(nodename="compute2", ncpus=6, rhome="/opt/R/R-current/")))

mail.from = "local-cluster-functions-example@p-value.net"
mail.to = "olafm@p-value.net"
mail.control = list()
