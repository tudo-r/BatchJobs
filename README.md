# BatchJobs

## Core features
* Create, submit and control [R](http://www.r-project.org/) jobs on batch systems
* Provides the functional programming tools Map, Reduce and Filter to operate on the cluster
* Supported systems include Torque/PBS, SGE, SLURM and LSF
* Support for makeshift SSH clusters and local (multicore) execution
* Convenient collection and aggregation of results
* Further Map and Reduce results from previous jobs as batch jobs
* Optional mail sending using [sendmailR](http://cran.r-project.org/web/packages/sendmailR) after job completion
* Query status of jobs and display log files inside R
* Possibility to write your own simple cluster interface if your architecture is not supported
* [BatchExperiments](https://github.com/tudo-r/Batchexperiments) extends this package with functionality required for comprehensive computer experiments and simulation studies.


## Quickstart
To install the latest stable release from CRAN:
```splus
install.packages("BatchJobs")
```
To install the development version use [devtools](http://cran.r-project.org/web/packages/devtools):
```splus
library(devtools)
install_github("BatchJobs", username="tudo-r")
```
Sometimes it might be required to update BatchJobs' dependencies as well.
If you encounter errors during installation, try upgrading by [BBmisc](https://github.com/berndbischl/BBmisc) and [fail](https://github.com/mllg/fail) before filing a bug report:
```splus
library(devtools)
install_github("BBmisc", username="berndbischl")
install_github("fail", username="mllg")
```
A fresh installation defaults to a local execution mode.
Proceed to [Configuration](../../wiki/Configuration) to set up cluster execution.

## Documentation
Currently the best introduction to the package is our [technical report](http://sfb876.tu-dortmund.de/PublicPublicationFiles/bischl_etal_2012a.pdf).
For more detailed information on the functions consult the [R documentation](http://tudo-r.github.io/BatchJobs/).
We also provide a [FAQ](../../wiki/FAQ) in our [wiki](../../wiki).

We also have a [mailing list](http://groups.google.com/group/batchjobs).

[![Build Status](https://travis-ci.org/tudo-r/BatchJobs.png)](https://travis-ci.org/tudo-r/BatchJobs)
