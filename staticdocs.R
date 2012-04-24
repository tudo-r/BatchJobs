library(staticdocs)
# see https://github.com/hadley/staticdocs/
library(BBmisc)
options(BBmisc.ProgressBar.style="off")

ddir = "html"
if(file.exists(ddir))
  unlink(ddir, recursive = TRUE)
build_package("BatchJobs", base_path = ddir)
