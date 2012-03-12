#!/usr/bin/env Rscript
required_packages <- c("roxygen2", "RUnit", "digest", "stringr", "RSQLite",
                       "sendmailR", "BBmisc")

installed_packages <- rownames(installed.packages())
missing_packages <- setdiff(required_packages, installed_packages)
if (length(missing_packages) > 0) {
  message("Installing the following missing packages:")
  print(missing_packages)
  install.packages(missing_packages,
                   repos="http://cran.at.r-project.org")
} else {
  message("All dependencies installed.")
}
