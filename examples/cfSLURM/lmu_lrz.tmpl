#!/bin/bash
## Job Resource Interface Definition
##
## walltime [integer(1)]:     Walltime for this job, in seconds.
##                            Must be between 1 and 3600 * 24 * 2.
## memory   [integer(1)]:     Memory in megabytes for each cpu.
##                            Must be between 100 and 64000.
##
## Default resources can be set in your .BatchJobs.R by defining the variable
## 'default.resources' as a named list.
<%
d = setdiff(names(resources), c("walltime", "memory"))
if (length(d) > 0L)
  stopf("Illegal resources used: %s", collapse(d))

walltime = asInt(resources$walltime, lower = 1L, upper = 172800L)
memory = asInt(resources$memory, lower = 100L, upper = 64000L)

cmd = "R CMD BATCH --no-save --no-restore"

-%>

## general settings
#SBATCH -J <%= job.name %>
## direct stream to our logfile (-o is stdout and error)
#SBATCH -o <%= log.file %>
#SBATCH --clusters=serial
#SBATCH --partition=serial_std
#SBATCH --get-user-env
#SBATCH --mail-user=bernd_bischl@gmx.net
#SBATCH --export=NONE

## resources
## SLURM wants minutes
#SBATCH --time=<%= ceiling(walltime / 60L)%>
#SBATCH --mem-per-cpu=<%= memory %>

source /etc/profile

## Run R:
## we merge R output with stdout from SLURM, which gets then logged via --output option

source /etc/profile.d/modules.sh
module load R/3.1
<%= cmd %> "<%= rscript %>" /dev/stdout


