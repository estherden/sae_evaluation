## Esther Denecke
## This is to collect the batchtools results for the SVD model. Run this on
#     the same computer as 06-b. Ideally an HPC cluster. Collects one
#     exposure size at the time. See prerequisites.

## Prerequisites: Change arguments in "arguments to (possibly) adjust".

## Info:
#   Results for expos 1000, 5000, and 10000 come from date_registry == "20230219"
#   Others come from date_registry == "20230224".
#   This is because registry 20230219 broke down in the middle. The saved output 
#   for new registry is smaller and does therefore not cause problems.


## arguments to (possibly) adjust ----------------------------------------------

# need to replace accordingly
date_data = "20230215" # date when 05-b-make_data_for_simulation_SVD was run and saved
date_registry = "20230224"# "20230219"# date when 06-b-_run_simulation_SVD.R was run and saved

# expos size to collect: this needs to be adjusted each time
expos = 50000 # which results should be collected here?

## prep ------------------------------------------------------------------------

options(scipen = 999) # display numbers differently
options(batchtools.progress = FALSE) # options


# packages
library(batchtools)
library(data.table)
library(here)

# functions
source(here("code", "00-functions_to_analyze_results.R"))

# for loading the correct registry
platform = Sys.info()['sysname']

if (platform == "Linux") {
  node = "amp033"
} else if (platform == "Windows") {
  node = "HYDRA11"
}

# load registry
reg = loadRegistry(file.dir = paste0("registrySVD_",
                                     platform, "_",
                                     node, "_",
                                     date_registry),
                   conf.file = ".batchtoolsSVD.conf.R",
                   writeable = TRUE)

# create directory to save results
dir.create(path = here("results", paste0("registrySVD_",
                                         platform, "_",
                                         node, "_",
                                         date_registry)))


## get job info ----------------------------------------------------------------

jobPars = unwrap(getJobPars())


## collect diagnostics ---------------------------------------------------------

## expos
ids_expos = jobPars[exposSizeArg == expos, job.id]

all(ids_expos %in% findDone()[, job.id])

# find jobs that finished running
ids_expos = findDone()[, job.id][findDone()[, job.id] %in% ids_expos]

# collect diagnostics
res_diag = reduceResultsDataTable(ids = ids_expos,
                                  fun = collectResults,
                                  selectedResult = "res_diag")

# merge with jobPars to get jobinfo
res_diag2 = merge(x = jobPars, y = res_diag, by = "job.id", all = TRUE)

# expand to see diagnostics
res_diag3 = res_diag2[, rbindlist(result),
                      by = .(job.id, yearsSave, exposSizeArg, simRunArg, demKnow)]

# save res_diag3
saveRDS(file = here("results", 
                    paste0("registrySVD_", platform, "_", node, "_", date_registry), 
                    paste0("res_diag_expos", expos, ".rds")), 
        object = res_diag3)

# clean up
rm(list = c("res_diag", "res_diag2", "res_diag3"))


## collect estimates -----------------------------------------------------------

# collect estimates
res_logmx = reduceResultsDataTable(ids = ids_expos,
                                   fun = collectResults,
                                   selectedResult = "res_logmx")

# merge with jobPars to get jobinfo
res_logmx2 = merge(x = jobPars, y = res_logmx, by = "job.id", all = TRUE)

# expand to actually see results
res_logmx3 = res_logmx2[, rbindlist(result),
                        by = .(job.id, yearsSave, exposSizeArg, simRunArg, demKnow)]

# save res_logmx3
saveRDS(file = here("results", 
                    paste0("registrySVD_", platform, "_", node, "_", date_registry), 
                    paste0("res_logmx_expos", expos, ".rds")), 
        object = res_logmx3)

# clean up
rm(list = c("res_logmx", "res_logmx2", "res_logmx3"))
