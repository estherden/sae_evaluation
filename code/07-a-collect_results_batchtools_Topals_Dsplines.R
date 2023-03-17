## Esther Denecke
# This script is to collect the results from 06-a. We collect the results
# "in batches" due to memory considerations. 

## Prerequisites: Change date in "arguments to (possibly) adjust" to
#                 whenever then registry in 06-a was created.


## arguments to (possibly) adjust ----------------------------------------------

# date registry
date_06 = "20230314"


## preparation -----------------------------------------------------------------

library(batchtools)
library(here)

# source functions
source(here("code", "00-functions_to_analyze_results.R"))

# load registry
reg = loadRegistry(file.dir = paste0("registryTopalsDsplines_", date_06), 
                   conf.file = ".batchtools.conf.R")

# make directory to save results
dir.create(path = here("results", paste0("TopalsDsplines_", date_06)))

## collect ---------------------------------------------------------------------

jobPars = unwrap(getJobPars())

## A bit cumbersome and needs to be adjusted manually if the number of jobs
#   changes. 
x = list(p1 = 1:25000,
         p2 = 25001:50000,
         p3 = 50001:75000,
         p4 = 75001:100000,
         p5 = 100001:125000,
         p6 = 125001:150000,
         p7 = 150001:175000,
         p8 = 175001:(nrow(jobPars)))

# get actual results
for (i in 1:length(x)) {
  
  res = reduceResultsDataTable(ids = x[[i]], 
                               fun = collectResults, selectedResult = "res")
  tab = ijoin(jobPars, res)
  saveRDS(object = tab, 
          file = here("results", 
                      paste0("TopalsDsplines_", date_06), 
                      paste0("res_p", i, ".rds")))
  rm(list = c("res", "tab"))
  
}


# get diagnostics
for (i in 1:length(x)) {
  
  res = reduceResultsDataTable(ids = x[[i]], 
                               fun = collectResults, selectedResult = "resDiag")
  tab = ijoin(jobPars, res)
  saveRDS(object = tab, 
          file = here("results", 
                      paste0("TopalsDsplines_", date_06), 
                      paste0("resDiag_p", i, ".rds")))
  rm(list = c("res", "tab"))
  
}

# save 
jobPars = unwrap(getJobPars())

combns = summarizeExperiments(by = c("problem", 
                                     "sexArg", "exposSizeArg", #"simRunArg",
                                     "algorithm", 
                                     "demKnow", "estimator", "yearsSave", "groupedAges"))

metaData = list(jobPars = jobPars,
                combinations = combns,
                sInfo = sessionInfo())


saveRDS(metaData, here("results", 
                       paste0("TopalsDsplines_", date_06), 
                       "metaDataJobs.rds"))
