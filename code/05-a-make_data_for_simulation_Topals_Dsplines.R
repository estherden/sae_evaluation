## Esther Denecke
# This script is to pre-process the data for 06-run_simulation.R
# In particular, we need one object that contains the simulated data
# as well as all the demographic knowledge.
# The output of this script will be called by batchtools (see 06-a to see that
# it is loaded when creating the registry using function 
# batchtools::makeExperimentRegistry().

## Prerequisites: Change dates in "arguments to (possibly) adjust" to
#                 whenever scripts with 03 and 04 were run 
#                 and the output saved. Differentiate 04 for Topals and
#                 Dsplines. 

## This script also needs quite a lot of memory within R because of the size
#   of the data!


## arguments to (possibly) adjust ----------------------------------------------

date_03 = "20230125"
date_04_Topals = "20230126"
date_04_Dsplines = "20230130"

# package ----------------------------------------------------------------------

library(data.table) # data wrangling
library(here) # paths


## arguments -------------------------------------------------------------------

yearSave = 2000 # we only run the simulation for the year 2000

# date
today = format(Sys.Date(), format = "%Y%m%d")


## read data  ------------------------------------------------------------------

# data from step 03
datSim = readRDS(here("data", "dataProcessed",
                      paste0("03_a_data_simulated_", date_03, ".rds")))
                      

# demographic knowledge
dkTopals = readRDS(file = here("data", "dataProcessed", 
                               paste0("04_a_dkTopals_", date_04_Topals, ".rds")))
dkDspline = readRDS(file = here("data", "dataProcessed", 
                                paste0("04_a_dkDspline_", date_04_Dsplines, ".rds")))


## wrangling -------------------------------------------------------------------

# remove seeds and smaller100 that we don't need it for batchtools
datSim[, ":="(seeds = NULL, smaller100 = NULL)]

# keep only year 2000 & remove expos_size and Nx_true as not needed
for (i in 1:nrow(datSim)) {
  datSim[i, simData := list(datSim[i, simData][[1]][year == yearSave, ])]
  datSim[i, simData][[1]][, ':='(Nx_true = NULL, expos_size = NULL)]
}

# data into list
dat = list(datSim = datSim,
           dkTopals = dkTopals,
           dkDspline = dkDspline)


## save data -------------------------------------------------------------------

save(object = dat, 
     file = here("data", "dataProcessed", 
                 paste0( "05_a_data_for_batchtools_", today, ".RData" )))
