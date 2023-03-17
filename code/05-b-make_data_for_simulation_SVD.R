## Esther Denecke
# This script is just to pre-process the data for 06-run_simulation.R
# In particular, we need one object that contains the simulated data
# as well as all the demographic knowledge.


## Prerequisites: Change date in "arguments to (possibly) adjust" to whenever
#                 03-b was run.

## This code should be run on the same machine as 06-b. The reason is that the
#     stan model is precompiled. Running it on different platforms might 
#     cause 06-b to fail.


## arguments to (possibly) adjust ----------------------------------------------

date_03_grouped = "20221212"


# package ----------------------------------------------------------------------

library(here)
library(data.table)
library(rstan) # to precompile the stan model

# date
today = format(Sys.Date(), format = "%Y%m%d")

# platform & node
platform = Sys.info()['sysname']
node = Sys.info()['nodename']

## read data  ------------------------------------------------------------------

# data from step 03
datSim = readRDS(here("data", "dataProcessed",
                      paste0("03_b_data_simulated_grouped_", date_03_grouped, ".rds")))

# demographic knowledge
dkSVD = readRDS(file = here("data", "dataProcessed", "04_dkSVD.rds"))


## wrangling -------------------------------------------------------------------

# remove seeds and smaller100 and we don't need it for batchtools
datSim[, ":="(seeds = NULL, smaller100 = NULL)]

# remove females
datSim = datSim[sex == "male", ]

dkSVD$female = NULL

# remove expos_size and Nx_true as not needed
for (i in 1:nrow(datSim)) {
  datSim[i, simData][[1]][, ':='(Nx_true = NULL, expos_size = NULL)]
}

# pre-compile the model --------------------------------------------------------

# compile
pre_base_model_1mean_Ameer_stan = stan_model(file = here("code",
                                                         "base_model_1mean.stan"),
                                             model_name = "base_model_1mean_Ameer.stan")


# data into list ---------------------------------------------------------------

dat = list(datSim = datSim,
           dkSVD = dkSVD,
           pre_base_model_1mean_Ameer_stan = pre_base_model_1mean_Ameer_stan)

## save data -------------------------------------------------------------------

save(object = dat, file = here("data", 
                               "dataProcessed", 
                               paste0("05_b_data_for_batchtools_SVD_", 
                                      platform, "_",
                                      node, "_",
                                      today, ".RData")))
