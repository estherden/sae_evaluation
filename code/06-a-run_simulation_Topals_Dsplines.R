## Esther Denecke
# This script is to run the simulations for TOPALS and D-splines with 
# batchtools.
# "Optimized" to run on an HPC cluster with many jobs in parallel but can also 
#  be run sequentially (beware that this will take a long time). In parallel
#  this takes about 2 hours (more or less) on the HPC cluster.


## Prerequisites: Adjust the date for the data to be loaded in "registry" 
#                 Section, i.e. this line:
#                 load = c("data/dataProcessed/05_a_data_for_batchtools_20221202.RData"),
#                 The date should be the one when 05-a was run and the output saved.


## seed ------------------------------------------------------------------------

## strictly speaking: no seeds necessary here because data are already simulated 
# and everything is thus deterministic
seed_reg = 7945 # seed for the registry
seed_problem = 37 # seed for the problem

## prep ------------------------------------------------------------------------

library(batchtools)

# date
today = format(Sys.Date(), format = "%Y%m%d")

## registry --------------------------------------------------------------------

# make registry
reg = makeExperimentRegistry(file.dir = paste0("registryTopalsDsplines_", today), 
                             conf.file = ".batchtools.conf.R",
                             packages = c("data.table", "here", "tictoc"),
                             source = c("code/00-functions.R",
                                        "code/00-topals.R",
                                        "code/00-dsplines.R"),
                             load = c("data/dataProcessed/05_a_data_for_batchtools_20230130.RData"),
                             seed = seed_reg)

# reg = loadRegistry(file.dir = paste0("registryTopalsDsplines_", "20230313"),
#                    conf.file = ".batchtools.conf.R",
#                    writeable = TRUE)

## arguments -------------------------------------------------------------------

reps = 1 # this needs to be 1 as the data are already simulated 

# nsims
nsim = 1000

# arguments for problem design
sexArg = "male"
exposSizeArg = c(1000, 5000, 10000, 25000, 50000, 75000, 100000, 1000000)
simRunArg = 1:nsim
## Possible improvement to code: Would have been better to also include method 
#   as selectData() has this argument. However, default of selectData() is 
#   Topals and D-splines, so okay like this.

# arguments for algorithm designs
yearsSaveArg = 2000 # year to save -> year analyzed
estimatorArg = c("D1", "D2", "DLC") # estimators for D-Splines
demKnowArg = c("dk1", "dk2", "dk3") # demographic knowledge (changes with sex, algorithm & estimator)
groupedAgesArg = c(TRUE, FALSE) # input in age groups or single years?


## problem and algorithm designs -----------------------------------------------

# problem design 
# name pdes copied from https://mllg.github.io/batchtools/articles/batchtools.html (20221119)
pdes = list(selectSimData = CJ(sexArg, exposSizeArg, simRunArg))

# algorithm design 
# name ades copied from https://mllg.github.io/batchtools/articles/batchtools.html (20221119)
ades = list(
  dspline = CJ(estimator = estimatorArg, 
               demKnow = demKnowArg, 
               yearsSave = yearsSaveArg,
               groupedAges = groupedAgesArg),
  topals = CJ(demKnow = demKnowArg, 
              yearsSave = yearsSaveArg,
              groupedAges = groupedAgesArg)
)


## add problem -----------------------------------------------------------------

addProblem(name = "selectSimData", 
           data = dat, 
           fun = selectData, 
           seed = seed_problem)


# add algorithms ---------------------------------------------------------------

# add algorithms
addAlgorithm(name = "dspline",
             fun = dspline_wrapper)

addAlgorithm(name = "topals", 
             fun = topals_wrapper)


# add experiments --------------------------------------------------------------

addExperiments(prob.designs = pdes, 
               algo.designs = ades, 
               repls = reps)

# testing ----------------------------------------------------------------------

# This section is just for testing batchtools. Does not need to be run, so 
#  commented out.

# # summarize experiments
# summarizeExperiments()
# summarizeExperiments(by = c("problem", 
#                             "sexArg", "exposSizeArg",
#                             "algorithm", 
#                             "demKnow", "estimator", "yearsSave", "groupedAges"))
# 
# findExperiments(algo.name = "dspline")
# 
# getJobPars(ids = 1)$algo.pars
# getJobPars(ids = 2)$algo.pars
# 
# tt1 = testJob(id = 1)
# tt2 = testJob(id = 2)
# 
# findExperiments(algo.name = "topals")


# submit jobs ------------------------------------------------------------------

jobids = getJobTable()[, job.id]

# Make chunks: The single jobs (as per job.id) run very quickly, so they should 
# be run in chunks.
chunking = data.table(job.id = 1:(length(jobids)), 
                      chunk = as.numeric(NA))
chunking[, chunk := lpt(x = chunking$job.id, n.chunks = (length(jobids) / 500))]

submitJobs(ids = chunking, 
           resources = list(walltime = 20000, # almost 6 hours per chunk
                            memory = 4000, 
                            ncpus = 1))
