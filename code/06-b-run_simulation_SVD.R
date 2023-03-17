## Esther Denecke
# This script is to run the simulations for the SVD model. Should be run on an 
# HPC cluster. Otherwise, it will 'never' finish.

## I ran this code with date_registry = "20230219" and "20230224". 
#   Exposure sizes 1000, 5000, and 10000 from "20230219". All the others 
#   from "20230224".


## adjust accordingly ----------------------------------------------------------

date_registry = "20230224" # "20230219" # date_registry
date_data = "20230215" # date when 06-b-_run_simulation_SVD.R was run and saved


## seed ------------------------------------------------------------------------

seed_reg = 8904 # seed for the registry
seed_problem = 145 # seed for the problem


## prep ------------------------------------------------------------------------

library(batchtools)

# for loading the correctly compiled stan model
platform = Sys.info()['sysname']

if (platform == "Linux") {
  node = "amp033"
} else if (platform == "Windows") {
  node = "HYDRA11"
}


## registry --------------------------------------------------------------------

## Either make the registry (makeExperimentRegistry) or load registry 
#     (loadRegistry) depending on whether the registry already exsists or not.

# make registry
reg = makeExperimentRegistry(file.dir = paste0("registrySVD_",
                                               platform, "_",
                                               node, "_",
                                               date_registry), 
                             conf.file = ".batchtoolsSVD.conf.R",
                             packages = c("data.table", "here", "rstan", "tictoc"),
                             source = c("code/00-functions.R"),
                             load = c(paste0("data/dataProcessed/05_b_data_for_batchtools_SVD_", 
                                             platform, "_",
                                             node, "_",
                                             date_data, ".RData")),
                             seed = seed_reg)


# load registry (non-writeable)
reg = loadRegistry(file.dir = paste0("registrySVD_",
                                     platform, "_",
                                     node, "_",
                                     date_registry),
                   conf.file = ".batchtoolsSVD.conf.R",
                   writeable = TRUE)


## arguments -------------------------------------------------------------------

# nsims
nsim = 1000

reps = 1 # needs to be one because data already simulated

# problem design
sexArg = "male"
exposSizeArg = c(1000, 5000, 10000, 25000, 50000, 75000, 100000, 1000000)
simRunArg = 1:nsim
method = "SVD"

stan_iter_high = 6000
stan_warmup_high = 1000
stan_iter_low = 3000
stan_warmup_low = 500
stan_chains = 4
stan_adapt_delta = 0.99
stan_max_treedepth = 12
stan_cores = 1 # depending on where this code is run.


# algorithm design
demKnow = c("dk1", "dk2", "dk3") # demographic knowledge
yearsSave = 2000



## problem and algorithm designs -----------------------------------------------

# problem design 
# name pdes copied from https://mllg.github.io/batchtools/articles/batchtools.html (20221119)
pdes = list(selectSimData = CJ(sexArg, 
                               exposSizeArg, 
                               method, 
                               simRunArg,
                               stan_chains,
                               stan_adapt_delta,
                               stan_max_treedepth,
                               stan_cores))

pdes$selectSimData[exposSizeArg >= 10000, stan_iter := stan_iter_high]
pdes$selectSimData[exposSizeArg <= 5000, stan_iter := stan_iter_low]

pdes$selectSimData[exposSizeArg >= 10000, stan_warmup := stan_warmup_high]
pdes$selectSimData[exposSizeArg <= 5000, stan_warmup := stan_warmup_low]

# algorithm design 
# name ades copied from https://mllg.github.io/batchtools/articles/batchtools.html (20221119)
ades = list(
  svd = CJ(demKnow,
           yearsSave)
)

## add problem -----------------------------------------------------------------

addProblem(name = "selectSimData", 
           data = dat, 
           fun = selectData, 
           seed = seed_problem)


# add algorithms ---------------------------------------------------------------

addAlgorithm(name = "svd",
             fun = stan_wrapper)


# add experiments --------------------------------------------------------------

addExperiments(prob.designs = pdes, 
               algo.designs = ades, 
               repls = reps)


##### REGISTRY 20230219 --------------------------------------------------------

jobPars = unwrap(getJobPars())

# get job.id's
ids_10800 = jobPars[exposSizeArg %in% c(1000, 5000), job.id]
ids_21600 = jobPars[exposSizeArg %in% c(10000, 25000, 50000, 75000, 100000), job.id]
ids_43200 = jobPars[exposSizeArg == 1000000, job.id]
## The naming was supposed to indicate the specified runtime. However, did
#   not use these runtimes in the end. Just the naming stayed.

# submit ids_10800
submitJobs(ids = ids_10800,
           resources = list(walltime = 7000, 
                            memory = 10000, 
                            ncpus = 1))

# there's a maximum of 4999 jobs at the same time..

## if expired
jobs_expired = findExpired()[, job.id]

# grep logs
grepLogs(ids = jobs_expired, pattern = "DUE TO TIME LIMIT")

# reset jobs
resetJobs(ids = jobs_expired, reg = reg)

# resubmit with more time
submitJobs(ids = jobs_expired,
           resources = list(walltime = 14000, 
                            memory = 10000, 
                            ncpus = 1))

## next batch of walltime 10000
submitJobs(ids = 5000:6000,
           resources = list(walltime = 10000, 
                            memory = 10000, 
                            ncpus = 1))

jobPars = unwrap(getJobPars())

# submit ids 21600: one more batch
submitJobs(ids = ids_21600[1:3000],
           resources = list(walltime = 16000, 
                            memory = 15000, 
                            ncpus = 1))

## if expired
jobs_expired = findExpired()[, job.id] # 6311

# grep logs
grepLogs(ids = jobs_expired, pattern = "DUE TO TIME LIMIT")

# reset jobs
resetJobs(ids = jobs_expired, reg = reg)

submitJobs(ids = jobs_expired,
           resources = list(walltime = 25000, 
                            memory = 15000, 
                            ncpus = 1))


# Registry 20230224 ------------------------------------------------------------

jobPars = unwrap(getJobPars())

# get job.id's
ids_10800 = jobPars[exposSizeArg %in% c(1000, 5000), job.id]
ids_21600 = jobPars[exposSizeArg %in% c(10000, 25000, 50000, 75000, 100000), job.id]
ids_43200 = jobPars[exposSizeArg == 1000000, job.id]

### SUBMIT 1 - expos25000 - DONE 20230224 
submitJobs(ids = ids_21600[3001:6000],
           resources = list(walltime = 16000, 
                            memory = 15000, 
                            ncpus = 1))

### RE-SUBMIT 1 - expos25000 - DONE 20230225
# resubmit expired
jobs_expired = findExpired()[, job.id] 

# grep logs
grepLogs(ids = jobs_expired, pattern = "DUE TO TIME LIMIT")

# reset jobs
resetJobs(ids = jobs_expired, reg = reg)

submitJobs(ids = jobs_expired, 
           resources = list(walltime = 43200, # 12 hours
                            memory = 15000, 
                            ncpus = 1))


### SUBMIT 2 - expos50000 - DONE 20230225
submitJobs(ids = ids_21600[6001:9000],
           resources = list(walltime = 20000, 
                            memory = 15000, 
                            ncpus = 1))

### RE-SUBMIT 2 - expos50000 - DONE 20230226
# resubmit expired
jobs_expired = findExpired()[, job.id] 
# 12975 13012 13287 13289 13290 13291 13327 13413 14329 14456 14505 14514
# 14523

# grep logs
grepLogs(ids = jobs_expired, pattern = "DUE TO TIME LIMIT")

# reset jobs
resetJobs(ids = jobs_expired, reg = reg)

submitJobs(ids = jobs_expired, 
           resources = list(walltime = 43200, # 12 hours
                            memory = 15000, 
                            ncpus = 1))


### SUBMIT 3 - expos75000 - DONE 20230226
submitJobs(ids = ids_21600[9001:12000],
           resources = list(walltime = 16000, 
                            memory = 15000, 
                            ncpus = 1))


### RE-SUBMIT 3 - expos75000 - DONE 20230227
# resubmit expired
jobs_expired = findExpired()[, job.id] 
# 15108 15132 15202 15230 15232 15286 15321 15360 15435 15501 15537 15549
# 15559 15834 15837 15838 15839 15840 15841 15842 15843 15845 15846 15852
# 15854 15855 15856 15860 15861 15862 15863 15864 15865 15866 15867 15868
# 15869 15870 15871 15872 15873 15874 15875 15876 15877 15878 15879 15880
# 15881 15882 15883 15884 15885 15886 15887 15888 15889 15890 15891 15904
# 15913 15989 15999 16109 16446 16581 16650 16677 16682 16728 16734 16863
# 16865 16875 16886 16890 16891 16892 16893 16894 16895 16896 16897 16898
# 16899 16900 16901 16902 17046 17157 17161 17352 17381 17385 17438 17607

# grep logs
grepLogs(ids = jobs_expired, pattern = "DUE TO TIME LIMIT")

# reset jobs
resetJobs(ids = jobs_expired, reg = reg)

submitJobs(ids = jobs_expired, 
           resources = list(walltime = 43200, # 12 hours
                            memory = 15000, 
                            ncpus = 1))


### SUBMIT 4 - expos100000 - DONE 20230227
submitJobs(ids = ids_21600[12001:15000],
           resources = list(walltime = 16000, 
                            memory = 15000, 
                            ncpus = 1))

### RE-SUBMIT 4 - expos100000 - 20230301
# resubmit expired
jobs_expired = findExpired()[, job.id]  # length is 421 but 15841 is expos75000
# 15841 18012 18027 18029 18030 18033 18039 18045 18060 18069 18075 18078 18081 18082 18087
# 18092 18093 18103 18121 18129 18153 18174 18186 18243 18246 18306 18320 18321 18322 18323
# 18324 18325 18326 18327 18328 18329 18330 18331 18332 18333 18334 18335 18336 18337 18338
# 18339 18357 18362 18368 18369 18378 18400 18405 18414 18415 18417 18418 18419 18420 18432
# 18459 18471 18495 18507 18512 18520 18528 18537 18540 18543 18546 18556 18565 18586 18592
# 18593 18594 18595 18596 18597 18598 18599 18600 18601 18602 18603 18610 18643 18649 18654
# 18655 18658 18663 18664 18665 18675 18676 18677 18678 18679 18680 18681 18682 18683 18684
# 18685 18695 18711 18717 18725 18761 18762 18793 18795 18813 18822 18825 18840 18841 18842
# 18847 18848 18852 18894 18895 18907 18908 18923 18924 18934 18935 18947 18948 18954 18955
# 18975 18980 18987 18993 19014 19023 19031 19032 19038 19052 19075 19076 19077 19078 19079
# 19080 19081 19082 19083 19084 19085 19086 19087 19088 19089 19090 19092 19093 19094 19095
# 19096 19097 19098 19099 19101 19102 19120 19140 19141 19142 19143 19144 19145 19146 19147
# 19148 19149 19150 19151 19152 19153 19154 19155 19156 19160 19161 19162 19163 19164 19188
# 19194 19209 19212 19239 19278 19350 19360 19464 19467 19479 19503 19505 19506 19507 19508
# 19509 19510 19511 19512 19513 19514 19540 19546 19560 19563 19600 19601 19602 19629 19652
# 19661 19662 19688 19698 19752 19755 19758 19761 19773 19791 19793 19807 19830 19832 19859
# 19869 19874 19876 19877 19878 19879 19880 19882 19883 19884 19886 19887 19891 19893 19894
# 19908 19911 19923 19938 19949 19950 19962 19963 19964 19966 19967 19968 19969 19995 20004
# 20011 20014 20028 20044 20052 20055 20056 20060 20061 20066 20068 20069 20080 20081 20099
# 20106 20107 20114 20116 20118 20119 20124 20139 20143 20150 20152 20161 20171 20179 20184
# 20188 20190 20191 20193 20194 20196 20202 20208 20213 20219 20220 20221 20233 20237 20238
# 20246 20250 20255 20256 20257 20258 20259 20260 20261 20262 20263 20264 20274 20279 20280
# 20281 20285 20299 20301 20313 20314 20315 20319 20331 20343 20358 20361 20363 20385 20399
# 20400 20422 20469 20497 20541 20550 20575 20579 20584 20596 20600 20640 20658 20667 20674
# 20675 20681 20685 20691 20695 20704 20705 20708 20709 20715 20716 20723 20728 20743 20749
# 20756 20757 20768 20769 20778 20786 20787 20797 20808 20817 20825 20830 20839 20841 20861
# 20862 20864 20865 20869 20871 20872 20882 20892 20893 20896 20902 20918 20920 20922 20924
# 20928 20930 20932 20940 20945 20946 20951 20957 20961 20976 20981 20994 20995 20996 20997
# 20998

# grep logs
grepLogs(ids = jobs_expired, pattern = "DUE TO TIME LIMIT")

# reset jobs
resetJobs(ids = jobs_expired, reg = reg)

submitJobs(ids = jobs_expired, 
           resources = list(walltime = 43200, # 12 hours
                            memory = 15000, 
                            ncpus = 1))


### SUBMIT 5 - expos1000000 - DONE 20230301
submitJobs(ids = ids_43200,
           resources = list(walltime = 43200, 
                            memory = 15000, 
                            ncpus = 1))
