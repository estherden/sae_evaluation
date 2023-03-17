## Esther Denecke
## This script is to look at the diagnostics of the SVD simulation and save
#   output to analyze.

## Prerequisites: Change arguments in "arguments to (possibly) adjust". 

## arguments to (possibly) adjust ----------------------------------------------

date_data = "20230215" # date when 05-b-make_data_for_simulation_SVD was run and saved
date_registry = "20230219"# date when 06-b-_run_simulation_SVD.R was run and saved

platform = "Linux" # platform on which 06-b was run
node = "amp033" # node on which model was compiled (NOT where 06-b was run!)


# packages ---------------------------------------------------------------------

library(data.table)
library(here)
library(checkmate)


# read in ----------------------------------------------------------------------

res_path = here("results", paste0("registrySVD_", platform, "_", 
                                  node, "_", date_registry))

res_diag = readRDS(here(res_path, "res_diag_all.rds"))
res_logmx = readRDS(here(res_path, "res_logmx_all.rds"))

# true data & keep only true and simulated data year 2000
load(file = here("data", "dataProcessed", 
                 paste0("05_b_data_for_batchtools_SVD_", 
                        platform, "_", node, "_", date_data, ".RData")))

# first checks: monitor --------------------------------------------------------

# add variable that summarizes Rhat, bulkESS, and tailESS of monitor()
res_diag[, passed_monitor := ifelse(passed_Rhat_monitor == TRUE & 
                                      passed_bulkESS_monitor == TRUE &
                                      passed_tailESS_monitor == TRUE, 
                                    TRUE, FALSE)]

# count of successful passes
res_diag[, .(sum_monitor = sum(passed_monitor)),
         by = .(exposSizeArg, demKnow)][order(exposSizeArg), ]


# first checks: divergent, treedepth, BFMI -------------------------------------

# divergent
res_diag[, .(sum_divergent = sum(passed_num_divergent)),
             by = .(exposSizeArg, demKnow)][order(exposSizeArg), ]

# treedepth
res_diag[, .(sum_treedepth = sum(passed_max_treedepth)),
         by = .(exposSizeArg, demKnow)][order(exposSizeArg), ] 

# BFMI
res_diag[, .(sum_BFMI = sum(passed_BFMI)),
         by = .(exposSizeArg, demKnow)][order(exposSizeArg), ]


# combine checks ---------------------------------------------------------------

# we want to know how many passed all checks
res_diag[, passed_all := ifelse(passed_monitor == TRUE &
                                  passed_num_divergent == TRUE &
                                  passed_max_treedepth == TRUE &
                                  passed_BFMI == TRUE, 
                                TRUE, FALSE)]

sum_tab = res_diag[, .('checks passed' = sum(passed_all),
                       'Rhat' = sum(passed_Rhat_monitor),
                       'bulk ESS' = sum(passed_bulkESS_monitor),
                       'tail ESS' = sum(passed_tailESS_monitor),
                       'divergence' = sum(passed_num_divergent),
                       'treedepth' = sum(passed_max_treedepth),
                       'BFMI' = sum(passed_BFMI),
                       'completed runs' = .N),
                   by = .(exposSizeArg, demKnow)][order(exposSizeArg), ]
setnames(sum_tab, old = c("exposSizeArg", "demKnow"),
         new = c("exposure", "dk"))

print(xtable::xtable(sum_tab[order(exposure, dk)]), include.rownames = FALSE)


# which jobs (without expos 1000000) did not complete in the given time?
which(!(1:21000 %in% res_diag[, job.id])) # 20497

# remove jobs that won't be analyzed -------------------------------------------

# which ones did not pass all checks?
ids_np = res_diag[passed_all == FALSE, job.id]

# reformat datSim 
tmp = dat[["datSim"]]
tmp2 = tmp[, rbindlist(simData), by = .(sex, exposSize)]
datSim = tmp2[year == 2000, ]
datSim[, simRunArg := as.numeric(unlist(lapply(strsplit(x = as.character(sim_run), 
                                                        split = "sim"), '[', 2)))]

setnames(datSim, old = c("exposSize"), new = c("exposSizeArg"))

# clean up
rm(list = c("dat", "tmp", "tmp2"))


# merge results with true and simulated data
tmp = merge(x = res_logmx,
            y = datSim[sex == "male", .(exposSizeArg, simRunArg, age, id_ab, 
                                        year, logmx_true, NxAdj, Dx)],
            by = c("exposSizeArg", "simRunArg", "id_ab", "age", "year"))

# wrangling --------------------------------------------------------------------

# keep only jobs that passed checks
tmp2 = tmp[!(job.id %in% ids_np), ]

# remove expos 75000 and 100000 with dk3
tmp3 = tmp2[!(exposSizeArg == 100000 & demKnow == "dk3"), ]
assertDataTable(tmp3, nrows = nrow(tmp2) - 205800) # 205800 rows with 100000 and dk3
# the exact numbers could change in future runs because it is so difficult
# (practically impossible??) to reproduce numbers from Bayesian methods exactly

tmp4 = tmp3[!(exposSizeArg == 75000 & demKnow == "dk3")] 
assertDataTable(tmp4, nrows = nrow(tmp3) - 265020) # 265020 rows with 75000 and dk3

# remove expos 1000000
res = tmp4[exposSizeArg != 1000000, ]
assertDataTable(res, nrows = nrow(tmp4) - 123900) # 123900 rows with expos 1000000

# remove columns that we don't use
res[, ':='(yearsSave = NULL,
           rn = NULL,
           n_eff = NULL,
           Rhat = NULL)]

# clean-up 
rm(list = c("tmp", "tmp2", "tmp3", "tmp4", "res_logmx"))

resAll = list(datSim = datSim, res = res)

# save -------------------------------------------------------------------------

saveRDS(object = resAll,
        file = here(res_path, "09_b_res_all.rds"))
