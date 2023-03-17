## Esther Denecke
## This script reads in and merges all the diagnostics for Topals and D-splines.
#   The results (estimates of logmx) will be read in and brought into a nicer
#   format in a later script. This is because the result objects are huge,
#   i.e. they need a lot of memory and it also takes some time to work with 
#   them. So it makes sense to do this work after looking at the diagnostics.

## Prerequisites: Change date in "arguments to (possibly) adjust" to
#                 whenever then registry in 06-a was created.


## arguments to (possibly) adjust ----------------------------------------------

# date registry
date_06 = "20230314"


## prep ------------------------------------------------------------------------

library(here)
library(data.table)

source(here("code", "00-functions_to_analyze_results.R"))
source(here("code", "00-functions.R"))

# diagnostics ------------------------------------------------------------------

# resDiag
files_resDiag = list.files(here("results", paste0("TopalsDsplines_", date_06)), 
                                pattern = "resDiag_p")

path_resDiag = here("results", paste0("TopalsDsplines_", date_06), files_resDiag)
names(path_resDiag) = c("e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8")

resDiag = mapply(readRDS, file = path_resDiag, SIMPLIFY = FALSE)

resDiag = rbindlist(l = resDiag)

# some wrangling, so that everything works
resD_topals = resDiag[algorithm == "topals", rbindlist(result), 
                      by = .(job.id, algorithm, sexArg, exposSizeArg, simRunArg, 
                             demKnow, groupedAges, estimator)]

resD_dspline = resDiag[algorithm == "dspline", rbindlist(result), 
                       by = .(job.id, algorithm, sexArg, exposSizeArg, simRunArg, 
                              demKnow, groupedAges, estimator)]

resD_topals[, niter := as.numeric(NA)]

resD = rbindlist(l = list(resD_dspline, resD_topals), use.names = TRUE)

# save
saveRDS(resD, here("results", paste0("TopalsDsplines_", date_06), "resDiag.rds"))
