## Esther Denecke
# This script is to look at the simulation diagnostics of TOPALS and D-splines.

## Prerequisites: Change date in "arguments to (possibly) adjust" to
#                 whenever then registry in 06-a was created.


## arguments to (possibly) adjust ----------------------------------------------

# date registry
date_06 = "20230314"

# preliminaries ----------------------------------------------------------------

options(scipen = 999) # display numbers differently

# packages
library(data.table)
library(here)
library(ggplot2)
library(cowplot)


# read diagnostics from 08-a ---------------------------------------------------

resD = readRDS(here("results", 
                    paste0("TopalsDsplines_", date_06), "resDiag.rds"))


# check for errors -------------------------------------------------------------

resD[error == TRUE, .N] # 6129 regions failed (NOT jobs as in batchtools)

# more detailed
resD[error == TRUE, .N, 
     by = .(algorithm, 
            exposSizeArg, 
            demKnow, 
            groupedAges, 
            id_ab)][order(id_ab)]
# errors only for dspline with dk2 and groupedAges.

# check also by estimator
resD[error == TRUE, .N, 
     by = .(algorithm, 
            exposSizeArg, 
            demKnow, 
            groupedAges, 
            id_ab, 
            estimator)][order(id_ab, estimator)]
# all estimators are included

resD[error == TRUE, .N, 
     by = .(algorithm, 
            exposSizeArg, 
            demKnow, 
            groupedAges, 
            id_ab, 
            estimator)][order(id_ab, estimator)][, .N, by = id_ab]

resD[error == TRUE, .N,
     by = .(algorithm, 
            exposSizeArg,
            demKnow,
            estimator)]

length(sort(unique(resD[error == TRUE, .N,
                        by = .(algorithm, 
                               exposSizeArg,
                               demKnow,
                               estimator,
                               id_ab)][, id_ab])))

resD[error == TRUE & id_ab %in% c(2, 8, 9), .N,
     by = .(algorithm, 
            exposSizeArg,
            demKnow,
            estimator)]

# check for converge and maxiter -----------------------------------------------

stopifnot(isTRUE(unique(resD[error == FALSE, converge]))) # we want TRUE

stopifnot(isFALSE(unique(resD[error == FALSE, maxiter]))) # we want FALSE
