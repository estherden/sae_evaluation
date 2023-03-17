## Esther Denecke
# Read in results and calculate performance measures.

## Changelog
# 20230221: Updated to work with batchtools output

## Prerequisites: Change arguments in "arguments to (possibly) adjust". If 
#                 everything of 06-b was run in one registry only, then only
#                 one date_registry is needed and the rest of this script
#                 needs to be adjusted accordingly.


## arguments to (possibly) adjust ----------------------------------------------

# need to replace accordingly
date_data = "20230215" # date when 05-b-make_data_for_simulation_SVD was run and saved
date_registry = "20230219"# date when 06-b-_run_simulation_SVD.R was run and saved
date_registry2 = "20230224"

platform = "Linux" # platform on which 06-b was run
node = "amp033" # node on which model was compiled (NOT where 06-b was run!)

# packages ---------------------------------------------------------------------

library(data.table)
library(here)


# arguments --------------------------------------------------------------------

res_path = here("results", paste0("registrySVD_", platform, "_", 
                                   node, "_", date_registry))

res_path2 = here("results", paste0("registrySVD_", platform, "_", 
                                   node, "_", date_registry2))


# read diagnostics -------------------------------------------------------------

## res path 1
files_diag = list.files(res_path, pattern = "res_diag_expos")

path_diag = here(res_path, files_diag)
names(path_diag) = c("e1", "e2", "e3")

res_diag = mapply(readRDS, file = path_diag, SIMPLIFY = FALSE)

# bind
res_diag2 = rbindlist(res_diag)

# clean up
rm(list = c("files_diag", "path_diag", "res_diag"))

## res_path 2
files_diag = list.files(res_path2, pattern = "res_diag_expos")

path_diag = here(res_path2, files_diag)
names(path_diag) = c("e1", "e2", "e3", "e4", "e5")

res_diag = mapply(readRDS, file = path_diag, SIMPLIFY = FALSE)

# bind
res_diag3 = rbindlist(res_diag)


## bind with res_diag2
res_diag = rbindlist(l = list(res_diag2, res_diag3))

res_diag = res_diag[order(job.id), ]

# clean up
rm(list = c("files_diag", "path_diag", "res_diag2", "res_diag3"))

saveRDS(object = res_diag,
        file = here(res_path, "res_diag_all.rds"))


# read logmx -------------------------------------------------------------------

## res_path 1
files_logmx = list.files(res_path, pattern = "res_logmx_expos")

path_logmx = here(res_path, files_logmx)
names(path_logmx) = c("e1", "e2", "e3")

res_logmx = mapply(readRDS, file = path_logmx, SIMPLIFY = FALSE)

res_logmx2 = rbindlist(res_logmx)

# clean up
rm(list = c("files_logmx", "path_logmx", "res_logmx"))


## res_path 2
files_logmx = list.files(res_path2, pattern = "res_logmx_expos")

path_logmx = here(res_path2, files_logmx)
names(path_logmx) = c("e1", "e2", "e3", "e4", "e5")

res_logmx = mapply(readRDS, file = path_logmx, SIMPLIFY = FALSE)

res_logmx3 = rbindlist(res_logmx)

# clean up
rm(list = c("files_logmx", "path_logmx", "res_logmx"))

# bind both
res_logmx = rbindlist(l = list(res_logmx2, res_logmx3))

# order by job.id
res_logmx = res_logmx[order(job.id), ]

# save
saveRDS(object = res_logmx,
        file = here(res_path, "res_logmx_all.rds"))
