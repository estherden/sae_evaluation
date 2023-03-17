## Esther Denecke
# This script is to simulate data (single year ages).

## Prerequisites: Change date in "arguments to (possibly) adjust" to
#                 whenever 02-smooth_data_and_apply_parametric_model.R was run 
#                 and the output saved.

## seed ------------------------------------------------------------------------

## Seed is set in for loop. Same seed for beginning each time (which might
#   cause correlations between datasets (Morris et al. (2019)).

## arguments to (possibly) adjust ----------------------------------------------

date_02 = "20221201" # date of file 02-data_smoothed_and_Brass_applied_20221201.rds

## packages --------------------------------------------------------------------

library(checkmate) # checks
library(data.table) # data wrangling
library(here) # paths


## source ----------------------------------------------------------------------

source(here("code", "00-functions.R"))
source(here("code", "00-functions_for_testing.R"))


## arguments -------------------------------------------------------------------

filename = paste0("02-data_smoothed_and_Brass_applied_", date_02, ".rds")

nsim = 1000 # number of simulated data sets per setting

regionVar = "DEUTNP"
yearMin = 1995
yearMax = 2005

sexes = c("male", "female")
exposSizes = c(1000, 5000, 10000, 25000, 50000, 75000, 100000, 1000000)

yearVar = yearMin:yearMax

# data table to save the final results
resAll = data.table(sex = sort(rep(sexes, length(exposSizes))),
                    exposSize = rep(exposSizes, length(sexes)),
                    seeds = rep(list(), length(exposSizes)),
                    simData = rep(list(), length(exposSizes)),
                    smaller100 = rep(list(), length(exposSizes)))

assertDataTable(x = resAll, nrows = 2 * 8) # 2 sexes * 8 exposure sizes

# date
today = format(Sys.Date(), format = "%Y%m%d")


## read in data ----------------------------------------------------------------

dat = readRDS(here("data", "dataProcessed", filename))


## simulate data for Topals & D-Splines ----------------------------------------

## The rationale of the code is as follows:
#   We iterate over both sexes, all exposure sizes, all regions (id_ab's), and
#   all years. For each of these combinations we simulate data using the 
#   function simulateData(). This function takes nsim as argument and therefore
#   returns the number of desired datasets per DGP.

for (sx in 1:length(sexes)) {
  
  # choose subset of dataset
  dat2 = dat[age != 100 & 
               sex == sexes[sx] & 
               region == regionVar & 
               year %in% yearVar, 
             .(year, age, region, sex, year_region_sex, id_ab, Nx, mx)]
  
  assertDataTable(x = dat2, nrows = 11 * 100 * 20) # 11 years * 100 ages * 20 regions (id_ab)
  
  for (ex in 1:length(exposSizes)) {
    
    set.seed(3475) 
    # the seed is set for each exposSize, possibly leading to correlations 
    # between the datasets which can reduce monte carlo se
    # (Section 5.4 in Morris et al., (2019))
    
    # region id's
    id_ab_Brass = unique(dat2[, id_ab])
    
    # this will be used to save the results
    res = NULL
    
    for (id in 1:length(id_ab_Brass)) {
      
      for (yrs in 1:length(yearVar)) {
        
        # simulate data for the chosen exposure size, sex, id_ab, and year
        tmp = simulateData(Nx = dat2[id_ab == id_ab_Brass[id] & year == yearVar[yrs], Nx],
                           mx = dat2[id_ab == id_ab_Brass[id] & year == yearVar[yrs], mx], 
                           nsim = nsim,
                           expos_size = exposSizes[ex], 
                           two_deaths_smaller_100 = TRUE,
                           grouped = FALSE)
        
        # add info on id_ab, year and exposure size
        tmp$simData[, ':='(id_ab = id_ab_Brass[id], 
                           year = yearVar[yrs], 
                           expos_size = exposSizes[ex])]
        
        tmp$smaller100 = data.table(smaller100 = tmp$smaller100, 
                                    id_ab = id_ab_Brass[id],
                                    year = yearVar[yrs],
                                    expos_size = exposSizes[ex])
        
        tmp$randNumberStates[, ':='(id_ab = id_ab_Brass[id], 
                                    year = yearVar[yrs], 
                                    expos_size = exposSizes[ex])]
        
        # depending on whether this is the first iteration create the result
        # list or bind list entries with newly simulated data.
        if (is.null(res)) {
          res = tmp
          rm(tmp)
        } else {
          res$simData = rbindlist(l = list(res$simData, tmp$simData))
          res$smaller100 = rbindlist(l = list(res$smaller100, tmp$smaller100))
          res$randNumberStates = rbindlist(l = list(res$randNumberStates, tmp$randNumberStates))
          
          rm(tmp)
        } # end of      if (is.null(res)) {
        
      } # end of     for (yrs in 1:length(years)) {
      
    } # end of   for (id in 1:length(id_ab_Brass)) {
    
    # save simulated data in this format of lists within data.table entries
    resAll[sex == sexes[sx] & exposSize == exposSizes[ex], 
           ':='(simData = list(res$simData),
                seeds = list(res$randNumberStates),
                smaller100 = list(res$smaller100))]
    
    rm(res)
  
  } # for (ex in 1:length(exposSizes)) {
  
} # for (sx in 1:length(sexes)) {


# checks -----------------------------------------------------------------------

# These checks are not exhaustive

assertDataTable(resAll[sex == "male" & exposSize == 1000, simData][[1]], 
                nrows = 11 * 100 * 20 * nsim) # 11 years * 100 ages * 20 id_ab's * nsim

assertDataTable(resAll[sex == "male" & exposSize == 1000, smaller100][[1]], 
                nrows = 11 * 20) # 11 years * 20 id_ab's

assertDataTable(resAll[sex == "male" & exposSize == 1000, seeds][[1]], 
                nrows = 11 * 20 * (nsim + 1)) # 11 years * 20 id_ab's * (nsim + 1) 
# (nsim + 1) because we save the state before every iteration and once in the end


# Save output ------------------------------------------------------------------

saveRDS(object = resAll, 
        file = here("data", "dataProcessed", 
                    paste0("03_a_data_simulated_", today, ".rds")))
