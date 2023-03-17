## Esther Denecke

# This script is to simulate data for age-grouped data (both input and output
#   age grouped).

## Prerequisites: Change date in "arguments to (possibly) adjust" to
#                 whenever 02-smooth_data_and_apply_parametric_model.R was run 
#                 and the output saved.


## arguments to (possibly) adjust ----------------------------------------------

date_02 = "20221201"


## seed ------------------------------------------------------------------------

## Seed is set in for loop. Same seed for beginning each time (which might
#   cause correlations between datasets (Morris et al. (2019)).


## packages --------------------------------------------------------------------

library(checkmate) # checks
library(data.table) # data wrangling
library(here) # paths


## source ----------------------------------------------------------------------

source(here("code", "00-functions.R"))
source(here("code", "00-functions_for_testing.R"))


## arguments -------------------------------------------------------------------

# file name of data created in 02
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
                    expos_size = rep(exposSizes, length(sexes)),
                    seeds = rep(list(), length(exposSizes)),
                    simData = rep(list(), length(exposSizes)),
                    smaller100 = rep(list(), length(exposSizes)))

assertDataTable(x = resAll, nrows = 2 * 8) # 2 sexes * 8 exposure sizes

# date to save the data
today = format(Sys.Date(), format = "%Y%m%d")


## read in data ----------------------------------------------------------------

dat = readRDS(here("data", "dataProcessed", filename))


## simulate data for SVD-model -------------------------------------------------

for (sx in 1:length(sexes)) {
  
  # choose subset of data
  dat2 = dat[age != 100 & 
             sex == sexes[sx] & 
             region == regionVar & 
             year %in% yearVar, 
             .(year, age, region, sex, year_region_sex, id_ab, Nx, mx)]
  
  ## from Table 2 in paper: Aggregate into age groups
  
  # (i) Calculate age specific death counts
  dat2[, Dx := mx * Nx]
  
  # (ii) Aggregate Dx and Nx by age groups (should lead to 21 * 11 * 20 = 4620 rows)
  dat_age = data.table(age = 0:99, age_group = c(0, rep(1, 4), sort(rep(seq(5, 95, by = 5), 5))))
  dat2 = merge(dat2, dat_age, by = "age")
  setorder(dat2, year, id_ab, age, sex)
  dat3 = dat2[, .(Dg = sum(Dx), Ng = sum(Nx)), by = .(year, sex, id_ab, age_group)]
  
  # (iii) Calculate the 'true' mortality rate for age grouped data
  dat3[, mg := Dg / Ng]
  
  
  # save dat3 as data_smoothed_and_Brass_applied_grouped
  files = list.files(here("data", "dataProcessed"), 
                     pattern = "03_b_data_smoothed_and_Brass_applied_grouped")
  
  if (length(files) == 0) {
    saveRDS(object = dat3, 
            file = here("data", "dataProcessed", 
                        paste0("03_b_data_smoothed_and_Brass_applied_grouped_sex_", 
                               sexes[sx], "_", today, ".rds")))
  } else {
    saveRDS(object = dat3, 
            file = here("data", "dataTest", 
                        paste0("03_b_data_smoothed_and_Brass_applied_grouped_sex_", 
                               sexes[sx], "_", today, ".rds")))
    
  }

  for (ex in 1:length(exposSizes)) {
    
    set.seed(80350) 
    # the seed is set for each exposSize leading to correlations 
    # between the datasets which can reduce monte carlo se (section 5.4 in 
    # Norris et al. (2019)
    
    # region id's
    id_ab_Brass = unique(dat2[, id_ab])
    
    # this will be used to save the results
    res = NULL
    
    for (id in 1:length(id_ab_Brass)) {
      
      for (yrs in 1:length(yearVar)) {
        
        # simulate data for the chosen exposure size, sex, id_ab, and year
        tmp = simulateData(Nx = dat3[id_ab == id_ab_Brass[id] & year == yearVar[yrs], Ng],
                           mx = dat3[id_ab == id_ab_Brass[id] & year == yearVar[yrs], mg], 
                           nsim = nsim,
                           expos_size = exposSizes[ex], 
                           two_deaths_smaller_100 = TRUE,
                           grouped = TRUE)
        
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
    resAll[sex == sexes[sx] & expos_size == exposSizes[ex], 
           ':='(simData = list(res$simData),
                seeds = list(res$randNumberStates),
                smaller100 = list(res$smaller100))]
    
    rm(res)
    
  } # for (ex in 1:length(exposSizes)) {
  
} # for (sx in 1:length(sexes)) {


# checks -----------------------------------------------------------------------

assertDataTable(resAll[sex == "male" & expos_size == 1000, simData][[1]], 
                nrows = 11 * 21 * 20 * nsim) # 11 years * 21 age groups * 20 id_ab's * nsim

assertDataTable(resAll[sex == "male" & expos_size == 1000, smaller100][[1]], 
                nrows = 11 * 20) # 11 years * 20 id_ab's

assertDataTable(resAll[sex == "male" & expos_size == 1000, seeds][[1]], 
                nrows = 11 * 20 * (nsim + 1)) # 11 years * 20 id_ab's * (nsim + 1) 
# (nsim + 1) because we save the state before every iteration and once in the end


# Save output ------------------------------------------------------------------

saveRDS(object = resAll, 
        file = here("data", "dataProcessed", 
                    paste0("03_b_data_simulated_grouped_", today, ".rds")))
