## Esther Denecke
# This script is to create the underlying demographic knowledge for
# TOPALS and D-splines as described in the main paper.
# Contains code for dk for both male and female schedules. 


## Prerequisites: 1. Change dates in "arguments to (possibly) adjust" to
#                    whenever scripts with 01 and 02 were run 
#                    and the output saved.
#                 2. HMD data: From working directory there should a path
#                    data/HMD/e0_per/E0per
#                    This directory should contain textfiles with data for 
#                    each country in the HMD. These will be read-in in the 
#                    course of executing this script.


## arguments to (possibly) adjust ----------------------------------------------

date_01 = "20221122"
date_02 = "20221201"

## packages --------------------------------------------------------------------

library(checkmate) # checks
library(data.table) # data wrangling
library(ggplot2) # plotting
library(here) # paths


## source ----------------------------------------------------------------------

source(here("code", "00-functions.R"))


## prep ------------------------------------------------------------------------

# date
today = format(Sys.Date(), format = "%Y%m%d")


## read in data ----------------------------------------------------------------

dat = readRDS(here("data", "dataProcessed", 
                   paste0("02-data_smoothed_and_Brass_applied_", date_02, ".rds")))

# HMD data -> not smoothed
datHMD = readRDS(here("data", "dataProcessed", 
                      paste0("01_HMD_data_1x1_prepared_", date_01, ".rds")))
datHMD1x10 = readRDS(here("data", "dataProcessed", 
                          paste0("01_HMD_data_1x10_prepared_", date_01, ".rds")))
                          
# e0
path_e0 = here("data", "HMD", "e0_per", "E0per")
e0 = readBindHmdZips(path_e0, type = "e0")


## reformatting ----------------------------------------------------------------

## extract true data, i.e. the smoothed GERMAN data (DEUTNP) and NOT the
#     20 subpopulations/regions
datTrue = unique(dat[, .(year, 
                         age, 
                         region, 
                         sex, 
                         year_region_sex, 
                         Nx, 
                         Dx_fit1D, 
                         mx_fit1D)])

assertDataTable(datTrue, nrows = 11 * 101 * 2) # 11 years * 101 ages * 2 sexes

## Do some reformatting like removing age 100 
#   that we do not use. Also add a column logmx, remove mx_fit1D which we
#   don't need anymore and add two indicators for the truth: column truth = TRUE
#   and id_ab = 100 (artifical regions are 1:20).
datTrue = datTrue[, .(year, region, sex, year_region_sex, age, mx_fit1D)]
datTrue[, logmx := log(mx_fit1D)] # some NaN because of age 100 
datTrue[, mx_fit1D := NULL]
datTrue[, truth := TRUE]
datTrue[, id_ab := 100]


## Do the same thing for the artificial regions, now called datBrass.
#   Would have been a bit nicer to have column names indicating that these
#   are produced via Brass model, e.g. column name mx_brass instead of mx.
datBrass = dat[, .(year, region, sex, year_region_sex, age, mx, id_ab)]
datBrass[, logmx := log(mx)]
datBrass[, truth := FALSE]
datBrass[, mx := NULL]

assertDataTable(datBrass, nrows = nrow(dat)) 


# stack datTrue and datBrass
datAll = rbindlist(l = list(datTrue, datBrass), use.names = TRUE)

assertDataTable(datAll, nrows = 44440 + 2222)

# add unique identifier
datAll[, year_region_sex_truth_idab := paste0(year_region_sex, "_", 
                                              truth, "_", 
                                              id_ab)]


# plot true data ---------------------------------------------------------------

ggplot(data = datTrue[age != 100 & sex == "male", ],
       aes(x = age, y = logmx, col = as.factor(year), group = as.factor(year))) +
  geom_line()

ggplot(data = datTrue[age != 100 & sex == "female", ],
       aes(x = age, y = logmx, col = as.factor(year), group = as.factor(year))) +
  geom_line()

# to save demographic knowledge ------------------------------------------------

## Male is for underlying male data and does NOT indicate from which sex the
#   demographic knowledge is built!

dkTopals = list(female = list(),
                male = list())

dkDspline = list(D1 = list(female = list(),
                           male = list()),
                 D2 = list(female = list(),
                           male = list()),
                 DLC = list(female = list(),
                            male = list()))


# make demographic knowledge TOPALS --------------------------------------------

## dk1 ----------------------------------------

# 1. The national schedule (smoothed). We'll use the year 2000 here as this is 
#       the year shown in the paper.

yearChoose = 2000

# males
dkTopals[["male"]]$dk1 = datAll[age != 100 & 
                                  sex == "male" & 
                                  year == yearChoose & 
                                  truth == TRUE, logmx]

# females
dkTopals[["female"]]$dk1 = datAll[age != 100 &
                                    sex == "female" & 
                                    year == yearChoose & 
                                    truth == TRUE, logmx]


# visualize
ggplot(data = datAll[age != 100 & 
                       sex == "male" & 
                       year == yearChoose, ],
       aes(x = age, 
           y = logmx, 
           col = truth, 
           group = year_region_sex_truth_idab)) +
  geom_line() +
  ylim(c(-11.5, 0)) +
  ylab("log mortality rate") +
  scale_color_manual(name = "demographic knowledge",
                     values = c("gray", "black")) +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(filename = here("figures", "sm_dk1_TOPALS_male.pdf"),
       width = 7, height = 5)


ggplot(data = datAll[age != 100 & 
                       sex == "female" & 
                       year == yearChoose, ],
       aes(x = age, 
           y = logmx, 
           col = truth, 
           group = year_region_sex_truth_idab)) +
  geom_line() +
  ylim(c(-11.5, 0)) +
  ylab("log mortality rate") +
  scale_color_manual(name = "demographic knowledge",
                     values = c("gray", "black")) +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(filename = here("figures", "sm_dk1_TOPALS_female.pdf"),
       width = 7, height = 5)

# clean-up
rm("yearChoose")


## dk2 ----------------------------------------

# 2. Smoothed mortality schedules from the same sex but different country with
#       similar life expectancy

yearChoose = 2000

## males

e0[year == yearChoose, .(year, male, region)][order(male)]
# use FRATNP as fairly large country

countryChoose = "FRATNP"

# choose subset of data
dat_male = datHMD[region == countryChoose & 
                    year == yearChoose & 
                    sex == "male", ]

# smooth (includes age 100 as in 02-smooth_data_and_apply_parametric_model.R)
dat_male[, Dx_fit1D := smoothDeaths(D = Dx, 
                                    N = Nx, 
                                    ages = age, 
                                    includeAgeZero = FALSE), 
              by = .(sex, region, year)]

# create column logmx
dat_male[, logmx := log(Dx_fit1D / Nx)]

# choose specific columns only & remove age 100
dat_male = dat_male[age != 100, .(year, age, region, year_region_sex, logmx)]

# save
dkTopals[["male"]]$dk2 = dat_male[, logmx]


## female

e0[year == yearChoose, .(year, female, region)][order(female)]
# CAN looks pretty close but for reasons of consistency we'll use FRATNP 
# as well.

# choose subset of data
dat_female = datHMD[region == "FRATNP" & 
                      year == yearChoose & 
                      sex == "female", ]

# smooth
dat_female[, Dx_fit1D := smoothDeaths(D = Dx, 
                                      N = Nx, 
                                      ages = age, 
                                      includeAgeZero = FALSE), 
                by = .(sex, region, year)]

# create column logmx
dat_female[, logmx := log(Dx_fit1D / Nx)]

# choose specific columns only and remove age 100
dat_female = dat_female[age != 100, .(year, age, region, year_region_sex, logmx)]

# save 
dkTopals[["female"]]$dk2 = dat_female[, logmx]


## visualize

## males

# mock columns for plotting
dat_male[, ':='(year_region_sex_truth_idab = paste0(year_region_sex, "_FALSE_200"),
                truth = TRUE)] 
# not really the truth

ggplot(data = datAll[age != 100 & 
                       sex == "male" & 
                       year == yearChoose &
                       truth == FALSE, ],
       aes(x = age, 
           y = logmx, 
           col = truth,
           group = year_region_sex_truth_idab)) +
  geom_line() +
  geom_line(data = dat_male[age != 100, ], 
            aes(x = age, y = logmx, col = truth)) +
  ylim(c(-11.5, 0)) +
  ylab("log mortality rate") +
  scale_color_manual(name = "demographic knowledge",
                     values = c("gray", "black")) +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(filename = here("figures", "sm_dk2_TOPALS_male.pdf"),
       width = 7, height = 5)



## females

# mock columns for plotting
dat_female[, ':='(year_region_sex_truth_idab = paste0(year_region_sex, "_FALSE_200"),
                  truth = TRUE)] 
# not really the truth. 

ggplot(data = datAll[age != 100 & 
                       sex == "female" & 
                       year == yearChoose &
                       truth == FALSE, ],
       aes(x = age, 
           y = logmx, 
           col = truth,
           group = year_region_sex_truth_idab)) +
  geom_line() +
  geom_line(data = dat_female[age != 100, ], 
            aes(x = age, y = logmx, col = truth)) +
  ylim(c(-11.5, 0)) +
  ylab("log mortality rate") +
  scale_color_manual(name = "demographic knowledge",
                     values = c("gray", "black")) +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(filename = here("figures", "sm_dk2_TOPALS_female.pdf"),
       width = 7, height = 5)


# clean-up
rm(list = c("dat_male", "dat_female", "yearChoose", "countryChoose"))


## dk3 ----------------------------------------


## male
e0[year == 2000 & region == "DEUTNP", ] # male e0 = 74.86
e0[female < 74.8 & female > 74.4, .(year, female, region)][order(female)]

## pretty random choice of FRATNP in the year 1965. Again: Rather large country.
#    Also AUS in year 1969 totally possible and probably many others.
yearChoose = 1965
countryChoose = "FRATNP"

# choose subset of data
dattmp_fem = datHMD[region == countryChoose & 
                      year == yearChoose & 
                      sex == "female", ]

# smooth
dattmp_fem[, Dx_fit1D := smoothDeaths(D = Dx, 
                                      N = Nx, 
                                      ages = age, 
                                      includeAgeZero = FALSE), 
           by = .(sex, region, year)]

# add column logmx
dattmp_fem[, logmx := log(Dx_fit1D / Nx)]

# choose specific columns only and remove age 100
dattmp_fem = dattmp_fem[age != 100, .(year, age, region, year_region_sex, logmx)]

# save
dkTopals[["male"]]$dk3 = dattmp_fem[, logmx]



## females

## male
e0[year == 2000 & region == "DEUTNP", ] # female e0 = 80.99
e0[region == "AUS", ]

## pretty random choice of FRATNP in the year 1965. Again: Rather large country.
#    Also AUS in year 1969 totally possible and probably many others.
yearChoose = 2017
countryChoose = "AUS"

# choose subset of data
dattmp_male = datHMD[region == countryChoose & 
                     year == yearChoose & 
                     sex == "male", ]

# smooth
dattmp_male[, Dx_fit1D := smoothDeaths(D = Dx, 
                                       N = Nx, 
                                       ages = age, 
                                       includeAgeZero = FALSE), 
            by = .(sex, region, year)]

# add column logmx
dattmp_male[, logmx := log(Dx_fit1D / Nx)]

# choose specific columns only and remove age 100
dattmp_male = dattmp_male[age != 100, .(year, age, region, year_region_sex, logmx)]

# save
dkTopals[["female"]]$dk3 = dattmp_male[, logmx]



## visualize

## males

# mock columns for plotting
dattmp_fem[, ':='(year_region_sex_truth_idab = paste0(year_region_sex, "_FALSE_200"),
                  truth = TRUE)] 

ggplot(data = datAll[age != 100 & 
                       sex == "male" & 
                       year == 2000 &
                       truth == FALSE, ],
       aes(x = age, 
           y = logmx, 
           col = truth,
           group = year_region_sex_truth_idab)) +
  geom_line() +
  geom_line(data = dattmp_fem[age != 100, ], 
            aes(x = age, y = logmx, col = truth)) +
  ylim(c(-11.5, 0)) +
  ylab("log mortality rate") +
  scale_color_manual(name = "demographic knowledge",
                     values = c("gray", "black")) +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(filename = here("figures", "sm_dk3_TOPALS_male.pdf"),
       width = 7, height = 5)


## females

# mock columns for plotting
dattmp_male[, ':='(year_region_sex_truth_idab = paste0(year_region_sex, "_FALSE_200"),
                   truth = TRUE)] 

ggplot(data = datAll[age != 100 & 
                       sex == "female" & 
                       year == 2000 &
                       truth == FALSE, ],
       aes(x = age, 
           y = logmx, 
           col = truth,
           group = year_region_sex_truth_idab)) +
  geom_line() +
  geom_line(data = dattmp_male[age != 100, ], 
            aes(x = age, y = logmx, col = truth)) +
  ylim(c(-11.5, 0)) +
  ylab("log mortality rate") +
  scale_color_manual(name = "demographic knowledge",
                     values = c("gray", "black")) +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(filename = here("figures", "sm_dk3_TOPALS_female.pdf"),
       width = 7, height = 5)





# Save output TOPALS -----------------------------------------------------------

saveRDS(object = dkTopals,
        file = here("data", "dataProcessed", 
                    paste0("04_a_dkTopals_", today, ".rds")))

rm(list = c("dattmp_fem", "dattmp_male", "dkTopals", "countryChoose"))


# make demographic knowledge D-splines -----------------------------------------

## dk1, D-splines ------------------------------------

## 1. demographic knowledge similar to Schmertmann2021DR: remove year_min < 1970. 
#       but also year_max > 2019 also remove DEUTNP, DEUTE, and DEUTW as 
#       these are (subsets) of how data were simulated.

## males

inputCalcDspline = calcDsplineHelper(data = datHMD1x10, 
                                     sex_choose = "male", 
                                     max_age = 99, 
                                     min_year = "1970", 
                                     max_year = "2019", 
                                     exclude_regions = c("DEUTNP", "DEUTW", "DEUTE"))

# estimator D1 and dk1
dkDspline[["D1"]][["male"]]$dk1 = calcDsplineConstants(logmx = inputCalcDspline, 
                                                       method = "D1", 
                                                       smooth = TRUE)

# estimator D2 and dk1
dkDspline[["D2"]][["male"]]$dk1 = calcDsplineConstants(logmx = inputCalcDspline, 
                                                       method = "D2", 
                                                       smooth = TRUE)

# estimator DLC and dk1
dkDspline[["DLC"]][["male"]]$dk1 = calcDsplineConstants(logmx = inputCalcDspline, 
                                                        method = "DLC", 
                                                        smooth = TRUE)

# clean-up
rm("inputCalcDspline")


## females

inputCalcDspline = calcDsplineHelper(data = datHMD1x10, 
                                     sex_choose = "female", 
                                     max_age = 99, 
                                     min_year = "1970", 
                                     max_year = "2019", 
                                     exclude_regions = c("DEUTNP", "DEUTW", "DEUTE"))


# estimator D1, dk1, female
dkDspline[["D1"]][["female"]]$dk1 = calcDsplineConstants(logmx = inputCalcDspline, 
                                                         method = "D1", 
                                                         smooth = TRUE)

# estimator D2, dk1, female
dkDspline[["D2"]][["female"]]$dk1 = calcDsplineConstants(logmx = inputCalcDspline, 
                                                         method = "D2", 
                                                         smooth = TRUE)

# estimator DLC, dk1, female
dkDspline[["DLC"]][["female"]]$dk1 = calcDsplineConstants(logmx = inputCalcDspline, 
                                                          method = "DLC", 
                                                          smooth = TRUE)


# clean-up
rm(list = c("inputCalcDspline"))

## dk2, D-splines ------------------------------------------

# 2. historic data only: Before 1970

## males

inputCalcDspline = calcDsplineHelper(data = datHMD1x10, 
                                     sex_choose = "male", 
                                     max_age = 99, 
                                     min_year = NULL, 
                                     max_year = "1969", 
                                     exclude_regions = c("DEUTNP", "DEUTW", "DEUTE"))


dkDspline[["D1"]][["male"]]$dk2 = calcDsplineConstants(logmx = inputCalcDspline, 
                                                       method = "D1", 
                                                       smooth = TRUE)

dkDspline[["D2"]][["male"]]$dk2 = calcDsplineConstants(logmx = inputCalcDspline, 
                                                       method = "D2", 
                                                       smooth = TRUE)

dkDspline[["DLC"]][["male"]]$dk2 = calcDsplineConstants(logmx = inputCalcDspline, 
                                                        method = "DLC", 
                                                        smooth = TRUE)

# clean-up
rm("inputCalcDspline")


## females

inputCalcDspline = calcDsplineHelper(data = datHMD1x10, 
                                     sex_choose = "female", 
                                     max_age = 99, 
                                     min_year = NULL, 
                                     max_year = "1969", 
                                     exclude_regions = c("DEUTNP", "DEUTW", "DEUTE"))

dkDspline[["D1"]][["female"]]$dk2 = calcDsplineConstants(logmx = inputCalcDspline, 
                                                         method = "D1", 
                                                         smooth = TRUE)

dkDspline[["D2"]][["female"]]$dk2 = calcDsplineConstants(logmx = inputCalcDspline, 
                                                         method = "D2", 
                                                         smooth = TRUE)

dkDspline[["DLC"]][["female"]]$dk2 = calcDsplineConstants(logmx = inputCalcDspline, 
                                                          method = "DLC", 
                                                          smooth = TRUE)

# clean-up
rm("inputCalcDspline")


## dk3, D-splines ------------------------------------------

## Wrong sex: Use dk1 but the wrong sex.

# males
dkDspline[["D1"]][["male"]]$dk3 = dkDspline[["D1"]][["female"]]$dk1
dkDspline[["D2"]][["male"]]$dk3 = dkDspline[["D2"]][["female"]]$dk1
dkDspline[["DLC"]][["male"]]$dk3 = dkDspline[["DLC"]][["female"]]$dk1

# females
dkDspline[["D1"]][["female"]]$dk3 = dkDspline[["D1"]][["male"]]$dk1
dkDspline[["D2"]][["female"]]$dk3 = dkDspline[["D2"]][["male"]]$dk1
dkDspline[["DLC"]][["female"]]$dk3 = dkDspline[["DLC"]][["male"]]$dk1


# save demographic knowledge D-splines -----------------------------------------

saveRDS(object = dkDspline, 
        file = here("data", "dataProcessed", 
                    paste0("04_a_dkDspline_", today, ".rds")))
