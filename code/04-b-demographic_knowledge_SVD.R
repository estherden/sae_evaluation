## Esther Denecke
# This script is to create the underlying demographic knowledge for 
#   the SVD-model.


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
date_03_Brass = "20221211"


## packages --------------------------------------------------------------------

library(cowplot)
library(data.table)
library(here)
library(ggplot2)


## source ----------------------------------------------------------------------

source(here("code", "00-functions.R"))


## read in data ----------------------------------------------------------------

dat_grouped = readRDS(here("data",
                           "dataProcessed", 
                           paste0("03_b_data_smoothed_and_Brass_applied_grouped_", 
                                  date_03_Brass, ".rds")))

dat_single = readRDS(here("data", 
                          "dataProcessed", 
                          paste0("02-data_smoothed_and_Brass_applied_",
                                 date_02, ".rds")))

# HMD data -> not smoothed
datHMD = readRDS(here("data", 
                      "dataProcessed", 
                      paste0("01_HMD_data_1x1_prepared_", date_01, ".rds")))

datHMD1x10 = readRDS(here("data", 
                          "dataProcessed", 
                          paste0("01_HMD_data_1x10_prepared_", date_01, ".rds")))

# e0
path_e0 = here("data", "HMD", "e0_per", "E0per")
e0 = readBindHmdZips(path_e0, type = "e0")


# to save results --------------------------------------------------------------

dkSVD = list(male = list(),
             female = list())

## reformatting ----------------------------------------------------------------

# extract true data
datTrue_single = unique(dat_single[, .(year, age, region, sex, year_region_sex, 
                         Nx, Dx_fit1D, mx_fit1D)])

datTrue_single = datTrue_single[age != 100, .(year, region, sex, year_region_sex, 
                                              age, Dx_fit1D, Nx)]
datTrue_single[, truth := TRUE]
datTrue_single[, id_ab := 100]
dat_age = data.table(age = 0:99, 
                     age_group = c(0, rep(1, 4), sort(rep(seq(5, 95, by = 5), 5))))
datTrue_single = merge(datTrue_single, dat_age, by = "age")

# remove duplicates

datTrue_grouped = datTrue_single[, .(Dg = sum(Dx_fit1D), 
                                     Ng = sum(Nx)), 
                                 by = .(year, region, sex, age_group)]
datTrue_grouped[, mg := Dg / Ng]
datTrue_grouped[, logmx := log(mg)] # naming not great but used to logmx
setorder(datTrue_grouped, sex, year, age_group)

datTrue_grouped_male = datTrue_grouped[sex == "male", ]
datTrue_grouped_female = datTrue_grouped[sex == "female", ]

datTrue_grouped_male_wide = dcast(data = datTrue_grouped_male, 
                                  formula = year ~ age_group, 
                                  value.var = "logmx")
datTrue_grouped_male_wide[, year := NULL]

ggplot(data = datTrue_grouped_male, 
       aes(x = age_group, y = logmx, group = year, col = as.factor(year))) + 
  geom_line()

datTrue_grouped_female_wide = dcast(data = datTrue_grouped_female, 
                                  formula = year ~ age_group, 
                                  value.var = "logmx")
datTrue_grouped_female_wide[, year := NULL]

ggplot(data = datTrue_grouped_female, 
       aes(x = age_group, y = logmx, group = year, col = as.factor(year))) + 
  geom_line()


# demKnow 1 --------------------------------------------------------------------
# from German national data 1995 - 2005 

svd_male = svd(datTrue_grouped_male_wide)$v[, 1:3]
plot(svd_male[, 1], type = "b")
plot(-svd_male[, 1], type = "b")
plot(svd_male[, 2], type = "b")
plot(svd_male[, 3], type = "b")


svd_female = svd(datTrue_grouped_female_wide)$v[, 1:3]

plot(svd_female[, 1], type = "b")
plot(-svd_female[, 1], type = "b")
plot(svd_female[, 2], type = "b")
plot(svd_female[, 3], type = "b")

dkSVD$male$dk1 = svd_male
dkSVD$female$dk1 = svd_female

# plot dk1 for supplementary material
dk1_long = melt(as.data.table(dkSVD$male$dk1), 
                measure.vars = c("V1", "V2", "V3"))
dk1_long[variable == "V1", value := -1 * value]
dk1_long[variable == "V1", principal_component := "pc1"]
dk1_long[variable == "V2", principal_component := "pc2"]
dk1_long[variable == "V3", principal_component := "pc3"]
dk1_long[, age_group := rep(c(0, 1, seq(5, 95, by = 5)), 3)]

p_dk1 = ggplot(data = dk1_long, aes(x = age_group, y = value)) +
  geom_line() +
  facet_grid(principal_component ~ ., scales = "free_y") +
  theme_bw() +
  ggtitle("(a) dk1") +
  xlab("age group")

# demKnow 2 --------------------------------------------------------------------
# from all HMD countries 1970:2018

datHMD_we_male = datHMD[!(region %in% c("DEUTNP", "DEUTW", "DEUTE")) & 
                              year %in% 1970:2018 & 
                              sex == "male" & 
                              age < 100, ]
datHMD_we_female = datHMD[!(region %in% c("DEUTNP", "DEUTW", "DEUTE")) & 
                                year %in% 1970:2018 & 
                                sex == "female" &
                                age < 100, ]

datHMD_we_male = merge(datHMD_we_male, dat_age, by = "age")
datHMD_we_female = merge(datHMD_we_female, dat_age, by = "age")

datHMD_we_male = datHMD_we_male[, .(Dg = sum(Dx), Ng = sum(Nx)), 
                                        by = .(age_group, year)]
datHMD_we_female = datHMD_we_female[, .(Dg = sum(Dx), Ng = sum(Nx)), 
                                            by = .(age_group, year)]

datHMD_we_male[, logmx := log(Dg / Ng)]
datHMD_we_female[, logmx := log(Dg / Ng)]

datHMD_we_male_wide = dcast(data = datHMD_we_male, 
                                year ~ age_group, 
                                value.var = "logmx")
datHMD_we_female_wide = dcast(data = datHMD_we_female, 
                                  year ~ age_group, 
                                  value.var = "logmx")

datHMD_we_male_wide[, year := NULL]
datHMD_we_female_wide[, year := NULL]

svd_datHMD_we_male = svd(datHMD_we_male_wide)$v[, 1:3]
plot(svd_datHMD_we_male[, 1], type = "b")
plot(-svd_datHMD_we_male[, 1], type = "b")
plot(svd_datHMD_we_male[, 2], type = "b")
plot(svd_datHMD_we_male[, 3], type = "b")


svd_datHMD_we_female = svd(datHMD_we_female_wide)$v[, 1:3]
plot(svd_datHMD_we_female[, 1], type = "b")
plot(-svd_datHMD_we_female[, 1], type = "b")
plot(svd_datHMD_we_female[, 2], type = "b")
plot(svd_datHMD_we_female[, 3], type = "b")


dkSVD$male$dk2 = svd_datHMD_we_male
dkSVD$female$dk2 = svd_datHMD_we_female


# plot dk2 for supplementary material
dk2_long = melt(as.data.table(dkSVD$male$dk2), 
                measure.vars = c("V1", "V2", "V3"))
dk2_long[variable == "V1", value := -1 * value]
dk2_long[variable == "V1", principal_component := "pc1"]
dk2_long[variable == "V2", principal_component := "pc2"]
dk2_long[variable == "V3", principal_component := "pc3"]
dk2_long[, age_group := rep(c(0, 1, seq(5, 95, by = 5)), 3)]

p_dk2 = ggplot(data = dk2_long, aes(x = age_group, y = value)) +
  geom_line() +
  facet_grid(principal_component ~ ., scales = "free_y") +
  theme_bw() +
  ggtitle("(b) dk2") +
  xlab("age group")



## demKnow 3 -------------------------------------------------------------------

# other way around

dkSVD$male$dk3 = dkSVD$female$dk1
dkSVD$female$dk3 = dkSVD$male$dk1

# plot dk2 for supplementary material
dk3_long = melt(as.data.table(dkSVD$male$dk3), 
                measure.vars = c("V1", "V2", "V3"))
dk3_long[variable == "V1", value := -1 * value]
dk3_long[variable == "V1", principal_component := "pc1"]
dk3_long[variable == "V2", principal_component := "pc2"]
dk3_long[variable == "V3", principal_component := "pc3"]
dk3_long[, age_group := rep(c(0, 1, seq(5, 95, by = 5)), 3)]

p_dk3 = ggplot(data = dk3_long, aes(x = age_group, y = value)) +
  geom_line() +
  facet_grid(principal_component ~ ., scales = "free_y") +
  theme_bw() +
  ggtitle("(c) dk3") +
  xlab("age group")


## plot all together -----------------------------------------------------------

pdf(here("figures", "sm_plot_pcs_SVD_model.pdf"),
    width = 9, height = 6)
plot_grid(p_dk1, p_dk2, p_dk3, ncol = 3, 
          align = "h")
dev.off()

## save everything -------------------------------------------------------------
saveRDS(object = dkSVD, file = here("data", "dataProcessed", "04_dkSVD.rds"))
