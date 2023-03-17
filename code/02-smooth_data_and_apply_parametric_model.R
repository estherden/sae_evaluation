## Esther Denecke
# Smooth HMD data and apply parametric model.

## Prerequisites: Change date in "arguments to (possibly) adjust" to
#                 whenever 01-prepare_HMD_data.R was run and the output saved.


## Some comments on the use of life table functions. I am using functions from 
#   different sources:
#   - LifeTable::LT(), LifeTable::
#   - calc_e0 (renamed to calc_e0()) from Rau and Schmertmann (2020)
## Some background on the reasoning: We use the LifeTable package to calculate
#   lx and mx providing our own ax (with 0.1 for first age group and 0.5 else 
#   if n = 1, as in Rau and Schmertmann (2020)). 
#   We use another function for calculating life expectancy (adapted from 
#   Rau and Schmertmann (2020).
#   Package LifeTable also provides some functions for 
#   converting life table columns to other columns, e.g. LifeTable::lx2qx().
#   We write our own functions when the one we need is missing - using the same
#   naming style as LifeTable.


## arguments to (possibly) adjust ----------------------------------------------

date_01 = "20221122"

## seed ------------------------------------------------------------------------

set.seed(957303)

## packages --------------------------------------------------------------------

library(checkmate) # code checks
library(data.table) # data wrangling
library(ggplot2) # plotting
library(here) # paths
library(MortalitySmooth) # smoothing schedules
library(LifeTable) # LifeTable calculations (but not ALL!)
library(xtable) # to make LaTeX tables

## source ----------------------------------------------------------------------

source(here("code", "00-functions.R"))
source(here("code", "00-functions_for_testing.R"))

## read in data ----------------------------------------------------------------

datHMD = readRDS(here("data", "dataProcessed", 
                      paste0("01_HMD_data_1x1_prepared_", date_01 , ".rds")))

## arguments -------------------------------------------------------------------

countries = "DEUTNP" # HMD country code
years = 1995:2005 # years
ax = c(0.1, rep(0.5, 100)) # ax for life table calculations (only for LifeTable::LT)
nreg = 20 # number of subpopulations to create

# min & max values for Brass model parameters
# source: uniform distribution & ranges from Alexander et al. (2017)
a_min = -0.75
a_max = 0.75
b_min = 0.7
b_max = 1.3

# date
today = format(Sys.Date(), format = "%Y%m%d")

## smooth ----------------------------------------------------------------------

dat2 = datHMD[region %in% countries & year %in% years, ] 

assertDataTable(x = dat2, nrows = 101 * 11 * 2, any.missing = FALSE) # 101 ages, 11 years, 2 sexes

# any 0's in Nx?
assertDataTable(x = dat2[age > 98 & Nx == 0, ], nrows = 0)

# smooth by sex, year, region
dat2[, Dx_fit1D := smoothDeaths(D = Dx, N = Nx, ages = age, includeAgeZero = FALSE), 
     by = .(sex, region, year)]

dat2[, logmx_Dx_fit1D := log(Dx_fit1D / Nx)]

# look at schedules
ggplot(data = dat2[sex == "male", ],
       aes(x = age, y = logmx_Dx_fit1D, 
           col = as.factor(year), 
           group = as.factor(year))) +
  geom_line() +
  facet_wrap(region ~ ., ncol = 2) +
  theme_bw()

ggplot(data = dat2,
       aes(x = age, y = logmx_Dx_fit1D, 
           col = as.factor(sex), 
           group = as.factor(sex))) +
  geom_line() +
  facet_wrap(year ~ .) +
  theme_bw()

## Brass model -----------------------------------------------------------------

# get vectors a and b
# source: uniform distribution & ranges from Alexander et al. (2017)
ab_brass = data.table(a = runif(n = nreg, min = a_min, max = a_max) , 
                      b = runif(n = nreg, min = b_min, max = b_max),
                      id_ab = 1:nreg)

## save Brass parameters -------------------------------------------------------

saveRDS(ab_brass, here("data", "dataProcessed", paste0("02_ab_brass_", today, ".rds")))


## apply Brass model -----------------------------------------------------------

# calculate lx column for each combination of year_region_sex
dat2[, ':='(lx_fit1D = LifeTable::LT(Nx = Nx,
                                     Dx = Dx_fit1D,
                                     ages = age,
                                     axmethod = ax,
                                     mxsmooth = FALSE,
                                     axsmooth = FALSE,
                                     radix = 1)$lx,
            mx_fit1D = LifeTable::LT(Nx = Nx,
                                     Dx = Dx_fit1D,
                                     ages = age,
                                     axmethod = ax,
                                     mxsmooth = FALSE,
                                     axsmooth = FALSE,
                                     radix = 1)$mx),
     by = year_region_sex]


# create combinations of a, b, year_region_sex and id_ab
tmp = unique(dat2$year_region_sex) # each of these needs each pair of a and b
tmp2 = as.data.table(expand.grid(a = ab_brass[, a], year_region_sex = tmp))

assertDataTable(x = tmp2, nrows = 20 * 22) # 20 brass parameters * combinations of year_region_sex

tmp3 = merge(tmp2, ab_brass, by = "a")

assertDataTable(x = tmp3, nrows = 440) # 440 rows as tmp3

dat3 = merge(tmp3, 
             dat2[, .(year_region_sex, year, age, region, sex,
                      Nx, Dx_fit1D, mx_fit1D, lx_fit1D)], 
                      by = "year_region_sex", allow.cartesian = TRUE)

assertDataTable(x = dat3, nrows = 20 * 2222) # 20 Brass parameters * 2222 nrows(dat3)

# reorder columns
dat3 = dat3[, .(year, age, region, sex, year_region_sex, 
                a, b, id_ab, Nx, Dx_fit1D, mx_fit1D, lx_fit1D)]

# reorder rows
setorder(dat3, year_region_sex, id_ab)

# some sanity checks
assertNumeric(x = unique(dat3[, mx_fit1D]), len = 2222) # 11 years * 2 sexes * 101 ages
# This shows that the same mortality rates are imposed on each region using this 
#     setup.

assertNumeric(x = unique(dat3[, lx_fit1D]), len = 2201) 
# (11 years * 2 sexes * 101 ages) - 21 sex_year combinations at age 0 where lx = 1 by definition


# Brass model
dat3[, Yx := calcNewYxBrass(a = a, b = b, lxOrig = lx_fit1D)]
dat3[, lx := Yx2lx(Yx = Yx)]
dat3[, qx := LifeTable::lx2qx(lx = lx)]
dat3[, mx := qx2mx(qx = qx, ax = ax, n = 1)]

# all regions by year
ggplot(data = dat3[sex == "male" & age != 100, ],
       aes(x = age, y = log(mx), col = as.factor(id_ab), group = as.factor(id_ab))) +
  geom_line() +
  facet_wrap(sex ~ year) +
  theme_bw()

# all years by region
ggplot(data = dat3[sex == "male" & age != 100, ], 
       aes(x = age, y = log(mx), col = as.factor(year))) +
  geom_line() + 
  facet_wrap(sex ~ id_ab) +
  theme_bw()


# save data for regions --------------------------------------------------------

saveRDS(dat3, here("data", "dataProcessed", 
                   paste0("02-data_smoothed_and_Brass_applied_", today, ".rds")))

# clean-up
rm(list = c("dat2", "datHMD", "tmp2", "tmp3", "a_max", "a_min", 
            "b_max", "b_min", "countries", "files", "tmp"))


# calculate e0 -----------------------------------------------------------------

e0_cutoff = dat3[age != 100 & sex == "male",
                 .(e0_cut = calc_e0(logmx = log(mx), n = 1)),
                 by = .(year, id_ab)]

assertDataTable(x = e0_cutoff, nrows = 11 * 20, any.missing = FALSE) # 11 years * 20 subpopulations


## save e0 ---------------------------------------------------------------------

saveRDS(e0_cutoff, here("data", "dataProcessed", 
                        paste0("02-e0_of_data_smoothed_and_Brass_applied_", today, ".rds")))


## material for supplementary material -----------------------------------------

print(xtable(ab_brass), include.rownames = FALSE)

# plot eo (cutoff)
ggplot(data = e0_cutoff, 
       aes(x = as.factor(year), y = e0_cut, col = as.factor(id_ab), group = as.factor(id_ab))) +
  geom_line() +
  geom_point() +
  ylab("life expectancy") +
  xlab("year") +
  ylim(c(55, 90)) +
  scale_color_discrete(name = "region") +
  theme_bw()

ggsave(filename = here("figures", "sm_e0_cutoff_true_subpopulations.pdf"), 
       width = 7, height = 5.5)



# plot logmx
ggplot(data = dat3[sex == "male" & age != 100, ], 
       aes(x = age, y = log(mx), group = as.factor(year), col = as.factor(year))) +
  geom_line() +
  facet_wrap(id_ab ~ ., ncol = 4) +
  scale_color_discrete(name = "year") +
  ylim(c(-11, 0)) +
  theme_bw()
ggsave(filename = here("figures", "sm_logmx_over_time_by_idab_subpopulations.pdf"), 
       width = 7, height = 8)


ggplot(data = dat3[sex == "male" & age != 100, ], 
       aes(x = age, y = log(mx), group = as.factor(id_ab), col = as.factor(id_ab))) +
  geom_line() +
  facet_wrap(year ~ ., ncol = 3) +
  scale_color_discrete(name = "region") +
  ylim(c(-11, 0)) +
  theme_bw()
ggsave(filename = here("figures", "sm_logmx_over_time_by_year_subpopulations.pdf"), 
       width = 7, height = 8)


# sessionInfo() ----------------------------------------------------------------

# R version 4.2.1 (2022-06-23 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows Server x64 (build 17763)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252   
# [3] LC_MONETARY=German_Germany.1252 LC_NUMERIC=C                   
# [5] LC_TIME=German_Germany.1252    
# 
# attached base packages:
#   [1] splines   stats     graphics  grDevices datasets  utils     methods   base     
# 
# other attached packages:
#   [1] xtable_1.8-4          LifeTable_2.0.2       MortalitySmooth_2.3.4
# [4] lattice_0.20-45       svcm_0.1.2            Matrix_1.4-1         
# [7] here_1.0.1            ggplot2_3.3.6         data.table_1.14.2    
# [10] checkmate_2.1.0      
# 
# loaded via a namespace (and not attached):
#   [1] pillar_1.8.0      compiler_4.2.1    tools_4.2.1       digest_0.6.29    
# [5] lifecycle_1.0.3   tibble_3.1.7      gtable_0.3.0      pkgconfig_2.0.3  
# [9] rlang_1.0.6       cli_3.4.1         DBI_1.1.3         rstudioapi_0.13  
# [13] withr_2.5.0       dplyr_1.0.9       systemfonts_1.0.4 generics_0.1.3   
# [17] vctrs_0.4.1       rprojroot_2.0.3   grid_4.2.1        tidyselect_1.2.0 
# [21] glue_1.6.2        R6_2.5.1          textshaping_0.3.6 fansi_1.0.3      
# [25] farver_2.1.1      magrittr_2.0.3    backports_1.4.1   scales_1.2.0     
# [29] ellipsis_0.3.2    assertthat_0.2.1  colorspace_2.0-3  renv_0.15.5      
# [33] ragg_1.2.2        labeling_0.4.2    utf8_1.2.2        munsell_0.5.0 