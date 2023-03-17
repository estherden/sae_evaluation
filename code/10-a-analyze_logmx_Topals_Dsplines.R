## Esther Denecke
## This script is to analyze the results of Topals and D-Splines. The script
#   includes the calculation of the performance measures.

## Careful: The result objects are REALLY LARGE. You need A LOT of memory
#           for this. It also takes a bit of time to run.

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
library(checkmate)
library(ggplot2)
library(cowplot)

source(here("code", "00-functions_to_analyze_results.R"))
source(here("code", "00-functions.R"))

# areas to show
id_ab_high = 2
id_ab_low = 8
id_ab_med = 9

# alpha (for CI's)
alpha = 0.05


# read data --------------------------------------------------------------------

# diagnostics from 08
resD = readRDS(here("results", 
                    paste0("TopalsDsplines_", date_06), "resDiag.rds"))

# actual results
res1 = readRDS(file = here("results", paste0("TopalsDsplines_", date_06), "res_p1.rds"))
res2 = readRDS(file = here("results", paste0("TopalsDsplines_", date_06), "res_p2.rds"))
res3 = readRDS(file = here("results", paste0("TopalsDsplines_", date_06), "res_p3.rds"))
res4 = readRDS(file = here("results", paste0("TopalsDsplines_", date_06), "res_p4.rds"))
res5 = readRDS(file = here("results", paste0("TopalsDsplines_", date_06), "res_p5.rds"))
res6 = readRDS(file = here("results", paste0("TopalsDsplines_", date_06), "res_p6.rds"))
res7 = readRDS(file = here("results", paste0("TopalsDsplines_", date_06), "res_p7.rds"))
res8 = readRDS(file = here("results", paste0("TopalsDsplines_", date_06), "res_p8.rds"))


# wrangling --------------------------------------------------------------------

# bind results
resAll = rbindlist(l = list(res1, res2, res3, res4, res5, res6, res7, res8), 
                   use.names = TRUE)

# clean-up
rm(list = c("res1", "res2", "res3", "res4", "res5", "res6", "res7", "res8"))

resAll_grouped = resAll[groupedAges == TRUE,
                        rbindlist(result),
                        by = .(job.id, algorithm, exposSizeArg, simRunArg, 
                               estimator, demKnow, groupedAges)]

resAll_single = resAll[groupedAges == FALSE,
                        rbindlist(result),
                        by = .(job.id, algorithm, exposSizeArg, simRunArg, 
                               estimator, demKnow, groupedAges)]

# clean-up
rm("resAll")

# some wrangling, so that everything works (different columns in results...)
resAll_grouped[algorithm == "topals", estimator := "topals"]
resAll_single[algorithm == "topals", estimator := "topals"]

# not needed. we know this.
resAll_grouped[, age_lower := NULL]


# filter results for areas to show ---------------------------------------------

# this is to reduce memory requirements...

resAll_grouped = resAll_grouped[id_ab %in% c(id_ab_low, id_ab_med, id_ab_high), ]
assertDataTable(resAll_grouped, nrows = ((192000 / 2) * 100 * 3)) 
# ((192000 jobs in total) / (2 because single in other data.table)) * (100 ages) * (3 id_ab's)

resAll_single = resAll_single[id_ab %in% c(id_ab_low, id_ab_med, id_ab_high), ]
assertDataTable(resAll_grouped, nrows = ((192000 / 2) * 100 * 3)) 
# 192000 jobs in total / 2 because single in other data.table * 100 ages * 3 id_ab's


# wrangling --------------------------------------------------------------------

not_run = unique(resAll_grouped[is.na(resAll_grouped[, logmx_hat]), 
                                .(id_ab, simRunArg, estimator)]) # 56 of id_ab == 9

# remove missing results from grouped
resAll_grouped2 = resAll_grouped[!(is.na(resAll_grouped[, logmx_hat])), ]
rm(resAll_grouped)

assertDataTable(resAll_grouped2, 
                nrows = ((192000 / 2) * 100 * 3) - nrow(not_run) * 100)
# (nrow(not_run) * 100) are the number of unsuccessful runs times 100 ages

# merge grouped and single again
resAll = rbindlist(l = list(resAll_single, resAll_grouped2))

assertDataTable(resAll,
                nrows = ((192000 / 2) * 100 * 3) * 2 - nrow(not_run) * 100)

# clean up
rm(list = c("resAll_grouped2", "resAll_single"))

# calculate performance measures -----------------------------------------------

resAll_mean_logmx = resAll[, .(mean_logmx_hat = mean(logmx_hat)),
                           by = .(exposSizeArg, age, id_ab, year, 
                                  demKnow, estimator, groupedAges)]

assertDataTable(resAll_mean_logmx, 
                nrows = 8 * 100 * 3 * 3 * 4 * 2)
# 8 exposSizeArg * 100 ages * 3 id_ab's * 3 dk's * 4 methods * 2 groupedAge

resAll = merge(x = resAll, y = resAll_mean_logmx,
               by = c("exposSizeArg", "age", "id_ab", "year", 
                      "demKnow", "estimator", "groupedAges"))

assertDataTable(resAll,
                nrows = ((192000 / 2) * 100 * 3) * 2 - nrow(not_run) * 100)

rm("resAll_mean_logmx")

# CI upper and lower
resAll[, CI_upper := logmx_hat + qnorm(1 - (alpha / 2)) * se_logmx_hat]
resAll[, CI_lower := logmx_hat - qnorm(1 - (alpha / 2)) * se_logmx_hat]

res_pm = resAll[, .(bias = calcBias(estimate = logmx_hat, 
                                    truth = logmx_true),
                    
                    empSE = calcEmpSE(estimate = logmx_hat, 
                                      estimate_mean = mean_logmx_hat),
                    
                    varSim = calcVar(estimate = logmx_hat, 
                                     estimate_mean = mean_logmx_hat),
                    
                    RMSE = calcRMSE(estimate = logmx_hat, 
                                    truth = logmx_true),
                    
                    MSE = calcMSE(estimate = logmx_hat, 
                                  truth = logmx_true),
                    
                    covCI = calcCovCI(truth = logmx_true, 
                                      CI_u = CI_upper, 
                                      CI_l = CI_lower),
                    
                    widCI = calcWidCI(CI_u = CI_upper, 
                                      CI_l = CI_lower)), 
                
                by = .(exposSizeArg, age, id_ab, year, 
                       demKnow, groupedAges, estimator)]

assertDataTable(res_pm, nrows = 8 * 100 * 3 * 3 * 4 * 2)

# more data wrangling ----------------------------------------------------------

# mortality regime in regions
res_pm[id_ab == id_ab_high, mortality := "high"]
res_pm[id_ab == id_ab_med, mortality := "medium"]
res_pm[id_ab == id_ab_low, mortality := "low"]

# convert to long format
res_pm_long = melt(data = res_pm,
                    id.vars = c("exposSizeArg", "age", "id_ab", 
                                "demKnow", "groupedAges", "estimator"),
                    measure.vars = c("bias", 
                                     "empSE", "varSim", 
                                     "RMSE", "MSE",
                                     "covCI", "widCI"),
                    variable.name = "perfMeasure",
                    value.name = "value")

res_pm_long[groupedAges == FALSE, ages := "single-year"]
res_pm_long[groupedAges == TRUE, ages := "grouped"]

assertDataTable(res_pm_long, nrows = nrow(res_pm) * 7) # 7 performance measures


# prepare for plotting ---------------------------------------------------------

expos_show = c(1000, 5000, 50000, 100000, 1000000)

# values for horizontal line
dat_hline = data.table(perfMeasure = c("bias", 
                                       "empSE", "Var",
                                       "RMSE", "MSE",
                                       "covCI", "widCI"),
                       hlineValue = c(0, 
                                      0, 0, 
                                      0, 0,
                                      0.95, 0))


# plot over exposures ----------------------------------------------------------

# plot for actual paper
pdf(here("figures", 
         paste0("plot_over_exposures_idab9_male_for_paper_", date_06, ".pdf")), 
    width = 9, height = 6)
print(makePlotExposure(resultPM = res_pm_long, 
                       dk = "dk1", 
                       expos = expos_show, 
                       idab = id_ab_med, 
                       dat_hline = dat_hline,
                       ylim_bias = c(-1, 1),
                       ylim_empSE = c(0, 1),
                       ylim_RMSE = c(0, 1),
                       ylim_covCI = c(0, 1),
                       ylim_widCI = c(0, 3),
                       title_info = FALSE))
dev.off()


# plot for appendix
pdf(here("figures", paste0("plot_over_exposures_idab9_male_for_sm_", date_06, ".pdf")), 
    width = 9, height = 6)
print(makePlotExposure(resultPM = res_pm_long, 
                       dk = "dk1", 
                       expos = c(10000, 25000, 50000, 75000, 100000), 
                       idab = id_ab_med, 
                       dat_hline = dat_hline,
                       ylim_bias = c(-1, 1),
                       ylim_empSE = c(0, 1),
                       ylim_RMSE = c(0, 1),
                       ylim_covCI = c(0, 1),
                       ylim_widCI = c(0, 3),
                       title_info = FALSE))
dev.off()


# compare MSE to RMSE ---------------------------------------------

pdf(here("figures", 
         paste0("plot_over_exposures_idab9_male_RMSE_MSE_", date_06, ".pdf")), 
    width = 9, height = 6)
print(ggplot(data = res_pm_long[perfMeasure %in% c("RMSE", "MSE") & 
                                  exposSizeArg %in% expos_show & 
                                  id_ab == 9 &
                                  demKnow == "dk1", ],
             aes(x = age, 
                 y = value, 
                 group = interaction(estimator, ages),
                 col = estimator,
                 linetype = ages)) +
        geom_line() +
        facet_grid(perfMeasure ~ exposSizeArg) +
        scale_color_manual(values = c("mediumblue", "darkorange", "red2", "green4")) +
        scale_linetype_manual(values = c("dashed", "solid")) +
        theme_bw())
dev.off()


pdf(here("figures", 
         paste0("plot_over_exposures_idab9_male_RMSE_MSE_otherExpos_", date_06, ".pdf")), 
    width = 9, height = 6)
print(ggplot(data = res_pm_long[perfMeasure %in% c("RMSE", "MSE") & 
                                  exposSizeArg %in% c(10000, 25000, 50000, 75000, 100000) & 
                                  id_ab == 9 &
                                  demKnow == "dk1", ],
             aes(x = age, 
                 y = value, 
                 group = interaction(estimator, ages),
                 col = estimator,
                 linetype = ages)) +
        geom_line() +
        facet_grid(perfMeasure ~ exposSizeArg) +
        scale_color_manual(values = c("mediumblue", "darkorange", "red2", "green4")) +
        scale_linetype_manual(values = c("dashed", "solid")) +
        theme_bw())
dev.off()


# compare empSE to varSim ---------------------------------------------

pdf(here("figures", 
         paste0("plot_over_exposures_idab9_male_empSE_Var_", date_06, ".pdf")), 
    width = 9, height = 6)
print(ggplot(data = res_pm_long[perfMeasure %in% c("empSE", "varSim") & 
                                  exposSizeArg %in% expos_show & 
                                  id_ab == 9 &
                                  demKnow == "dk1", ],
             aes(x = age, 
                 y = value, 
                 group = interaction(estimator, ages),
                 col = estimator,
                 linetype = ages)) +
        geom_line() +
        facet_grid(perfMeasure ~ exposSizeArg) +
        scale_color_manual(values = c("mediumblue", "darkorange", "red2", "green4")) +
        scale_linetype_manual(values = c("dashed", "solid")) +
        theme_bw())
dev.off()


pdf(here("figures", 
         paste0("plot_over_exposures_idab9_male_empSE_VAR_otherExpos_", date_06, ".pdf")), 
    width = 9, height = 6)
print(ggplot(data = res_pm_long[perfMeasure %in% c("empSE", "varSim") & 
                                  exposSizeArg %in% c(10000, 25000, 50000, 75000, 100000) & 
                                  id_ab == 9 &
                                  demKnow == "dk1", ],
             aes(x = age, 
                 y = value, 
                 group = interaction(estimator, ages),
                 col = estimator,
                 linetype = ages)) +
        geom_line() +
        facet_grid(perfMeasure ~ exposSizeArg) +
        scale_color_manual(values = c("mediumblue", "darkorange", "red2", "green4")) +
        scale_linetype_manual(values = c("dashed", "solid")) +
        theme_bw())
dev.off()


## plot over demographic knowledge ---------------------------------------------

# plot for actual paper
pdf(here("figures", 
         paste0("plot_over_demKnow_expos50000_idab9_male_for_paper_", date_06, ".pdf")), 
    width = 9, height = 6)
print(makePlotDemKnow(resultPM = res_pm_long, 
                      expos = 50000, 
                      dk = c("dk1", "dk2", "dk3"), 
                      idab = id_ab_med, 
                      dat_hline = dat_hline,
                      ylim_bias = c(-1, 1),
                      ylim_empSE = c(0, 1),
                      ylim_RMSE = c(0, 1),
                      ylim_covCI = c(0, 1),
                      ylim_widCI = c(0, 3),
                      title_info = FALSE))
dev.off()


## plot over region ------------------------------------------------------------

pdf(here("figures", 
         paste0("plot_over_methods_many_regions_expos50000_idab9_male_for_paper_", date_06, ".pdf")), 
    width = 9, height = 6)
print(makePlotRegion(resultPM = res_pm_long, 
                     expos = 50000, 
                     dk = "dk1", 
                     idab = c(id_ab_low, id_ab_med, id_ab_high), 
                     dat_hline = dat_hline,
                     ylim_bias = c(-1, 1),
                     ylim_empSE = c(0, 1),
                     ylim_RMSE = c(0, 1),
                     ylim_covCI = c(0, 1),
                     ylim_widCI = c(0, 3),
                     title_info = FALSE,
                     col_reg = TRUE))
dev.off()


## calculate e0 for script 11-a ------------------------------------------------

res_e0 = resAll[, .(e0_hat = calc_e0(logmx = logmx_hat, n = 1),
                    e0_true = calc_e0(logmx = logmx_true, n = 1),
                    e0_raw = calc_e0(logmx = log(Dx_sim / NxAdj), n = 1)),
                by = .(exposSizeArg, id_ab, year, demKnow, 
                       estimator, groupedAges, simRunArg)]

assertDataTable(res_e0, nrows = (8 * 3 * 3 * 4 * 2 * 1000) - 1330)
# (8 exposures * 3 id_ab's * 3 demKnow * 4 estimator * 2 groupedAges * 1000 simRunArg) - 1330 not_run

# save e0
saveRDS(object = res_e0, 
        file = here("results", 
                    paste0("TopalsDsplines_", date_06), 
                    "res_e0.rds"))
