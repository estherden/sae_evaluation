## Esther Denecke
# This script is to analyze estimates of life expectancy by TOPALS and D-splines.

## Prerequisites: Change date in "arguments to (possibly) adjust" to
#                 whenever then registry in 06-a was created.


## arguments to (possibly) adjust ----------------------------------------------

# date registry
date_06 = "20230314"


# preliminaries ----------------------------------------------------------------

options(scipen = 999) # display numbers differently

# packages
library(cowplot)
library(data.table)
library(here)
library(ggplot2)

# e0
res_e0 = readRDS(here("results", 
                      paste0("TopalsDsplines_", date_06), 
                      "res_e0.rds"))

# arguments --------------------------------------------------------------------

expos_show = c(1000, 5000, 50000, 100000, 1000000)
sex_show = "male"

expos_other = c(10000, 25000, 50000, 75000, 100000)

# areas to show
id_ab_high = 2
id_ab_low = 8
id_ab_med = 9


# data wrangling ---------------------------------------------------------------

res_e0[id_ab == id_ab_med, mortality := "medium mortality rates"]
res_e0[id_ab == id_ab_high, mortality := "high mortality rates"]
res_e0[id_ab == id_ab_low, mortality := "low mortality rates"]

## make one long data set

# get all e0_hat's
tmp1 = res_e0[groupedAges == FALSE, .(exposSizeArg, id_ab, year, demKnow, 
                                      estimator, groupedAges, simRunArg,
                                      e0_hat, mortality)]
setnames(tmp1, old = "e0_hat", new = "e0")

# get all e0_true's
tmp2 = res_e0[groupedAges == FALSE, .(id_ab, year, demKnow, e0_true, mortality)]
unique(tmp2)
tmp2 = unique(tmp2[, .(mortality, id_ab, e0_true)])
setnames(tmp2, old = "e0_true", new = "e0")
tmp2[, ':='(estimator = "true")] # not an estimator but does its job here
 

# get all e0_raw's
tmp3 = res_e0[groupedAges == FALSE, 
              .(exposSizeArg, id_ab, year, demKnow, estimator, 
                simRunArg, mortality, e0_raw)]

tmp3 = dcast(tmp3, 
             exposSizeArg + id_ab + mortality + simRunArg ~ demKnow + estimator,
             value.var = "e0_raw")

# compare columns: a little cumbersome but on the saver side
all.equal(tmp3$dk1_D1, tmp3$dk1_D2)
all.equal(tmp3$dk1_D1, tmp3$dk1_DLC)
all.equal(tmp3$dk1_D1, tmp3$dk1_topals)
all.equal(tmp3$dk1_D1, tmp3$dk2_D2)
all.equal(tmp3$dk1_D1, tmp3$dk2_DLC)
all.equal(tmp3$dk1_D1, tmp3$dk2_topals)
all.equal(tmp3$dk1_D1, tmp3$dk3_D2)
all.equal(tmp3$dk1_D1, tmp3$dk3_DLC)
all.equal(tmp3$dk1_D1, tmp3$dk3_topals)

tmp3 = tmp3[, .(exposSizeArg, id_ab, mortality, simRunArg, dk1_D1)]
setnames(tmp3, old = "dk1_D1", new = "e0")

tmp3[, ':='(estimator = "raw", 
            groupedAges = FALSE,
            demKnow = "none")]

# bind together
res_e0_long = rbindlist(l = list(tmp1, tmp2, tmp3), 
                        use.names = TRUE, 
                        fill = TRUE)

res_e0_long[, grouped_estimator := factor(estimator, 
                                          levels = c("D1", "D2", "DLC", 
                                                     "topals", "raw", "true"))]


# plot -------------------------------------------------------------------------

p_e0_box_low = ggplot(data = res_e0_long[groupedAges == FALSE & 
                                           id_ab == id_ab_low & 
                                           exposSizeArg %in% expos_show &
                                           estimator %in% c("D1", "D2", "DLC", 
                                                            "topals", "raw")], 
                      aes(x = as.factor(exposSizeArg), 
                          y = e0, 
                          fill = as.factor(demKnow))) +
  geom_boxplot() +
  facet_grid(mortality ~ grouped_estimator) +
  ylim(c(55, 100)) +
  theme_bw() +
  theme(legend.position = "none", 
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0), "cm")) +
  ylab("life expectancy") +
  geom_hline(yintercept = res_e0_long[grouped_estimator == "true" &
                                        id_ab == id_ab_low, e0])


p_e0_box_med = ggplot(data = res_e0_long[groupedAges == FALSE & 
                                           id_ab == id_ab_med & 
                                           exposSizeArg %in% expos_show &
                                           estimator %in% c("D1", "D2", "DLC", 
                                                            "topals", "raw")], 
                      aes(x = as.factor(exposSizeArg), 
                          y = e0, 
                          fill = as.factor(demKnow))) +
  geom_boxplot() +
  facet_grid(mortality ~ grouped_estimator) +
  ylim(c(50, 95)) +
  theme_bw() +
  theme(legend.position = "none", 
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0), "cm")) +
  ylab("life expectancy") +
  geom_hline(yintercept = res_e0_long[grouped_estimator == "true" &
                                        id_ab == id_ab_med, e0])


p_e0_box_high = ggplot(data = res_e0_long[groupedAges == FALSE & 
                                            id_ab == id_ab_high & 
                                            exposSizeArg %in% expos_show &
                                            estimator %in% c("D1", "D2", "DLC", 
                                                             "topals", "raw")], 
                       aes(x = as.factor(exposSizeArg), 
                           y = e0, 
                           fill = as.factor(demKnow))) +
  geom_boxplot() +
  facet_grid(mortality ~ grouped_estimator) +
  ylim(c(35, 80)) +
  theme_bw() +
  theme(legend.position = "bottom", 
        legend.margin = margin(0,0,0,0),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
  guides(fill = guide_legend("demographic knowledge")) +
  ylab("life expectancy") +
  xlab("exposure size") +
  geom_hline(yintercept = res_e0_long[grouped_estimator == "true" &
                                        id_ab == id_ab_high, e0])


pdf(here("figures", paste0("plot_e0_boxplot_TopalsDsplines_", date_06, ".pdf")), 
    width = 13, height = 9)
plot_grid(p_e0_box_low,
          p_e0_box_med, 
          p_e0_box_high,
          ncol = 1, 
          align = "v",
          rel_heights = c(0.28, 0.28, 0.36))
dev.off()

# clean up
rm(list = c("p_e0_box_low", "p_e0_box_med", "p_e0_box_high"))

## plots for supplementary material --------------------------------------------

p_e0_box_low = ggplot(data = res_e0_long[groupedAges == FALSE & 
                                           id_ab == id_ab_low & 
                                           exposSizeArg %in% expos_other &
                                           estimator %in% c("D1", "D2", "DLC", 
                                                            "topals", "raw")], 
                      aes(x = as.factor(exposSizeArg), 
                          y = e0, 
                          fill = as.factor(demKnow))) +
  geom_boxplot() +
  facet_grid(mortality ~ grouped_estimator) +
  ylim(c(55, 100)) +
  theme_bw() +
  theme(legend.position = "none", 
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0), "cm")) +
  ylab("life expectancy") +
  geom_hline(yintercept = res_e0_long[grouped_estimator == "true" &
                                        id_ab == id_ab_low, e0])

p_e0_box_med = ggplot(data = res_e0_long[groupedAges == FALSE & 
                                           id_ab == id_ab_med & 
                                           exposSizeArg %in% expos_other &
                                           estimator %in% c("D1", "D2", "DLC", 
                                                            "topals", "raw")], 
                      aes(x = as.factor(exposSizeArg), 
                          y = e0, 
                          fill = as.factor(demKnow))) +
  geom_boxplot() +
  facet_grid(mortality ~ grouped_estimator) +
  ylim(c(50, 95)) +
  theme_bw() +
  theme(legend.position = "none", 
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0), "cm")) +
  ylab("life expectancy") +
  geom_hline(yintercept = res_e0_long[grouped_estimator == "true" &
                                        id_ab == id_ab_med, e0])

p_e0_box_high = ggplot(data = res_e0_long[groupedAges == FALSE & 
                                            id_ab == id_ab_high & 
                                            exposSizeArg %in% expos_other &
                                            estimator %in% c("D1", "D2", "DLC", 
                                                             "topals", "raw")], 
                       aes(x = as.factor(exposSizeArg), 
                           y = e0, 
                           fill = as.factor(demKnow))) +
  geom_boxplot() +
  facet_grid(mortality ~ grouped_estimator) +
  ylim(c(35, 80)) +
  theme_bw() +
  theme(legend.position = "bottom", 
        legend.margin = margin(0,0,0,0),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
  guides(fill = guide_legend("demographic knowledge")) +
  ylab("life expectancy") +
  xlab("exposure size") +
  geom_hline(yintercept = res_e0_long[grouped_estimator == "true" &
                                        id_ab == id_ab_high, e0])


pdf(here("figures", paste0("plot_e0_boxplot_TopalsDsplines_sm_", date_06, ".pdf")), 
    width = 13, height = 9)
plot_grid(p_e0_box_low,
          p_e0_box_med, 
          p_e0_box_high,
          ncol = 1, 
          align = "v",
          rel_heights = c(0.28, 0.28, 0.36))
dev.off()
