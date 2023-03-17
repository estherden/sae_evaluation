## Esther Denecke
# This script is to analyze the results of SVD: e0

## Prerequisites: Change arguments in "arguments to (possibly) adjust". 


## arguments to (possibly) adjust ----------------------------------------------

date_data = "20230215" # date when 05-b-make_data_for_simulation_SVD was run and saved
date_registry = "20230219"# date when 06-b-_run_simulation_SVD.R was run and saved

platform = "Linux" # platform on which 06-b was run
node = "amp033" # node on which model was compiled (NOT where 06-b was run!)


# packages ---------------------------------------------------------------------

library(checkmate)
library(cowplot)
library(data.table)
library(here)
library(ggplot2)

# arguments --------------------------------------------------------------------

options(scipen = 999) # display numbers differently

res_path = here("results", paste0("registrySVD_", platform, "_", 
                                  node, "_", date_registry))

# areas to show
id_ab_high = 2
id_ab_low = 8
id_ab_med = 9

dat_hline = data.table(perfMeasure = c("bias", "empSE", "RMSE", "covCI", "widCI"),
                       hlineValue = c(0, 0, 0, 0.95, 0))


# read in ----------------------------------------------------------------------

# estimates of log(mx)
res = readRDS(here(res_path, "09_b_res_all.rds"))

# functions 
source(here("code", "00-functions_to_analyze_results.R"))
source(here("code", "00-functions.R"))


# calculate life expectancy ----------------------------------------------------

res$res = res$res[order(exposSizeArg, job.id, simRunArg, id_ab, demKnow, age), ]

res_e0 = res$res[, .(SVD = calc_e0(logmx = p_50, n = 5),
                 true = calc_e0(logmx = logmx_true, n = 5)),
             by = .(exposSizeArg, simRunArg, id_ab, demKnow)]

res_e0_raw = res$datSim[, .(e0 = calc_e0(mx = Dx / NxAdj, n = 5)),
                        by = .(exposSizeArg, sim_run, id_ab)]

res_e0_raw[, demKnow := "none"]

res_e0_raw = res_e0_raw[exposSizeArg != 1000000, ]

res_e0_raw[, method := "raw"]

setnames(res_e0_raw, old = "sim_run", new = "simRunArg")

# data wrangling ---------------------------------------------------------------

# long data
res_e0_long = melt(data = res_e0,
                   id.vars = c("exposSizeArg", "simRunArg", "id_ab", "demKnow"),
                   measure.vars = c("SVD", "true"),
                   variable.name = "method",
                   value.name = "e0")

res_e0_all = rbindlist(l = list(res_e0_long, res_e0_raw), use.names = TRUE)

# mortality regime in regions
res_e0_all[id_ab == id_ab_high, mortality := "high mortality rates"]
res_e0_all[id_ab == id_ab_med, mortality := "medium mortality rates"]
res_e0_all[id_ab == id_ab_low, mortality := "low mortality rates"]


# plots ------------------------------------------------------------------------

p_e0_box_high_mx = ggplot(data = res_e0_all[id_ab == id_ab_high & method != "true"],
                      aes(x = as.factor(exposSizeArg), y = e0, fill = demKnow)) +
  geom_boxplot() +
  facet_grid(mortality ~ method) +
  ylim(c(35, 80)) +
  theme_bw() +
  theme(legend.position = "none", 
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0), "cm")) +
  ylab("life expectancy") +
  geom_hline(yintercept = unique(res_e0[id_ab == id_ab_high, true])) 


p_e0_box_med_mx = ggplot(data = res_e0_all[id_ab == id_ab_med & method != "true"],
                         aes(x = as.factor(exposSizeArg), y = e0, fill = demKnow)) +
  geom_boxplot() +
  facet_grid(mortality ~ method) +
  ylim(c(50, 95)) +
  theme_bw() +
  theme(legend.position = "none", 
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0), "cm")) +
  ylab("life expectancy") +
  geom_hline(yintercept = unique(res_e0[id_ab == id_ab_med, true]))


p_e0_box_low_mx = ggplot(data = res_e0_all[id_ab == id_ab_low & method != "true"],
                         aes(x = as.factor(exposSizeArg), y = e0, fill = demKnow)) +
  geom_boxplot() +
  facet_grid(mortality ~ method) +
  ylim(c(55, 100)) +
  theme_bw() +
  xlab("exposure size") +
  theme(legend.position = "bottom", 
        plot.margin = unit(c(0.1, 0.1, 0.1, 0), "cm")) +
  ylab("life expectancy") +
  guides(fill = guide_legend(title = "demographic knowledge")) +
  geom_hline(yintercept = unique(res_e0[id_ab == id_ab_low, true]))


# save plot
pdf(here("figures", paste0("plot_e0_boxplot_SVD_", date_registry, ".pdf")), 
    width = 8, height = 10)
plot_grid(p_e0_box_high_mx,
          p_e0_box_med_mx, 
          p_e0_box_low_mx,
          ncol = 1, 
          align = "v",
          rel_heights = c(0.28, 0.28, 0.35))
dev.off()
