## Esther Denecke
# Plotting the results of the SVD model.

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


# calculate performance measures -----------------------------------------------

resMean_p50 = res$res[, .(mean_p_50 = mean(p_50)), 
                      by = .(exposSizeArg, age, id_ab, year, demKnow)]

res$res = merge(res$res, resMean_p50, 
                by = c("exposSizeArg", "age", "id_ab", "year", "demKnow"))


res_pm = res$res[, .(bias = calcBias(estimate = p_50, truth = logmx_true),
                     empSE = calcEmpSE(estimate = p_50, estimate_mean = mean_p_50),
                     varSim = calcVar(estimate = p_50, estimate_mean = mean_p_50),
                     RMSE = calcRMSE(estimate = p_50, truth = logmx_true),
                     MSE = calcMSE(estimate = p_50, truth = logmx_true),
                     covCI = calcCovCI(truth = logmx_true, CI_u = p_97_5, CI_l = p_2_5),
                     widCI = calcWidCI(CI_u = p_97_5, CI_l = p_2_5)),
                 by = .(exposSizeArg, age, id_ab, demKnow)]

# mortality regime in regions
res_pm[id_ab == id_ab_high, mortality := "high"]
res_pm[id_ab == id_ab_med, mortality := "medium"]
res_pm[id_ab == id_ab_low, mortality := "low"]

# plots ------------------------------------------------------------------------

res_pm_long = melt(data = res_pm, 
                   id.vars = c("exposSizeArg", "age", "id_ab", "demKnow", "mortality"),
                   measure.vars = c("bias", "empSE", "RMSE", "covCI", "widCI", "varSim", "MSE"),
                   variable.name = "perfMeasure",
                   value.name = "performance")

p_bias = ggplot(data = res_pm_long[perfMeasure == "bias" & id_ab %in% c(2, 8, 9), ],
                aes(x = age, 
                    y = performance, 
                    group = interaction(mortality, demKnow), 
                    col = as.factor(mortality),
                    linetype = as.factor(demKnow))) +
  geom_line(size = 0.2) +
  facet_grid(perfMeasure ~ exposSizeArg) +
  geom_hline(data = dat_hline[perfMeasure == "bias"], aes(yintercept = hlineValue)) +
  scale_color_manual(values = c("mediumblue", "darkorange", "red2")) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0), "cm"),
        plot.title = element_text(size = 10)) +
  ylim(c(-1.5, 1.5))


p_empSE = ggplot(data = res_pm_long[perfMeasure == "empSE" & id_ab %in% c(2, 8, 9), ],
                 aes(x = age, 
                     y = performance, 
                     group = interaction(mortality, demKnow), 
                     col = as.factor(mortality),
                     linetype = as.factor(demKnow))) +
  geom_line(size = 0.2) +
  facet_grid(perfMeasure ~ exposSizeArg) +
  geom_hline(data = dat_hline[perfMeasure == "empSE"], aes(yintercept = hlineValue)) +
  scale_color_manual(values = c("mediumblue", "darkorange", "red2")) +
  theme_bw() +
  theme(legend.position = "none",
        strip.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0), "cm"),
        plot.title = element_text(size = 10)) +
  ylim(c(0, 0.6))

p_varSim = ggplot(data = res_pm_long[perfMeasure == "varSim" & id_ab %in% c(2, 8, 9), ],
                 aes(x = age, 
                     y = performance, 
                     group = interaction(mortality, demKnow), 
                     col = as.factor(mortality),
                     linetype = as.factor(demKnow))) +
  geom_line(size = 0.2) +
  facet_grid(perfMeasure ~ exposSizeArg) +
  scale_color_manual(values = c("mediumblue", "darkorange", "red2")) +
  theme_bw() +
  theme(legend.position = "none",
        strip.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0), "cm"),
        plot.title = element_text(size = 10)) +
  ylim(c(0, 0.6))

p_RMSE = ggplot(data = res_pm_long[perfMeasure == "RMSE" & id_ab %in% c(2, 8, 9), ],
                aes(x = age, 
                    y = performance, 
                    group = interaction(mortality, demKnow), 
                    col = as.factor(mortality),
                    linetype = as.factor(demKnow))) +
  geom_line(size = 0.2) +
  facet_grid(perfMeasure ~ exposSizeArg) +
  geom_hline(data = dat_hline[perfMeasure == "RMSE"], aes(yintercept = hlineValue)) +
  scale_color_manual(values = c("mediumblue", "darkorange", "red2")) +
  theme_bw() +
  theme(legend.position = "none",
        strip.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0), "cm"),
        plot.title = element_text(size = 10)) +
  ylim(c(0, 1.5))

p_MSE = ggplot(data = res_pm_long[perfMeasure == "MSE" & id_ab %in% c(2, 8, 9), ],
                aes(x = age, 
                    y = performance, 
                    group = interaction(mortality, demKnow), 
                    col = as.factor(mortality),
                    linetype = as.factor(demKnow))) +
  geom_line(size = 0.2) +
  facet_grid(perfMeasure ~ exposSizeArg) +
  scale_color_manual(values = c("mediumblue", "darkorange", "red2")) +
  theme_bw() +
  theme(legend.position = "none",
        strip.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0), "cm"),
        plot.title = element_text(size = 10))

p_covCI = ggplot(data = res_pm_long[perfMeasure == "covCI" & id_ab %in% c(2, 8, 9), ],
                 aes(x = age, 
                     y = performance, 
                     group = interaction(mortality, demKnow), 
                     col = as.factor(mortality),
                     linetype = as.factor(demKnow))) +
  geom_line(size = 0.2) +
  facet_grid(perfMeasure ~ exposSizeArg) +
  geom_hline(data = dat_hline[perfMeasure == "covCI"], aes(yintercept = hlineValue)) +
  scale_color_manual(values = c("mediumblue", "darkorange", "red2")) +
  theme_bw() +
  theme(legend.position = "none",
        strip.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0), "cm"),
        plot.title = element_text(size = 10)) +
  ylim(c(0, 1))

p_widCI = ggplot(data = res_pm_long[perfMeasure == "widCI" & id_ab %in% c(2, 8, 9), ],
                 aes(x = age, 
                     y = performance, 
                     group = interaction(mortality, demKnow), 
                     col = as.factor(mortality),
                     linetype = as.factor(demKnow))) +
  geom_line(size = 0.2) +
  facet_grid(perfMeasure ~ exposSizeArg) +
  geom_hline(data = dat_hline[perfMeasure == "widCI"], aes(yintercept = hlineValue)) +
  scale_color_manual(values = c("mediumblue", "darkorange", "red2")) +
  xlab("age group") +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.text.x = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0), "cm"),
        legend.box.spacing = unit(0, "cm"),
        axis.title.y = element_blank(),
        legend.margin=margin(0,0,0,0)) +
  labs(col = "mortality rates",
       linetype = "demographic knowledge") +
  ylim(c(0, 4))


pdf(here("figures", paste0("plot_logmx_SVD_male_for_paper_", date_registry, ".pdf")),
   width = 9, height = 6)
plot_grid(p_bias, p_empSE, p_RMSE, p_covCI, p_widCI, ncol = 1, 
          align = "v",
          rel_heights = c(0.21, 0.18, 0.18, 0.18, 0.29))
dev.off()


# compare empSE and varSim as well as MSE and RMSE -----------------------------

## compare empSE and varSim
plot_grid(p_empSE, p_varSim, ncol = 1)

## compare MSE and RMSE
plot_grid(p_RMSE, p_MSE, ncol = 1)
