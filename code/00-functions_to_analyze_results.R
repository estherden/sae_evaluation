## Esther Denecke
## Function to analyze results.


# collectResults ---------------------------------------------------------------

## collectResults()
## Function to collect results from batchtools. Objects needs to be lists.
#   This function was written to use batchtools::reduceResultsDataTable()
#   for collecting results from an experiment registry.
## Input:
#     res [list]: List as results are saved.
#     selectedResult [character]: List entry to retrieve.
## Output: List entry res[[selectedResults]].
collectResults = function (res, selectedResult) {
  
  tmp = res[[selectedResult]]
  
  return(tmp)
  
}


# calc*Performance*Measures*----------------------------------------------------

# Several functions to calculate the performance measures.


## calcBias()
## Function to calculate the bias.
## Input:
#     estimate [numeric]: Estimates.
#     truth [numeric]: True value.
## Output: [numeric] Bias.
calcBias = function (estimate, truth) {
  res = mean(estimate - truth)
  return(res)
}


## calcEmpSE()
## Function to calculate the empirical standard error.
## Input:
#     estimate: [numeric] Estimates.
#     estimate_mean: [numeric] Mean value of estimates over all simulation runs.
## Output: [numeric] Empirical standard error.
calcEmpSE = function (estimate, estimate_mean) {
  res1 = (1 / (length(estimate) - 1)) * sum((estimate - estimate_mean)^2)
  res2 = sqrt(res1)
  return(res2)
}


## calcRMSE()
## Function to calculate the root mean squared error.
## Input:
#     estimate: [numeric] Estimates.
#     truth: [numeric] True value.
## Output: [numeric] Root mean squared error.
calcRMSE = function (estimate, truth) {
  res1 = (estimate - truth)^2
  res2 = (1 / length(estimate)) * sum(res1)
  res3 = sqrt(res2)
  return(res3)
}


## calcCovCI()
## Function to calculate the coverage of uncertainty intervals.
## Input:
#     truth: [numeric] True value.
#     CI_u: [numeric] Upper bounds of CI's.
#     CI_l: [numeric] Lower bounds of CI's.
## Output: [numeric] Coverage of CI.
calcCovCI = function (truth, CI_u, CI_l) {
  res1 = ifelse(truth < CI_u & truth > CI_l, 1, 0)
  res2 = (1 / length(truth)) * sum(res1)
  return(res2)
}


## calcWidCI()
## Function to calculate the width of the CI.
## Input:
#     CI_u: [numeric] Upper bounds of CI's.
#     CI_l: [numeric] Lower bounds of CI's.
## Output: [numeric] Width of CI's.
calcWidCI = function (CI_u, CI_l) {
  res1 = (1 / length(CI_u)) * sum(abs(CI_u - CI_l))
  return(res1)
}


## calcMSE()
## Function to calculate the mean squared error.
## Input:
#     estimate: [numeric] Estimates.
#     truth: [numeric] True value.
## Output: [numeric] Mean squared error.
calcMSE = function (estimate, truth) {
  res1 = (estimate - truth)^2
  res2 = (1 / length(estimate)) * sum(res1)
  return(res2)
}

## calcVar()
## Returns output of calcEmpSE()^2)
## Input:
#     estimate: [numeric] Estimates.
#     estimate_mean: [numeric] Mean value of estimates over all simulation runs.
## Output: [numeric] calcEmpSE()^2
calcVar = function (estimate, estimate_mean) {
  res = (1 / (length(estimate) - 1)) * sum((estimate - estimate_mean)^2)
  return(res)
}



# makePlotExposure() -----------------------------------------------------------

## makePlotExposure()
## Function to make a plot over exposure sizes for one region.
## Input:
#   - resultPM_long: [data.table] with columns exposSizeArg, age, id_ab, demKnow,
#                    groupedAges, estimator, perfMeasure, value, ages
#   - dk: [character] One of "dk1", "dk2", "dk3"
#   - expos: [numeric] Exposure sizes to plot.
#   - idab: [number] Region/subpopulation aka id_ab to plot
#   - dat_hline: [data.table] Table with limits for vertical axis
#   - ylim...: [numeric] Limits for vertical axis depending on 
#              performance measure.
#   - title_info: [logical] Whether to add a (predefined) title
## Output: A plot.
makePlotExposure = function (resultPM_long, 
                             dk, 
                             expos, 
                             idab, 
                             dat_hline,
                             ylim_bias = c(-1, 1),
                             ylim_empSE = c(0, 0.5),
                             ylim_RMSE = c(0, 1),
                             ylim_covCI = c(0, 1),
                             ylim_widCI = c(0, 3),
                             title_info = FALSE) {
  
  # argument checks
  checkmate::assertDataTable(resultPM_long)
  checkmate::assertCharacter(dk)
  checkmate::assertNumeric(expos)
  checkmate::assertNumber(idab)
  checkmate::assertDataTable(dat_hline)
  checkmate::assertNumeric(ylim_bias)
  checkmate::assertNumeric(ylim_empSE)
  checkmate::assertNumeric(ylim_RMSE)
  checkmate::assertNumeric(ylim_covCI)
  checkmate::assertNumeric(ylim_widCI)
  checkmate::assertLogical(title_info)
  
  # filter data
  tmp = resultPM_long[demKnow == dk &
                       exposSizeArg %in% expos &
                       id_ab == idab, ]
  
  # plot bias
  p_bias = ggplot(data = tmp[perfMeasure == "bias", ],
                  aes(x = age, 
                      y = value,
                      group = interaction(estimator, ages),
                      col = estimator,
                      linetype = ages)) +
    geom_line(size = 0.2) +
    facet_grid(perfMeasure ~ exposSizeArg) +
    geom_hline(data = dat_hline[perfMeasure == "bias"], 
               aes(yintercept = hlineValue)) +
    scale_color_manual(values = c("mediumblue", "darkorange", "red2", "green4")) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0), "cm"),
          plot.title = element_text(size = 10)) +
    ylim(ylim_bias)
  
  # if add title
  if (isTRUE(title_info)) {
    p_bias = p_bias +     
      ggtitle(paste0("demographic knowledge: ", dk, ", sex: ", sex, ", region: ", idab))
  }
  
  # plot empSE
  p_empSE = ggplot(data = tmp[perfMeasure == "empSE", ],
                   aes(x = age, 
                       y = value,
                       group = interaction(estimator, ages),
                       col = estimator,
                       linetype = ages)) +
    geom_line(size = 0.2) +
    facet_grid(perfMeasure ~ exposSizeArg) +
    geom_hline(data = dat_hline[perfMeasure == "empSE"], 
               aes(yintercept = hlineValue)) +
    scale_color_manual(values = c("mediumblue", "darkorange", "red2", "green4")) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    theme_bw() +
    theme(legend.position = "none", 
          legend.title = element_blank(),
          strip.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0), "cm")) +
    ylim(ylim_empSE)
  
  # plot RMSE
  p_RMSE = ggplot(data = tmp[perfMeasure == "RMSE", ],
                  aes(x = age, 
                      y = value,
                      group = interaction(estimator, ages),
                      col = estimator,
                      linetype = ages)) +
    geom_line(size = 0.2) +
    facet_grid(perfMeasure ~ exposSizeArg) +
    geom_hline(data = dat_hline[perfMeasure == "RMSE"], 
               aes(yintercept = hlineValue)) +
    scale_color_manual(values = c("mediumblue", "darkorange", "red2", "green4")) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    theme_bw() +
    theme(legend.position = "none", 
          legend.title = element_blank(),
          strip.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0), "cm")) +
    ylim(ylim_RMSE)
  
  # plot covCI
  p_covCI = ggplot(data = tmp[perfMeasure == "covCI", ],
                   aes(x = age, 
                       y = value,
                       group = interaction(estimator, ages),
                       col = estimator,
                       linetype = ages)) +
    geom_line(size = 0.2) +
    facet_grid(perfMeasure ~ exposSizeArg) +
    geom_hline(data = dat_hline[perfMeasure == "covCI"], 
               aes(yintercept = hlineValue)) +
    scale_color_manual(values = c("mediumblue", "darkorange", "red2", "green4")) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    theme_bw() +
    theme(legend.position = "none", 
          legend.title = element_blank(),
          strip.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0), "cm")) +
    ylim(ylim_covCI)
  
  # plot widCI
  p_widCI = ggplot(data = tmp[perfMeasure == "widCI", ],
                   aes(x = age, 
                       y = value,
                       group = interaction(estimator, ages),
                       col = estimator,
                       linetype = ages)) +
    geom_line(size = 0.2) +
    facet_grid(perfMeasure ~ exposSizeArg) +
    geom_hline(data = dat_hline[perfMeasure == "widCI"], 
               aes(yintercept = hlineValue)) +
    scale_color_manual(values = c("mediumblue", "darkorange", "red2", "green4")) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    theme_bw() +
    theme(legend.position = "bottom",
          strip.text.x = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0), "cm"),
          legend.box.spacing = unit(0, "cm"),
          axis.title.y = element_blank(),
          legend.margin=margin(0,0,0,0)) +
    labs(col = "method",
         linetype = "input ages") +
    ylim(ylim_widCI)
  
  # arrange all in one figure
  p_all = plot_grid(p_bias, p_empSE, p_RMSE, p_covCI, p_widCI, ncol = 1, 
                    align = "v",
                    rel_heights = c(0.25, 0.19, 0.19, 0.19, 0.30))
  
  return(p_all)
  
}


# makePlotDemKnow() ------------------------------------------------------------

## makePlotDemKnow()
## Function to make a plot over demographic knowledge for one region.
## Input:
#   - resultPM_long: [data.table] with columns exposSizeArg, age, id_ab, demKnow,
#                    groupedAges, estimator, perfMeasure, value, ages
#   - dk: [character] One of "dk1", "dk2", "dk3"
#   - expos: [numeric] Exposure size(s) to plot.
#   - idab: [number] Region/subpopulation aka id_ab to plot.
#   - dat_hline: [data.table] Table with limits for vertical axis
#   - ylim...: [numeric] Limits for vertical axis depending on 
#              performance measure.
#   - title_info: [logical] Whether to add a (predefined) title
## Output: A plot.
makePlotDemKnow = function (resultPM_long, 
                            dk, 
                            expos,
                            idab, 
                            dat_hline,
                            ylim_bias = c(-1, 1),
                            ylim_empSE = c(0, 1),
                            ylim_RMSE = c(0, 1),
                            ylim_covCI = c(0, 1), 
                            ylim_widCI = c(0, 3),
                            title_info = FALSE) {

  # argument checks
  checkmate::assertDataTable(resultPM_long)
  checkmate::assertCharacter(dk)
  checkmate::assertNumeric(expos)
  checkmate::assertNumber(idab)
  checkmate::assertDataTable(dat_hline)
  checkmate::assertNumeric(ylim_bias)
  checkmate::assertNumeric(ylim_empSE)
  checkmate::assertNumeric(ylim_RMSE)
  checkmate::assertNumeric(ylim_covCI)
  checkmate::assertNumeric(ylim_widCI)
  checkmate::assertLogical(title_info)
  
  # filter
  tmp = resultPM_long[demKnow %in% dk & 
                        exposSizeArg %in% expos & 
                        id_ab == idab, ]
  
  # plot bias
  p_bias = ggplot(data = tmp[perfMeasure == "bias", ],
                  aes(x = age, 
                      y = value,
                      group = interaction(estimator, ages),
                      col = estimator,
                      linetype = ages)) +
    geom_line() +
    facet_grid(perfMeasure ~ demKnow) +
    geom_hline(data = dat_hline[perfMeasure == "bias"], 
               aes(yintercept = hlineValue)) +
    scale_color_manual(values = c("mediumblue", "darkorange", "red2", "green4")) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(), 
          axis.title.y = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
    ylim(ylim_bias)
  
  # add title
  if (isTRUE(title_info)) {
    p_bias = p_bias +     
      ggtitle(paste0("exposure: ", expos, ", sex: ", sex, ", region: ", idab))
  }
  
  # plot empSE
  p_empSE = ggplot(data = tmp[perfMeasure == "empSE", ],
                   aes(x = age, 
                       y = value,
                       group = interaction(estimator, ages),
                       col = estimator,
                       linetype = ages)) +
    geom_line() +
    facet_grid(perfMeasure ~ demKnow) +
    geom_hline(data = dat_hline[perfMeasure == "empSE"], 
               aes(yintercept = hlineValue)) +
    scale_color_manual(values = c("mediumblue", "darkorange", "red2", "green4")) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    theme_bw() +
    theme(legend.position = "none", 
          legend.title = element_blank(),
          strip.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
    ylim(ylim_empSE)
  
  # plot RMSE
  p_RMSE = ggplot(data = tmp[perfMeasure == "RMSE", ],
                  aes(x = age, 
                      y = value,
                      group = interaction(estimator, ages),
                      col = estimator,
                      linetype = ages)) +
    geom_line() +
    facet_grid(perfMeasure ~ demKnow) +
    geom_hline(data = dat_hline[perfMeasure == "RMSE"], 
               aes(yintercept = hlineValue)) +
    scale_color_manual(values = c("mediumblue", "darkorange", "red2", "green4")) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    theme_bw() +
    theme(legend.position = "none", 
          legend.title = element_blank(),
          strip.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
    ylim(ylim_RMSE)
  
  # plot covCI
  p_covCI = ggplot(data = tmp[perfMeasure == "covCI", ],
                   aes(x = age, 
                       y = value,
                       group = interaction(estimator, ages),
                       col = estimator,
                       linetype = ages)) +
    geom_line() +
    facet_grid(perfMeasure ~ demKnow) +
    geom_hline(data = dat_hline[perfMeasure == "covCI"], 
               aes(yintercept = hlineValue)) +
    scale_color_manual(values = c("mediumblue", "darkorange", "red2", "green4")) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    theme_bw() +
    theme(legend.position = "none", 
          legend.title = element_blank(),
          strip.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
    ylim(ylim_covCI)
  
  # plot widCI
  p_widCI = ggplot(data = tmp[perfMeasure == "widCI", ],
                   aes(x = age, 
                       y = value,
                       group = interaction(estimator, ages),
                       col = estimator,
                       linetype = ages)) +
    geom_line() +
    facet_grid(perfMeasure ~ demKnow) +
    geom_hline(data = dat_hline[perfMeasure == "widCI"], 
               aes(yintercept = hlineValue)) +
    scale_color_manual(values = c("mediumblue", "darkorange", "red2", "green4")) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    theme_bw() +
    theme(legend.position = "bottom",
          strip.text.x = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
          legend.box.spacing = unit(0, "cm"),
          axis.title.y = element_blank(),
          legend.margin=margin(0, 0, 0, 0)) +
    labs(col = "method",
         linetype = "input ages") +
    ylim(ylim_widCI)
  
  # arrange all plots in one figure
  p_all = plot_grid(p_bias, p_empSE, p_RMSE, p_covCI, p_widCI, ncol = 1, 
                    align = "v",
                    rel_heights = c(0.23, 0.19, 0.19, 0.19, 0.3))
  
  return(p_all)
  
}


# makePlotRegion() -------------------------------------------------------------

## makePlotRegion()
## Function to make a plot over methods for several regions.
## Input:
#   - resultPM_long: [data.table] with columns exposSizeArg, age, id_ab, demKnow,
#                    groupedAges, estimator, perfMeasure, value, ages
#   - dk: [character] One of "dk1", "dk2", "dk3"
#   - expos: [number] Exposure size to plot.
#   - idab: [numeric] Regions/subpopulations aka id_ab's to plot.
#   - dat_hline: [data.table] Table with limits for vertical axis
#   - ylim...: [numeric] Limits for vertical axis depending on 
#              performance measure.
#   - title_info: [logical] Whether to add a (predefined) title
## Output: A plot.
makePlotRegion = function (resultPM_long,
                           dk,
                           expos,
                           idab,
                           dat_hline,
                           ylim_bias = c(-1, 1),
                           ylim_empSE = c(0, 1),
                           ylim_RMSE = c(0, 1),
                           ylim_covCI = c(0, 1),
                           ylim_widCI = c(0, 3),
                           title_info = FALSE,
                           col_reg = TRUE) {
  
  # argument checks
  checkmate::assertDataTable(resultPM_long)
  checkmate::assertCharacter(dk)
  checkmate::assertNumber(expos)
  checkmate::assertNumeric(idab)
  checkmate::assertDataTable(dat_hline)
  checkmate::assertNumeric(ylim_bias)
  checkmate::assertNumeric(ylim_empSE)
  checkmate::assertNumeric(ylim_RMSE)
  checkmate::assertNumeric(ylim_covCI)
  checkmate::assertNumeric(ylim_widCI)
  checkmate::assertLogical(title_info)
  
  # colors
  if (isTRUE(col_reg)) {
    col_region = c("mediumblue", "darkorange", "red2")
    label_region = c("high", "low", "medium")
  } else {
    col_region = rep("black", length(idab))
    label_region = c("")
  }
  
  # filter
  tmp = resultPM_long[demKnow == dk & 
                        exposSizeArg == expos & 
                        id_ab %in% idab, ]
  
  # plot bias
  p_bias = ggplot(data = tmp[perfMeasure == "bias", ],
                  aes(x = age, 
                      y = value,
                      group = interaction(id_ab, ages),
                      col = as.factor(id_ab),
                      linetype = ages)) +
    geom_line() +
    facet_grid(perfMeasure ~ estimator) +
    geom_hline(data = dat_hline[perfMeasure == "bias"], 
               aes(yintercept = hlineValue)) +
    scale_color_manual(values = col_region) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_blank(), 
          axis.title.y = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
    ylim(ylim_bias)
  
  # plot title
  if (isTRUE(title_info)) {
    
    if (length(idab == 20)) {
      idabT = "all"
    } else {
      idabT = idab
    }
    p_bias = p_bias +     
      ggtitle(paste0("exposure: ", expos, 
                     ", sex: ", sex, 
                     ", dk: ", dk, 
                     ", regions: ", paste(sort(idabT), collapse = ", ")))
  }

  # plot empSE
  p_empSE = ggplot(data = tmp[perfMeasure == "empSE", ],
                   aes(x = age, 
                       y = value,
                       group = interaction(id_ab, ages),
                       col = as.factor(id_ab),
                       linetype = ages)) +
    geom_line() +
    facet_grid(perfMeasure ~ estimator) +
    geom_hline(data = dat_hline[perfMeasure == "empSE"], 
               aes(yintercept = hlineValue)) +
    scale_color_manual(values = col_region) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    theme_bw() +
    theme(legend.position = "none", 
          legend.title = element_blank(),
          strip.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
    ylim(ylim_empSE)
  
  # plot RMSE
  p_RMSE = ggplot(data = tmp[perfMeasure == "RMSE", ],
                  aes(x = age, 
                      y = value,
                      group = interaction(id_ab, ages),
                      col = as.factor(id_ab),
                      linetype = ages)) +
    geom_line() +
    facet_grid(perfMeasure ~ estimator) +
    geom_hline(data = dat_hline[perfMeasure == "RMSE"], 
               aes(yintercept = hlineValue)) +
    scale_color_manual(values = col_region) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    theme_bw() +
    theme(legend.position = "none", 
          legend.title = element_blank(),
          strip.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
    ylim(ylim_RMSE)
  
  # plot covCI
  p_covCI = ggplot(data = tmp[perfMeasure == "covCI", ],
                   aes(x = age, 
                       y = value,
                       group = interaction(id_ab, ages),
                       col = as.factor(id_ab),
                       linetype = ages)) +
    geom_line() +
    facet_grid(perfMeasure ~ estimator) +
    geom_hline(data = dat_hline[perfMeasure == "covCI"], 
               aes(yintercept = hlineValue)) +
    scale_color_manual(values = col_region) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    theme_bw() +
    theme(legend.position = "none", 
          legend.title = element_blank(),
          strip.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
    ylim(ylim_covCI)
  
  # plot widCI
  p_widCI = ggplot(data = tmp[perfMeasure == "widCI", ],
                   aes(x = age, 
                       y = value,
                       group = interaction(id_ab, ages),
                       col = as.factor(id_ab),
                       linetype = ages)) +
    geom_line() +
    facet_grid(perfMeasure ~ estimator) +
    geom_hline(data = dat_hline[perfMeasure == "widCI"], 
               aes(yintercept = hlineValue)) +
    scale_color_manual(values = col_region, labels = label_region) +
    scale_linetype_manual(values = c("dashed", "solid")) +
    theme_bw() +
    theme(legend.position = "bottom",
        strip.text.x = element_blank(),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
        legend.box.spacing = unit(0, "cm"),
        axis.title.y = element_blank(),
        legend.margin=margin(0, 0, 0, 0)) +
    ylim(ylim_widCI)
  
  # add legend
  if (isTRUE(col_reg)) {
    p_widCI = p_widCI + 
      theme(legend.position = "bottom",
            strip.text.x = element_blank(),
            plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
            legend.box.spacing = unit(0, "cm"),
            axis.title.y = element_blank(),
            legend.margin = margin(0, 0, 0, 0)) +
      labs(col = "mortality rates",
           linetype = "input ages")
  } else {
    p_widCI = p_widCI +
      guides(color = "none")
  }

  # arrange all in one grid
  p_all = plot_grid(p_bias, p_empSE, p_RMSE, p_covCI, p_widCI, ncol = 1, 
                    align = "v",
                    rel_heights = c(0.25, 0.19, 0.19, 0.19, 0.30))
  
  return(p_all)
  
}
