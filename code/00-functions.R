## Esther Denecke
# Helper functions. 
# References as comments throughout the script.

# readBindHmdZips() ------------------------------------------------------------

## readBindHmdZips()
## - Function for reading in all the zipped HMD files stored in one folder
## - Input: - path: [character] path to the files
#           - type: [character] type of data to be read in
## - Output: [data.table] with all the read in data
## - CAUTION: 1. Function assumes (i) that naming convention of HMD files is 
#                countryCode.zzz.txt and (ii) that only HMD files to be read in
#                are stored in the respective directory.
#             2. Classes of columns as read in.
readBindHmdZips = function (path, type = c("Nx", "Dx", "e0")) {
  
  # list all files in directory
  files = list.files(path)
  
  # go through files and read in sequentially
  for (i in 1:length(files)) {
    
    # get country name
    country = unlist(strsplit(files[i], ".", fixed = TRUE))[1]
    
    # read in
    tmp = data.table::fread(here(path, paste0(files[i])))  
    
    # set region to country
    tmp[, region := country]
    
    # bind together
    if (i == 1) {
      dat = tmp
    } else {
      dat = rbindlist(l = list(dat, tmp))
    }
    
    # clean-up
    rm(list = c("country", "tmp"))
    
  }
  
  # rename
  data.table::setnames(x = dat, 
                       old = colnames(dat), 
                       new = tolower(colnames(dat)))
  
  return(dat)
  
}


# smoothDeaths() --------------------------------------------------------------

## Reference: 
#     Function originated from function create.ref.pop() by 
#     Rau and Schmertmann (2020).
#     This original function can be found at 
#     https://schmert.net/german-district-mortality/CodeReproduce/code/
#     in Step04-Create-Reference-Mortality.R
#     It was published under the license: GPL-2 which can be 
#     found here: https://www.gnu.org/licenses/old-licenses/gpl-2.0.html

## Changelog:
#  - 20221114: added includeAgeZero = TRUE
#  - 20221117: added line from LifeTable::LT() and
#                changed the names of the arguments and
#                changed <- to =.
#              Package LifeTable is here: https://github.com/timriffe/LifeTable
#                 and its license is GPL-2.
#                 latest commit is 63fb484.
#  - 20230118: added description of includeAgeZero input
#  - 20230116: added checks with checkmate
#  - 20230125: added MortalitySmooth::

## smoothDeaths()
## - Function for smoothing death rates
## - Input: - D: [numeric vector] deaths
#           - N: [numeric vector] exposures
#           - ages: [integer vector] ages (1-year)
#           - includeAgeZero [logical]: indicates whether to include age 0 in 
#                             smoothing (as sometimes causes numerical problems)
## - Output: [numeric vector] smooth Dx
smoothDeaths = function (D, N, ages, includeAgeZero = TRUE, ...) {
  
  checkmate::assertNumeric(D)
  checkmate::assertNumeric(N)
  checkmate::assertNumeric(ages)
  checkmate::assertLogical(includeAgeZero)
  
  ages1 = ages[1]
  d1 = D[1]
  n1 = N[1]
  
  if (!includeAgeZero) {
    age = ages[-1]
    death = D[-1]
    expos = N[-1]
  } else {
    age = ages
    death = D
    expos = N
  }
  
  # line w copied from LifeTable::LT() -> changed only names of arguments and <- to =
  w = ifelse(death/expos == 0 | is.null(death/expos) | is.na(death/expos) | log(expos) < 0, 0, 1)
  fit = MortalitySmooth::Mort1Dsmooth(x = age, y = death, offset = log(expos), w = w, ...)
  
  if (!includeAgeZero) {
    Dx_fit = c(d1, fit$fitted.values)
  } else {
    Dx_fit = fit$fitted.values
  }
  
  return(Dx_fit)
  
}


# logit() ----------------------------------------------------------------------

## Some Background:
# http://demographicestimation.iussp.org/content/introduction-model-life-tables
# accessed 20221114
## Original source Brass (1971) on 20230118.
#   In Brass (1971) (p.73f.): Yx = 0.5 * log((1 - lx) / lx) which is just
#   -0.5 * log(lx / (1 - lx)) rearranged.

## logit()
## Function to calculate the (demographic) logit. Demographic 
#     logit is multiplied with -0.5.
## Input:
#     p [numeric]: A proportion.
#     demographicLogit [logical]: If TRUE (default), then the output is the
#                                 demographic logit.
## Output: [numeric] The (demographic) logit.
logit = function (p, demographicLogit = TRUE) {
  
  checkmate::assertNumeric(p, lower = 0, upper = 1)
  checkmate::assertLogical(demographicLogit)
  
  if (demographicLogit) {
    fact = 0.5 
  } else {
    fact = 1
  }
  
  res = fact * log(p / (1 - p))
  
  return(res)
  
}


# calcNewYxBrass() -------------------------------------------------------------

## calcNewYxBrass()
# Function for calculating new Yx's from lxOrig based on the Brass relational
# model.
## Input:
#     a [numeric]: Brass parameter (termed "level of mortality" in Brass (1971))
#     b [numeric]: Brass parameter
#     lxOrig [numeric]: lx values of references population (radix = 1)
## Output:
#     [numeric] Yx values of "new" population.
calcNewYxBrass = function (a, b, lxOrig) {
  
  checkmate::assertNumeric(a)
  checkmate::assertNumeric(b)
  checkmate::assertNumeric(lxOrig, lower = 0, upper = 1)
  
  YxOrig = logit(p = (1 - lxOrig), demographicLogit = TRUE)
  
  Yx = a + b * YxOrig

  return(Yx)
  
}


# Yx2lx ------------------------------------------------------------------------

## References: 
#    Function name follows the naming conventions in Tim Riffe's 
#    R package LifeTable.
#    Package LifeTable is here: https://github.com/timriffe/LifeTable
#    and its license is GPL-2. Latest commit is 63fb484.

## Yx2lx()
## Input: Yx [numeric]: Yx value to convert to lx
## Output: lx [numeric]
Yx2lx = function (Yx) {
  
  checkmate::assertNumeric(Yx)
  
  lx = 1 / (1 + exp(2 * Yx))
  
}


# qx2mx() ----------------------------------------------------------------------

## Reference: 
#    Function name follows the naming conventions in Tim Riffe's 
#    R package LifeTable.
#    Package LifeTable is here: https://github.com/timriffe/LifeTable
#    and its license is GPL-2. Latest commit is 63fb484.

## qx2mx()
## Input:
#     qx [numeric]: qx values (life table convention)
#     ax [numeric]: ax values (life table convertion)
#     n [numeric]: width of age interval
## Output:
#     mx [numeric]: mortality rate
qx2mx = function (qx, ax, n = 1) {
  
  checkmate::assertNumeric(qx)
  checkmate::assertNumeric(ax)
  checkmate::assertNumeric(n)
  
  mx = (qx) / (n - (qx * n) + (qx * ax))

}



# calc_e0() --------------------------------------------------------------------

## Reference: 
#     Function originated from function e0.fun() by 
#     Rau and Schmertmann (2020).
#     This original function can be found at 
#     https://schmert.net/german-district-mortality/CodeReproduce/code/
#     in Step09_Estimate_e0.R
#     It was published under the license: GPL-2 which can be 
#     found here: https://www.gnu.org/licenses/old-licenses/gpl-2.0.html

## changelog:
# - 20221012: added option for mx instead of logmx
# - 20221221: added width of years n with default 1 
# - 20230120: changed name of function from e0.fun (name by Rau and Schmertmann (2020)) 
#             to calc_e0
# - 20230120: added argument checks with checkmate
# - 20230120: removed argument value and added logmx and mx & stop checks for them
# - 20230120: cosmetic change: <- to =
# - 20230123: cosmetic change: <- to =

## calc_e0()
## Function to calculate life expectancy from either single year or age-grouped
#     data. If age groups, must be 5-year intervals (except for youngest ages
#     which are <1 and 1-4.)
## Input:
#     logmx [numeric]: age-specific log mortality rates
#                      Default: NULL
#     mx [numeric]: age-specific mortality rates
#                   Default NULL
#     n [numeric]: width of age interval. Must be either 1 or 5.
## Output: e0
calc_e0 = function(logmx = NULL, mx = NULL, n = 1) {
  
  checkmate::assertNumeric(x = logmx, null.ok = TRUE)
  checkmate::assertNumeric(x = mx, null.ok = TRUE)
  checkmate::assertNumber(x = n, lower = 1, upper = 5)
  
  if (is.null(logmx) && is.null(mx)) {
    stop("Both logmx and mx are NULL. Specify one of them.")
  } 
  
  if (!(is.null(logmx)) && !(is.null(mx))) {
    stop("Both logmx and mx are specified. Specify only one of them.")
  }
  
  if (!(is.null(logmx))) {
    mx = exp(logmx)
  }
  
  # this part with n = 1 is from Rau and Schmertmann (2020) (changed <- to =)
  if (n == 1) {
    ax = rep(0.5, length(mx))
    ax[1] = 0.1
    qx = mx / ( 1 + (1 - ax) * mx)
    qx[qx < 0] = 0
    qx[qx > 1] = 1
    px = 1 - qx
    lx = 100000 * cumprod(c(1, px))[1:(length(px))]
    dx = c(-diff(lx), lx[length(lx)])
    Lx = c(lx[-1],0) + ax * dx
    Tx = rev(cumsum(rev(Lx)))
    ex = Tx/lx
    res = ex[1]
  } else {
    n = c(1, 4, rep(5, 19))
    ax = rep(2.5, length(mx))
    ax[1] = 0.1
    ax[2] = 2
    qx = (n * mx) / (1 + (n - ax) * mx)
    qx[1] = (1 * mx[1]) / (1 + (1 - ax[1]) * mx[1])
    qx[2] = (4 * mx[2]) / (1 + (4 - ax[2]) * mx[2])
    qx[length(qx)] = 1 # added as in LifeTable::LT() 
    qx[qx < 0] = 0
    qx[qx > 1] = 1
    px = 1 - qx
    lx = 100000 * cumprod(c(1, px))[1:(length(px))]
    dx = c(-diff(lx), lx[length(lx)])
    Lx = n * c(lx[-1],0) + ax * dx
    Tx = rev(cumsum(rev(Lx)))
    ex = Tx/lx
    res = ex[1]
  }
  
  return(res)
}  



# simulateData() ---------------------------------------------------------------

## simulateData()
## Simulate data as described in the main paper (Table 2). Function
#     designed to work with 100 single-year age groups (0:99) or 21 age 
#     groups [c(0, 1, seq(5, 95, by = 5)].
## Input:
#     - Nx [numeric]: A vector of age-specific exposure sizes. Will be re-weighted
#                     to desired exposure_size.
#     - mx [numeric]: A vector of age-specific mortality rates.
#     - nsim [numeric]: Number of simulation runs, i.e. simulated datasets.
#     - expos_size [numeric]: Desired exposure size per dataset.
#     - two_deaths_smaller_100 [logical]: Default is TRUE.
#                                         Simulate until there are at least to
#                                         deaths? This follows the simulation
#                                         set-up in Schmertmann2021DR. As far as
#                                         I understand & tested, this is needed
#                                         for D2 estimator.
#     - grouped [logical]: Default is FALSE.
#                          Single year or grouped ages?
## Output: [list] of length 3.
#     - simData: A data.table containing the simulated data as well as the
#                underlying true data.
#     - smaller100: A counter indicating how many times a dataset was rejected
#                   because there were less than 2 deaths.
#     - randNumberStates: The random number states before and after simulating
#                         each dataset. This follows advice in Morris et al. (2019).
simulateData = function (Nx, 
                         mx, 
                         nsim, 
                         expos_size, 
                         two_deaths_smaller_100 = TRUE, 
                         grouped = FALSE) {
  
  # argument checks
  checkmate::assertNumeric(Nx, len = length(mx))
  checkmate::assertNumeric(mx, len = length(Nx))
  checkmate::assertNumber(nsim)
  checkmate::assertNumeric(expos_size)
  checkmate::assertLogical(two_deaths_smaller_100)
  checkmate::assertLogical(grouped)
  
  if (grouped) {
    if (length(Nx) != 21) {
      stop("There must be 21 age groups with lower bounds c(0, 1, seq(5, 95, by = 5).")
    }
  } else {
    if (length(Nx) != 100) {
      stop("There must be 100 single year ages (0:99).")
    }
  }
  
  # Re-weigthing of exposure size following Schmertmann (2021)
  NxAdj = prop.table(Nx) * expos_size
  
  ## create different data.tables/number for saving results:
  # - sim1x for data itself
  # - randNumberStates for random number states
  # - smaller100 for counting how many times less than 2 deaths occurred
  if (grouped) {
    sim1x = data.table(age = c(0, 1, seq(5, 95, by = 5)),
                       logmx_true = log(mx),
                       Nx_true = Nx,
                       NxAdj = NxAdj)
  } else {
    sim1x = data.table(age = 0:99, 
                       logmx_true = log(mx),
                       Nx_true = Nx,
                       NxAdj = NxAdj)
  }
  randNumberStates = data.table(sim_run = 1:(nsim + 1),
                                state = as.list(NA))
  
  smaller100 = 0
  
  # Simulate data and do this nsim times.
  for (i in 1:nsim) {
    
    ## Following the advice in Morris et al. (2019) to save random number states 
    #   BEFORE simulating the data.
    rstate = get0(x = ".Random.seed", envir = .GlobalEnv, inherits = FALSE) 
    randNumberStates[sim_run == i, state := list(rstate)]
    rm(rstate)
    
    # add a columns with simulated death counts
    sim1x[, paste0("sim", i) := rpois(n = length(NxAdj), lambda = mx * NxAdj)]
    
    ## If two_deaths_smaller_100 is true AND there are less than two deaths,
    #   keep simulating data for this i until there are at least two deaths.
    #   Again, this follows the idea in Schmertmann (2021).
    if (two_deaths_smaller_100) {
      while (sim1x[, sum(get(paste0("sim", i))) < 2]) {
        smaller100 = smaller100 + 1
        sim1x[, paste0("sim", i) := rpois(n = length(NxAdj), lambda = mx * NxAdj)]
      }
    }
  }
  
  ## Following the advice in Morris et al. (2019) to save random number states
  #   AFTER simulating the data.
  rstate = get0(x = ".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  randNumberStates[sim_run == i + 1, state := list(rstate)]
  rm(rstate)
  
  # aggregate
  sim1x_long = data.table::melt(data = sim1x, 
                                id.vars = c("age", "logmx_true", "Nx_true", "NxAdj"), 
                                measure.vars = paste0("sim", 1:nsim),
                                variable.name = "sim_run", 
                                value.name = "Dx")
  
  # results list
  res = list(simData = sim1x_long, 
             smaller100 = smaller100, 
             randNumberStates = randNumberStates)
  
  return(res)
  
}


# calcDsplineHelper() ----------------------------------------------------------

## Function that makes output for calcDsplinesConstants
## Input:
#     - data [data.table]: The data to use. Should be a stacked dataset with
#                           columns age, region, year_min, year_max, logmx, and
#                           year_region_sex.
#     - sex_choose [character]: Either "male" or "female".
#     - max_age [numeric]: Maximum age in dataset. Will cut-off.
#     - min_year [character]: Minimum year.
#     - max_year [character]: Maximum year.
#     - exclude_regions [character]: Vector of region names to excplude.
calcDsplineHelper = function (data, 
                              sex_choose,
                              max_age, 
                              min_year, 
                              max_year, 
                              exclude_regions) {
  
  checkmate::assertDataTable(data)
  checkmate::assertCharacter(sex_choose, max.len = 1)
  checkmate::assertCharacter(min_year, null.ok = TRUE) # would be better if was numeric
  checkmate::assertCharacter(max_year, null.ok = TRUE) # would be better if was numeric
  checkmate::assertCharacter(exclude_regions)
  
  if (is.null(min_year)) {
    min_year = min(datHMD1x10[, year_min])
  }
  
  if (is.null(max_year)) {
    max_year = max(datHMD1x10[, year_max])
  }
  
  # choose data accordingly
  dattmp = data[sex == sex_choose &
                  age <= max_age &
                  year_min >= min_year &
                  year_max <= max_year &
                  (!(region %in% exclude_regions)),
                .(age, year_region_sex, logmx)]
  
  # wide format
  dattmp_wide = data.table::dcast(data = dattmp, 
                                  formula = age ~ year_region_sex, 
                                  value.var = "logmx")
  
  # Remove columns with inifinite logmx values
  indexNA = which(apply(apply(dattmp_wide, 2, is.finite), 2, all))
  dattmp_wide = dattmp_wide[, ..indexNA]
  
  # remove columns with NA logmx's
  indexNA = which(!(apply(apply(dattmp_wide, 2, is.na), 2, any)))
  dattmp_wide = dattmp_wide[, ..indexNA]
  
  # remove age columns and make class matrix
  dattmp_wide[, age := NULL]
  dattmp_wide = as.matrix(dattmp_wide)
  
  return(dattmp_wide)
  
}


# calcDsplineConstants() -------------------------------------------------------

## Reference: Specific pieces of this code (all the calculations for the
#   D-spline constants) are copied/adapted from Carl Schmertmann's code to the 
#   paper Schmertmann2021DR: I changed some syntax and naming.
#   Check STEP-2-BIG-cross-validated-HMD-test.R in supplementary code
#   to Schmertmann (2021).

## calcDsplineConstants()
## Function for calculating D-spline constants.
## Input: 
#       - logmx: [matrix] Matrix of log(mx) values. rows are ages and columns 
#                         countries/times
#       - estimator: [character] One of D1, D2 or DLC.
## Output: 
#       [list] List with difference matrix (or equivalent for DLC), constants
#       and inverse of empirical variance-covariance matrix
calcDsplineConstants = function(logmx, 
                                method = c("D1", "D2", "DLC"), 
                                smooth = TRUE) {

  checkmate::assertMatrix(logmx)
  checkmate::assertCharacter(method)
  checkmate::assertLogical(smooth)
  
  if (method == "D1") {
    # Schmertmann (2021)
    
    D = diff(diag(100), diff = 1) 
    const = rowMeans(D %*% logmx) 
    
    if (smooth) { 
      
      B1 = splines::bs(x = 0:98, 
                       knots = c(seq(0, 20, 2), seq(30, 100, 10)), degree = 2)
      Proj1 = B1 %*% MASS::ginv(crossprod(B1)) %*% t(B1) 
      const = as.vector(Proj1 %*% const)
    
    }
    
  } else if (method == "D2") {
    # Schmertmann (2021)
    
    D = diff(diag(100), diff = 2)
    const = rowMeans(D %*% logmx)
    
    if (smooth) { 
      
      B2 = splines::bs(x = 0:97, 
                       knots = c(seq(0, 20, 2), seq(30, 100, 10)), degree = 2)
      Proj2 = B2 %*% MASS::ginv(crossprod(B2)) %*% t(B2) 
      const = as.vector(Proj2 %*% const)
      
    }
    
  } else if (method == "DLC") {
    # Schmertmann (2021)
    
    a = rowMeans(logmx)
    tmp = logmx - a
    tmp2 = svd(tmp)
    b = tmp2$u[, 1]
    
    D = diag(length(b)) - tcrossprod(b) 
    const = as.vector(D %*% a)
    
  }
  
  # inverse of variance covariance of residuals
  eps = D %*% logmx - const
  V = var(t(eps))
  V_inv = MASS::ginv(V)
  
  res = list(D = D, const = const, V_inv = V_inv)
  
  return(res)
  
}


# selectData() -----------------------------------------------------------------

## selectData()
## Wrapper for batchtools to collect the "correct" data, i.e. the simulated
#     data that should be used to fit the model.
## Input:
#     - data: handled by batchtools
#     - job: handled by batchtools
#     - sexArg: [character] Either "male" or "female".
#     - exposSizeArg: [numeric] Exposure size to select.
#     - simRunArg: [numeric] Number of simulation run.
#     - method: [character] Either "Topals_Dsplines" or "SVD" depending on
#                           the method.
#     - stan_iter: [numeric] Will be passed on to rstan::sampling().
#     - stan_warmup: [numeric] Will be passed on to rstan::sampling().
#     - stan_chains: [numeric] Will be passed on to rstan::sampling().
#     - stan_adapt_delta: [numeric] Will be passed on to rstan::sampling().
#     - stan_max_treedepth: [numeric] Will be passed on to rstan::sampling().
#     - stan_cores: [numeric] Will be passed on to rstan::sampling().
## Output: List with selected simulated data, dk, and more details needed in
#          next steps (e.g. sex, exposure size, infos for stan - if relevant).
selectData = function (data, 
                       job, 
                       sexArg, 
                       exposSizeArg, 
                       simRunArg, 
                       method = "Topals_Dsplines",
                       stan_iter = NULL,
                       stan_warmup = NULL,
                       stan_chains = NULL,
                       stan_adapt_delta = NULL,
                       stan_max_treedepth = NULL,
                       stan_cores = NULL) {
  
  tmp = list()
  
  tmp$simData = data$datSim[sex == sexArg & exposSize == exposSizeArg, 
                            simData][[1]][sim_run == paste0("sim", simRunArg)]
  
  if (method == "Topals_Dsplines") {
    tmp$dkTopals = data$dkTopals
    tmp$dkDspline = data$dkDspline
  } else if (method == "SVD") {
    tmp$dkSVD = data$dkSVD
    tmp$pre_base_model_1mean_Ameer_stan = data$pre_base_model_1mean_Ameer_stan
    tmp$stan_iter = stan_iter
    tmp$stan_warmup = stan_warmup
    tmp$stan_chains = stan_chains
    tmp$stan_adapt_delta = stan_adapt_delta
    tmp$stan_max_treedepth = stan_max_treedepth
    tmp$stan_cores = stan_cores
  }

  tmp$sexArg = sexArg
  tmp$exposSizeArg = exposSizeArg
  tmp$simRunArg = simRunArg
  
  # rename Dx to Dx_sim to make things clearer
  setnames(x = tmp$simData, old = "Dx", new = "Dx_sim")
  
  return(tmp)
  
}


# dspline_wrapper() ------------------------------------------------------------

## dspline_wrapper()
## Function to use with batchtools. Calls dspline(). For reference to
#     understanding this see https://mllg.github.io/batchtools/articles/batchtools.html
#     Function follows naming conventions from batchtools, i.e. use of "wrapper".
## Input:
#   - data: Handled by batchtools and is the data loaded with the registry
#   - job: Handled by batchtools.
#   - instance: Output of selectData().
#   - estimator: [character] Type of estimator ("D1", "D2", "DLC").
#   - demKnow: [character] Type of demographic knowledge ("dk1", "dk2", "dk3").
#   - yearsSave: [numeric] Years to run and save.
#   - groupedAges: [logical]: Are the input ages grouped?
#   - ...
## Output: List containing results and some diagnostics.
dspline_wrapper = function (data, 
                            job, 
                            instance, 
                            estimator, 
                            demKnow, 
                            yearsSave, 
                            groupedAges,
                            ...) {

  # remove data (for memory reasons)
  rm(data)
  
  stopifnot(estimator %in% c("D1", "D2", "DLC"))
  
  # choose DSpline constants corresponding to the estimator
  Amatrix = instance[["dkDspline"]][[estimator]][[instance$sexArg]][[demKnow]]$D
  cvector = instance[["dkDspline"]][[estimator]][[instance$sexArg]][[demKnow]]$const
  SIGMA.INV = instance[["dkDspline"]][[estimator]][[instance$sexArg]][[demKnow]]$V_inv
  
  if (groupedAges) {
    
    # need age grouped input
    ageDT = data.table(age_lower = sort(c(0, rep(1, 4), rep(seq(5, 95, by = 5), 5))), 
                       age = 0:99)
    instance$simData = merge(instance$simData, ageDT, by = "age")
    instance$simData = instance$simData[order(year, id_ab, age)]
    
    instance$simDataAG = instance$simData[, .(Dx_sim = sum(Dx_sim), 
                                              NxAdj = sum(NxAdj)), 
                                          by = .(id_ab, year, age_lower)]
    
    age_bound_lower = unique(instance$simDataAG[, age_lower])
    age_bound_upper = age_bound_lower + c(1, 4, rep(5, 19))
    
  } else {
    
    instance$simDataAG = copy(instance$simData)
    
    age_bound_lower = unique(instance$simDataAG[, age])
    age_bound_upper = age_bound_lower + 1
    
  }

  year_no = unique(instance$simDataAG[year %in% yearsSave, year])
  id_ab_no = unique(instance$simDataAG[year %in% yearsSave, id_ab])
  
  res = NULL
  resDiag = NULL
  
  tic.clearlog()
  
  for (yrs in 1:length(year_no)) {
    
    for (abs in 1:length(id_ab_no)) {
      
      tmp = instance$simDataAG[year == year_no[yrs] & id_ab == id_ab_no[abs]]
      tmp_AS = instance$simData[year == year_no[yrs] & id_ab == id_ab_no[abs]]

      tic(paste0("id_ab = ", id_ab_no[abs])) # measure runtime
      fit = try(dspline(N = tmp[, NxAdj], 
                        D = tmp[, Dx_sim], 
                        age_group_lower_bounds = age_bound_lower, 
                        age_group_upper_bounds = age_bound_upper, 
                        Amatrix = Amatrix, 
                        cvector = cvector, 
                        SIGMA.INV = SIGMA.INV, 
                        knots = seq(from = 3, to = 96, by = 3),
                        max_iter = 50,
                        theta_tol = 0.00005,
                        details = TRUE),
                silent = TRUE)
      toc(log = TRUE, quiet = TRUE) # end measuring of runtime
      
      if (class(fit) == "try-error") {
        tmp2 = data.table(year = year_no[yrs],
                          id_ab = id_ab_no[abs],
                          age = 0:99,
                          logmx_hat = as.numeric(NA),
                          se_logmx_hat = as.numeric(NA))
        
        tmp3 = merge(tmp2, tmp_AS, by = c("year", "age", "id_ab"))
        
        dg = data.table(year = year_no[yrs],
                        id_ab = id_ab_no[abs],
                        maxiter = as.logical(NA),
                        niter = as.numeric(NA),
                        converge = as.logical(NA),
                        error = TRUE)

      } else {
        tmp2 = data.table(year = year_no[yrs],
                          id_ab = id_ab_no[abs],
                          age = 0:99,
                          logmx_hat = fit$lambda.hat,
                          se_logmx_hat = sqrt(diag(fit$B %*% fit$covar %*% t(fit$B)))) 
        # se calculation (just rewritten without pipes) 
        # from https://github.com/schmert/D-spline-replication/blob/main/code/D-splines-with-age-group-data.Rmd 
        # commit: a720ba7
        
        tmp3 = merge(tmp2, tmp_AS, by = c("year", "age", "id_ab"))
        
        dg = data.table(year = year_no[yrs],
                        id_ab = id_ab_no[abs],
                        maxiter = fit$maxiter,
                        niter = fit$niter,
                        converge = fit$converge,
                        error = FALSE)
        
      }
      
      if (is.null(res)) {
        res = copy(tmp3)
        resDiag = copy(dg)
      } else {
        res = rbindlist(l = list(res, tmp3), use.names = TRUE)
        resDiag = rbindlist(l = list(resDiag, dg), use.names = TRUE)
      }
      
      # clean up
      rm(list = c("tmp2", "tmp3", "dg"))
      
    }
    
  } 
  
  # add time elapsed
  resDiag[, time := unname(unlist(lapply(tic.log(format = FALSE), "[", 2)) -
                             unlist(lapply(tic.log(format = FALSE), "[", 1)))]
  
  res2 = list(res = res, 
              resDiag = resDiag)
  
  # remove instance (for memory reasons)
  rm(instance)
  
  return(res2)
  
}


# topals_wrapper() -------------------------------------------------------------

## topals_wrapper()
## Function to use with batchtools. Calls topals(). For reference to
#     understanding this see https://mllg.github.io/batchtools/articles/batchtools.html
#     Function follows naming conventions from batchtools, i.e. use of "wrapper".
## Input:
#   - data: Handled by batchtools and is the data loaded with the registry
#   - job: Handled by batchtools.
#   - instance: Output of selectData().
#   - demKnow: [character] Type of demographic knowledge ("dk1", "dk2", "dk3").
#   - yearsSave: [numeric] Years to run and save.
#   - groupedAges: [logical]: Are the input ages grouped?
#   - ...
## Output: List containing results and some diagnostics.
topals_wrapper = function (data, 
                           job, 
                           instance, 
                           demKnow, 
                           yearsSave, 
                           groupedAges, 
                           ...) {
  
  # remove data (for memory reasons)
  rm(data)
  
  if (groupedAges) {
    
    # need age grouped input
    ageDT = data.table(age_lower = sort(c(0, rep(1, 4), rep(seq(5, 95, by = 5), 5))), 
                       age = 0:99)
    instance$simData = merge(instance$simData, ageDT, by = "age")
    instance$simData = instance$simData[order(year, id_ab, age)]
    
    instance$simDataAG = instance$simData[, .(Dx_sim = sum(Dx_sim), NxAdj = sum(NxAdj)), 
                                          by = .(id_ab, year, age_lower)]
    
    age_bounds = c(0, unique(instance$simDataAG[, age_lower]) + c(1, 4, rep(5, 19)))
    
  } else {
    
    instance$simDataAG = copy(instance$simData)
    
    age_bounds = 0:100
    
  }

  year_no = unique(instance$simDataAG[year %in% yearsSave, year])
  id_ab_no = unique(instance$simDataAG[year %in% yearsSave, id_ab])
  
  # get standard
  std = instance[["dkTopals"]][[instance$sexArg]][[demKnow]]
  
  res = NULL
  resDiag = NULL
  
  tic.clearlog()
  
  for (yrs in 1:length(year_no)) {
    
    for (abs in 1:length(id_ab_no)) {
      
      tmp = instance$simDataAG[year == year_no[yrs] & id_ab == id_ab_no[abs]]
      tmp_AS = instance$simData[year == year_no[yrs] & id_ab == id_ab_no[abs]]
      
      tic(paste0("id_ab = ", id_ab_no[abs])) # measure runtime
      fit = try(topals(N = tmp[, NxAdj], 
                       D = tmp[, Dx_sim], 
                       std = std, 
                       age_group_bounds = age_bounds,
                       knot_positions = c(0, 1, 10, 20, 40, 70),
                       penalty_precision = 2,
                       max_iter = 50,
                       alpha_tol = 0.00005,
                       details = TRUE),
                silent = TRUE)
      toc(log = TRUE, quiet = TRUE) # end measuring of runtime
      
      if (class(fit) == "try-error") {
        tmp2 = data.table(year = year_no[yrs],
                          id_ab = id_ab_no[abs],
                          age = 0:99,
                          logmx_hat = as.numeric(NA),
                          se_logmx_hat = as.numeric(NA))
        
        tmp3 = merge(tmp2, tmp_AS, by = c("year", "age", "id_ab"))
        
        dg = data.table(year = year_no[yrs],
                        id_ab = id_ab_no[abs],
                        maxiter = as.logical(NA),
                        niter = as.numeric(NA),
                        converge = as.logical(NA),
                        error = TRUE)
        
      } else {
        tmp2 = data.table(year = year_no[yrs],
                          id_ab = id_ab_no[abs],
                          age = 0:99,
                          logmx_hat = as.vector(fit$logm),
                          se_logmx_hat = sqrt(diag(fit$B %*% fit$covar %*% t(fit$B))))
        # se (with super small adjustments to my coding style) from 
        # https://github.com/schmert/TOPALS/blob/master/uncertainty%20in%20TOPALS%20fits.Rmd
        # commit: c510edc
        
        tmp3 = merge(tmp2, tmp_AS, by = c("year", "age", "id_ab"))
        
        dg = data.table(year = year_no[yrs],
                        id_ab = id_ab_no[abs],
                        maxiter = fit$maxiter,
                        niter = fit$niter,
                        converge = fit$converge,
                        error = FALSE)
      }
      
      if (is.null(res)) {
        res = copy(tmp3)
        resDiag = copy(dg)
      } else {
        res = rbindlist(l = list(res, tmp3), use.names = TRUE)
        resDiag = rbindlist(l = list(resDiag, dg), use.names = TRUE)
      }
      
    } # end of     for (abs in 1:length(id_ab_no)) {
    
  } # end of   for (yrs in 1:length(year_no)) {
  
  # add time elapsed
  resDiag[, time := unname(unlist(lapply(tic.log(format = FALSE), "[", 2)) -
                             unlist(lapply(tic.log(format = FALSE), "[", 1)))]
  
  res2 = list(res = res, 
              resDiag = resDiag)
  
  # remove instance (for memory reasons)
  rm(instance)
  
  return(res2)
  
} # end of topals_wrapper()
    

# stan_wrapper() ---------------------------------------------------------------

## stan_wrapper()
## Function to use with batchtools. Calls rstan::sampling(). For reference to
#     understanding this see https://mllg.github.io/batchtools/articles/batchtools.html
#     Function follows naming conventions from batchtools, i.e. use of "wrapper".
## Input:
#   - data: Handled by batchtools and is the data loaded with the registry
#   - job: Handled by batchtools.
#   - instance: Output of selectData().
#   - demKnow: [character] Type of demographic knowledge ("dk1", "dk2", "dk3").
#   - yearsSave: [numeric] Years to save.
#   - ...
## Output: List containing results and some diagnostics.
stan_wrapper = function (data,
                         job, 
                         instance,
                         demKnow,
                         yearsSave,
                         ...) {
  
  # remove data to save at least some memory
  rm("data")
  
  # get demographic knowledge
  dk = instance[["dkSVD"]][[instance$sexArg]][[demKnow]]

  ## make stan data ------------------------------------------------------------
  # make year
  dat_year_stan = data.table(year = unique(instance$simData[, year]),
                             year_stan = 1:(length(unique(instance$simData[, year]))))
  inst = merge(instance$simData, dat_year_stan, by = "year")
  
  # pcs
  n_pcs = 3 # use 3 pcs
  pcs_adj = rep(1, n_pcs)
  
  # region is already numeric from 1 to 20
  
  # age
  dat_age_stan = data.table(age = unique(instance$simData[, age]),
                            age_stan = 1:(length(unique(instance$simData[, age]))))
  inst = merge(inst, dat_age_stan, by = "age")
  
  # reorder by state
  inst = inst[order(id_ab, year, age), ]
  
  # data in stan format (list)
  stanData = list(n = nrow(inst),
                  n_state = 1,
                  n_code = length(unique(inst[, id_ab])),
                  n_sex = 1,
                  n_year = length(unique(inst[, year_stan])),
                  n_ages = length(unique(inst[, age_stan])),
                  state = rep(1, length(unique(inst[, id_ab]))),
                  code = inst[, id_ab],
                  sex = rep(1, nrow(inst)),
                  year = inst[, year_stan],
                  ages = inst[, age_stan],
                  deaths = inst[, Dx_sim],
                  pop = inst[, NxAdj],
                  n_pcs = n_pcs,
                  pcs = dk %*% diag(pcs_adj))
  # This is adapted from example code that Ameer Dharamshi shared with us.
  
  # clean up
  rm(list = c("dat_age_stan", "n_pcs", "pcs_adj", "dat_year_stan"))
  
  ## fit -----------------------------------------------------------------------
  tic() # measure runtime
  fit = sampling(object = instance$pre_base_model_1mean_Ameer_stan,
                 data = stanData,
                 iter = instance$stan_iter,
                 save_warmup = FALSE,
                 warmup = instance$stan_warmup,
                 chains = instance$stan_chains,
                 cores = instance$stan_cores,
                 control = list(adapt_delta = instance$stan_adapt_delta,
                                max_treedepth = instance$stan_max_treedepth))
  toc(log = TRUE, quiet = TRUE) # end measuring of runtime
  
  ## diagnostics ---------------------------------------------------------------
  
  fit_monitor = rstan::monitor(fit,
                               probs = c(0.025, 0.5, 0.975),
                               print = FALSE) 
  # monitor can use a stanfit object (print first lines of code to see)
  
  # automated checks + computational time + initial stan seed
  res_diag = data.table(passed_Rhat_monitor = all(fit_monitor[["Rhat"]] < 1.01),
                        passed_bulkESS_monitor = all(fit_monitor[["Bulk_ESS"]] > 400),
                        passed_tailESS_monitor = all(fit_monitor[["Tail_ESS"]] > 400),
                        comp_time = unname(unlist(lapply(tic.log(format = FALSE), "[", 2)) - 
                                              unlist(lapply(tic.log(format = FALSE), "[", 1))),
                        stan_seed = get_seed(object = fit))
  
  rm("fit_monitor")
  
  fit_summary = as.data.table(rstan::summary(fit)$summary, keep.rownames = TRUE)
  
  res_diag[, ':='(passed_Rhat = all(fit_summary[, Rhat] <= 1.01),
                  passed_neff = all(fit_summary[, n_eff] >= 400),
                  passed_num_divergent = ifelse(get_num_divergent(fit) == 0, 
                                                TRUE, 
                                                FALSE),
                  passed_max_treedepth = ifelse(get_num_max_treedepth(fit) < instance$stan_max_treedepth, 
                                                TRUE, 
                                                FALSE),
                  passed_BFMI = ifelse(length(get_low_bfmi_chains(fit)) == 0, 
                                       TRUE, 
                                       FALSE))]
  
  ## get estimated logmx -------------------------------------------------------
  
  # extract logmx
  res_logmx = reformatStanSummary(fit_summary = fit_summary, inst = inst)
  
  rm("fit_summary")
  
  # filter for desired year(s)
  res_logmx = res_logmx[year %in% yearsSave, ]
  
  ## save desired output 
  
  res_all = list(res_logmx = res_logmx,
                 res_diag = res_diag)
  
  # return
  return(res_all)
  
}


# reformatStanSummary() --------------------------------------------------------

## reformatStanSummary()
## Function to reformat summary so that we can actually understand the
#   output, i.e. no indexes to logmx but actual columns of what these mean.
## Input:
#   - fit_summary: Output of summary(fit).
#   - inst: Slightly reformatted instance.
## Output: [data.table] containing all the info we need.
reformatStanSummary = function (fit_summary, inst) {
  
  logmx_index = grep(pattern = "logmx", x = fit_summary[, rn])
  logmx_stan = fit_summary[logmx_index, ]
  
  # extract all numbers from logmx to understand the indexes
  tt = gregexpr("[0-9]+", logmx_stan[, rn])
  tt2 = regmatches(x = logmx_stan[, rn], m = tt)
  
  # index 1 is region aka code
  unique(unlist(lapply(tt2, '[', 1)))
  ind1 = as.numeric(unlist(lapply(tt2, '[', 1)))
  
  # index 2 is year
  unique(unlist(lapply(tt2, '[', 2)))
  ind2 = as.numeric(unlist(lapply(tt2, '[', 2)))
  
  # index 3 is age
  unique(unlist(lapply(tt2, '[', 3)))
  ind3 = as.numeric(unlist(lapply(tt2, '[', 3)))
  
  # index 4 is sex
  unique(unlist(lapply(tt2, '[', 4)))
  ind4 = as.numeric(unlist(lapply(tt2, '[', 4)))
  
  # add unique indexes as columns to logmx_stan
  logmx_stan[, stan_code := ind1]
  logmx_stan[, stan_year := ind2]
  logmx_stan[, stan_age := ind3]
  logmx_stan[, stan_sex := ind4]
  
  logmx_stan_merge = merge(x = inst[, .(age, year, id_ab, year_stan, age_stan)],
                           y = logmx_stan, 
                           by.x = c("id_ab", "year_stan", "age_stan"),
                           by.y = c("stan_code", "stan_year", "stan_age"))
  
  # rename
  setnames(logmx_stan_merge, 
           old = c("2.5%", "25%", "50%", "75%", "97.5%"), 
           new = c("p_2_5", "p_25", "p_50", "p_75", "p_97_5"))
  
  logmx_stan_merge = logmx_stan_merge[, .(rn, 
                                          age, 
                                          year,
                                          id_ab,
                                          mean, 
                                          se_mean, 
                                          sd, 
                                          p_2_5,
                                          p_50,
                                          p_97_5,
                                          n_eff,
                                          Rhat)]
  
  return(logmx_stan_merge)
  
} # end of reformatStanSummary()
