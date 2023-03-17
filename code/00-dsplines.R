## Source: Code from Carl Schmertmann.
## Web: https://github.com/schmert/D-spline-replication from 
#         https://github.com/schmert/D-spline-replication/blob/main/code/D-splines-with-age-group-data.Rmd
## latest commit when downloaded: a720ba736411b70fa282f93d1ad07323a77bb411

## changelog ED:
# - renamed the function and did some visual changes
# - changed default max_iter to 50 (from 20)
# - 20221127: replaced %>% for calculating weights
# - 20230305: added my own documentation for function



## dspline()
## Function for fitting D-splines with single-year or age grouped input.
## Input:
#     - N [numeric]: Vector of age-specific exposures (ordered by age).
#     - D [numeric]: Vector of age-specific death counts (ordered by age).
#     - age_group_lower_bounds [numeric]: Vector of lower bounds of 
#                                         age groups.
#                                         Default: single-year ages 0:99.
#     - age_group_upper_bounds [numeric]: Vector of upper bounds of
#                                         age groups.
#                                         Default single-year ages 1:100.
#     - Amatrix [matrix]: Precalibrated matrix (D in the notation of our paper).
#     - cvector [numeric]: Precalibrated vector.
#     - SIGMA.INV [matrix]: Precalibrated covariance matrix.
#     - knots [numeric]: Knots for splines.
#                        Default: seq(from = 3, to = 96, by = 3).
#     - max_iter [numeric]: Maximum number of iterations in Newton-Raphson.
#                           Default: 50.
#     - theta_tol [numeric]: stop criterion (approx. error tolerance)
#                            Default: 0.00005
#     - details [logical]: Whether to add additional output (e.g. for 
#                          calculating pointwise confidence intervals.)
#                          Default: FALSE.
## Output:
#     If details = FALSE, the spline coefficients are returned. Otherwise,
#     a list of objects including lambda.hat which are the estimated 
#     age-specific log mortality rates.
dspline = function(N, 
                   D,
                   age_group_lower_bounds = 0:99,
                   age_group_upper_bounds = 1:100,
                   Amatrix, 
                   cvector, 
                   SIGMA.INV,
                   knots = seq(from = 3, to = 96, by = 3),   
                   max_iter = 50,
                   theta_tol = 0.00005,
                   details = FALSE) {
  
  # cubic spline basis
  B = splines::bs(0:99, knots = knots, degree = 3, intercept = TRUE)
  # number of spline parameters
  K = ncol(B)  
  
  ## number and width of age groups
  age_group_labels = paste0('[', age_group_lower_bounds, ',', age_group_upper_bounds, ')')
  
  G = length(age_group_lower_bounds)   
  nages = age_group_upper_bounds - age_group_lower_bounds
  
  ## weighting matrix for mortality rates (assumes uniform
  ## distribution of single-year ages within groups)
  W = outer(seq(G), 0:99,
            function(g, x){1 * (x >= age_group_lower_bounds[g]) *
                (x <  age_group_upper_bounds[g])})
  W = prop.table(W, margin = 1)
  
  dimnames(W) = list(age_group_labels, 0:99)
  
  ## penalized log lik function
  pen_log_lik = function (theta) {
    
    lambda.hat = as.numeric(B %*% theta)
    eps = Amatrix %*% lambda.hat - cvector 
    penalty = 1/2 * t(eps) %*% SIGMA.INV %*% eps
    
    M = W %*% exp(B %*% theta)   # mortality rates by group
    logL = sum(D * log(M) - N * M)
    return(logL - penalty)
    
  }
  
  ## expected deaths function
  Dhat = function (theta) {
    
    M = W %*% exp(B %*% theta)   # mortality rates by group
    return(as.numeric(N * M))
  
  }      
  
  ## gradient function (1st deriv of pen_log_lik wrt theta) 
  gradient = function(theta) {
    
    lambda.hat = as.numeric( B %*% theta)
    eps = Amatrix %*% lambda.hat - cvector
    
    mx = exp(lambda.hat)
    Mg = as.numeric(W %*% mx)
    X = W %*% diag(mx) %*% B
    return(t(X) %*% diag(1 / Mg) %*% (D - Dhat(theta)) - 
              t(B) %*% t(Amatrix) %*% SIGMA.INV %*% eps)
    
  }
  
  hessian = function(theta) {
    
    mu = as.vector(exp(B %*% theta)) 
    M = as.vector(W %*% mu) 
    
    Dhat = N * M
    
    construct_zvec = function(k) {
      
      part1 = diag(B[, k]) %*% diag(mu) %*% t(W) %*% diag(1 / M) %*% (D - Dhat)
      part2 = diag(mu) %*% t(W) %*% diag(as.vector(W %*% diag(mu) %*% B[,k])) %*% diag(1 / (M^2)) %*% D
      part3 = t(Amatrix) %*% SIGMA.INV %*% Amatrix %*% B[, k]
      
      return(part1 - part2 - part3)
      
    }
    
    Z = sapply(1:K, construct_zvec)
    
    H = t(B) %*% Z
    
    # slight clean-up to guarantee total symmetry
    return((H + t(H))/2)
    
  } # hessian
  
  #------------------------------------------------
  # iteration function: 
  # next theta vector as a function of current theta
  #------------------------------------------------
  next_theta = function(theta) {
    
    H = hessian(theta)
    return(as.vector(solve(H, H %*% theta - gradient(theta))))
    
  }
  ## main iteration:     
  th = rep(log(sum(D) / sum(N)), K)  #initialize at overall avg log rate
  niter = 0
  
  repeat {
    
    niter = niter + 1
    last_param = th
    th = next_theta(th)  # update
    change = th - last_param
    
    converge = all(abs(change) < theta_tol)
    overrun = (niter == max_iter)
    
    if (converge | overrun) { break }
    
  } # repeat
  
  if (details | !converge | overrun) {
    if (!converge) print('did not converge')
    if (overrun) print('exceeded maximum number of iterations')
    
    dhat = Dhat(th)
    H = hessian(th)
    g = gradient(th)
    
    BWB = t(B) %*% t(W) %*% diag(dhat) %*% W %*% B
    BAVAB = t(B) %*% t(Amatrix) %*% SIGMA.INV %*% Amatrix %*% B
    df = sum(diag(solve(BWB + BAVAB) %*% BWB)) # trace of d[Dhat]/d[D'] matrix
    
    lambda.hat = B %*% th
    
    dev = 2 * sum((D > 0) * D * log(D / dhat), na.rm = TRUE)
    
    return(list(N = N,
                D = D,
                  
                age_group_lower_bounds = age_group_lower_bounds,
                age_group_upper_bounds = age_group_upper_bounds,
                  
                B = B,
                theta = as.vector(th), 
                lambda.hat = as.vector(lambda.hat),
                gradient = as.vector(g),
                dev = dev,
                df = df,
                bic = dev + df * log(length(D)),
                aic = dev + 2 * df,
                fitted.values = as.vector(dhat),
                obs.values = D,
                obs.expos = N,
                hessian = H,
                covar = solve(-H), 
                pen_log_lik = pen_log_lik(th),
                niter = niter,
                converge = converge, 
                maxiter = overrun))
  } else {
    return(th) 
  } 
  
} # end of dspline()
