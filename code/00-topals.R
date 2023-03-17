## Source: Code from Carl Schmertmann.
## Web: https://github.com/schmert/TOPALS/blob/master/TOPALS_fit.R
# latest commit when downloaded: 1666cb3b48431c40eab032cd9f067663f4ce6143


## changelog ED:
# - renamed the function and did some visual changes
# - 20221127: replaced %>% for calculating weights
# - 20230305: added my own documentation of function

## topals()
## Function for fitting frequentist topals with single-year or age-grouped data.
## Input:
#     - N [numeric]: Vector of age-specific exposures (ordered by age).
#     - D [numeric]: Vector of age-specific death counts (ordered by age).
#     - std [numeric]: Standard. Vector of age-specific log-mortality rates
#                      (ordered by age).
#     - age_group_bounds [numeric]: Vector of lower and upper and age bounds.
#                                   Default: single-year ages 0:100 (highest 
#                                   age is 99).
#     - knot_positions [numeric]: Knots for splines. 
#                                 Default: c(0, 1, 10, 20, 40, 70)
#     - penalty_precision [numeric]: Used to build matrix for penalty term.
#              See https://github.com/schmert/TOPALS/blob/master/TOPALS_fitting_with_grouped_data.pdf
#              equation 3 and text below.
#              Default: 2.
#     - max_iter [numeric]: Maximum number of iterations for PIRWLS.
#                           Default: 50.
#     - alpha_tol [numeric]: Stopping criterion for PIRWLS.
#                            Default: 0.00005.
#     - details [logical]: Whether to add additional output (e.g. for 
#                          calculating pointwise confidence intervals.)
#                          Default: FALSE.
## Output:
#     If details = FALSE, the spline coefficients are returned. Otherwise,
#     a list of objects including logm which are the estimated 
#     age-specific log mortality rates.
topals = function(N, 
                  D, 
                  std,
                  age_group_bounds = 0:100,
                  knot_positions = c(0, 1, 10, 20, 40, 70), 
                  penalty_precision = 2,
                  max_iter = 50,
                  alpha_tol = 0.00005,
                  details = FALSE) {
  
  ## single years of age from 0 to (A-1)
  A = length(std)
  age = 0:(A-1)
  
  ## B is an AxK matrix. Each column is a linear B-spline basis function
  B = splines::bs(age, knots = knot_positions, degree = 1)
  K = ncol(B) 
  
  D1 = diff(diag(K), diff = 1)
  P = penalty_precision * crossprod(D1)
  
  ## number and width of age groups
  G = length(age_group_bounds) - 1   
  nages = diff(age_group_bounds)
  
  ## weighting matrix for mortality rates (assumes uniform
  ## distribution of single-year ages within groups)
  W = matrix(0, nrow = G, ncol = A, 
             dimnames = list(head(age_group_bounds, -1) , age))
  
  offset = 0
  for (g in 1:G) {
    W[g, offset + 1:nages[g]] = 1/nages[g]
    offset = offset + nages[g]
  }
  
  ## penalized log lik function
  Q = function(alpha) {
    M = W %*% exp(std + B %*% alpha)
    likelihood = sum(D * log(M) - N * M)
    penalty = 1/2 * t(alpha) %*% P %*% alpha
    return(likelihood - penalty)
  }
  
  #------------------------------------------------
  # iteration function: 
  # next alpha vector as a function of current alpha
  #------------------------------------------------
  next_alpha = function(alpha) {
    mu = as.vector(exp(std + B %*% alpha))
    M = as.vector(W %*% mu)
    
    Dhat = N * M
    
    X = W %*% diag(mu) %*% B
    A = diag(N / M)
    
    y = (D - Dhat) / N + X %*% alpha
    
    updated_alpha = solve(t(X) %*% A %*% X + P, t(X) %*% A %*% y)
    return(as.vector(updated_alpha))
  }
  
  ## main iteration:     
  a = rep(0, K)
  
  niter = 0
  repeat {
    niter = niter + 1
    last_param = a
    a = next_alpha( a )  # update
    change = a - last_param
    
    converge = all(abs(change) < alpha_tol)
    overrun = (niter == max_iter)
    
    if (converge | overrun) { break }
    
  } # repeat
  
  if (details | !converge | overrun) {
    if (!converge) print('did not converge')
    if (overrun) print('exceeded maximum number of iterations')
    
    mu = as.vector(exp(std + B %*% a))
    M = as.vector(W %*% mu)
    dhat = N * M
    
    X = W %*% diag(mu) %*% B
    A = diag(N/M)
    
    covar = solve(t(X) %*% A %*% X + P)
    
    return(list(alpha = a, 
                D = D,
                N = N,
                age_group_bounds = age_group_bounds,
                knots = knot_positions,
                std = std,
                B = B,
                logm = std + B %*% a,
                covar = covar,
                Qvalue = Q(a),
                converge = converge, 
                maxiter = overrun))
  } else return(a) 
  
} # end of topals()
