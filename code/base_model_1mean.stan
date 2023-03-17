// This code was written by Ameer Dharamshi who generously shared it with the 
// authors of this paper and granted permission to upload it to the git repo 
// of the paper.

data {
  int n;       //number of observations
  int n_state; //number of states
  int n_code;  //number of county codes
  int n_sex;   //number of sexes to model
  int n_year;  //number of years to be modelled
  int n_ages;  //number of age groups in the mortality curve 
  
  int state[n_code]; //state corresponding to counties
  int code[n];       //observed county code
  int sex[n];        //observed sex
  int year[n];       //observed year 
  int ages[n];       //observed age category
  
  vector[n] deaths;    //observed deaths
  vector[n] pop;       //observed population
  
  int n_pcs; //number of principal components
  matrix[n_ages, n_pcs] pcs; //principal components
}

parameters {
  matrix[n_pcs, n_sex] eps[n_code, n_year];     
  matrix[n_ages, n_sex] u[n_code, n_year];      
  
  matrix[n_pcs, n_year] mu_beta[n_state];                 
  matrix<lower=0>[n_pcs, n_year] sigma_beta[n_state]; 
  vector<lower=0>[n_ages] sigma_x;
  vector<lower=0>[n_pcs] sigma_mu_beta;

}

transformed parameters {
  matrix[n_pcs, n_sex] beta[n_code, n_year];    
  matrix[n_ages, n_sex] logmx[n_code, n_year];  
  
  for (co in 1:n_code) {
    for (yr in 1:n_year) {
      beta[co, yr] = rep_matrix(mu_beta[state[co]][:, yr], n_sex) + diag_pre_multiply(sigma_beta[state[co]][:, yr],eps[co, yr]);
      logmx[co, yr] = pcs * beta[co, yr] + rep_matrix(sigma_x, n_sex) .* u[co, yr];
    }
  }
 
}

model {

  {
    vector[n] logmx_ord;
    for (i in 1:n){
      logmx_ord[i] = logmx[code[i], year[i]][ages[i], sex[i]];
    }
    target += -dot_product(pop, exp(logmx_ord)) + dot_product(deaths, logmx_ord);
  }

// PRIORS

  for (co in 1:n_code) {
    for (yr in 1:n_year) {
      to_vector(eps[co, yr]) ~ std_normal();
      to_vector(u[co, yr]) ~ std_normal();
    }
  }
  
  sigma_x ~ normal(0, 0.25);
  sigma_mu_beta ~ lognormal(-1.5, 0.5);
  
  for (s in 1:n_state) {
    for (p in 1:n_pcs){
      mu_beta[s][p, 3:n_year] ~ normal(2 * mu_beta[s][p, 2:(n_year - 1)] - mu_beta[s][p, 1:(n_year-2)], sigma_mu_beta[p]);
    }
    
    to_vector(sigma_beta[s]) ~ std_normal();
  }
}
