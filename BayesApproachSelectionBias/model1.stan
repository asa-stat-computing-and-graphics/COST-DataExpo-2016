data {
  int<lower=1> N_train;
  int<lower=1> N;      
  int<lower=1> G;      
  int<lower=1> J[G];   
  int BLTE[N];         
  int CITY[N];         
  int COND[N];         
  int LGHT[N];         
  int SIGN[N];         
  int SLIM[N];         
  int TFFC[N];
  int YEAR[N];         
  vector[N] EXPR;      
  int count[N];        
}
transformed data {
  vector[N] offset;
  offset = log(EXPR);
}
parameters {
  real offset_e;
  vector[J[1]] BLTE_eta;
  vector[J[2]] CITY_eta;
  vector[J[3]] COND_eta;
  vector[J[4]] LGHT_eta;
  vector[J[5]] SIGN_eta;
  vector[J[6]] SLIM_eta;
  vector[J[7]] TFFC_eta;
  vector[J[8]] YEAR_eta;
  vector<lower=0>[G + 1] sds;
  vector[N_train] cell_eta;
  real mu;
}
transformed parameters {
  vector[J[1]] BLTE_e;
  vector[J[2]] CITY_e;
  vector[J[3]] COND_e;
  vector[J[4]] LGHT_e;
  vector[J[5]] SIGN_e;
  vector[J[6]] SLIM_e;
  vector[J[7]] TFFC_e; 
  vector[J[8]] YEAR_e;
  vector[N_train] cell_e;
  vector[N_train] mu_indiv;
  
  BLTE_e = sds[1]     * BLTE_eta; 
  CITY_e = sds[2]     * CITY_eta;
  COND_e = sds[3]     * COND_eta;
  LGHT_e = sds[4]     * LGHT_eta;
  SIGN_e = sds[5]     * SIGN_eta;
  SLIM_e = sds[6]     * SLIM_eta;
  TFFC_e = sds[8]     * TFFC_eta;
  YEAR_e = sds[7]     * YEAR_eta;
  cell_e = sds[G + 1] * cell_eta;

  for(n in 1:N_train)
  mu_indiv[n] = mu + offset_e * offset[n]
                   + BLTE_e[BLTE[n]]
                   + CITY_e[CITY[n]]
                   + COND_e[COND[n]]
                   + LGHT_e[LGHT[n]]
                   + SIGN_e[SIGN[n]]
                   + SLIM_e[SLIM[n]]
                   + TFFC_e[TFFC[n]]
                   + YEAR_e[YEAR[n]]
                   + cell_e[n];
}
model {
  BLTE_eta ~ normal(0,1);
  CITY_eta ~ normal(0,1);
  COND_eta ~ normal(0,1);
  LGHT_eta ~ normal(0,1);
  SIGN_eta ~ normal(0,1);
  SLIM_eta ~ normal(0,1);
  TFFC_eta ~ normal(0,1);
  YEAR_eta ~ normal(0,1);
  cell_eta ~ normal(0,1);
  offset_e ~ normal(0,1);
  sds      ~ normal(0, 1);
  mu       ~ normal(0, 10);
  
 for(n in 1:N_train){
    target += poisson_log_lpmf(count[n] | mu_indiv[n]);
    target += -log1m_exp(-exp(mu_indiv[n]));
 }
}
generated quantities {
  real BLTE_sd;
  real CITY_sd;
  real COND_sd;
  real LGHT_sd;
  real SIGN_sd;
  real SLIM_sd;
  real TFFC_sd;
  real YEAR_sd;
  real cell_sd;

  BLTE_sd = sd(BLTE_e);
  CITY_sd = sd(CITY_e);
  COND_sd = sd(COND_e);
  LGHT_sd = sd(LGHT_e);
  SIGN_sd = sd(SIGN_e);
  SLIM_sd = sd(SLIM_e);
  TFFC_sd = sd(TFFC_e);
  YEAR_sd = sd(YEAR_e);
  cell_sd = sd(cell_e);
}
