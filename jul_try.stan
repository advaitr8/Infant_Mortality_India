data{
  int N;
  vector[N] imr;
  vector[N] percap;
  int state_id[N];
  int year_id[N];
}
parameters{
  real alpha_t[8];
  real alpha_s[30];
  real beta[30];
  real mu_beta;
  real<lower = 0> tau_beta;
  real mu_alpha_s;
  real<lower = 0> tau_alpha_s;
  real mu_alpha_t;
  real<lower = 0> tau_alpha_t;
  real<lower = 0> sigma;
}
model{
  for(i in 1:N){
    imr[i] ~ normal(alpha_t[year_id[i]] 
                    + alpha_s[state_id[i]]
                    + beta[state_id[i]]*percap[i], sigma);
  }
  for(i in 1:30){
    beta[i] ~ normal(mu_beta, tau_beta); 
  }
  for(i in 1:30){
    alpha_s[i] ~ normal(mu_alpha_s, tau_alpha_s); 
  }
  for(i in 1:8){
    alpha_t[i] ~ normal(mu_alpha_t, tau_alpha_t); 
  }
  mu_alpha_t ~ normal(10, 10);
  mu_alpha_s ~ normal(10, 10);
  mu_beta ~ normal(0, 1);
}
generated quantities{
  real y_pred[N];
  for(i in 1:N){
    y_pred[i] = normal_rng(alpha_t[year_id[i]] 
                           + alpha_s[state_id[i]]
                           + beta[state_id[i]]*percap[i], sigma);
  }
}
