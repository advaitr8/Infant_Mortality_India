data{
  int <lower = 0> N;
  int <lower = 0> year_id [N];
  int <lower = 0> state_id [N];
  vector [N] imr;
  vector [N] percap_exp;
}
parameters{
  vector [30] alpha_state;
  real mu_s;
  real <lower = 0> tau_s;
  vector [7] alpha_time;
  real mu_time;
  real <lower = 0> tau_time;
  vector [30] beta_exp;
  real mu_beta;
  real <lower = 0> tau_beta;
  real <lower = 0> sigma;
}
model{
  for(i in 1:N){
    target += normal_lpdf(imr[i] | alpha_state[state_id[i]]
                                  + alpha_time[year_id[i]]
                                  + beta_exp[state_id[i]]*percap_exp[i],
                                       sigma);
  } 
  alpha_state ~ normal(mu_s, tau_s);
  alpha_time ~ normal(mu_time, tau_time);
  beta_exp ~ normal(mu_beta, tau_beta);
  mu_s ~ normal(0,10);
  tau_s ~ cauchy(0,3);
  mu_time ~ normal(0,10);
  tau_time ~ cauchy(0,3);
  mu_beta ~ normal(0,1);
  tau_beta ~ cauchy(0,3);
  sigma ~ cauchy(0,5);
}
generated quantities{
  vector [N] y_pred;
  for(i in 1:N){
    y_pred[i] = normal_rng(alpha_state[state_id[i]]
                                       + alpha_time[year_id[i]]
                                       + beta_exp[state_id[i]]*percap_exp[i],
                                       sigma);
  }
}
