data{
  int <lower = 0> N;
  vector [N] imr;
  vector [N] percap_exp;
  int state_id[N];
  int year_id[N];
}
parameters{
  real a0;
  real a1;
  real b0;
  real b1;
}
transformed parameters {
 vector [N] alpha;
 vector [N] beta;
 for (i in 1:N){
  alpha [i] = a0 + a1*percap_exp[i];
  beta [i] = b1 + b1*percap_exp[i];
  }
}
model {
  for (i in 1:N){
    target += gamma_lpdf(imr[i] | alpha[i], beta[i]);
  }
  a0 ~ normal(0,3);
  a1 ~ normal(0,3);
  b0 ~ normal(0,3);
  b1 ~ normal(0,3);
}
generated quantities{
  vector [N] y_pred;
  for (i in 1:N){
    y_pred[i] = gamma_rng(alpha[i], beta[i]);
  }
}

