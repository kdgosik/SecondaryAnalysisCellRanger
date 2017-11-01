data {
  int<lower=0> N;
  int<lower=0> n_cells;
  int<lower=0> n_genes;
  int<lower=1,upper=n_cells> cell_id[N];
  int<lower=1,upper=n_genes> gene_id[N];
  vector[N] y;
}
parameters {
  vector[n_cells] gamma;
  vector[n_genes] delta;
  real mu;
  real<lower=0,upper=100> sigma_gamma;
  real<lower=0,upper=100> sigma_delta;
  real<lower=0,upper=100> sigma_y;
}
transformed parameters {
  vector[N] y_hat;

  for (i in 1:N)
    y_hat[i] = mu + gamma[cell_id[i]] + delta[gene_id[i]];
}
model {
  //# mu ~ unif(-inf, +inf); # implicit
  gamma ~ normal(0, sigma_gamma);
  delta ~ normal(0, sigma_delta);
  y ~ normal(y_hat, sigma_y);
}