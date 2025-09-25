//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

data {
  int T; // elections

  int C; // candidates

  int G;

  int K; // latent space dimensionality

  matrix[G, T] share;

  array[T] int<lower=1, upper=C> cand_a;

  array[T] int<lower=1, upper=C> cand_b;

  vector[C] dime;

}

parameters {

  real<lower=0> sigma; // residual variance

  matrix[G, K] prec_ideo;

  matrix[C, K] cand_ideo;

//  vector[K] dim_scale;

}

model {

  matrix[G, T] lin_pred = prec_ideo * (cand_ideo[cand_a] - cand_ideo[cand_b])';

  to_vector(share) ~ normal(to_vector(lin_pred), sigma);

  sigma ~ exponential (1);
  
  for (k in 2:K) {

    prec_ideo[, k] ~ std_normal();

    cand_ideo[, k] ~ std_normal();

  }

  prec_ideo[, 1] ~ std_normal();

  cand_ideo[, 1] ~ normal(dime, 0.0005);
}
