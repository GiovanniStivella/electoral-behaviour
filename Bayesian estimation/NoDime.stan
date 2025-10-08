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

  int G; // precincts
  
  int M; // demographic variables
  
  int K; // number of dimensions

  matrix[G, T] share;

  array[T] int<lower=1, upper=C> cand_a;

  array[T] int<lower=1, upper=C> cand_b;

  vector[C] dime;
  
  matrix[G,M] demo;
  
  real<lower=1e-6> w_sigma;

}

parameters {

  real<lower=1e-6> sigma; // residual variance

  matrix[G, K] prec_ideo;

  matrix[C, K] cand_ideo;
  
  vector[K] weights;
  
  matrix[M, K] demo_impact;
  
}

model {

  matrix[G, T] lin_pred = (prec_ideo) * diag_matrix(weights) * (cand_ideo[cand_a] - cand_ideo[cand_b])' + 
  
  demo * demo_impact * (cand_ideo[cand_a] - cand_ideo[cand_b])';

  to_vector(share) ~ normal(to_vector(lin_pred), sigma);
  
  sigma ~ exponential (1);

  prec_ideo[, 1] ~ normal(0,3);

  cand_ideo[, 1] ~ normal(0,3);
  
  demo_impact[, 1] ~ normal(0, 2);
  
  weights ~ normal(0.5,w_sigma);
  
  for (k in 2:K) {

    prec_ideo[, k] ~ normal(0, 3);

    cand_ideo[, k] ~ normal(0, 3);
    
    demo_impact[, k] ~ normal(0, 2);

  }
  


}
