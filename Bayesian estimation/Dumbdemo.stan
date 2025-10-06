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

  int G; // precincts
  
  int M; // demographic variables

  matrix[G, T] share;

  vector[T] dime_diff;

  matrix[G,M] demo;
  
  real<lower=0> dime_prior;
}

parameters {

  real<lower=1e-6> sigma; // residual variance

  vector[G] prec_ideo;

  vector[T] ideo_diff;
  
  vector[M] demo_impact;

}

model {

  matrix[G, T] lin_pred = (prec_ideo) * (ideo_diff)' + 
  
  demo * demo_impact * (ideo_diff)';

  to_vector(share) ~ normal(to_vector(lin_pred), sigma);
  
  sigma ~ exponential (1);

  prec_ideo ~ std_normal();
  
  demo_impact ~ normal(0, 2);

  ideo_diff ~ normal(dime_diff, dime_prior);

}
