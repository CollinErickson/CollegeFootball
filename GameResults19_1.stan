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

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> Ngames;
  int<lower=0> Nteams;
  int home_id[Ngames];
  int away_id[Ngames];
  vector[Ngames] pt_diff;
  vector[Ngames] neutral_site;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  //real mu;
  vector[Nteams] team_strength;
  real<lower=0> sigma;
  real<lower=0> sigma_team_strength;
  real HFA;
  //real<lower=1> t_df;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  pt_diff ~ student_t(8, team_strength[home_id] - team_strength[away_id] + neutral_site * HFA, sigma);
  HFA ~ normal(0, 5);
  team_strength ~ normal(0, sigma_team_strength);
  sigma_team_strength ~ exponential(1);
  sigma ~ exponential(1);
  //t_df ~ uniform(1,100);
}

