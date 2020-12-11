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
  int<lower=0> Nconfs;
  int home_id[Ngames];
  int away_id[Ngames];
  //int home_confid[Ngames];
  //int away_confid[Ngames];
  int team_confid[Nteams];
  vector[Ngames] pt_diff;
  vector[Ngames] neutral_site;
  real<lower=0> transpow;
}

transformed data {
  //vector[Ngames] sqrt_pt_diff = pow(fabs(pt_diff), 0.8) .* pt_diff ./ fabs(pt_diff);
  vector[Ngames] sqrt_pt_diff; // = pow(fabs(pt_diff), 0.8) .* pt_diff ./ fabs(pt_diff);
  for (i in 1:Ngames) {
    sqrt_pt_diff[i] = pow(fabs(pt_diff[i]), transpow) * pt_diff[i] / fabs(pt_diff[i]);
  }
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  //real mu;
  vector[Nteams] team_strength_around_conf;
  vector[Nconfs] conf_strength;
  real<lower=0> sigma_pt_diff;
  real<lower=0> sigma_team_strength_within_conf;
  //real<lower=0> sigma_conf_strength;
  real HFA;
  //real<lower=1> t_df;
}

transformed parameters {
  vector[Nteams] team_strength;
  team_strength = team_strength_around_conf + conf_strength[team_confid];
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  sqrt_pt_diff ~ student_t(8, team_strength[home_id] - team_strength[away_id] + (1-neutral_site) * HFA, sigma_pt_diff);
  HFA ~ normal(0, 5);
  //team_strength ~ normal(0, sigma_team_strength);
  conf_strength ~ normal(0, 20);
  team_strength_around_conf ~ normal(0, sigma_team_strength_within_conf);
  //sigma_team_strength ~ exponential(1);
  sigma_team_strength_within_conf ~ normal(10, 10); //exponential(1);
  sigma_pt_diff ~ exponential(1);
  //t_df ~ uniform(1,100);
}

