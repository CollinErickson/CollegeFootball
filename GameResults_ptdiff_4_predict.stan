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
  int<lower=0> Ngames_obs;
  int<lower=0> Ngames_mis;
  int<lower=0> Nteams;
  int<lower=0> Nconfs;
  int home_id_obs[Ngames_obs];
  int away_id_obs[Ngames_obs];
  int home_id_mis[Ngames_mis];
  int away_id_mis[Ngames_mis];
  //int home_confid[Ngames];
  //int away_confid[Ngames];
  int team_confid[Nteams];
  vector[Ngames_obs] pt_diff_obs;
  vector[Ngames_obs] neutral_site_obs;
  vector[Ngames_mis] neutral_site_mis;
  
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  //real mu;
  vector[Ngames_mis] pt_diff_mis;
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
  pt_diff_obs ~ student_t(8, team_strength[home_id_obs] - team_strength[away_id_obs] + (1-neutral_site_obs) * HFA, sigma_pt_diff);
  pt_diff_mis ~ student_t(8, team_strength[home_id_mis] - team_strength[away_id_mis] + (1-neutral_site_mis) * HFA, sigma_pt_diff);
  HFA ~ normal(0, 5);
  //team_strength ~ normal(0, sigma_team_strength);
  conf_strength ~ normal(0, 20);
  team_strength_around_conf ~ normal(0, sigma_team_strength_within_conf);
  //sigma_team_strength ~ exponential(1);
  sigma_team_strength_within_conf ~ normal(10, 10); //exponential(1);
  sigma_pt_diff ~ exponential(1);
  //t_df ~ uniform(1,100);
}

