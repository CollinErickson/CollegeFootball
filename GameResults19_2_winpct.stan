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
  int home_win[Ngames];
  vector[Ngames] neutral_site;
  real beta_shape1[Nteams];
  real beta_shape2[Nteams];
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  //real mu;
  vector<lower=0,upper=1>[Nteams] team_strength;
  //real<lower=0> sigma;
  //real<lower=0> sigma_team_strength;
  //real<lower=0, upper=1>  HFA;
  real HFA_logodds;
  //real<lower=1> t_df;
  real<lower=0> stretch_mult;
  real<lower=0,upper=1> shrink_factor;
}

transformed parameters {
  vector[Ngames] home_win_prob_no_HFA;
  vector[Ngames] home_win_prob_before;
  vector[Ngames] home_win_prob;
  //home_win_prob = team_strength[home_id];
  // Use log 5 rule for win prob
  home_win_prob_no_HFA = (team_strength[home_id] - team_strength[home_id] .* team_strength[away_id]) ./ (
    team_strength[home_id] + team_strength[away_id] - 2 * (team_strength[home_id] .* team_strength[away_id]));
  //home_win_prob = home_win_prob_no_HFA + (1 - neutral_site) * 2 * HFA .* (1 - home_win_prob_no_HFA);
  //home_win_prob = 1 / (1 + exp(-HFA_logodds) * (1 / home_win_prob_no_HFA - 1)); 
  home_win_prob_before = 1 ./ (1 + exp(-HFA_logodds * (1 - neutral_site)) .* (1 ./ home_win_prob_no_HFA - 1));
  //home_win_prob = 1 ./ (1 + exp(- stretch_mult * log(1 ./ (1-home_win_prob_before))));
  home_win_prob = 1 ./ (1 + exp(-stretch_mult * log(home_win_prob_before ./ (1 - home_win_prob_before)))) * shrink_factor + (1-shrink_factor)/2;
  //home_win_prob_no_HFA + (1 - neutral_site) * 2 * HFA .* (1 - home_win_prob_no_HFA);
  //for (i in 1:Ngames) {}
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  //home_win ~ student_t(8, team_strength[home_id] - team_strength[away_id] + neutral_site * HFA, sigma);
  home_win ~ bernoulli(home_win_prob);
  //home_win_prob <- team_strength[home_id];
  //HFA ~ normal(0, .1);
  HFA_logodds ~ normal(0, .5);
  //team_strength ~ normal(0, sigma_team_strength);
  //team_strength ~ beta(2,3);
  team_strength ~ beta(beta_shape1, beta_shape2);
  //sigma_team_strength ~ exponential(1);
  //sigma ~ exponential(1);
  //t_df ~ uniform(1,100);
  //stretch_mult ~ lognormal(0, 1);
  stretch_mult ~ lognormal(.4,.4);
  shrink_factor ~ beta(50, 1);
}

