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
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  //real mu;
  vector<lower=0,upper=1>[Nteams] team_strength;
  real<lower=0> sigma;
  real<lower=0> sigma_team_strength;
  real HFA;
  //real<lower=1> t_df;
}

transformed parameters {
  vector[Ngames] home_win_prob;
  //home_win_prob = team_strength[home_id];
  home_win_prob = (team_strength[home_id] - team_strength[home_id] .* team_strength[away_id]) ./ (
    team_strength[home_id] + team_strength[away_id] - 2 * (team_strength[home_id] .* team_strength[away_id]));
  //for (i in 1:Ngames) {}
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  //home_win ~ student_t(8, team_strength[home_id] - team_strength[away_id] + neutral_site * HFA, sigma);
  home_win ~ bernoulli(home_win_prob);
  //home_win_prob <- team_strength[home_id];
  HFA ~ normal(0, 5);
  //team_strength ~ normal(0, sigma_team_strength);
  team_strength ~ beta(1,1);
  sigma_team_strength ~ exponential(1);
  sigma ~ exponential(1);
  //t_df ~ uniform(1,100);
}

