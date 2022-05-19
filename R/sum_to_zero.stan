// gaussian random walk
// polls with sum-to-zero constraint on house effects
// known start point, 2019 election result
// assume 2022 election will be May 21 2022

data {
  // data size
  int<lower=1> NPOLLS; // number of polls
  int<lower=1> NDAYS; // number of days
  int<lower=1> NHOUSES; // number of polling houses
  
  // assumed standard deviation for all polls
  real<lower=0> sigma[NPOLLS];
  
  // poll data
  vector<lower=0,upper=1>[NPOLLS] y; // vote share
  //vector[NPOLLS] halfwidth;
  int<lower=1> j[NPOLLS];
  int<lower=1> day[NPOLLS];
  
  //vector<lower=0> [n_polls] poll_qual_adj; // poll quality adjustment
  
  // election result for start point
  real<lower=0,upper=1> election_result_2019; // historical election results
  
  // upper bound of tau_upper
  real tau_upper;

}

parameters {
  vector[NHOUSES] delta_raw;
  vector[NDAYS - 1] omega;
  real gamma;
  real<lower=0> tau;
}

transformed parameters {
  vector[NDAYS] xi;
  vector[NPOLLS] mu;
  vector[NHOUSES] delta;
  // this is necessary. If not centered the model is unidentified
  delta = delta_raw - mean(delta_raw);
  
  // dynamic model, gaussian RW
  xi[1] = election_result_2019;
  for (i in 2:NDAYS) {
    xi[i] = xi[i - 1] + (tau * omega[i - 1]);
  }
  
  // measurement model
  for (i in 1:NPOLLS) { // select house effects
      mu[i] = xi[day[i]] + delta[j[i]];
  }
}

model {
  // latent state innovations
  omega ~ normal(0.0, 1.0);
  // scale of innovations
  tau ~ uniform(0.0,tau_upper);

  // house effects model
  delta_raw ~ normal(0,.075);

  // -- observed data / measurement model
  //y ~ normal(delta[house] + xi[day], SampleSigma);
  y ~ normal(mu, sigma);
     
}
