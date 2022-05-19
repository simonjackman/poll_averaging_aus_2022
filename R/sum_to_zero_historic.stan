// gaussian random walk
// polls with sum-to-zero constraint on house effects
// known start point
// assume end point unknown

data {
  // data size
  int<lower=1> NPOLLS; // number of polls
  int<lower=1> NDAYS; // number of days
  int<lower=1> NHOUSES; // number of polling houses
  int<lower=1> NBREAKS;
  
  // assumed standard deviation for all polls
  real<lower=0> sigma[NPOLLS];
  
  // poll data
  vector<lower=0,upper=1>[NPOLLS] y; // vote share
  int<lower=1> j[NPOLLS];
  int<lower=1> day[NPOLLS];
  
  // period of discontinuity event
  int<lower=0> discontinuity[NDAYS]; // dummy discontinuity occurs
  
  // election result for start point
  real<lower=0,upper=1> prevElectionResult; // historical election results
  
  // upper bound of tau_upper
  real tau_upper;

}

parameters {
  vector[NHOUSES] delta_raw;
  vector[NDAYS - 1] omega;
  vector[NBREAKS] gamma;
  real<lower=0> tau;
}

transformed parameters {
  vector[NDAYS] xi;
  vector[NPOLLS] mu;
  vector[NHOUSES] delta;
  // this is necessary. If not centered the model is unidentified
  delta = delta_raw - mean(delta_raw);
  
  // dynamic model, gaussian RW
  xi[1] = prevElectionResult;
  for (i in 2:NDAYS) {
    xi[i] = xi[i - 1] + (tau * omega[i - 1]);
    if(discontinuity[i]!=0){
      xi[i] = xi[i] + gamma[discontinuity[i]];
    }
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

  // prior on discontinuity
  gamma ~ normal(0, 0.05);

  // -- observed data / measurement model
  y ~ normal(mu, sigma);
     
}
