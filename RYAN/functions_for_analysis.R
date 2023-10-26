####################
# SPECIFY FUNCTIONS
####################

rmse = function(m, o){
  sqrt(mean((m - o)^2))
}

normalize <- function(x){(x-min(x))/(max(x)-min(x))}

standard_error <- function(x) {sd(x) / sqrt(length(x))}

percent_change <- function(x) {((x - lead(x))/(x))*100}

zscore <- function(x) {(x - mean(x))/sd(x)}

slope <- function(x, y){
  mean_x <- mean(x)
  mean_y <- mean(y)
  nom <- sum((x - mean_x)*(y-mean_y))
  denom <- sum((x - mean_x)^2)
  m <- nom / denom
  return(m)
}

global_lme_jags <- function(){
  # Likelihood:
  for (i in 1:N){
    value[i] ~ dnorm(mu[i], tau) # tau is precision (1 / variance)

    mu[i] <- alpha[1] + fish[i] * alpha[2] +
      (beta[1] + beta[2] * fish[i]) * lake_temp[i] +
      (omega[1] + omega[2] * fish[i]) * Ca[i] +
      (phi[1] + phi[2] * fish[i]) * Chlorophyll[i] +
      a[site_code[i]]
  }
  # Priors:
  for (h in 1:4){
    alpha[h] ~ dnorm(0, 0.01)
    beta[h] ~ dnorm(0, 0.01)
    omega[h] ~ dnorm(0, 0.01) # Ca parameter
    phi[h] ~ dnorm(0, 0.01) # chlorophyll parameter
  }
  sigma ~ dunif(0, 100) # standard deviation
  tau <- 1 / (sigma * sigma) # sigma^2 doesn't work in JAGS

  sigma_a ~ dunif(0, 100) # standard deviation of random effect (variance between sites)
  tau_a <- 1 / (sigma_a * sigma_a) # convert to precision
  for (j in 1:sites){
    a[j] ~ dnorm(0, tau_a) # random intercept for each site
  }
}

global_fish_lme_jags <- function(){
  # Likelihood:
  for (i in 1:N){
    value[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha + a[site_code[i]] + beta * lake_temp[i] + omega * Ca[i] + phi * Chlorophyll[i]
  }

  # Priors:
  alpha ~ dnorm(0, 0.01)
  sigma_a ~ dunif(0, 100) # standard deviation of random effect (variance between sites)
  tau_a <- 1 / (sigma_a * sigma_a) # convert to precision
  for (j in 1:sites){
    a[j] ~ dnorm(0, tau_a) # random intercept for each site
  }

  beta ~ dnorm(0, 0.01)
  omega ~ dnorm(0, 0.01)
  phi ~ dnorm(0, 0.01)
  sigma ~ dunif(0, 100) # standard deviation
  tau <- 1 / (sigma * sigma) # sigma^2 doesn't work in JAGS

}


site_specific_lme_jags <- function(){
  # Likelihood:
  for (i in 1:N){
    value[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha + beta * lake_temp[i] + omega * Ca[i] + phi * Chlorophyll[i]
  }

  # Priors:
  alpha ~ dnorm(0, 1e-6)
  beta ~ dnorm(0, 1e-6)
  omega ~ dnorm(0, 1e-6)
  phi ~ dnorm(0, 1e-6)
  sigma ~ dunif(0, 1000) # standard deviation
  tau <- pow(sigma, -2) # sigma^2 doesn't work in JAGS

}
