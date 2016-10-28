
library(rjags)
library(R2jags) 
library(devtools)
library(jagstools)
library(gtools)
library(bayesboot)


 # Simulation to convince myself we can get post-hoc CIs for LE 

# 1. Simulate data for a poisson glm 
N = 100
x = rnorm(N, mean=10, sd=1)
mu = exp(1 + 0.2*x)
y = rpois(N, lambda=mu) 
plot(x,y)


# 2. MCMC model 

ds = list(x=x, y=y, N=N) 
df = data.frame(x=x,y=y)

head(ds)

model = function() {  
  for(i in 1:N) {
    y[i] ~ dpois(mu[i])
    log(mu[i]) <- beta0 + beta1*x[i] 
  }
  combo <- sum(mu[1:5]) / sum(mu[6:7])
  beta0 ~ dnorm(0, 0.001)
  beta1 ~ dnorm(0, 0.001)
}

params = c('mu', 'combo', 'beta0', 'beta1')
nchains = 2

jags_model = jags(data=ds, param=params, n.thin=1, n.chains=nchains, n.iter=10000, n.burnin=3000, model.file=model) 




#manual from posterior samples 

p_mcmc = as.mcmc(jags_model)  
p = data.frame(p_mcmc[[1]])   

combo = (p$mu.1. + p$mu.2. + p$mu.3. + p$mu.4. +p$mu.5. ) / (p$mu.6. + p$mu.7.)

#combo = log(pmu1*pmu2)*pmu3 + pmu4


print(c(jagsresults(x=jags_model, params=c('combo'))[, '2.5%'],
  jagsresults(x=jags_model, params=c('combo'))[, '97.5%']))
  
print(c(quantile(combo, 0.025),
        quantile(combo, 0.975)))



  

