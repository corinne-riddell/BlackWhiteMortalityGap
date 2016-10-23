
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
  combo <- mu[1]/mu[2]
  beta0 ~ dnorm(0, 0.001)
  beta1 ~ dnorm(0, 0.001)
}

params = c('mu', 'combo')
nchains = 2

jags_model = jags(data=ds, param=params, n.thin=3, n.chains=nchains, n.iter=20000, n.burnin=5000, model.file=model) 

p_mcmc = as.mcmc(jags_model)  
p = data.frame(p_mcmc[[1]])   

p_y1 = p$mu.1.  
p_y2 = p$mu.2.  

Nsamps = 100
Niters = 10000
theta = thetab = c() 

for(i in 1:Niters) {
  s1 = sample(p_y1, size=Nsamps, replace=T) 
  s2 = sample(p_y2, size=Nsamps, replace=T) 
  
  #s1b = rpois(Nsamps, s1) 
  #s2b = rpois(Nsamps, s2) 
  
  theta[i] = (mean(s1) / mean(s2)) 
  thetab[i] = (mean(s1b) / mean(s2b)) 
}



hist(thetab, 50, col='lightblue') 

quantile(theta, 0.025)
jagsresults(x=jags_model, params=c('combo'))[, '2.5%']

quantile(theta, 0.975)
jagsresults(x=jags_model, params=c('combo'))[, '97.5%']






