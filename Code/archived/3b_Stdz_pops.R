
require(rjags)
require(R2jags) 
require(devtools)
require(jagstools)

# Simulate data 
set.seed(1001)

nyears = 100
ds_jags1 = data.frame(pop = rpois(nyears, lambda=200))

ds_jags1$deaths = rpois(nyears, lambda=5)

ds_jags2 = ds_jags1
ds_jags2$pop = ds_jags1$pop/10

summary(ds_jags1$deaths/ds_jags1$pop) ; log(mean(ds_jags1$deaths/ds_jags1$pop))
summary(ds_jags2$deaths/ds_jags2$pop) ; log(mean(ds_jags2$deaths/ds_jags2$pop))

ds_jags1 = list(pop=ds_jags1$pop, deaths=ds_jags1$deaths, nyears=nyears)
ds_jags2 = list(pop=ds_jags2$pop, deaths=ds_jags2$deaths, nyears=nyears)

model1 = function() {  
  for(year in 1:nyears) {
    deaths[year] ~ dpois(mu[year]) 
    log(mu[year]) <- (pop[year] + b[year])
    mort_rate[year] <- exp(b[year])
    
  }
  
  for(year in 2:nyears) {
    b[year] ~ dnorm(b[year-1], tau) 
  }
  
  b[1] ~ dnorm(-3.75, 0.1) 
  tau ~ dgamma(0.01, 0.01)
}  


model2 = function() {  
  for(year in 1:nyears) {
    deaths[year] ~ dpois(mu[year]) 
    log(mu[year]) <- (pop[year] + b[year])
    mort_rate[year] <- exp(b[year]*10)
  }
  
  for(year in 2:nyears) {
    b[year] ~ dnorm(b[year-1], tau) 
  }
  
  b[1] ~ dnorm(-1.45, 0.1) 
  tau ~ dgamma(0.01, 0.01)
}  

params = c('mu', 'b', 'tau', 'mort_rate')
nchains = 2
inits = function() list('tau' = runif(1)) 

jags_model1 = jags(data=ds_jags1, param=params, n.chains=nchains, inits=inits,
                  n.iter=10000, n.burnin=2000, model.file=model1) 

jags_model2 = jags(data=ds_jags2, param=params, n.chains=nchains, inits=inits,
                   n.iter=10000, n.burnin=2000, model.file=model2) 


num_deaths1 = jagsresults(x=jags_model1, params=c('mu'))  
num_deaths2 = jagsresults(x=jags_model2, params=c('mu'))  

d = data.frame(d=ds_jags1$deaths, d1=num_deaths1[,'50%'],  d2= num_deaths2[,'50%'])

d$r = round(d$d1/d$d2,1)

mort_rate1 = jagsresults(x=jags_model1, params=c('mort_rate'))
mort_rate2 = jagsresults(x=jags_model2, params=c('mort_rate'))  


m = data.frame(m1=mort_rate1[,'50%'], m2=mort_rate2[,'50%'])

mean(m$m1)
mean(m$m2)





