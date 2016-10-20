
#convincing myself this makes sense

ds1 = subset_data(race='Black', sex='Male', cod='Injuries') 

ds2 = ds1$ds[ds1$ds$age==3, ] ; ds2
ds3 = ds2[order(ds2$censored),]
N = length(ds2$deaths[ds2$censored==FALSE]) 
Nc = length(ds2$deaths) 

ds = list(deaths=ds3$deaths, pop=ds3$pop, censored=ds3$censored, N=N, Nc=Nc) #, censor=rep(1,Nc)) 

model = function() {  
  for(year in 1:N) {
    deaths[year] ~ dpois(mu[year])
    mu[year] <- mort.rate[year] * pop[year]
    mort.rate[year] ~ dgamma(alpha, beta)  
  }
  
  for(year in (N+1):Nc) {
    censored[year] ~ dinterval(deaths[year], c(0, 9)) 
    deaths[year] ~ dpois(mu[year])
    mu[year] <- mort.rate[year] * pop[year]
    mort.rate[year] ~ dgamma(alpha, beta)  
  }
  alpha ~ dexp(1.0)
  beta ~ dgamma(0.1, 1.0)
}

params = c('mu', 'mort.rate')
nchains = 2 

imu=rep(10,Nc)
ideaths = c(rep(NA,N), rep(5,(Nc-N)))
inits = list(deaths=ideaths, alpha=0.5, beta = 0.25)
myinits = list(inits, inits) 

jags_model = jags(data=ds, param=params, n.chains=nchains, inits = myinits, n.iter=10000, n.burnin=4000, model.file=model) 

deaths = jagsresults(x=jags_model, params=c('mu'))
r = data.frame(deaths=ds$deaths, deaths=round(deaths[,'50%'],0)) ; r 

par(mfrow=c(3,1))
x = rnorm(1000, mean=65, sd=5)
hist(x, 50, col='purple', border='white', main='sd=5')

x = rnorm(1000, mean=65, sd=20)
hist(x, 50, col='purple', border='white', main='sd=20')

x = rnorm(1000, mean=65, sd=100)
hist(x, 50, col='purple', border='white', main='sd=100')

