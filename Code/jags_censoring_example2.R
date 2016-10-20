
#convincing myself this makes sense

ds1 = subset_data(race='Black', sex='Male', cod='Injuries')$ds 
head(ds1)

ds2 = ds1[order(ds1$censored),]
Ncensored = length(ds2$deaths[ds2$censored==FALSE])  
Nnotcensored = length(ds2$deaths) - Ncensored
Nagecats = unique(ds3$age) 

ds = list(deaths=ds3$deaths, pop=ds3$pop, censored=ds3$censored, Ncensored=Ncensored, 
          Nnotcensored=Nnotcensored)  

modelSomeCoarsenedData.text <- "model {	
for (i in 1:n.not.coarsened) {
reported.numbers[i] ~ dpois(mu[i])
log(mu[i]) <- o[i] + b[ age.bin[i], year[i] ]
}

for (i in n.not.coarsened.plus.1:n.rows) {
censored[i-n.not.coarsened] ~ dinterval(
reported.numbers[i], c(0, 9)) 
reported.numbers[i] ~ dpois(mu[i])
log(mu[i]) <- o[i] + b[ age.bin[i], year[i] ]
}  

for (j in 1:n.age.bins) {
b[j,1]~dnorm(-4,.1)
for (k in 2:n.years) {
b[j,k] ~ dnorm(b[j,k-1],tau1)
}
}
tau1~dgamma(.01,.01)
}"
	
model = function() {  
  for(year in 1:N) {
    deaths[year] ~ dpois(mu[year])
    log(mu[year]) <- mort.rate[year] + log(pop[year])
    
  }
  
  for(year in (N+1):Nc) {
    censored[year-N] ~ dinterval(deaths[year], c(0, 9)) 
    deaths[year] ~ dpois(mu[year])
    lo(mu[year]) <- mort.rate[year] + log(pop[year])
    mort.rate[year] ~ dgamma(alpha, beta)  
  }
  
  for (j in 1:n.age.bins) {
    mort.rate[j,1] ~ dnorm(-4,.1)
    for (k in 2:n.years) {
      b[j,k] ~ dnorm(b[j,k-1],tau1)
    }
  alpha ~ dexp(1.0)
  beta ~ dgamma(0.1, 1.0)
}


model_old = function() {  
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

