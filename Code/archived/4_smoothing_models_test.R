
# Exploring the modelling with simple simulated data to make sure I'm not wrong about all the things

source('/Users/kathryn/Dropbox/BlackWhiteGap/Code/4_smoothing_models_src.R') 

set.seed(1001)
N = 500
mean_mortrate = runif(N, 0.002, 0.8)
pop = round(runif(N,50000,100000),0)
deaths = round(mean_mortrate * pop)

model = function() {  
  
  for(i in 1:N) {
    deaths[i] ~ dpois(mu[i])
    mu[i] <- mortrate[i]*pop[i]
  }
  
  for(i in 2:(N)) {
    mortrate[i] ~ dnorm(mortrate[i-1], tau)
  }
  
  mortrate[1] ~ dnorm(0.05, tau)
  tau ~ dgamma(0.01, 0.01)
}  

params = c('mu', 'mortrate') 
nchains = 2
#inits = function() list('tau' = runif(1)) 

ds_jags = list(N=N, deaths=deaths, pop=pop)

jags_model = jags(data=ds_jags, param=params, n.chains=nchains, 
                  n.iter=1000, n.burnin=200, model.file=model) 

mu1 = jagsresults(x=jags_model, params=c('mu'))[,1]
mortrate1 = jagsresults(x=jags_model, params=c('mortrate'))[,1]


d1 = data.frame(mu1, mu2, diff=(mu1-mu2)/mu1)
d2 = data.frame(mortrate1, mortrate2, diff=(mortrate1-mortrate2)/mortrate1)

hist(mortrate[ ,1], 50, col='lightgreen')
mean(mortrate[, '50%'])



# selecting priors

tau = 10000
x = seq(0, 0.1, 0.0001)
y = dnorm(x, mean=0.02, sd=1/sqrt(tau))
plot(x,y, type='l')





# Gavin's example of rw smoothing 
# Single site, one pollutant (note likelihood calculations because of the cyclical model)
n = 1000
tau.v = 0.1
y = rnorm(n, mean=10, sd=1/sqrt(tau.v))


ds_GS = list(n=n, y=y) 


model_GS= function() {
  for (t in 2:(n-1)) {
    y[t] ~ dnorm(mu[t],tau.v)
    mu[t] <- (theta[t-1]+theta[t+1])/2
    theta[t] ~ dnorm(mu[t],tau.w)
}
  for (t in 1:n) {
    theta.adj[t] <- theta.adj[t]-mean(theta[])
  }

theta[1]~dnorm(theta[2],tau.w)
theta[n]~dnorm(theta[n-1],tau.w)
y[1]~dnorm(theta[1],tau.v)
y[n]~dnorm(theta[n],tau.v)         

tau.v ~ dgamma(1,0.01)
tau.w ~ dgamma(1,0.01)

sigma.v<-1/sqrt(tau.v)
sigma.w <- 1 / sqrt(tau.w)
}

params = c('theta')
nchains = 2
#inits = function() list('tau' = runif(1)) 

jags_model = jags(data=ds_GS, param=params, n.chains=nchains, 
                  n.iter=1000, n.burnin=200, model.file=model_GS) 









