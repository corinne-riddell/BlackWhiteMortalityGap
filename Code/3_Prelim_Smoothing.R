
# Preamble

require(rjags)
require(R2jags) 
require(devtools)
require(jagstools)

load('/Users/kathryn/Dropbox/BlackWhiteGap/Data/alabama_only.Rdata')
head(dat.clean.alabama)

selected_vars = c('Age2', 'Sex2', 'Race2', 'COD2', 'Year2', 'Count', 'Population')
ds = dat.clean.alabama[ dat.clean.alabama$COD2=='Injuries', selected_vars] 
head(ds) ; length(ds$Age2) 

ds1 = ds[(ds$Race2=='Black' & ds$Age2=='60-64 years'), ] 
nyears = length(ds1$Age2)
ds_jags = list( deaths=ds1$Count, pop = ds1$Population, nyears = nyears) 


# Exploring the data a bit
summary(ds1$Count/ds1$Population)

# More reasonable prior?  

# Just smoothing by years, for one age group

ds_jags2 = ds_jags1 = ds_jags
ds_jags1$pop =  ds_jags$pop/100


ds_jags$pop = seq(100,500,by=10)
exp(ds_jags$pop)
ds_jags$deaths = seq(10,50,by=1) 
ds_jags$nyears = length(ds_jags$deaths)

ds_jags2 = ds_jags
ds_jags2$pop = ds_jags$pop/5


model = function() {  
  for(year in 1:nyears) {
    deaths[year] ~ dpois(mu[year]) 
    log(mu[year]) <- (pop[year] + b[year])
    mort_rate[year] <- exp(b[year])
    
  }
  
  for(year in 2:nyears) {
    b[year] ~ dnorm(b[year-1], tau) 
  }
  
  b[1] ~ dnorm(-4, 0.1) 
  beta ~ dnorm(0, 0.1) 
  tau ~ dgamma(0.01, 0.01)
}  

params = c('mu', 'b', 'tau', 'mort_rate')

nchains = 2
inits = function() list('tau' = runif(1)) 

# run the model 
jags_model = jags(data=ds_jags, param=params, n.chains=nchains, inits=inits,
                  n.iter=10000, n.burnin=2000, model.file=model) 

jags_model2 = jags(data=ds_jags2, param=params, n.chains=nchains, inits=inits,
                  n.iter=10000, n.burnin=2000, model.file=model) 


num_deaths1 = jagsresults(x=jags_model, params=c('mu'))  
num_deaths2 = jagsresults(x=jags_model2, params=c('mu'))  

cbind( ds_jags$deaths, num_deaths1[,'50%'],  num_deaths2[,'50%'])


mort_rate1 = jagsresults(x=jags_model, params=c('mort_rate'))
mort_rate2 = jagsresults(x=jags_model2, params=c('mort_rate'))  

d = data.frame(d=ds_jags$deaths, m1=mort_rate1[,'50%'], m2=mort_rate2[,'50%'])


