
ds = subset_data(race='Black', sex='Male', cod='Injuries')$ds_jags 

head(ds)

model = function() {  
    for(year in 1:nyears) {
      for(agecat in 1:nagecats) {
        censor[year, agecat] ~ dinterval(deaths[year, agecat], c(0,200)) 
        deaths[year, agecat] ~ dpois( mu[year, agecat])
        mu[year, agecat] <- mort.rate[year, agecat] * pop[year, agecat]
        mort.rate[year, agecat] ~ dgamma(alpha, beta)  
      }
    }
    alpha ~ dexp(1.0)
    beta ~ dgamma(0.1, 1.0)
  }
  
  params = c('mu', 'mort.rate')
  nchains = 2
  
jags_model = jags(data=ds, param=params, n.chains=nchains, n.iter=10000, n.burnin=2000, model.file=model) 
  
jags_model

deaths = jagsresults(x=jags_model, params=c('mu'))
r = data.frame(deaths=ds$deaths, smoothed_deaths=round(deaths[,'50%'],0)) 







