
# Subset dataset 

subset_data = function(race='Black', sex='Female', cod='Injuries') {

  selected_vars = c('Age', 'Sex2', 'Race2', 'COD2', 'Year', 'Count', 'Population')

  ds = dat.clean.alabama[(dat.clean.alabama$Race2==race & 
                         dat.clean.alabama$Sex2==sex & 
                          dat.clean.alabama$COD2==cod), selected_vars]
  
  df = data.frame(deaths=ds$Count, pop = ds$Population, age=(ds$Age+1), year=(ds$Year+1)) 
  
  # Need wide format for jags   
  
  nagecats = length(unique(ds$Age))  
  nyears = length(unique(ds$Year))  
  jags_deaths = as.data.frame(matrix(NA, nrow=nyears, ncol=nagecats)) 
  jags_pop = as.data.frame(matrix(NA, nrow=nyears, ncol=nagecats)) 
  
  for(year in 1:nyears) {
    for(agecat in 1:nagecats) {
      jags_deaths[year, agecat] = df$deaths[df$age==agecat& df$year==year]
      jags_pop[year, agecat] = df$pop[df$age==agecat& df$year==year]
    }
  }
  
  colnames(jags_deaths) = colnames(jags_pop) = unique(df$age)
  
  ds_jags = list(deaths = jags_deaths, 
                 pop = jags_pop) 
  
  ds_jags$nagecats = length(unique(ds$Age)) 
  ds_jags$nyears = length(unique(ds$Year)) 
  
  return(list(ds=ds, ds_jags=ds_jags)) 
}



# Run smoothing model v0: simple poisson estimation (no smooth)

jags_simple = function(ds) { 

  model = function() {  
    for(year in 1:nyears) {
      for(agecat in 1:nagecats) {
        deaths[year, agecat] ~ dpois( mu[year, agecat] ) 
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
  
  return(jags_model)
}



# Run smoothing model v1: smoothing by year 

jags_smooth_year = function(ds) { 

  model = function() {  
    for(year in 1:nyears) {
      for(agecat in 1:nagecats) {
        deaths[year, agecat] ~ dpois( mu[year, agecat] ) 
        mu[year, agecat] <- mort.rate[year, agecat] * pop[year, agecat]
      }
    }
    
    for(year in 2:nyears) {
      for(agecat in 1:nagecats) {
        mort.rate[year, agecat] ~ dnorm(mort.rate[year-1, agecat], 10000000) 
      }
    }
    
    for(agecat in 1:nagecats) {
        mort.rate[1, agecat] ~ dnorm(mort.rate.mean, 10000000) 
    }
    
    mort.rate.mean ~  dgamma(alpha,beta)
    alpha ~ dexp(1.0)
    beta ~ dgamma(0.1, 1.0)
    #tau ~ dgamma(0.1, 0.1) 
  }
  
  
  params = c('mu', 'mort.rate')
  nchains = 2
  inits = function() list('tau' = runif(1)) 
  
  jags_model = jags(data=ds, param=params, n.chains=nchains, inits=inits, n.iter=10000, n.burnin=2000, model.file=model) 
  
  return(jags_model)
  }
  


 
# Run smoothing model v2: smoothing by year and agecat 

jags_smooth_year_agecat = function(ds, pop_div=100) { 
  
  df = data.frame(deaths=ds$Count, pop = ds$Population/pop_div, age=(ds$Age+1), year=(ds$Year+1)) 
  
  # Need wide format for jags   
  
  nagecats = length(unique(ds$Age))  
  nyears = length(unique(ds$Year))  
  jags_deaths = as.data.frame(matrix(NA, nrow=nyears, ncol=nagecats)) 
  jags_pop = as.data.frame(matrix(NA, nrow=nyears, ncol=nagecats)) 
  
  for(year in 1:nyears) {
    for(agecat in 1:nagecats) {
      jags_deaths[year, agecat] = df$deaths[df$age==agecat& df$year==year]
      jags_pop[year, agecat] = df$pop[df$age==agecat& df$year==year]
    }
  }
  
  colnames(jags_deaths) = colnames(jags_pop) = unique(df$age)
  
  ds_jags = list(deaths = jags_deaths, 
                 pop = jags_pop) 
  
  ds_jags$nagecats = length(unique(ds$Age)) 
  ds_jags$nyears = length(unique(ds$Year)) 
  
  for(year in 1:ds_jags$nyears) {
    for(agecat in 1:ds_jags$nagecats) {
      if ( exp(ds_jags$pop[year, agecat]) == Inf) { 
        return('exp(pop) does not exist') 
      }
    }
  }
  
  
  model = function() {  
    for(year in 1:nyears) {
      for(agecat in 1:nagecats) {
        deaths[year, agecat] ~ dpois(mu[year, agecat]) 
        log(mu[year, agecat]) <- (pop[year, agecat] + b[year, agecat])
      }
    }
    
    for(year in 2:nyears) {
      for(agecat in 2:nagecats) {
        b[year, agecat] ~ dnorm(b[year-1, agecat-1], tau) 
      }
    }
    
    b[1, 2:19] ~ dnorm(-4,0.1)
    b[2:45, 1] ~ dnorm(-4,0.1)
    
    tau ~ dgamma(0.01, 0.01)
  }  
  
  b[1,1] ~ dnorm(-4,0.1)
  params = c('mu', 'b', 'tau') 
  nchains = 2
  inits = function() list('tau' = runif(1)) 
  
  jags_model = jags(data=ds_jags, param=params, n.chains=nchains, inits=inits,
                    n.iter=1000, n.burnin=200, model.file=model) 
  
  return(jags_model)
}

