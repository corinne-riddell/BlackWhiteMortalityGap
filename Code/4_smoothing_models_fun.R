
# Subset dataset 

subset_data = function(race='Black', sex='Female', cod='Injuries') {

  selected_vars = c('Age', 'Sex2', 'Race2', 'COD2', 'Year', 'Count', 'Population')

  ds = dat.clean.alabama[(dat.clean.alabama$Race2==race & 
                         dat.clean.alabama$Sex2==sex & 
                          dat.clean.alabama$COD2==cod), selected_vars]
  
  df = data.frame(deaths=ds$Count, pop = ds$Population, age=(ds$Age+1), year=(ds$Year+1)) 
  
  df$censored = ifelse(is.na(df$deaths),1,0)
  
  # Need wide format for jags, ordered by censoring ID 
  
  nagecats = length(unique(ds$Age))  
  nyears = length(unique(ds$Year))  
  jags_deaths = as.data.frame(matrix(NA, nrow=nyears, ncol=nagecats)) 
  jags_pop = as.data.frame(matrix(NA, nrow=nyears, ncol=nagecats)) 
  jags_censored = as.data.frame(matrix(NA, nrow=nyears, ncol=nagecats)) 
  
  for(year in 1:nyears) {
    for(agecat in 1:nagecats) {
      jags_deaths[year, agecat] = df$deaths[df$age==agecat& df$year==year]
      jags_pop[year, agecat] = df$pop[df$age==agecat& df$year==year]
      jags_censored[year, agecat] = is.na(df$deaths[df$age==agecat& df$year==year])
    }
  }
  
  colnames(jags_deaths) = colnames(jags_pop) = unique(df$age)
  
  ds_jags = list(deaths = jags_deaths, 
                 pop = jags_pop, 
                 censored = jags_censored) 
  
  ds_jags$nagecats = length(unique(ds$Age)) 
  ds_jags$nyears = length(unique(ds$Year)) 
  
  return(list(ds=df, ds_jags=ds_jags)) 
}


# JAGS model without temporal or age-category smoothing 

jags_simple = function(ds) { 
  
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
  ideaths = c(rep(NA,N), rep(1,(Nc-N)))
  inits = list(deaths=ideaths, alpha=0.5, beta = 0.25)
  myinits = list(inits, inits) 
  
  jags_model = jags(data=ds, param=params, n.chains=nchains, inits = myinits, n.iter=10000, n.burnin=4000, model.file=model) 
  
  return(jags_model)
}

