
# User-defined subsets 
subset_data = function(ds, state, sex, race) {
  ds_sub = ds[ds$state==state & ds$sex==sex & ds$race==race, ]
  return(ds_sub)
}

# Prepare data for JAGS and put into list-by-COD format 
jagsify_data = function(ds_sub) { #subset by COD, put in lists 
  
  cods = unique(ds_sub$COD)
  n.cods = length(cods)
  ds_jagsified = list() 
  
  ds_sub$age.n = ds_sub$age.n - (min(ds_sub$age.n)-1)
  ds_sub$year.n = ds_sub$year.n - (min(ds_sub$year.n)-1)
  ds_sub$upper_bound = ifelse(ds_sub$population<9, ds_sub$population, 9) 
  ds_sub$censored = ifelse(is.na(ds_sub$deaths),1,0)
  
  for(i in 1:n.cods) {
    
    ds2 = ds_sub[ds_sub$COD == cods[i], ]
    ds2 = ds2[order(ds2$censored), ]  # check on this 
    
    ds_jagsified[[i]] = list(deaths = ds2$deaths, 
                   lnpop = log(ds2$population), 
                   age.bin = ds2$age.n, 
                   year = ds2$year.n, 
                   upper_bound = ds2$upper_bound, 
                   binned = is.na(ds2$deaths), 
                   not.binned = !is.na(ds2$deaths), 
                   n.binned = sum(is.na(ds2$deaths)), 
                   n.not.binned = sum(!is.na(ds2$deaths)),
                   #n.not.binned.plus.1 = sum(!is.na(ds2$deaths)) + 1,
                   n.rows = sum(is.na(ds2$deaths)) + sum(!is.na(ds2$deaths)),
                   n.age.bins = length(unique(ds2$age.n)),
                   n.years = length(unique(ds2$year)), 
                   binned.id = ds2$censored) 
    
    }
  return(list(jagsified = ds_jagsified, sub = ds_sub))
}

jags_smoothing_model = function(ds_jagsified_bycod) {  #can this function live outside of run_smoothing_model? - i moved it and am testing it
  
  model = function() {  
    
    for(i in 1:n.not.binned) {
      deaths[i] ~ dpois(mu[i])
      log(mu[i]) <- lnrate[age.bin[i], year[i]] + lnpop[i]
    }
    
    for(i in (n.not.binned + 1):n.rows) {
      binned.id[i] ~ dinterval(deaths[i], c(0, upper_bound[i])) 
      deaths[i] ~ dpois(mu[i])
      log(mu[i]) <- lnrate[age.bin[i], year[i]] + lnpop[i]
    }
    
    for (j in 1:n.age.bins) {
      lnrate[j, 1] ~ dnorm(-4, 0.1)
      for (k in 2:n.years) {
        lnrate[j, k] ~ dnorm(lnrate[j, k-1],tau)
      }
    } 
    tau ~ dgamma(0.01, 0.01)
  }
  
  params = c('lnrate') 
  nchains = 2 
  
  ideaths = c(rep(NA, ds_jagsified_bycod$n.not.binned), rep(5, ds_jagsified_bycod$n.binned))
  ilnrate = matrix(-4, ds_jagsified_bycod$n.age.bins, ds_jagsified_bycod$n.years) 
  inits = list(deaths = ideaths, tau = 0.001, lnrate = ilnrate)
  
  myinits = list(inits, inits) 
  
  jags_model = jags(data = ds_jagsified_bycod, param = params, n.chains = nchains,
                    inits = myinits, n.iter = 10000, n.burnin = 2000, model.file = model) 
  
  return(jags_model)
}

# Takes the jagsify_data output as a single parameter and runs the smoothing models
run_smoothing_model = function(data) {

#removed the other function 
  
  cods = unique(data$sub$COD)
  n.cods = length(cods)
  jags_model = list()  
  for(i in 1:n.cods) {
    jags_model[[i]] = jags_smoothing_model(data$jagsified[[i]])
  } 
  return(jags_model)
}

# Takes the jagsify_data output, the run_smoothing_model output, and the desired year as parameters
# Outputs the original data, subset by sex, race, state, and year (not age or COD), with smoothed rates and smoothed/imputed deaths 
clean_smoothing_results = function(data, jags_model, year, n.posterior.samples) {
  ds_sub = data$sub
  cods = unique(ds_sub$COD)
  n.cods = length(cods)
  n.age = length(unique(ds_sub$age))
  year_id = data$sub$year.n[data$sub$year == year][1]
  
  results_list = list()
  
  ds_sub_year = ds_sub[ds_sub$year.n == year_id, ]
  ds_sub_year$smoothed_rate = rep(NA, length(ds_sub_year[ ,1]))
  
  for(k in 1:n.posterior.samples) { 
    
    for(cod_i in 1:n.cods) {
      
      p_mcmc = as.mcmc(jags_model[[cod_i]]) 
      p = data.frame(p_mcmc[[1]])  
      
      for(age_i in 1:n.age) {
        name_rate = paste0('lnrate.', age_i, ".", year_id, ".") 
        
        ds_sub_year$smoothed_rate[ds_sub_year$age.n == age_i & 
                                    ds_sub_year$COD == cods[cod_i]] = exp(p[name_rate])[[1]][k] 
      }
    }
    #ds_sub_year$smoothed_deaths = ds_sub_year$smoothed_rate * ds_sub_year$population
    results_list[[k]] = ds_sub_year
  }
  return(results_list)
}


clean_smoothing_results_faster = function(data, jags_model, year, n.posterior.samples) {
  ds_sub = data$sub
  cods = unique(ds_sub$COD)
  n.cods = length(cods)
  n.age = length(unique(ds_sub$age))
  year_id = data$sub$year.n[data$sub$year == year][1]
  
  results_list = list() 
  
  ds_sub_year = ds_sub[ds_sub$year.n == year_id, ]
  ds_sub_year$smoothed_rate = rep(NA, length(ds_sub_year[ ,1]))
  
  for(k in 1:n.posterior.samples) { 

    for(cod_i in 1:n.cods) {
      
      p_mcmc = as.mcmc(jags_model[[cod_i]]) 
      p = data.frame(p_mcmc[[1]])  
      
      for(age_i in 1:n.age) {
        name_rate = paste0('lnrate.', age_i, ".", year_id, ".") 
        
        ds_sub_year$smoothed_rate[ds_sub_year$age.n == age_i & 
                                    ds_sub_year$COD == cods[cod_i]] = exp(p[name_rate])[[1]][k] 
      }
    }
    #ds_sub_year$smoothed_deaths = ds_sub_year$smoothed_rate * ds_sub_year$population
    results_list[[k]] = ds_sub_year
  }
  return(results_list)
}


 

