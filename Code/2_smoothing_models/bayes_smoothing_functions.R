
# User-defined subsets 
subset_data = function(ds, sex, race) {
  ds_sub = ds[ds$sex==sex & ds$race==race, ]
  return(ds_sub)
}

# Prepare data for JAGS and put into list-by-COD format 
jagsify_data = function(ds_sub) { #subset by COD, put in lists 
  
  cods = unique(ds_sub$COD)
  n.cods = length(cods)
  ds_jagsified = list() 
  
  ds_sub$age.n = ds_sub$age.n - (min(ds_sub$age.n) - 1)
  ds_sub$year.n = ds_sub$year.n - (min(ds_sub$year.n) - 1)
  ds_sub$upper_bound = ifelse(ds_sub$population < 9, ds_sub$population, 9) 
  ds_sub$censored = ifelse(is.na(ds_sub$deaths), 1, 0)
  
  for(i in 1:n.cods) {
    
    ds2 = ds_sub[ds_sub$COD == cods[i], ]
    ds2 = ds2[order(ds2$censored), ]  # check on this 
    
    ds_jagsified[[i]] = list(deaths = ds2$deaths, 
                   lnpop = log(ds2$population), 
                   age.bin = ds2$age.n, 
                   year = ds2$year.n, 
                   upper_bound = ds2$upper_bound, 
                   #binned = is.na(ds2$deaths), 
                   #not.binned = !is.na(ds2$deaths), 
                   n.binned = sum(is.na(ds2$deaths)), 
                   n.not.binned = sum(!is.na(ds2$deaths)),
                   n.rows = sum(is.na(ds2$deaths)) + sum(!is.na(ds2$deaths)),
                   n.age.bins = length(unique(ds2$age.n)),
                   n.years = length(unique(ds2$year)), 
                   binned.id = ds2$censored,
                   RID = ds2$RID) 
                   #cod = cods[i]) 
  }
  
  ds_sub_onlyRID = ds_sub[ ,c('RID', 'deaths')] 
  return(list(jagsified = ds_jagsified, sub = ds_sub_onlyRID))
}

jags_smoothing_model = function(ds_jagsified_bycod) { 
  
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
  nchains = 1  #we're convinced it's converged, let's minimize computation 
  
  ideaths = c(rep(NA, ds_jagsified_bycod$n.not.binned), rep(5, ds_jagsified_bycod$n.binned))
  ilnrate = matrix(-4, ds_jagsified_bycod$n.age.bins, ds_jagsified_bycod$n.years) 
  inits = list(deaths = ideaths, tau = 0.001, lnrate = ilnrate)
  
  myinits = list(inits) 
  
  jags_model = jags(data = ds_jagsified_bycod, param = params, n.chains = nchains,
                    inits = myinits, n.iter = 10000, n.burnin = 2000, n.thin = 8, model.file = model, DIC=FALSE)  #shortened to speed up for testing 
  
  return(jags_model)
}

# Takes the jagsify_data output as a single parameter and runs the smoothing models
run_smoothing_model = function(data, n.cods) {
  n.cods = n.cods
  jags_model = list()  
  for(i in 1:n.cods) {
    jags_model[[i]] = jags_smoothing_model(data$jagsified[[i]])
  } 
  return(jags_model)
}

# Takes the jagsify_data output, the run_smoothing_model output, and the desired year as parameters
# Outputs the original data, subset by sex, race, state, and year (not age or COD), with smoothed rates and smoothed/imputed deaths 
clean_smoothing_results = function(data.jags, jags.model, n.cods) {
  #cods = unique(ds_sub$COD)
  n.age = jags.model[[1]]$model$data()$n.age.bins
  n.year = jags.model[[1]]$model$data()$n.years
  n.iters = jags.model[[1]]$BUGSoutput$n.keep
  result_allcods = list()
  
  for(cod_i in 1:n.cods) {
    p_mcmc = melt(jags.model[[cod_i]]$BUGSoutput$sims.matrix) 
    result = data.frame(RID = rep(data.jags$jagsified[[cod_i]]$RID, each=n.iters))
    result$smoothed_rate = exp(p_mcmc$value)
    result$postID = p_mcmc$Var1
    result_allcods[[cod_i]] = result 
  }
  
  results.final = do.call(rbind, result_allcods)
  return(results.final)
}
 
 
# testing params 
# dataset = dat.clean ; state1 = 'Albama' ; sex1 = 'Male' ; race1 = 'Black' 
run_analysis <- function(dataset, state1, sex1, race1, n.cods) {
  ds_sub = subset_data(ds = dataset, sex = sex1, race = race1)
  data.jags = jagsify_data(ds_sub)
  jags.model = run_smoothing_model(data.jags, n.cods=n.cods)  
  results.final = clean_smoothing_results(data = data.jags, jags.model = jags.model, n.cods=n.cods)
  return(results.final)
} 




# testing function? 
run_analysis_on_subset <- function(subsetted_data) {
  data.jags = jagsify_data(subsetted_data)
  jags.model = run_smoothing_model(data.jags)  
  return(clean_smoothing_results(data.jags = data.jags, jags_model = jags.model))
}


