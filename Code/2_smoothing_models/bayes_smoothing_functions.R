

# prepare data for JAGS and put into list-by-COD format 
jagsify_data = function(ds_sub) { #subset by COD, put in lists 
  
  cods = unique(ds_sub$COD)
  n.cods = length(cods)
  ds_jagsified = list() 
  
  ds_sub$age.n = ds_sub$age.n - (min(ds_sub$age.n) - 1)
  ds_sub$year.n = ds_sub$year.n - (min(ds_sub$year.n) - 1)
  ds_sub$upper_bound = ifelse(ds_sub$population < 9, ds_sub$population, 9) 
  ds_sub$censored = ifelse(is.na(ds_sub$deaths), 1, 0)
  ds2_save = list()
  
  for(i in 1:n.cods) {
    
    ds2 = ds_sub[ds_sub$COD == cods[i], ]
    ds2_save[[i]] = ds2
    
    ds2 = ds2[order(ds2$censored), ]  
    
    ds_jagsified[[i]] = list(deaths = ds2$deaths, 
                   lnpop = log(ds2$population), 
                   age.bin = ds2$age.n, 
                   year = ds2$year.n, 
                   upper_bound = ds2$upper_bound, 
                   n.binned = sum(is.na(ds2$deaths)), 
                   n.not.binned = sum(!is.na(ds2$deaths)),
                   n.rows = sum(is.na(ds2$deaths)) + sum(!is.na(ds2$deaths)),
                   n.age.bins = length(unique(ds2$age.n)),
                   n.years = length(unique(ds2$year)), 
                   binned.id = ds2$censored,
                   RID = ds2$RID) 
    
                   
  }
  return(list(jagsified = ds_jagsified, ds_ordered = ds2_save))
}

# the jags model 
jags_smoothing_model = function(ds_jagsified_bycod, seed) { 
  
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
  
  set.seed(seed)
  jags_model = jags(data = ds_jagsified_bycod, param = params, n.chains = nchains,
                    inits = myinits, n.iter = 10000, n.burnin = 2000, n.thin = 8, model.file = model, DIC=FALSE)  #shortened to speed up for testing 
  
  return(jags_model)
}

# takes the jagsify_data output as a single parameter and runs the smoothing models
run_smoothing_model = function(data, n.cods, seed) {
  jags_model = list()  
  for(i in 1:n.cods) {
    jags_model[[i]] = jags_smoothing_model(data$jagsified[[i]], seed)
  } 
  return(jags_model)
}

# clean the otuput for analysis
clean_smoothing_results = function(data.jags, jags.model, n.cods) {
  #cods = unique(ds_sub$COD)
  n.age = jags.model[[1]]$model$data()$n.age.bins
  n.year = jags.model[[1]]$model$data()$n.years
  n.iters = jags.model[[1]]$BUGSoutput$n.keep
  result_allcods = list()
  result = c()
  
  for(cod_i in 1:n.cods) {
    
    p_mcmc = as.mcmc(jags.model[[cod_i]]) 
    p = melt(data.frame(p_mcmc[[1]])) 
    result = data.frame(smoothed_rate = exp(p$value))  
    result$post.samp = rep(1:n.iters, n.age*n.year)
    result$smoothed_rate = do.call(rbind, as.list(exp(jags.model[[cod_i]]$BUGSoutput$sims.array[ , 1,  ])))
    result$RID = rep(data.jags$ds_ordered[[cod_i]]$RID, each=n.iters)
    result$deaths = rep(data.jags$ds_ordered[[cod_i]]$deaths, each=n.iters)
    result$population = rep(data.jags$ds_ordered[[cod_i]]$population, each=n.iters)
    result$smoothed_deaths = result$population * result$smoothed_rate
    result_allcods[[cod_i]] = result 
  }
  results.final = do.call(rbind, result_allcods)
  return(results.final)
}

# pull all the functions together 

#run_analysis(ds = dat.clean, sex='Male', race = 'Black', n.cods=n.cods, seed=seed) 

run_analysis <- function(dataset, sex, race, n.cods, seed) {
  ds_sub = dataset[dataset$sex==sex & dataset$race==race, ] 
  data.jags = jagsify_data(ds_sub) ; data.jags[[1]]
  jags.model = run_smoothing_model(data.jags, n.cods=n.cods, seed=seed)  
  results.final = clean_smoothing_results(data.jags = data.jags, jags.model = jags.model, n.cods = n.cods)
  return(results.final)
} 



