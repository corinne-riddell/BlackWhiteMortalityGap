
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
                   binned = is.na(ds2$deaths), 
                   not.binned = !is.na(ds2$deaths), 
                   n.binned = sum(is.na(ds2$deaths)), 
                   n.not.binned = sum(!is.na(ds2$deaths)),
                   n.rows = sum(is.na(ds2$deaths)) + sum(!is.na(ds2$deaths)),
                   n.age.bins = length(unique(ds2$age.n)),
                   n.years = length(unique(ds2$year)), 
                   binned.id = ds2$censored,
                   cod = cods[i]) 
    
    }
  return(list(jagsified = ds_jagsified, sub = ds_sub))
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
clean_smoothing_results = function(data, jags_model) {
  ds_sub = data$sub
  cods = unique(ds_sub$COD)
  n.cods = length(cods)
  n.age = length(unique(ds_sub$age))
  n.year = length(unique(ds_sub$year))

  for(cod_i in 1:n.cods) {
    p_mcmc <- as.mcmc(jags_model[[cod_i]]) 
    p <- melt(data.frame(p_mcmc[[1]]))
    rm(p_mcmc)
    p <- p[p$variable != "deviance", ]
    p$post.samp <- rep(1:1000, n.year*n.age)
    p$new.cols <- str_split_fixed(p$variable, "\\.", 3)
    p <- p[ , -1]
    names(p)[1] <- c("smoothed_lnrate")
    p$year.n <- p$new.cols[ , 3]
    p$age.n <- p$new.cols[ , 2]
    p <- p[ , -3]
    p$year.n <- stringr::str_sub(p$year.n, 1, -2)
    p$year.n <- as.numeric(p$year.n)
    p$age.n <- as.numeric(p$age.n)
    p$smoothed_rate <- exp(p$smoothed_lnrate)
    p$COD <- cods[cod_i]
    
    assign(paste0("smooth.rates.1000.", stringr::str_sub(cods[cod_i],1, 3)), p)
  }
  
  smoothed.rates.1000 <- rbind(smooth.rates.1000.Car, smooth.rates.1000.Can, smooth.rates.1000.Com,
                               smooth.rates.1000.Non, smooth.rates.1000.Inj, smooth.rates.1000.All)
  
  return(merge(data$sub, smoothed.rates.1000, by = c("year.n", "age.n", "COD")))
}
 

