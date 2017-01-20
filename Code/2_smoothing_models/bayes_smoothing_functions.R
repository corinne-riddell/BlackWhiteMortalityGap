
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
    
    ds2 = ds_sub2[ds_sub2$COD == cods[i], ]
    ds2 = ds2[order(ds2$censored), ]  # check on this 
    
    ds_jagsified[[i]] = list(deaths = ds2$deaths, 
                   lnpop = log(ds2$population), 
                   age.bin = ds2$age.n, 
                   year = ds2$year, 
                   upper_bound = ds2$upper_bound, 
                   binned = is.na(ds2$deaths), 
                   not.binned = !is.na(ds2$deaths), 
                   n.binned = sum(is.na(ds2$deaths)), 
                   n.not.binned = sum(!is.na(ds2$deaths)),
                   n.not.binned.plus.1 = sum(!is.na(ds2$deaths)) + 1,
                   n.rows = sum(is.na(ds2$deaths)) + sum(!is.na(ds2$deaths)),
                   n.age.bins = length(unique(ds2$age.n)),
                   n.years = length(unique(ds2$year)), 
                   binned.id = ds2$censored) 
    
    }
  return(ds_jagsified)
}


run_smoothing_model = function(ds, ds_jagsified) {

  jags_smoothing_model = function(ds_jagsified_bycod) { 
    
    model = function() {  
      
      for(i in 1:n.not.binned) {
        deaths[i] ~ dpois(mu[i])
        log(mu[i]) <- lnrate[age.bin[i], year[i]] + lnpop[i]
      }
      
      for(i in n.not.binned.plus.1:n.rows) {
        binned.id[i] ~ dinterval(deaths[i], c(0, upper_bound[i])) 
        deaths[i] ~ dpois(mu[i])
        log(mu[i]) <- lnrate[age.bin[i], year[i]] + lnpop[i]
      }
      
      for (j in 1:n.age.bins) {
        lnrate[j,1]~dnorm(-4, 0.1)
        for (k in 2:n.years) {
          lnrate[j,k] ~ dnorm(lnrate[j,k-1],tau)
        }
      } 
      tau~dgamma(0.01, 0.01)
    }
    
    params = c('lnrate') 
    nchains = 2 
    
    ideaths = c(rep(NA, ds_jagsified_bycod$n.not.binned), rep(5, ds_jagsified_bycod$n.binned))
    ilnrate = matrix(-4,ds_jagsified_bycod$n.age.bins,ds_jagsified_bycod$n.years) 
    inits = list(deaths =ideaths, tau =0.001, lnrate =ilnrate)
    
    myinits = list(inits, inits) 
    
    jags_model = jags(data=ds_jagsified_bycod, param=params, n.chains=nchains,
                      inits = myinits, n.iter=10000, n.burnin=2000, model.file=model) 
    
    return(jags_model)
  } 
  
  cods = unique(ds$COD)
  n.cods = length(cods)
  jags_model = list()  
  for(i in 1:n.cods) {
    jags_model[[i]] = jags_smoothing_model(ds_jagsified[[i]])
  } 
  return(jags_model)
}



#testing 
ds_sub = subset_data(ds, state, sex, race)
ds_jagsified = jagsify_data(ds_sub)
jags_model = run_smoothing_model(ds, ds_jagsified)






clean_smoothing_results = function(ds_sub, jags_model) {
  
  ds_sub$smoothed_rate = rep(NA, length(ds_sub[ ,1]))
  
  cods = unique(ds_jagsified$COD)
  n.cods = length(cods)
  
  n.age = length(unique(ds_sub$age))
  
  for(k in 1:n.posterior.samples) { # n.posterior.samples = 1
    
    ds_sub$smoothed_rate = rep(NA, length(ds_sub[ ,1]))
    
      for(cod in 1:n.cods) {
        jags_modeli = jags_model[[cod]] 
        p_mcmc = as.mcmc(jags_modeli) 
        p = data.frame(p_mcmc[[1]])  
        
        
        for(age_bini in 1:n_agebins) {
          name_rate_w = paste0('lnrate_w.',age_bini, ".", Year, ".") 
          name_rate_b = paste0('lnrate_b.',age_bini, ".", Year, ".") 
          temp_df$smooth_rate[temp_df$age_bin==age_bini 
                              & temp_df$race=='White' 
                              & temp_df$cod==cod_list[codi]] = exp(p[name_rate_w])[[1]][k] 
          
          temp_df$smooth_rate[temp_df$age_bin==age_bini 
                              & temp_df$race=='Black' 
                              & temp_df$cod==cod_list[codi]] = exp(p[name_rate_b])[[1]][k] 
        }
      }
      temp_df$smoothed_deaths = temp_df$smooth_rate * temp_df$population
      temp_df_list[[k]] = temp_df
    }
  }




# old functions (for temporary reference) 

run_smoothing_models_mulitple_states = function(df, states, cod_list, 
                                                colname_state, colname_pop, colname_deaths, colname_cod, 
                                                colname_year, colname_sex, colname_agebins, colname_race ) {
  nstates = length(states) 
  results_male = results_female = list()
  ds =  create_dataset(df,  colname_state, colname_pop, colname_deaths, colname_cod, colname_year, colname_sex, colname_agebins, colname_race)
  for(i in 1:nstates) {
    print(states[i])
    results_male = run_smoothing_models(ds, state=states[i], sex='Male', cod_list=cod_list)
    results_female = run_smoothing_models(ds, state=states[i], sex='Female', cod_list=cod_list)
  }
  name1 = paste0('results_male_', states[i], '.RData') 
  name2 = paste0('results_female_', states[i], '.RData') 
  save(results_male, file=name1) 
  save(results_female, file=name2) 
}




extract_mcmc_dist = function(year, state, sex, n_post_samps, base_df, cod_list) {
  sex = ifelse(sex=='Female', 'female', sex)
  sex = ifelse(sex=='Male', 'male', sex)
  Sex = ifelse(sex=='female', 'Female', sex) 
  Sex = ifelse(sex=='male', 'Male', sex) 
  Year = year - 1968
  name = paste0('results_',sex, '_', state, '.RData') 
  load(name)
  temp_df_list = list()
  temp_df = base_df[base_df$sex==Sex & base_df$state == state & base_df$year==Year, ]
  n_agebins = length(unique(temp_df$age_bin))
  
  for(k in 1:n_post_samps) {
    
    temp_df$smooth_rate = rep(NA, length(temp_df$X))
    
    
    if(Sex == 'Female') {
    
    for(codi in 1:length(cod_list)) {
      jags_modeli = results_female$jags_bw[[codi]]
      p_mcmc = as.mcmc(jags_modeli) 
      p = data.frame(p_mcmc[[1]])  
      
      
      for(age_bini in 1:n_agebins) {
        name_rate_w = paste0('lnrate_w.',age_bini, ".", Year, ".") 
        name_rate_b = paste0('lnrate_b.',age_bini, ".", Year, ".") 
        temp_df$smooth_rate[temp_df$age_bin==age_bini 
                            & temp_df$race=='White' 
                            & temp_df$cod==cod_list[codi]] = exp(p[name_rate_w])[[1]][k] 
        
        temp_df$smooth_rate[temp_df$age_bin==age_bini 
                            & temp_df$race=='Black' 
                            & temp_df$cod==cod_list[codi]] = exp(p[name_rate_b])[[1]][k] 
      }
    }
    temp_df$smoothed_deaths = temp_df$smooth_rate * temp_df$population
    temp_df_list[[k]] = temp_df
    }
    
    if(Sex == 'Male') {
      
      for(codi in 1:length(cod_list)) {
        jags_modeli = results_male$jags_bw[[codi]]
        p_mcmc = as.mcmc(jags_modeli) 
        p = data.frame(p_mcmc[[1]])  
        
        
        for(age_bini in 1:n_agebins) {
          name_rate_w = paste0('lnrate_w.',age_bini, ".", Year, ".") 
          name_rate_b = paste0('lnrate_b.',age_bini, ".", Year, ".") 
          temp_df$smooth_rate[temp_df$age_bin==age_bini 
                              & temp_df$race=='White' 
                              & temp_df$cod==cod_list[codi]] = exp(p[name_rate_w])[[1]][k] 
          
          temp_df$smooth_rate[temp_df$age_bin==age_bini 
                              & temp_df$race=='Black' 
                              & temp_df$cod==cod_list[codi]] = exp(p[name_rate_b])[[1]][k] 
        }
      }
      temp_df$smoothed_deaths = temp_df$smooth_rate * temp_df$population
      temp_df_list[[k]] = temp_df
    }
    
  }
  return(temp_df_list) 
}




