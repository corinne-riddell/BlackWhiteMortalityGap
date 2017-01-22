
# LIST OF FUNCTIONS
# subset_data_jags
# create_dataset
# bwmort_smooth_time
# combine_race_data
# merge_data
# run_smoothing_models
# get_allcod
# get_life_tables
# run_smoothing_models_mulitple_states
# extract_mcmc_dist

subset_data_jags = function(ds, race, sex, cod, state) { 
  
  ds2 = ds[order(ds$censored), ] 
  ds2 = ds2[ds2$state==state, ]
  
  ds_jags = list(deaths = ds2$deaths, 
                 lnpop = log(ds2$population), 
                 age.bin = ds2$agebins, 
                 year = ds2$year, 
                 upper_bound = ds2$upper_bound, 
                 binned = is.na(ds2$deaths), 
                 not.binned = !is.na(ds2$deaths), 
                 n.binned = sum(is.na(ds2$deaths)), 
                 n.not.binned = sum(!is.na(ds2$deaths)),
                 n.not.binned.plus.1 = sum(!is.na(ds2$deaths)) + 1,
                 n.rows = sum(is.na(ds2$deaths)) + sum(!is.na(ds2$deaths)),
                 n.age.bins = length(unique(ds2$agebins)),
                 n.years = length(unique(ds2$year)), 
                 binned.id = ds2$censored) 
}

create_dataset = function(df,  colname_state, colname_pop, colname_deaths, colname_cod, colname_year, colname_sex, colname_agebins, colname_race) {  

  selected_vars = c(colname_state, colname_pop, colname_deaths, colname_cod, colname_year, colname_sex, colname_agebins, colname_race)
  ds = df[ , selected_vars] 
  colnames(ds) = c('state', 'population', 'deaths', 'cod', 'year', 'sex', 'agebins', 'race')
  ds$agebins = ds$agebins - (min(ds$agebins)-1)
  ds$year = ds$year - (min(ds$year)-1)
  ds$upper_bound = ifelse(ds$population<9, ds$population, 9) 
  ds$censored = ifelse(is.na(ds$deaths),1,0)
  return(ds) 
} 

bwmort_smooth_time = function(ds_jags_bw) { 
  
  model = function() {  
    
    #BLACK 
    for(i in 1:n.not.binned_b) {
      deaths_b[i] ~ dpois(mu_b[i])
      log(mu_b[i]) <- lnrate_b[age.bin_b[i], year_b[i]] + lnpop_b[i]
    }
    
    for(i in n.not.binned.plus.1_b:n.rows_b) {
      binned.id_b[i] ~ dinterval(deaths_b[i], c(0, upper_bound_b[i])) 
      deaths_b[i] ~ dpois(mu_b[i])
      log(mu_b[i]) <- lnrate_b[age.bin_b[i], year_b[i]] + lnpop_b[i]
    }
    
    for (j in 1:n.age.bins_b) {
      lnrate_b[j,1]~dnorm(-4, 0.1)
      for (k in 2:n.years_b) {
        lnrate_b[j,k] ~ dnorm(lnrate_b[j,k-1],tau_b)
      }
    } 
    tau_b~dgamma(0.01, 0.01)
    
    #WHITE 
    for(i in 1:n.not.binned_w) {
      deaths_w[i] ~ dpois(mu_w[i])
      log(mu_w[i]) <- lnrate_w[age.bin_w[i], year_w[i]] + lnpop_w[i]
    }
    
    for(i in n.not.binned.plus.1_w:n.rows_w) {
      binned.id_w[i] ~ dinterval(deaths_w[i], c(0, upper_bound_w[i])) 
      deaths_w[i] ~ dpois(mu_w[i])
      log(mu_w[i]) <- lnrate_w[age.bin_w[i], year_w[i]] + lnpop_w[i]
    }
    
    for (j in 1:n.age.bins_w) {
      lnrate_w[j,1]~dnorm(-4, 0.1)
      for (k in 2:n.years_w) {
        lnrate_w[j,k] ~ dnorm(lnrate_w[j,k-1],tau_w)
      }
    } 
    tau_w~dgamma(0.01, 0.01)
    
  }
  
  params = c('lnrate_w', 'lnrate_b') 
  nchains = 2 
  
  ideaths_b = c(rep(NA, ds_jags_bw$n.not.binned_b), rep(5, ds_jags_bw$n.binned_b))
  ideaths_w = c(rep(NA, ds_jags_bw$n.not.binned_w), rep(5, ds_jags_bw$n.binned_w))
  ilnrate_b=matrix(-4,ds_jags_bw$n.age.bins_b,ds_jags_bw$n.years_b) 
  ilnrate_w=matrix(-4,ds_jags_bw$n.age.bins_w,ds_jags_bw$n.years_w) 
  inits = list(deaths_b=ideaths_b, deaths_w = ideaths_w, tau_b=0.001, tau_w=0.001, lnrate_b=ilnrate_b, 
               lnrate_w = ilnrate_w)
  myinits = list(inits, inits) 
  
  
  # RUN MCMC 
  
  #thinning model
  #jags_model = jags(data=ds_jags_bw, param=params, n.chains=nchains, n.thin=100,
  #                  inits = myinits, n.iter=100000, n.burnin=5000, model.file=model) 
  
  #no thinning
  jags_model = jags(data=ds_jags_bw, param=params, n.chains=nchains,
                    inits = myinits, n.iter=10000, n.burnin=2000, model.file=model) 
  
  return(jags_model)
} 

combine_race_data = function(dsb, dsw) {
  n = length(dsb) 
  names(dsb) = paste0(names(dsb),'_b') 
  names(dsw) = paste0(names(dsw),'_w') 
  return(c(dsb, dsw))
}

merge_data = function(ds, sex, state, cod_list) {
  
  ds_black = ds_white = ds_jags_bw = list() 
  N_COD = length(cod_list) 
  
  for(i in 1:N_COD) { 
    current_cod = cod_list[i] 
    ds_black = subset_data_jags(ds, race='Black', sex=sex, cod=current_cod, state=state) 
    ds_white = subset_data_jags(ds, race='White', sex=sex, cod=current_cod, state=state) 
    ds_jags_bw[[i]] = combine_race_data(ds_black, ds_white) 
  }
  return(ds_jags_bw)
}

run_smoothing_models = function(ds, state, sex, cod_list)  {  
  
  ds_jags_bw = merge_data(ds, sex=sex, state=state, cod_list=cod_list) 
  
  jags_bw = list() 
  
  N_COD = length(cod_list) 
  
  for(i in 1:N_COD) { 
    print(cod_list[i])
    jags_bw[[i]] = bwmort_smooth_time(ds_jags_bw[[i]])  
  }
  
  return(list(jags_bw=jags_bw, data=ds_jags_bw))
  
}



get_allcod = function(ds_jags_bw, r, race) {
  
  results_cod = list()
  N_COD = length(ds_jags_bw) 
  
  
  for(i in 1:N_COD) {
    
    if(race == 'Black') {
      smoothed_deaths = jagsresults(x=jags_bw[[i]], params=c('mu_b'))[, '50%']
      r = data.frame(year=ds_jags_bw[[i]]$year_b) 
      r$deaths = ds_jags_bw[[i]]$deaths_b
      r$smoothed_deaths = smoothed_deaths
      r$age_bins = ds_jags_bw[[i]]$age.bin_b
      r$pop = exp(ds_jags_bw[[i]]$lnpop_b)
      results_cod[[i]] = r[order(r$age_bins, r$year), ]
    }
    
    if(race == 'White') {
      smoothed_deaths = jagsresults(x=jags_bw[[i]], params=c('mu_w'))[, '50%']
      r = data.frame(year=ds_jags_bw[[i]]$year_w) 
      r$deaths = ds_jags_bw[[i]]$deaths_w
      r$smoothed_deaths = smoothed_deaths
      r$age_bins = ds_jags_bw[[i]]$age.bin_w
      r$pop = exp(ds_jags_bw[[i]]$lnpop_w)
      results_cod[[i]] = r[order(r$age_bins, r$year), ]
    }
  }
  
  
  temp = smoothed_allcod_deaths = c()
  nrows = length(results_cod[[1]]$smoothed_deaths)
  
  for(j in 1:nrows) { 
    for(i in 1:N_COD) { 
      temp[i] = results_cod[[i]]$smoothed_deaths[j]   
    }
    smoothed_allcod_deaths[j] = sum(temp) 
  }
  
  ds_allcod = data.frame(year=results_cod[[1]]$year, 
                         age_bin=results_cod[[1]]$age_bins,
                         pop = results_cod[[1]]$pop, 
                         smoothed_allcod_deaths) 
  return(ds_allcod)
}



get_life_tables = function(jags_bw, sex, year, state) {  #r = object from run_smoothing_models
  
  ds_jags_bw = merge_data(sex=sex, state=state)
  
  
  ds_allcod_black = get_allcod(ds_jags_bw, r, race='Black') 
  ds_allcod_white = get_allcod(ds_jags_bw, r, race='White') 
  
  year_current = year - 1968
  k = length(unique(ds_allcod_black$age_bin))
  ds_allcod_black$num_ages = c(1, 4, rep(5, k-2))
  ds_allcod_black = ds_allcod_black[ds_allcod_black$year==year_current, ]
  
  ds_allcod_white$num_ages = c(1, 4, rep(5, k-2))
  ds_allcod_white = ds_allcod_white[ds_allcod_white$year==year_current, ]
  
  
  lt_black = life.table(data=ds_allcod_black, num.ages.in.group='num_ages', 
                        death.counts='smoothed_allcod_deaths', population.counts='pop')
  
  lt_white = life.table(data=ds_allcod_white, num.ages.in.group='num_ages', 
                        death.counts='smoothed_allcod_deaths', population.counts='pop')
  
  return(list(lt_black=lt_black, lt_white=lt_white)) 
  
}



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




