

extract_mcmc_dist = function(year, state, sex, n_post_samps) {
  library(rjags)
  library(R2jags) 
  library(devtools)
  library(jagstools)
  library(gtools)
  library(bayesboot)
  library(coda)
  Sex = ifelse(sex=='female', 'Female', 'Male') 
  Year = year - 1968
  name = paste0('results_',sex, '_', state, '.RData') 
  load(name)
  temp_df_list = list()
  base_df = read.csv("base_df.csv")
  cod_list = c('Injuries', 'Cardiovascular', 'Cancers', 'Communicable', 
               'Non-communicable', 'All other causes')
  temp_df = base_df[base_df$sex==Sex & base_df$state == state & base_df$year==Year, ]
  n_agebins = length(unique(temp_df$age_bin))
  
  for(k in 1:n_post_samps) {
    
    temp_df$smooth_rate = rep(NA, length(temp_df$X))
    
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
    return(temp_df_list) 
}


