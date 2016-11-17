
# Standard errors for life expectancy from jags MCMC objects

# (1) Open original JAGS model object files

setwd('/Users/kathryn/Desktop/bwm_results/')
year = 1969
state = 'Alabama'
sex = 'female' 
post_samp = 1 # will need to iterate through these 1 to 1000 


for(i in 1:100) {

p = extract_mcmc_dist(year=1969, state='Alabama', sex='female', post_samp=i) 
corinne_function(p) 

}


extract_mcmc_dist = function(year, state, sex, post_samp) {
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
  
  base_df = read.csv("base_df.csv")
  temp_df = base_df[base_df$sex==Sex & base_df$state == state & base_df$year==Year, ]
  temp_df$smooth_rate = rep(NA, length(temp_df$X))
  
  current_selected_rate = c()
  
  cod_list = c('Injuries', 'Cardiovascular', 'Cancers', 'Communicable', 
                         'Non-communicable', 'All other causes')
  
  
  for(codi in 1:length(cod_list)) {
    jags_modeli = results_female$jags_bw[[codi]]
    p_mcmc = as.mcmc(jags_modeli) 
    p = data.frame(p_mcmc[[1]])  
    
    n_agebins = length(unique(temp_df$age_bin))
    for(age_bini in 1:n_agebins) {
      name_rate_w = paste0('lnrate_w.',age_bini, ".", Year, ".") 
      name_rate_b = paste0('lnrate_b.',age_bini, ".", Year, ".") 
      temp_df$smooth_rate[temp_df$age_bin==age_bini & temp_df$race=='White' & temp_df$cod==cod_list[codi]] = exp(p[name_rate_w])[[1]][post_samp] 
      temp_df$smooth_rate[temp_df$age_bin==age_bini & temp_df$race=='Black' & temp_df$cod==cod_list[codi]] = exp(p[name_rate_b])[[1]][post_samp] 
    }
  }
  temp_df$smoothed_deaths = temp_df$smooth_rate * temp_df$population
  return(df=temp_df) 
}
  





# Reference 




temp = read.csv('/Users/kathryn/Dropbox/BlackWhiteGap/smoothed_results.csv')

base_df = temp[, c('X', 'race', 'deaths', 'population', 'age_bin', 'year', 'sex', 'state', 'cod')]  
head(base_df)

write.csv(base_df, 'base_df.csv', row.names=FALSE) 

library(coda)
load('/Users/kathryn/Desktop/bwm_results/results_female_Alabama.RData')

jags_modeli = results_female$jags_bw[[1]]
class(jags_modeli)

p_mcmc = as.mcmc(jags_modeli) 
p = data.frame(p_mcmc[[2]])   

pp = p['mu_w.1.']
hist(pp$mu_w.1.)




# ------ 

source('/Users/kathryn/Documents/BlackWhiteMortalityGap/Code/life_expectancy_functions.R')

dim(results_female$jags_bw[[1]]$BUGSoutput$sims.array)

mcmc_samples = data.frame(p = matrix(results_female$jags_bw[[1]]$BUGSoutput$sims.array))
head(mcmc_samples)
summary(mcmc_samples$p)








# obtain the life tables for both races  
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

