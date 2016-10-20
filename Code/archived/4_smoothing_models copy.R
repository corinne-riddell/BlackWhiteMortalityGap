
  # Preamble
setwd("~/Documents/BlackWhiteMortalityGap/Code")
source('4_smoothing_models_src.R') 

  # Data
ds = subset_data(race='Black', sex='Male', cod='Injuries') 
ds_jags = ds$ds_jags 
jags_model = jags_simple(ds_jags)  

  # Run models 


  # Review results 


jags_model = jags_smooth_year(ds$ds_jags) 

smoothed_deaths = jagsresults(x=jags_model, params=c('mu'))  
mort_rate = jagsresults(x=jags_model, params=c('mort.rate'))

r = data.frame(deaths=ds$ds$Count, smoothed_deaths=round(smoothed_deaths[,'50%'],0)) 

r[is.na(r$deaths)==TRUE, ]



imputed = r$smoothed_deaths[is.na(r$deaths)==TRUE]
table(imputed<10)
hist(imputed)




