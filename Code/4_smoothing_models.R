


source('/Users/kathryn/Dropbox/BlackWhiteGap/Code/4_smoothing_models_src.R') 


data = subset_data(race='Black', sex='Female', cod='Injuries') 
head(data) 

age_cats = unique(data$Age) 

ds = data[data$Age2==age_cats[3], ] 

m = run_jags_model(ds, pop_div=100) ; m 

smoothed_deaths = jagsresults(x=m, params=c('mu'))  
mort_rate = jagsresults(x=m, params=c('b'))


r = data.frame(deaths=ds$Count, smoothed_deaths=round(smoothed_deaths[,'50%'],0)) 

r



