
load('~/SH_HA_data.Rdata')

source("~/BlackWhiteMortalityGap/Code/2_smoothing_models/bayes_smoothing_src.R")


ds = dtotal ; sex='Male' ; race = 'White' ; state = 'Alabama' 

ds_sub = subset_data(ds, state, sex, race)
data = jagsify_data(ds_sub)
jags_model = run_smoothing_model(data)

r1990 = clean_smoothing_results(data, jags_model, year=1990, n.posterior.samples=1)
r2009 = clean_smoothing_results(data, jags_model, year=2009, n.posterior.samples=1)


lt1 = life.table(data = r1990[[1]], num.ages.in.group = "age_bin_size",
                 death.counts = "smoothed_deaths", population.counts = "population")
round(lt1$e_x[1], 1) 


lt2 = life.table(data = r2009[[1]], num.ages.in.group = "age_bin_size",
                 death.counts = "smoothed_deaths", population.counts = "population")
round(lt2$e_x[1], 1) 





