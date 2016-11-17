
# Standard errors for life expectancy from jags MCMC objects

# (1) Open original JAGS model object files

setwd('/Users/kathryn/Desktop/bwm_results/')
source('/Users/kathryn/Documents/BlackWhiteMortalityGap/Code/life_expectancy_functions.R') 
source('/Users/kathryn/Documents/BlackWhiteMortalityGap/Code/4_smoothing_SE_fun.R') 
year = 1969
state = 'Alabama'
sex = 'female' 
post_samp = 1 # will need to iterate through these 1 to 1000 
saved_bayes = list() 

# comp time 

time_2  = system.time(extract_mcmc_dist(year=1969, state='Alabama', sex='female', n_post_samp=2))
time_5  = system.time(extract_mcmc_dist(year=1969, state='Alabama', sex='female', n_post_samp=5))
time_10 = system.time(extract_mcmc_dist(year=1969, state='Alabama', sex='female', n_post_samp=10))
time_20 = system.time(extract_mcmc_dist(year=1969, state='Alabama', sex='female', n_post_samp=20))

plot(x=c(2,5,10,20), y=c(time_2[[3]], time_5[[3]], time_10[[3]], time_20[[3]]), type='b')

system.time(life_expectancy_and_gap(p[[1]]))

# Prelim 
# Georgia, Missouri, Utah 
# Need to be in the directory that contains the .RData files e.g., results_female_Utah.RData
# Something like setwd(~/bwm_results) 
# Calculate CI for 1000, then investigate from 10 to 1000 by 1 
p = extract_mcmc_dist(year=1969, state='Alabama', sex='female', n_post_samp=2)
saved_bayes = lapply(X = p, FUN=life_expectancy_and_gap) 






