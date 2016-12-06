

# RUN-SCRIPT
# OBTAIN THE CREDIBLE INTERVALS FOR THE BAYESIAN SMOOTHING MODELS 
# NOV 29, 2016 

source('~/BlackWhiteMortalityGap/Code/4_smoothing_time_src.R')

# CR wrote this, will need to double check how it works 

setwd("~/bwm_results")


system.time(georgia_2013_females_thinned <- investigate_convergence(year = 2013, state = 'Georgia', sex = 'female', n_post_samp = 1000, "georgia_2013_females_thinned"))
#the above gives the error "non numeric arugment to binary operator" when n_post_samp == 1000, but I didn't encounter the error when 
#n_post_samp == 100. Need to determine what this is about, so going through the four functions called by investigate_convergence 1 by 1.
#first one:
system.time(mcmc_dist_g13ft <- extract_mcmc_dist(year = 2013, state = 'Georgia', sex = 'female', n_post_samp = 1000))
head(mcmc_dist_g13ft[[950]]) #for some reason all NAs starting at the 950th list element for the smoothed_deaths?
head(mcmc_dist_g13ft[[951]]) 
mcmc_dist_g13ft_2 <- mcmc_dist_g13ft[1:950] #so just store the first 950.

#then run the other functions:
saved_bayes = lapply(X = mcmc_dist_g13ft_2, FUN=life_expectancy_and_gap)
posterior.dataframe <- calc_running_med_CI_posterior(saved_bayes)
plots <- plot_estimate_CI_vs_posterior(posterior.dataframe, "georgia_2013_females_thinned", min = 10, max = 950)
georgia_2013_females_thinned <- list(mcmc_dist2, saved_bayes, posterior.dataframe, plots)
georgia_2013_females_thinned[[4]][1]
georgia_2013_females_thinned[[4]][2]
georgia_2013_females_thinned[[4]][3]

#utah
system.time(mcmc_dist_u69ft <- extract_mcmc_dist(year = 1969, state = 'Utah', sex = 'female', n_post_samp = 1000))
mcmc_dist_u69ft2 <- mcmc_dist_u69ft[1:950]
utah_1969_females_thinned <- investigate_convergence_rest(mcmc_dist_u69ft2, graph_file_name = "utah_1969_females_thinned", min = 10, max = 950)

#missouri
system.time(mcmc_dist_m85ft <- extract_mcmc_dist(year = 1985, state = 'Missouri', sex = 'female', n_post_samp = 1000))
mcmc_dist_m85ft2 <- mcmc_dist_m85ft[1:950]
miss_1985_females_thinned <- investigate_convergence_rest(mcmc_dist_m85ft2, graph_file_name = "missouri_1985_females_thinned", min = 10, max = 950)








system.time(saved_bayes_g13ft <- lapply(X = mcmc_dist_g13ft, FUN=life_expectancy_and_gap))
for (i in 1:1000) {
  temp.mcmc <- mcmc_dist_g13ft[[i]]
  test <- life_expectancy_and_gap(temp.mcmc)
  print(i)
  }
head(mcmc_dist_g13ft[[951]])
table(is.na(mcmc_dist_g13ft[[951]]$smooth_rate)) #all missing!

system.time(utah_1969_females_thinned <- investigate_convergence(year = 1969, state = 'Utah', sex = 'female', n_post_samp = 10, "utah_1969_females_thinned"))
system.time(missouri_1985_females_thinned <- investigate_convergence(year = 1985, state = 'Missouri', sex = 'female', n_post_samp = 10, "missouri_1985_females_thinned"))

save(list = c("utah_1969_females_thinned", "georgia_2013_females_thinned", "missouri_1985_females_thinned"), 
     file = '~/black_white_mortality_project/CI_vs_post_samples_results_thinned.RData')

