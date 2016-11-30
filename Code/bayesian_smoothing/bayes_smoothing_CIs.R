

# RUN-SCRIPT
# OBTAIN THE CREDIBLE INTERVALS FOR THE BAYESIAN SMOOTHING MODELS 
# NOV 29, 2016 

source('~/BlackWhiteMortalityGap/Code/4_smoothing_time_src.R') #CR note: I don't know why this doesn't work so i just manually run this file.

# CR wrote this, will need to double check how it works 

setwd("~/bwm_results")


system.time(georgia_2013_females_thinned <- investigate_convergence(year = 2013, state = 'Georgia', sex = 'female', n_post_samp = 1000, "georgia_2013_females_thinned"))
#mcmc_dist <- extract_mcmc_dist(year=2013, state='Georgia', sex="female", n_post_samp=2)
#saved_bayes = lapply(X = mcmc_dist, FUN=life_expectancy_and_gap)
#posterior.dataframe <- calc_running_med_CI_posterior(saved_bayes)
#plots <- plot_estimate_CI_vs_posterior(posterior.dataframe, "testweds")

utah_1969_females_thinned <- investigate_convergence(year = 1969, state = 'Utah', sex = 'female', n_post_samp = 1000, "utah_1969_females_thinned")
missouri_1985_females_thinned <- investigate_convergence(year = 1985, state = 'Missouri', sex = 'female', n_post_samp = 1000, "missouri_1985_females_thinned")

save(list = c("utah_1969_females_thinned", "georgia_2013_females_thinned", "missouri_1985_females_thinned"), 
     file = '~/black_white_mortality_project/CI_vs_post_samples_results_thinned.RData')

