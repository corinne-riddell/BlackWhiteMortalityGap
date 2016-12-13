

# RUN-SCRIPT
# OBTAIN THE CREDIBLE INTERVALS FOR THE BAYESIAN SMOOTHING MODELS 
# NOV 29, 2016 

source('~/BlackWhiteMortalityGap/Code/bayesian_smoothing/bayes_smoothing_src.R')

# CR wrote this, will need to double check how it works 

setwd("~/bwm_results")

time.a <- system.time(alabama_1990_males_thinned <- investigate_convergence(year = 1990, state = "Alabama", sex = "male", n_post_samp = 20, "alabama_1990_males_thinned", 10, 950))

#note for KTM: see line 246 of 4_smoothing_time_fun.R -- I think that line needs to be generalized to also include males. The code below produces an error
#if I change sex = "male".
time.a <- system.time(alabama_1990_females_thinned <- investigate_convergence(year = 1990, state = "Alabama", sex = "female", n_post_samp = 1000, "alabama_1990_females_thinned", 10, 950))
time.b <- system.time(alabama_2009_females_thinned <- investigate_convergence(year = 2009, state = "Alabama", sex = "female", n_post_samp = 1000, "alabama_2009_females_thinned", 10, 950))

time.c <- system.time(missouri_1990_females_thinned <- investigate_convergence(year = 1990, state = "Missouri", sex = "female", n_post_samp = 1000, "missouri_1990_females_thinned", 10, 950))
time.d <- system.time(missouri_2009_females_thinned <- investigate_convergence(year = 2009, state = "Missouri", sex = "female", n_post_samp = 1000, "missouri_2009_females_thinned", 10, 950))

time.e <- system.time(utah_1990_females_thinned <- investigate_convergence(year = 1990, state = "Utah", sex = "female", n_post_samp = 1000, "utah_1990_females_thinned", 10, 950))
time.f <- system.time(utah_2009_females_thinned <- investigate_convergence(year = 2009, state = "Utah", sex = "female", n_post_samp = 1000, "utah_2009_females_thinned", 10, 950))

wv_1990_females_thinned <- investigate_convergence(year = 1990, state = "West Virginia", sex = "female", n_post_samp = 1000, "wv_1990_females_thinned", 10, 950)
wv_2009_females_thinned <- investigate_convergence(year = 2009, state = "West Virginia", sex = "female", n_post_samp = 1000, "wv_2009_females_thinned", 10, 950)

neb_1990_females_thinned <- investigate_convergence(year = 1990, state = "Nebraska", sex = "female", n_post_samp = 1000, "neb_1990_females_thinned", 10, 950)
neb_2009_females_thinned <- investigate_convergence(year = 2009, state = "Nebraska", sex = "female", n_post_samp = 1000, "neb_2009_females_thinned", 10, 950)

ny_1990_females_thinned <- investigate_convergence(year = 1990, state = "New York", sex = "female", n_post_samp = 1000, "ny_1990_females_thinned", 10, 950)
ny_2009_females_thinned <- investigate_convergence(year = 2009, state = "New York", sex = "female", n_post_samp = 1000, "ny_2009_females_thinned", 10, 950)

cali_1990_females_thinned <- investigate_convergence(year = 1990, state = "California", sex = "female", n_post_samp = 1000, "cali_1990_females_thinned", 10, 950)
cali_2009_females_thinned <- investigate_convergence(year = 2009, state = "California", sex = "female", n_post_samp = 1000, "cali_2009_females_thinned", 10, 950)

flor_1990_females_thinned <- investigate_convergence(year = 1990, state = "Florida", sex = "female", n_post_samp = 1000, "flor_1990_females_thinned", 10, 950)
flor_2009_females_thinned <- investigate_convergence(year = 2009, state = "Florida", sex = "female", n_post_samp = 1000, "flor_2009_females_thinned", 10, 950)

dc_1990_females_thinned <- investigate_convergence(year = 1990, state = "Washington DC", sex = "female", n_post_samp = 1000, "dc_1990_females_thinned", 10, 950)
dc_2009_females_thinned <- investigate_convergence(year = 2009, state = "Washington DC", sex = "female", n_post_samp = 1000, "dc_2009_females_thinned", 10, 950)

ark_1990_females_thinned <- investigate_convergence(year = 1990, state = "Arkansas", sex = "female", n_post_samp = 1000, "ark_1990_females_thinned", 10, 950)
ark_2009_females_thinned <- investigate_convergence(year = 2009, state = "Arkansas", sex = "female", n_post_samp = 1000, "ark_2009_females_thinned", 10, 950)

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

save(list = c("utah_1969_females_thinned", "georgia_2013_females_thinned", "miss_1985_females_thinned"), 
     file = '~/black_white_mortality_project/CI_vs_post_samples_results_thinned.RData')

saved_bayes_test <- lapply(X = miss_1985_females_thinned[[1]][1:950], FUN=life_expectancy_and_gap)
post.test <- calc_running_med_CI_posterior(saved_bayes_test)
post.test2 <- calc_running_med_CI_age(saved_bayes_test)
post.test2b <- calc_running_med_CI_cod(saved_bayes_test)
post.test2c <- calc_running_med_CI_codage(saved_bayes_test)

post.test3 <- plot_estimate_CI_vs_posterior(LE_distn = post.test, age_decomp = post.test2, cod_decomp = post.test2b, 
                                            codage_decomp = post.test2c, min = 10, max = 950, file.name = "testdec7")

