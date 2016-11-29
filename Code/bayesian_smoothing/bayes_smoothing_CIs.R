

# RUN-SCRIPT
# OBTAIN THE CREDIBLE INTERVALS FOR THE BAYESIAN SMOOTHING MODELS 
# NOV 29, 2016 

setwd("~/BlackWhiteMortalityGap/Code")
source('4_smoothing_time_src.R') 

# CR wrote this, will need to double check how it works 

#utah_1969_females <- investigate_convergence(year=1969, state='Utah', sex='female', n_post_samp=1000, "utah_1969_females")
#georgia_2013_females <- investigate_convergence(year=2013, state='Georgia', sex='female', n_post_samp=1000, "georgia_2013_females")
#missouri_1985_females <- investigate_convergence(year=1985, state='Missouri', sex='female', n_post_samp=1000, "missouri_1985_females")

#save(list = c("utah_1969_females", "georgia_2013_females", "missouri_1985_females"), file = '~/black_white_mortality_project/CI_vs_post_samples_results.RData')

