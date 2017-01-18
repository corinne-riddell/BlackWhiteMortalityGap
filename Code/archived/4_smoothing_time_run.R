
# RUN THE BAYESIAN SMOOTHING MODELS FOR A GIVEN SET OF STATES 

setwd("~/BlackWhiteMortalityGap/Code")
source('4_smoothing_time_src.R') 

# Run models for all age-sex-year-race categories for a given state 
# variable 'states' is a vector of all 51 states; you can select any one or a subset or all

#Use <=4 cores during work hours or if the server is in use (htop to check); use up to 8 overnight when no one is using it 


states_to_run = 'Alabama'
mclapply(X=states_to_run, FUN=run_smoothing_models_mulitple_states, mc.cores=4) 










