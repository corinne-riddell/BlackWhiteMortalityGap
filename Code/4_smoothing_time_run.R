
# FOR MALES IN ALABAMA - PROOF OF CONCEPT 

  # Preamble
setwd("~/Documents/BlackWhiteMortalityGap/Code")
source('4_smoothing_time_src.R') 
source('4_smoothing_time_fun.R') 
source('life_expectancy_functions.R') 

# load full dataset 
load('/Users/kathryn/Dropbox/BlackWhiteGap/Data/main_datasets.Rdata')


# Run models for given population subset (state and gender) 

  r_m = run_smoothing_models(state='Alabama', sex='Male') 
  r_f = run_smoothing_models(state='Alabama', sex='Female') 

 # Calclate LE for a given population gender and year 

lt_males_69 = get_life_tables(jags_bw, sex='Male', year=1969)
lt_males_79 = get_life_tables(r_m, year=1979)


# Run models for all states and both genders 



r_bothsex_allstates = run_smoothing_models_allstates()







