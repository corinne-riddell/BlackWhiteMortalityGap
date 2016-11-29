
# RUN-SCRIPT
# BAYESIAN SMOOTHING MODELS FOR A GIVEN SET OF STATES 
# NOV 29, 2016 


setwd("~/BlackWhiteMortalityGap/Code/bayesian_smoothing")
source('bayes_smoothing_src.R') 

# Run models for all age-sex-year-race categories for a given state 
# variable 'states' is a vector of all 51 states; you can select any one or a subset or all

#Use <=4 cores during work hours or if the server is in use (htop to check); use up to 8 overnight when no one is using it 

# We will often want to run all states except the ones that have tiny populations, e.g.: 
excluded_states = unique(dat.clean$State2[dat.clean$Population<5]) 
included_states = setdiff(states, excluded_states) 

cod_list = c('Injuries', 'Cardiovascular', 'Cancers', 'Communicable', 
             'Non-communicable', 'All other causes')

load('~/black_white_mortality_project/main_datasets.Rdata') # change for SERVER 
#load('/Users/kathryn/Dropbox/BlackWhiteGap/Data/main_datasets.Rdata') # change for LOCAL 

colname_sex = 'Sex2'
colname_race = 'Race2'
colname_pop = 'Population'
colname_state = 'State2'
colname_cod = 'COD2'
colname_year = 'Year'
colname_deaths = 'Count'
colname_agebins = 'Age' 



mclapply(X=included_states, FUN=run_smoothing_models_mulitple_states, mc.cores=1, df=dat.clean, 
         cod_list, colname_state, colname_pop, colname_deaths, colname_cod, 
         colname_year, colname_sex, colname_agebins, colname_race) 










