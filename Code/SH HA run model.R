
setwd("~/BlackWhiteMortalityGap/Code/bayesian_smoothing")
source('bayes_smoothing_src.R') 
load("~/SH_HA_data.Rdata")


excluded_states <- unique(dtotal$State2[dtotal$Population < 5]) 
states <- unique(dtotal$State2)
included_states <- setdiff(states, excluded_states) 

dtotal.alabama <- subset(dtotal, State2 == "Alabama")
cod_list <- c('Total')
#cod_list <- c('Cancers')


dtotal$COD3 <- "Cancers"
dtotal$COD4 <- factor(dtotal$COD3)

colname_sex = 'Sex2'
colname_race = 'Race2'
colname_pop = 'Population'
colname_state = 'State2'
colname_cod = 'COD'
#colname_cod = 'COD4'
colname_year = 'Year'
colname_deaths = 'Count'
colname_agebins = 'Age' 

##########################################################################################
# Kathryn shooting troubles  
##########################################################################################

ds_Al = dtotal.alabama[, c(colname_state, colname_pop, colname_deaths, colname_cod, 
                       colname_year, colname_sex, colname_agebins, colname_race)]

ds_T = dtotal[, c(colname_state, colname_pop, colname_deaths, colname_cod, 
                           colname_year, colname_sex, colname_agebins, colname_race)]

head(ds_Al)
head(ds_T)

tail(ds_Al)
tail(ds_T)

table(is.na(ds_Al$Count))
table(is.na(ds_T$Count))

setwd("~/black_white_mortality_project/SH_HA_compare")

length(ds_Al$State2)

states = 'Alabama'
run_smoothing_models_mulitple_states(ds_T, states, cod_list, colname_state, colname_pop, colname_deaths, 
                                     colname_cod, colname_year, colname_sex, colname_agebins, colname_race) 
#Error in node binned.id_w[72015]
# Well there should only be 1520 rows when subset by one state, so this is clearly not subsetting properly! And CR's manual subset fixes that issue.

run_smoothing_models_mulitple_states(ds_Al, states, cod_list, colname_state, colname_pop, colname_deaths, 
                                     colname_cod, colname_year, colname_sex, colname_agebins, colname_race) 
str(ds_Al)
str(ds_T)

# I somehow removed part of the code that subsets by state in the functions - now it's fixed! 


##########################################################################################
  
##########################################################################################
  

#error when running the code below:
time.to.run <- system.time(mclapply(X = "Alabama", FUN=run_smoothing_models_mulitple_states, mc.cores=4, df=dtotal, 
                     cod_list, colname_state, colname_pop, colname_deaths, colname_cod, 
                     colname_year, colname_sex, colname_agebins, colname_race) )

#note for KTM: this model worked!
time.to.run <- system.time(mclapply(X = "Alabama", FUN=run_smoothing_models_mulitple_states, mc.cores=4, df=dtotal.alabama, 
                                    cod_list, colname_state, colname_pop, colname_deaths, colname_cod, 
                                    colname_year, colname_sex, colname_agebins, colname_race) )

source('~/BlackWhiteMortalityGap/Code/life_expectancy_functions.R') 


#note for KTM: wanting to get the following line to run. 
#need to generalize the extract_mcmc_dist() function -- cod_list is hard-coded in there and I think base_df will need to be changed
time.a <- system.time(alabama_1990_males_SHpaper<- investigate_convergence(year = 1990, state = "Alabama", sex = "female", n_post_samp = 20, "alabama_1990_males_SHpaper", 10, 950))


# Notes from KTM
# Now we need to provide a base dataframe for the resutls and a list of the causes of death. 
# The mysterious base df is a dataset to which your smoothed results will be appended. Include all the variables and 'smoothed deaths' will be added and then you can sort/subset as desired. 
# The base dataframe can contain all of the states, sexes, and years and will be subset as needed within the extract_mcmc_dist function. 

base_df = read.csv("~/black_white_mortality_project/base_df.csv")
cod_list = c('Injuries', 'Cardiovascular', 'Cancers', 'Communicable', 
             'Non-communicable', 'All other causes')

extract_mcmc_dist(year, state, sex, n_post_samps, base_df, cod_list) 


