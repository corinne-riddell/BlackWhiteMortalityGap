
source("~/BlackWhiteMortalityGap/Code/entire_analysis.R")
source('~/BlackWhiteMortalityGap/Code/life_expectancy_functions.R') 
source("~/BlackWhiteMortalityGap/Code/2_smoothing_models/bayes_smoothing_functions.R")

state_i = 'Alabama'
n.cods = 6 

dat.clean <- prepare_environment(state_i)
r.All <- bayes_smoothing_bystate(dat.clean=dat.clean, n.cods = n.cods, seed=12345)
age_stdz_mortality_bystate(dat.clean, state_i, r.All) 
le.calculations <- life_expectancy_and_decomp(dat.clean, state_i, r.All)

systime1 <- system.time(dat.clean <- prepare_environment(state_i)) # 1.923
systime2 <- system.time(r.All <- bayes_smoothing_bystate(dat.clean = dat.clean, n.cods = n.cods, seed = 12345))   #736.76 seconds
systime3 <- system.time(age_stdz_mortality_bystate(dat.clean, state_i, r.All)) # 84.821 
systime4 <- system.time(le.calculations <- life_expectancy_and_decomp(dat.clean, state_i, r.All))  # 106.5 mins 
life_expectancy_and_decomp_pt2(le.calculations, r.All)


load('le_calculations_savedseed12345_A.RData')
le.calculationsA = le.calculations 

load('le_calculations_savedseed12345_B.RData')
le.calculationsB = le.calculations 


# 2-ish hours...still a lot but it's something 


le.calculationsA[[5]]$calcs$`1`$le.df$LE_White
le.calculationsB[[5]]$calcs$`1`$le.df$LE_White

le.calculationsA[[5]]$calcs$`1`$le.df$LE_Black
le.calculationsB[[5]]$calcs$`1`$le.df$LE_Black
