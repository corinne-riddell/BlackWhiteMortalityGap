
library(rjags)
library(R2jags) 
library(devtools)
#install_github(repo='johnbaums/jagstools') 
library(jagstools)
library(ggplot2)
library(reshape2)
library(parallel)
library(gtools)
library(bayesboot)
library(coda)
library(ggplot2)
library(grid)
library(gridExtra)

source('bayes_smoothing_functions.R') 
#source('life_expectancy_functions.R') 


#dat.clean$upper_bound = ifelse(dat.clean$Population<9, dat.clean$Population, 9) 
states = unique(dat.clean$State2)
ds = dat.clean



