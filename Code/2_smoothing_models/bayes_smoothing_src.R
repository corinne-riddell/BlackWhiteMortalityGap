
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


 
source('~/BlackWhiteMortalityGap/Code/bayesian_smoothing/bayes_smoothing_functions.R') 
#source('life_expectancy_functions.R') 

load('~/SH_HA_data.Rdata')


