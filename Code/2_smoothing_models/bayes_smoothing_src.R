library(purrr)
library(ggplot2)
library(rjags)
library(R2jags) 
library(devtools)
#install_github(repo='johnbaums/jagstools') 
library(jagstools)
library(reshape2)
library(parallel)
library(gtools)
library(bayesboot)
library(coda)
library(grid)
library(gridExtra)


source("~/BlackWhiteMortalityGap/Code/2_smoothing_models/bayes_smoothing_functions.R")
source('~/BlackWhiteMortalityGap/Code/life_expectancy_functions.R') 




