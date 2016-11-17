
#Packages 

library(rjags)
library(R2jags) 
library(devtools)
library(jagstools)
library(ggplot2)
library(reshape2)

#load('../Data/alabama_only.Rdata')

dat.clean$upper_bound = ifelse(dat.clean$Population<9, dat.clean$Population, 9) 
#table(dat.clean$upper_bound)

