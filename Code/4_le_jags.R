

# Preamble
setwd("~/Documents/BlackWhiteMortalityGap/Code")
source('4_smoothing_time_src.R') 
source('4_smoothing_time_fun.R') 
source('life_expectancy_functions.R') 

# load full dataset 
load('/Users/kathryn/Dropbox/BlackWhiteGap/Data/main_datasets.Rdata')

ds = dat.clean.alabama[dat.clean.alabama$Sex2=='Female' &
                         dat.clean.alabama$Race2=='Black', ]

head(ds)

# JAGS model with binned likelihood and smoothing by year  
bwmort_smooth_time = function(ds_jags_bw) { 
  
  model = function() {  
    
    #BLACK 
    for(i in 1:n.not.binned_b) {
      deaths_b[i] ~ dpois(mu_b[i])
      log(mu_b[i]) <- lnrate_b[age.bin_b[i], year_b[i]] + lnpop_b[i]
    }
    
    for(i in n.not.binned.plus.1_b:n.rows_b) {
      binned.id_b[i] ~ dinterval(deaths_b[i], c(0, 9)) 
      deaths_b[i] ~ dpois(mu_b[i])
      log(mu_b[i]) <- lnrate_b[age.bin_b[i], year_b[i]] + lnpop_b[i]
    }
    
    for (j in 1:n.age.bins_b) {
      lnrate_b[j,1]~dnorm(-4, 0.1)
      for (k in 2:n.years_b) {
        lnrate_b[j,k] ~ dnorm(lnrate_b[j,k-1],tau_b)
      }
    } 
    tau_b~dgamma(0.01, 0.01)
    
    #WHITE 
    for(i in 1:n.not.binned_w) {
      deaths_w[i] ~ dpois(mu_w[i])
      log(mu_w[i]) <- lnrate_w[age.bin_w[i], year_w[i]] + lnpop_w[i]
    }
    
    for(i in n.not.binned.plus.1_w:n.rows_w) {
      binned.id_w[i] ~ dinterval(deaths_w[i], c(0, 9)) 
      deaths_w[i] ~ dpois(mu_w[i])
      log(mu_w[i]) <- lnrate_w[age.bin_w[i], year_w[i]] + lnpop_w[i]
    }
    
    for (j in 1:n.age.bins_w) {
      lnrate_w[j,1]~dnorm(-4, 0.1)
      for (k in 2:n.years_w) {
        lnrate_w[j,k] ~ dnorm(lnrate_w[j,k-1],tau_w)
      }
    } 
    tau_w~dgamma(0.01, 0.01)
    
  }
}
  