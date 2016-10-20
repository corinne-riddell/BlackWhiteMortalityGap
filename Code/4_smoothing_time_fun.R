

# Subset dataset 
subset_data = function(race='Black', sex='Female', cod='Injuries') {

  selected_vars = c('Age', 'Sex2', 'Race2', 'COD2', 'Year', 'Count', 'Population')

  ds = dat.clean.alabama[(dat.clean.alabama$Race2==race & 
                         dat.clean.alabama$Sex2==sex & 
                          dat.clean.alabama$COD2==cod), selected_vars]
  
  df = data.frame(deaths=ds$Count, pop = ds$Population, age=(ds$Age+1), year=(ds$Year+1)) 
  df$censored = ifelse(is.na(df$deaths),1,0)
  
  ds2 = df[order(df$censored), ]
  
  ds_jags = list(deaths = ds2$deaths, 
                 lnpop = log(ds2$pop), 
                 age.bin = ds2$age, 
                 year = ds2$year, 
                 binned = is.na(ds2$deaths), 
                 not.binned = !is.na(ds2$deaths), 
                 n.binned = sum(is.na(ds2$deaths)), 
                 n.not.binned = sum(!is.na(ds2$deaths)),
                 n.not.binned.plus.1 = sum(!is.na(ds2$deaths)) + 1,
                 n.rows = sum(is.na(ds2$deaths)) + sum(!is.na(ds2$deaths)),
                 n.age.bins = length(unique(ds2$age)),
                 n.years = length(unique(ds2$year)), 
                 binned.id = ds2$censored) 
  
  return(ds_jags) 
} 

combine_race_data = function(ds1, ds2) {
  n = length(ds1) 
    names(ds1) = paste0(names(ds1),'_b') 
    names(ds2) = paste0(names(ds2),'_w') 
   
    return(c(ds1, ds2))
}


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
  


  # MODEL INITILIZATION FOR MCMC 
  params = c('mu_w', 'mu_b') 
  nchains = 2 
  
  ideaths_b = c(rep(NA, ds_jags_bw$n.not.binned_b), rep(5, ds_jags_bw$n.binned_b))
  ideaths_w = c(rep(NA, ds_jags_bw$n.not.binned_w), rep(5, ds_jags_bw$n.binned_w))
  ilnrate_b=matrix(-4,ds_jags_bw$n.age.bins_b,ds_jags_bw$n.years_b) 
  ilnrate_w=matrix(-4,ds_jags_bw$n.age.bins_w,ds_jags_bw$n.years_w) 
  inits = list(deaths_b=ideaths_b, deaths_w = ideaths_w, tau_b=0.001, tau_w=0.001, lnrate_b=ilnrate_b, 
               lnrate_w = ilnrate_w)
  myinits = list(inits, inits) 
  
  
  # RUN MCMC 
  jags_model = jags(data=ds_jags_bw, param=params, n.chains=nchains, inits = myinits, n.iter=10000, n.burnin=2000, model.file=model) 
  
  return(jags_model)
} 



