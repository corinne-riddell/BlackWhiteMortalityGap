

ds_jags = subset_data(race='Black', sex='Male', cod='Injuries') 

  model = function() {  
    for(i in 1:n.not.binned) {
      deaths[i] ~ dpois(mu[i])
      log(mu[i]) <- lnrate[age.bin[i], year[i]] + lnpop[i]
    }
    
    for(i in n.not.binned.plus.1:n.rows) {
      binned.id[i] ~ dinterval(deaths[i], c(0, 9)) 
      deaths[i] ~ dpois(mu[i])
      log(mu[i]) <- lnrate[age.bin[i], year[i]] + lnpop[i]
    }
    
    for (j in 1:n.age.bins) {
      lnrate[j,1]~dnorm(-4, 0.1)
      for (k in 2:n.years) {
        lnrate[j,k] ~ dnorm(lnrate[j,k-1],tau)
      }
    } 
    tau~dgamma(0.01, 0.01)
  }
  
  params = c('mu', 'lnrate') 
  nchains = 2 

  
  ideaths = c(rep(NA, ds_jags$n.not.binned), rep(5, ds_jags$n.binned))
  ilnrate=matrix(-4,ds_jags$n.age.bins,ds_jags$n.years) 
  
  inits = list(deaths=ideaths, tau=0.001, lnrate=ilnrate)
  myinits = list(inits, inits) 
  
  jags_model = jags(data=ds_jags, param=params, n.chains=nchains, inits = myinits, n.iter=10000, n.burnin=2000, model.file=model) 

  jags_model
  
  
  
  