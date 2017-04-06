# ---- How are JAGS data ordered / what order are the results returned in?

#write.csv(ds2, 'test_data_mcmcoptim.csv', row.names=FALSE)
ds = read.csv('test_data_mcmcoptim.csv')
ds2 = ds[order(ds$censored), ]  

cbind(ds$censored, ds2$censored)
    
ds_jagsified = list(deaths = ds2$deaths, 
                             lnpop = log(ds2$population), 
                             age.bin = ds2$age.n, 
                             year = ds2$year.n, 
                             upper_bound = ds2$upper_bound, 
                             n.binned = sum(is.na(ds2$deaths)), 
                             n.not.binned = sum(!is.na(ds2$deaths)),
                             n.rows = sum(is.na(ds2$deaths)) + sum(!is.na(ds2$deaths)),
                             n.age.bins = length(unique(ds2$age.n)),
                             n.years = length(unique(ds2$year)), 
                             binned.id = ds2$censored,
                             RID = ds2$RID) 

  model = function() {  
    
    for(i in 1:n.not.binned) {
      deaths[i] ~ dpois(mu[i])
      log(mu[i]) <- lnrate[age.bin[i], year[i]] + lnpop[i]
    }
    
    for(i in (n.not.binned + 1):n.rows) {
      binned.id[i] ~ dinterval(deaths[i], c(0, upper_bound[i])) 
      deaths[i] ~ dpois(mu[i])
      log(mu[i]) <- lnrate[age.bin[i], year[i]] + lnpop[i]
    }
    
   
    for (j in 1:n.age.bins) {
      lnrate[j, 1] ~ dnorm(-4, 0.1)
      for (k in 2:n.years) {
        lnrate[j, k] ~ dnorm(lnrate[j, k-1],tau)
      }
    } 
    tau ~ dgamma(0.01, 0.01)
  }
  
  params = c('lnrate') 
  nchains = 1  #we're convinced it's converged, let's minimize computation 
  
  ideaths = c(rep(NA, ds_jagsified$n.not.binned), rep(5, ds_jagsified$n.binned))
  ilnrate = matrix(-4, ds_jagsified$n.age.bins, ds_jagsified$n.years) 
  inits = list(deaths = ideaths, tau = 0.001, lnrate = ilnrate)
  
  myinits = list(inits) 
  
  jags_model = jags(data = ds_jagsified, param = params, n.chains = nchains,
                    inits = myinits, n.iter = 10000, n.burnin = 2000, n.thin = 8, model.file = model, DIC=FALSE)  #shortened to speed up for testing 
  
  
  # output as: rates[agebin 1:n, year 1:n]  

jags.model = jags_model   

n.age.bins = 19
n.years = 45
lnrate = matrix(NA, nrow=n.age.bins*n.years, ncol=2)
for (j in 1:n.age.bins) {
  lnrate[j, 1]  = j*1
  for (k in 2:n.years) {
    lnrate[j, k] = j*k
  }
} 


head(ds)
  

p_mcmc <- as.mcmc(jags_model) 
p <- melt(data.frame(p_mcmc[[1]])) ; head(p)

n.iters = jags.model$BUGSoutput$n.keep
result = data.frame(smoothed_rate = exp(p$value))  ; head(result)
n.age = jags.model$model$data()$n.age.bins
n.year = jags.model$model$data()$n.years

result$post.samp = rep(1:n.iters, each=n.age*n.year) ; head(result)
#result$RID = rep(ds$RID, each=n.iters)

head(result)

ds_ordered = ds[, c('age.n', 'year.n', 'deaths', 'population', 'RID')] 
head(ds_ordered)

length(ds_ordered$age.n)

# does it match summary - YES 
rate_mean = cbind(exp(jags.model$BUGSoutput$summary[, 'mean']))
smoothed_deaths = rate_mean*ds_ordered$population
deaths = ds_ordered$deaths
plot(smoothed_deaths, deaths) 


# does it match matrix/array/list format- yes! 

smoothed_rate = do.call(rbind, as.list(exp(jags.model$BUGSoutput$mean$lnrate)))
smoothed_deaths2 = ds_ordered$population * smoothed_rate
plot(smoothed_deaths2, deaths, main='test2') 


result = data.frame(post.samp=rep(1:n.iters, n.age*n.year))
result$smoothed_rate = do.call(rbind, as.list(exp(jags.model$BUGSoutput$sims.array[ , 1,  ])))
head(result)
result$RID = rep(ds_ordered$RID, each=n.iters)




result$population = rep(ds_ordered$population, each= n.iters)
result$smoothed_deaths = result$population  * result$smoothed_rate
result$deaths = rep(ds_ordered$deaths, each = n.iters)



head(result)
plot(result$deaths, result$smoothed_deaths, main='please work') 




