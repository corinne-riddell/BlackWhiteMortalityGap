

#First you have to install JAGS on your computer (you can use it without R, just through the command line, but I've never done it cause screw that) 

# https://sourceforge.net/projects/mcmc-jags/?source=typ_redirect
#then install all these packages 

require(rjags)
require(R2jags) 
require(devtools)

# This is just a handy package to extract summaries by parameter, which is really nice if you want to automate extraction of results. You can also use coda if you want to summarize the posteriors yourself 

#install_github(repo='johnbaums/jagstools') 
require(jagstools)

# Simulate some simple data 
N = 1000
x = rnorm(N,5,1) 
mu = exp(1 + 0.5*x)
y = rpois(N, mu)

# Run a poisson GLM - you can also save a model externally in a .txt file 
model = function() {  
  for(i in 1:N) {
    y[i] ~ dpois(mu[i])  
    log(mu[i]) <- alpha + beta*x[i]
    #RR <- exp(beta) 
  }
  alpha  ~ dnorm(0,0.001)
  beta  ~ dnorm(0,0.001)
}  

# Create a JAGS-friendly dataset
ds = list('x','y','N')

# Define parameters to monitor 
params = c('alpha','beta','mu') 

# define inital values (you can do this manully, but it's nice to make a function like this so you can increase the # chains and it will automatically create the correct # of inits) 
nchains = 3
inits = function() list('alpha' = rnorm(1), 'beta' = rnorm(1) ) 

# run the model 
jags_model = jags(data=ds, param=params, n.chains=nchains, inits=inits,
                  n.iter=10000, n.burnin=2000, model.file=model) 

# view the model summmary 
print(jags_model)

# view the summary by parameter 
jagsresults(x=jags_model, params=c('alpha')) 
jagsresults(x=jags_model, params=c('beta')) 
jagsresults(x=jags_model, params=c('mu')) 


# Can also use jags.parallel to parallelize by chain, or you can use your model functions in mclapply to parallelize across iterations 

