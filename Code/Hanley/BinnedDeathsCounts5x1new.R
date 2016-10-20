model.text <- "model {	
for (i in 1:n.rows) {
   reported.numbers[i] ~ dpois(mu[i])
   log(mu[i]) <- o[i] + b[ age.bin[i], year[i] ]
}
for (j in 1:n.age.bins) {
      b[j,1]~dnorm(-4,.1)
      for (k in 2:n.years) {
      b[j,k] ~ dnorm(b[j,k-1],tau1)
      }
}
tau1~dgamma(.01,.01)
}"


modelSomeCoarsenedData.text <- "model {	
for (i in 1:n.not.coarsened) {
   reported.numbers[i] ~ dpois(mu[i])
   log(mu[i]) <- o[i] + b[age.bin[i], year[i] ]
}

for (i in n.not.coarsened.plus.1:n.rows) {
   censored[i-n.not.coarsened] ~ dinterval(
                     reported.numbers[i], c(0, 9)) 
   reported.numbers[i] ~ dpois(mu[i])
   log(mu[i]) <- o[i] + b[ age.bin[i], year[i] ]
}  

for (j in 1:n.age.bins) {
      b[j,1]~dnorm(-4,.1)
      for (k in 2:n.years) {
      b[j,k] ~ dnorm(b[j,k-1],tau1)
      }
}
tau1~dgamma(.01,.01)
}"
	
	
library('rjags')

mcmcSomeCoarsened = function(dummy) {

 jags <- jags.model(textConnection(modelSomeCoarsenedData.text),
  data = list("reported.numbers" = reported.numbers,
          "age.bin"  = age.bin,
          "year" = year,
          "o" = o,
          "n.rows" = n.rows,
          "n.not.coarsened" = n.not.coarsened,
          "n.not.coarsened.plus.1" = n.not.coarsened.plus.1,
          "n.age.bins" = n.age.bins,
          "n.years" = n.years),
  inits = list(b=matrix(-4,n.age.bins,n.years)),
  n.chains = 1,
  n.adapt = 1000
 )

 update(jags, 4000)
 
 results=jags.samples(jags,c('b','tau1'), 4000)

 estimates= list(b.hat =
   round(apply(results$b[,,,1],c(1,2),mean),2),
   tau.hat = round(mean(results$tau1[1,,1]),2)
 )
  return(estimates)

} # end mcmcSomeCoarsened


mcmc = function(dummy) {

 jags <- jags.model(textConnection(model.text),
  data = list("reported.numbers" = reported.numbers,
          "age.bin"  = age.bin,
          "year" = year,
          "o" = o,
          "n.rows" = n.rows,
          "n.age.bins" = n.age.bins,
          "n.years" = n.years),
  inits = list(b=matrix(-4,n.age.bins,n.years)),
  n.chains = 1,
  n.adapt = 1000
 )

 update(jags, 4000)
 
 results=jags.samples(jags,c('b','tau1'), 4000)

 estimates= list(b.hat =
   round(apply(results$b[,,,1],c(1,2),mean),2),
   tau.hat = round(mean(results$tau1[1,,1]),2)
 )
  return(estimates)

} # end mcmc


setwd("/Users/jameshanley/Dropbox/work/PoissonRegressionBinnedCounts")

# data from human mortality database
# http://www.mortality.org

# deaths 

D = read.table("FinlandDeaths.txt",as.is=TRUE,header=TRUE)

D$Age[D$Age=="0"] = "00-01"
D$Age[D$Age=="1-4"] = "01-04"
D$Age[D$Age=="5-9"] = "05-09"
str(D)
D=D[!( D$Age %in% c("90-94","95-99",
             "100-104","105-109","110+") ) &
       D$Year >= 1969,]
D$YA = paste(D$Age,D$Year) 
str(D)
head(D); tail(D)
length(D$Year)

# Population

P=read.table("FinlandPopulation.txt",
 as.is=TRUE,header=TRUE)
P$Age[P$Age=="0"] = "00-01"
P$Age[P$Age=="1-4"] = "01-04"
P$Age[P$Age=="5-9"] = "05-09"

P=P[!( P$Age %in% c("90-94","95-99",
             "100-104","105-109","110+") ) &
       P$Year >= 1969,]
P$YA = paste(P$Age,P$Year)
length(P$Year)
head(P)

DP=merge(P,D,by.x="YA", by.y="YA"); head(DP)
n.rows = length(DP$Year.x) ; n.rows

n.subsets = 10  # ****** Finland too big, so divide in 50ths

m = matrix(NA,n.rows,n.subsets)

for(i in 1:n.rows) {
  m[i,] = rmultinom(1, size = DP$Female.y[i], 
             prob = rep(1/n.subsets, n.subsets) )
}
head(m)

Pop = DP[,4]/n.subsets  # for regression offset

ds = cbind(DP[,c(2,3)], Pop, m )
str(ds)
head(ds)

ds$year = ds$Year.x - min(ds$Year.x)+1
which.bin =function(x) which(x == unique(ds$Age.x) )
ds$age.bin=unlist(lapply(ds$Age.x,which.bin))
head(ds); tail(ds)

# MCMC   1 per subset

for(s in 1: n.subsets) {
	
	n.deaths = ds[,3+s]
	print(c(s,sum(n.deaths)))
	
	year = ds$year
	age.bin  = ds$age.bin
	
	o = log(ds$Pop)
	
	print(table(n.deaths)[1:12])
	
	UPPER.plus.1 = 10 
	
	coarsened = (n.deaths >=1 & n.deaths < UPPER.plus.1)
	not.coarsened = (n.deaths ==0 |n.deaths >= UPPER.plus.1)
	n.not.coarsened = sum(not.coarsened)
	n.coarsened = sum(coarsened)
	n.not.coarsened.plus.1 = n.not.coarsened+1
	n.all = n.coarsened + n.not.coarsened
	print( noquote(" "))
	print(c(n.not.coarsened,n.coarsened))
	print( noquote(" "))
    
    # for MCMC
  
    if(n.coarsened > 0 ) {
    
   age.bin =c(age.bin[not.coarsened], age.bin[coarsened] )
	 year    =c(   year[not.coarsened],    year[coarsened] )
	 o       =c(      o[not.coarsened],        o[coarsened] )
	 reported.numbers = c(n.deaths[not.coarsened],
	                      rep(NA,n.coarsened))
	 ds = data.frame(age.bin, year, reported.numbers, id=c(1:n.coarsened, 1:n.not.coarsened))
	}
	                    
	 n.age.bins = max(age.bin)
	 n.years    = max(year)
	
	if(n.coarsened == 0) mcmc.est = mcmc(0)
	if(n.coarsened  > 0) mcmc.est = mcmcSomeCoarsened(0) 
	# beta.from.mcmc[s,] = round(mcmc.est[[1]],4) 
}

# with.1 = mcmc.est   with.0 = mcmc.est

# jh. 2016.10.13

table( round( with.1$b.hat - with.0$b.hat ,2 ) )
c( with.0$tau.hat , with.1$tau.hat)




