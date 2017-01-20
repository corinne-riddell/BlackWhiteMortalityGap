#setwd("/Users/jameshanley/Dropbox/work/PoissonRegressionBinnedCounts")
 
setwd("~/Documents/BlackWhiteMortalityGap/Code/Hanley")
# data from human mortality database
# http://www.mortality.org

# deaths  (> 30 y of age for now)

D = read.table("FinlandDeaths.txt",as.is=TRUE,header=TRUE) ; head(D)
D$Age=as.numeric(D$Age) 
D=D[!is.na(D$Age) & D$Year >= 1969 & D$Age >= 30, ] #subset for non-null age and year/age categories
D$YA = D$Year*1000+D$Age #unique age/year ID 
head(D); tail(D)
length(D$Year)

# Population

P=read.table("FinlandPopulation.txt",as.is=TRUE,header=TRUE)
P$Age=as.numeric(P$Age) ;
P=P[!is.na(P$Age) & P$Year >= 1969 & 
    P$Age >= 30 & P$Female > 0,]
P$YA = P$Year*1000+P$Age
length(P$Year)
head(P)

DP=merge(P,D,by.x="YA", by.y="YA"); head(DP) ; write.csv(DP, 'test_data.csv') 
n.rows = length(DP$Year.x)

n.subsets = 10  # Finland too big, so divide in 10ths

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

loglik=function(par) { # for comb. of binned and
	                   # unbinned counts 
	Linear.predictor = par[1] + 
	                   par[2]*age + 
	                   par[3]*year + o
	mu = exp(Linear.predictor)
	
	Prob.reported.data = rep(NA,length(mu))
	
	Prob.reported.data[not.binned] = 
	   dpois(n.deaths[not.binned],
	         mu[not.binned] )
	
	Prob.reported.data[binned] = 
	   ppois(9,mu[binned]) -
	   ppois(0,mu[binned])  
	   
	return( sum(log(Prob.reported.data)) )
}

# MCMC

#PoissonModel=readLines(,20)
model
{C <- 10000
 for (i in 1:n.all) {	  
   zeros[i] ~ dpois( zeros.mean[i] )
   zeros.mean[i] <- C - logL[i]
   logL[i] <- log(P.observed[i])
   LP[i] <- B.0+B.age*Age[i]+B.year*Year[i]+O[i]
   MU[i] = exp(LP[i])
 }   
 for (i in 1:n.not.binned) {
  P.observed[i] = dpois(reported.numbers[i],MU[i])
 }
 for (i in n.not.binned.plus.1:n.all) {
  P.observed[i] = ppois(9,MU[i]) - ppois(1,MU[i])
 }
  
 B.0    ~ dnorm(0,0.000001) 
 B.age  ~ dnorm(0,0.000001)
 B.year ~ dnorm(0,0.000001) 
}

writeLines(PoissonModel,'PoissonModel.txt')

library('rjags')

mcmc = function(dummy) {

 jags <- jags.model("PoissonModel.txt",
  data = list("zeros" = zeros,
     "reported.numbers"=reported.numbers,
     "Age"  = Age,
     "Year" = Year,
     "O" = O,
     "n.all" = n.all,
     "n.not.binned" = n.not.binned,
     "n.not.binned.plus.1" = n.not.binned.plus.1),
  inits = list(B.0=-5,B.age=0.05, B.year=-0.01),
  n.chains = 1,
  n.adapt = 1000
 )

 update(jags, 1000)
 
 results=jags.samples(jags, 
        c('B.0', 'B.age', 'B.year' ), 1000)

 estimates= c(
   mean(results$B.0[1,,1]),
   mean(results$B.age[1,,1]),
   mean(results$B.year[1,,1])
 )
  return(estimates)

} # end MCMC


beta.from.glm = matrix(NA,n.subsets,3) # no bin
beta.from.ML  = matrix(NA,n.subsets,3) # binning
beta.from.mcmc= matrix(NA,n.subsets,3) # binning

for(s in 1:1) {
	n.deaths = ds[,3+s]
	print(c(s,sum(n.deaths)))
	
	year = ds$Year.x - 1969
	age  = ds$Age.x
	
	o = log(ds$Pop)
	fit1=glm(n.deaths ~  + age + year + offset(o),
	    family=poisson)
	beta.from.glm[s,] = round(fit1$coefficients,4)
	
	print( noquote("SE's if no binning"))
	# SE print( summary(fit1)$coefficients[, 2] )
	
	binned = (n.deaths >=1 & n.deaths < 10)
	not.binned = (n.deaths ==0 |n.deaths >= 10)
	n.not.binned = sum(not.binned)
	n.binned = sum(binned)
	n.not.binned.plus.1 = n.not.binned+1
	n.all = n.not.binned + n.binned
	
	print( noquote(" "))
	
	print( noquote(" "))
	
	est=c(-11,.11,-.01)
	fit2 = optim(par=est,fn=loglik,
	        method="BFGS",
	        hessian=TRUE, 
	        control=list(fnscale=-1));
	        
    beta.from.ML[s,] = round(fit2$par,4)
    
    V = solve(-fit2$hessian);
    print( noquote("SE's if binning"))
    print( sqrt(diag(V)) );
    print( noquote(" ")) 
     cov2cor(V)
    print( noquote(" "))
    print( round( sum(binned)/n.rows,2))
    print( noquote(" "))
    print( noquote(" "))
    
    # for MCMC
    
    Age  =c(age[not.binned], age[binned] )
	Year =c(year[not.binned], year[binned] )
	O    =c(o[not.binned], o[binned])
	
	reported.numbers = 
	        c(n.deaths[not.binned],
	          rep(1.9,n.binned))
	zeros = rep(0,n.all)
	
	mcmc.est = mcmc(0) 
	beta.from.mcmc[s,] = round(mcmc.est,4) 
}


beta.from.glm[ 1:2,]  # no binning
beta.from.ML[  1:2,]   # binning
beta.from.mcmc[1:2,] # binning 

# jh. 2016.10.03




