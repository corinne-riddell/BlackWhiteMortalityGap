

par(mfrow=c(3,1))

x = seq(40,100,1)
y = dnorm(x, mean=71.69, sd=9.22)
plot(x,y, type='l')


mu = rnorm(100000, 71.69, sd=9.22) 
hist(mu)

z = rnorm(100000, mean=mu, sd=9) 


hist(z)
#
 
list.files() 
DP = read.csv('test_data.csv') 
head(DP)

ds1 = data.frame(YA=DP$YA, year=DP$Year.x, age = DP$Age.x, deaths=DP$Total.y, pop=DP$Total.x) 
ds2 = ds1[ds1$year>2000, ]
head(ds2) ; tail(ds2) ; hist(ds2$deaths/ds2$pop)

ds2$bins = ifelse( (ds2$deaths<=9 & ds2$deaths>=1), 1, 0)

ds2$pop = as.integer(ds2$pop)
ds2$deaths = as.integer(ds2$deaths)


m = glm(deaths ~ age + year + offset(log(pop)), data=ds2, family='poisson') 
summary(m)


year = ds2$year - 2000 
age  = ds2$age
o = log(ds2$pop)
n.deaths = ds2$deaths

binned = (ds2$bins==1) 
not.binned = (ds2$bins==0) 
n.not.binned = length(ds2$bins) - sum(ds2$bins) 
n.binned = sum(ds2$bins)
n.not.binned.plus.1 = n.not.binned+1
n.all = n.not.binned + n.binned

est=c(-11, 0.11, -0.01) # inital values 
fit2 = optim(par=est, fn=loglik,
             method="BFGS",
             hessian=TRUE, 
             control=list(fnscale=-1)); #maximize instead of min 




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
    ppois(0,mu[binned])  #isolating the density where we know binned counts to be   
  
  return( sum(log(Prob.reported.data)) )
}




