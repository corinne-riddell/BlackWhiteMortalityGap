


# Rich's analysis w/ openbugs and getting the LE SE 
  
library(R2OpenBUGS)
setwd('~/Documents/BlackWhiteMortalityGap/dataverse_files')

ratetorisk=function(x,t) {
  1-exp(-x*t)
}


#Matrix to collect ouput of Bayes decomp
Out = matrix(0, 15200, 46)
colnames(Out) = c("state","gender","age","year",
                   "total","totall","totalu",
                   "direct","directl","directu",
                   "indirect","indirectl","indirectu",
                   "atotal","atotall","atotalu",
                   "adirect","adirectl","adirectu",
                   "aindirect","aindirectl","aindirectu",                
                   "lew","lewl","lewu",
                   "leb","lebl","lebu",
                   "lediff","lediffl","lediffu",
                   "rw","rwl","rwu",
                   "rb","rbl","rbu",
                   "changeb","changebl","changebu",
                   "changew","changewl","changewu",
                   "change","changel","changeu"                
)
count = 1
#Flag any instances of risks > 1 among blacks and whites
flagb = rep(0, 3)
flagw = rep(0, 3)

# load data
dtotal = read.table("deaths-total.txt", na.strings=".")
colnames(dtotal) = c("state","age","gender","race","year","crude","deaths","pop")

for (istate in 0:11){
#for (istate in 13:25){
#for (istate in c(27:33)) {
#for (istate in c(35:40,42:44,46:50)) { #CR question: why running the for loop for istate over different ranges separately?
  
  for (igender in 0:1) {
    
    
    #do this next section once for whites then for blacks
    
    #Temoporary dataset for whites in istate and igender
    testw=dtotal[dtotal$state==istate&dtotal$race==0&dtotal$gender==igender,]
    
    #reorder, so first N1 obs have death data and last N2-N1 obs have no death data
    testw=testw[order(testw$deaths),]
    
    #number of records (always 380)
    N2w=length(testw$year)
    
    #outcome: deaths
    yw=testw$deaths
    
    #age group
    agew=testw$age
    
    #log of population for use in poisson model #good he did log it!! -ktm 
    popw=rep(0,380)
    
    popw[is.na(testw$deaths)==FALSE]=log(testw$pop[is.na(testw$deaths)==FALSE]-0.5*testw$deaths[is.na(testw$deaths)==FALSE])
    
    popw[is.na(testw$deaths)==TRUE]=log(testw$pop[is.na(testw$deaths)==TRUE]-2.5)
    
    #pop in binomial model #What binom model? -ktm 
    #popw=testw$pop
    #year as 1:20.  1= 1990 20=2009
    cyearw=as.numeric(testw$year-min(testw$year)+1)
    
    #age group: going from 1:19
    agew=testw$age+1
    
    #number of non missing death counts
    N1w=sum(is.na(yw)==FALSE)
    
    #get upperbound for the count of missing deaths: the max number of deaths when deaths=NA is either 9 or the population size
    upw=testw$pop
    upw[upw>9]=9
    upw=as.integer(upw)
    #Temporary data set for blacks in istate and igender
    testb=dtotal[dtotal$state==istate&dtotal$race==1&dtotal$gender==igender,]
    testb=testb[order(testb$deaths),]
    N2b=length(testb$year)
    yb=testb$deaths
    popb=rep(0,380)
    popb[is.na(testb$deaths)==FALSE]=log(testb$pop[is.na(testb$deaths)==FALSE]-0.5*testb$deaths[is.na(testb$deaths)==FALSE])
    popb[is.na(testb$deaths)==TRUE]=log(testb$pop[is.na(testb$deaths)==TRUE]-2.5)
    #popb=testb$pop
    cyearb=as.numeric(testb$year-min(testb$year)+1)
    ageb=testb$age+1
    N1b=sum(is.na(yb)==FALSE)
    upb=testb$pop
    upb[upb>9]=9
    upb=as.integer(upb)
    ######################################################################
    #BUGS steps
    #
    
    
    #Create BUGS data
    db=list(yb=yb,cyearb=cyearb,ageb=ageb,popb=popb,N1b=N1b,N2b=N2b,yw=yw,cyearw=cyearw,agew=agew,popw=popw,N1w=N1w,N2w=N2w,upw=upw,upb=upb)
    
    #bugsData(db,file="srj_datab.txt")
    bugs.data(db, data.file="srj_datab.txt") # -ktm
    #inits
    yb.init=upb
    yb.init[is.na(yb)==FALSE]=NA
    yb.init=as.integer(yb.init)
    yw.init=upw
    yw.init[is.na(yw)==FALSE]=NA
    yw.init=as.integer(yw.init)
    b.init=matrix(-4.5,19,20)
    init=list(tau1b=70,tau1w=70,b=b.init,w=b.init,yb=yb.init,yw=yw.init)
    #init=list(tau1b=70,tau1w=70,b=b.init,w=b.init)
    
    bugs.data(init, data.file="srj_inits.txt") # -ktm 
    #bugsData(init,file="srj_inits.txt")
    
    #run openbugs
    modelCheck("poisopenlt.bug")
    modelData(paste("srj_datab.txt",sep=""))
    modelCompile(numChains=1)
    modelInits("srj_inits.txt")
    modelGenInits()
    modelUpdate(100,thin=10)
    
    model = bugs(data, inits, model.file = "c:/schools/schools.bug",
                        + parameters = c("theta", "mu.theta", "sigma.theta"),
                        + n.chains = 3, n.iter = 1000)
    
    
    #samplesSet(c("b","tau1","y"))
    samplesSet(c("b","w","tau1w","tau1b"))
    modelUpdate(10000, thin=100)
    #Save trace plots
    fname=paste(as.character(c(istate,"_",igender,".pdf")),collapse="")
    pdf(fname)
    samplesHistory("b")
    samplesHistory("tau1b")
    samplesHistory("w")
    samplesHistory("tau1w")
    dev.off()
    
    # Take BUGS results and start making life-tables
    #age/year specific rates for blacks
    r.b=array(0,dim=c(19,20,samplesSize("b[1,1]")))
    namesb=samplesMonitors("b")
    #age/year specific rates for whites
    r.w=array(0,dim=c(19,20,samplesSize("w[1,1]")))
    namesw=samplesMonitors("w")
    #exponentiate MCMC iterates to get rates
    for (i in 1:19) {
      for (j in 1:20){
        r.b[i,j,]=exp(samplesSample(namesb[(i-1)*20 + j]))
        r.w[i,j,]=exp(samplesSample(namesw[(i-1)*20 + j]))       
      }
    }
    #if (sum(r.b>1)>0) flagb=rbind(flagb,c(istate,igender,sum(r.b>1)))
    #if (sum(r.w>1)>0) flagw=rbind(flagw,c(istate,igender,sum(r.w>1)))
    #r.b[r.b>1]=.9999
    #r.w[r.w>1]=.9999
    
    #This is new in latest version
    #results in r.b and r.w are rates of death in each age group. 
    #want risk over age interval for life table
    #convert rate to 5-year risk, except for first 2 years which are 1 yr and 4 yr risks
    r.b.yr=array(0,dim(r.b))
    r.w.yr=array(0,dim(r.w))
    #t is length of interval
    t=c(1,4,rep(5,16),1)
    #dy is amount of time a dead person spends in each interval
    dy=c(.5,2,rep(2.5,16),0)
    for (i in 1:18) {
      r.b.yr[i,,]=ratetorisk(r.b[i,,],t[i])
      r.w.yr[i,,]=ratetorisk(r.w[i,,],t[i])
    }
    #very,very rarely, the rate is high enough that the 5 year risk becomes 1. this alters that risk slightly downward.
    #actually, this was a concern with the older version of the model. don't think this matters now.
    r.b.yr[r.b.yr==1]=.999
    r.w.yr[r.w.yr==1]=.999
    # everyone dies in the final strata: 85+
    r.b.yr[19,,]=1
    r.w.yr[19,,]=1
    
    
    #direct effects for each age/year
    #number starting interval
    lw=array(0,dim=c(dim(r.b)))
    lb=array(0,dim=c(dim(r.b)))
    #number dying in interval
    fw=array(0,dim=c(dim(r.b)))
    fb=array(0,dim=c(dim(r.b)))
    #year accrued in interval
    nLw=array(0,dim=c(dim(r.b)))
    nLb=array(0,dim=c(dim(r.b)))
    #direct effect
    de=array(0,dim=c(dim(r.b)))
    #indirect effect
    ie=array(0,dim=c(dim(r.b)))
    #interaction
    int=array(0,dim=c(dim(r.b)))
    #total
    tot=array(0,dim=c(dim(r.b)))
    
    #direct/indirect/interaction/total over ages
    direct=matrix(0,dim(r.b)[2],dim(r.b)[3])
    indirect=matrix(0,dim(r.b)[2],dim(r.b)[3])
    subtotal=matrix(0,dim(r.b)[2],dim(r.b)[3])
    inter=matrix(0,dim(r.b)[2],dim(r.b)[3])
    total=matrix(0,dim(r.b)[2],dim(r.b)[3])
    
    #life expectancy & Diff
    lew=array(0,dim=c(dim(r.b)))
    leb=array(0,dim=c(dim(r.b)))
    lediff=array(0,dim=c(dim(r.b)))
    
    
    #Create life-tables...one for every year and iteration
    for (j in 1:20){
      # starting population at age=0
      lw[1,j,]=100000   
      # number of deaths in age 1, year j               
      fw[1,j,]=lw[1,j,]*r.w.yr[1,j,]
      #person years: deaths get 0.09 years
      nLw[1,j,]=(lw[1,j,]-fw[1,j,])*1+.09*fw[1,j,]
      
      lb[1,j,]=100000
      fb[1,j,]=lb[1,j,]*r.b.yr[1,j,]
      nLb[1,j,]=(lb[1,j,]-fb[1,j,])*1+.09*fb[1,j,]    
      
      for (i in 2:18) {
        lw[i,j,]=lw[i-1,j,]-fw[i-1,j,]
        fw[i,j,]=lw[i,j,]*r.w.yr[i,j,]
        nLw[i,j,]=(lw[i,j,]-fw[i,j,])*t[i]+fw[i,j,]*dy[i]
        
        lb[i,j,]=lb[i-1,j,]-fb[i-1,j,]
        fb[i,j,]=lb[i,j,]*r.b.yr[i,j,]
        nLb[i,j,]=(lb[i,j,]-fb[i,j,])*t[i]+fb[i,j,]*dy[i]
      }
      i=19
      lw[i,j,]=lw[i-1,j,]-fw[i-1,j,]
      fw[i,j,]=lw[i,j,]
      nLw[i,j,]=lw[i,j,]/r.w[i,j,]
      
      lb[i,j,]=lb[i-1,j,]-fb[i-1,j,]
      fb[i,j,]=lb[i,j,]
      nLb[i,j,]=lb[i,j,]/r.b[i,j,]
    }
    
    #Decomposition   & life expectancy 
    for (j in 1:20){
      
      de[1,j,]=(nLw[1,j,]/lw[1,j,]-nLb[1,j,]/lb[1,j,])*lb[1,j,]/lb[1,j,]
      ie[1,j,]=(lb[1,j,]*lw[2,j,]/lw[1,j,]-lb[2,j,])*colSums(nLb[-(1),j,])/(lb[1,j,]*lb[2,j,])
      int[1,j,]=(lb[1,j,]*lw[1+1,j,]/lw[1,j,]-lb[1+1,j,])*colSums(nLw[-1,j,])/(lb[1,j,]*lw[2,j,])-ie[1,j,]
      tot[1,j,]=ie[1,j,]+de[1,j,]+int[1,j,]
      lew[1,j,]=colSums(nLw[1:19,j,])/lw[1,j,]
      leb[1,j,]=colSums(nLb[1:19,j,])/lb[1,j,]
      lediff[1,j,]=lew[1,j,]-leb[1,j,]    
      for (i in 2:19) {        
        de[i,j,]=(nLw[i,j,]/lw[i,j,]-nLb[i,j,]/lb[i,j,])*lb[i,j,]/lb[1,j,]
        if (i<18) {
          ie[i,j,]=(lb[i,j,]*lw[i+1,j,]/lw[i,j,]-lb[i+1,j,])*colSums(nLb[-(1:i),j,])/(lb[1,j,]*lb[i+1,j,])
          int[i,j,]=(lb[i,j,]*lw[i+1,j,]/lw[i,j,]-lb[i+1,j,])*colSums(nLw[-(1:i),j,])/(lb[1,j,]*lw[i+1,j,])-ie[i,j,]
          lew[i,j,]=colSums(nLw[i:19,j,])/lw[i,j,]
          leb[i,j,]=colSums(nLb[i:19,j,])/lb[i,j,]
          lediff[i,j,]=lew[i,j,]-leb[i,j,]
        }
        if (i==18) {
          ie[i,j,]=(lb[i,j,]*lw[i+1,j,]/lw[i,j,]-lb[i+1,j,])*(nLb[19,j,])/(lb[1,j,]*lb[i+1,j,])
          int[i,j,]=(lb[i,j,]*lw[i+1,j,]/lw[i,j,]-lb[i+1,j,])*(nLw[19,j,])/(lb[1,j,]*lw[i+1,j,])-ie[i,j,]
          lew[i,j,]=colSums(nLw[i:19,j,])/lw[i,j,]
          leb[i,j,]=colSums(nLb[i:19,j,])/lb[i,j,]
          lediff[i,j,]=lew[i,j,]-leb[i,j,]
        }
        if (i==19) {
          lew[i,j,]=(nLw[i:19,j,])/lw[i,j,]
          leb[i,j,]=(nLb[i:19,j,])/lb[i,j,]
          lediff[i,j,]=lew[i,j,]-leb[i,j,]        
        }
        tot[i,j,]=ie[i,j,]+de[i,j,]+int[i,j,]
        
      }
    }
    cbind(lw[,2,4],nLw[,2,4],lew[,2,4],leb[,2,4],lediff[,2,4])
    
    #sum age specific effects
    direct=colSums(de)
    indirect=colSums(ie)
    total=colSums(tot)
    
    #temp stores the mean, 2.5th, 97.5th percentile of total, direct, indirect for each age/year combination
    #age, year, total x3, direct x3,indirect x3
    temp=matrix(0,380,(2+14*3))
    #c1 is counter
    c1=1
    for (i in 1:19) {
      for (j in 1:20) {  
        #        temp[c1,]=c(i,j,quantile(tot[i,j,],c(.025,.5,.975),na.rm=TRUE),quantile(de[i,j,],c(.025,.5,.975),na.rm=TRUE),quantile(ie[i,j,],c(.025,.5,.975),na.rm=TRUE))        
        temp[c1,]=c(i,j,
                     mean(tot[i,j,]),quantile(tot[i,j,],c(.025,.975)),
                     mean(de[i,j,]),quantile(de[i,j,],c(.025,.975)),
                     mean(ie[i,j,]),quantile(ie[i,j,],c(.025,.975)), 
                     mean(total[j,]),quantile(total[j,],c(.025,.957)), 
                     mean(direct[j,]),quantile(direct[j,],c(.025,.957)), 
                     mean(indirect[j,]),quantile(indirect[j,],c(.025,.957)),
                     mean(lew[i,j,]),quantile(lew[i,j,],c(.025,.975)),
                     mean(leb[i,j,]),quantile(leb[i,j,],c(.025,.975)),
                     mean(lediff[i,j,]),quantile(lediff[i,j,],c(.025,.975)),
                     mean(r.w[i,j,]),quantile(r.w[i,j,],c(.025,.975)),
                     mean(r.b[i,j,]),quantile(r.b[i,j,],c(.025,.975)),
                     mean(lew[1,20,]-lew[1,1,]),quantile(lew[1,20,]-lew[1,1,],c(.025,.975)),
                     mean(leb[1,20,]-leb[1,1,]),quantile(leb[1,20,]-leb[1,1,],c(.025,.975)),
                     mean(lediff[1,20,]-lediff[1,1,]),quantile(lediff[1,20,]-lediff[1,1,],c(.025,.975))                    
        )        
        c1=c1+1
      }
    }
    
    
    Out[count:(count+379),]=cbind(rep(istate,380),rep(igender,380),temp)
    count=count+380
    
  }
}

toc()


# the matrix "Out" must now be output as a datafile, using the table.write command and imported into Stata (using the "insheet" command)
# the resulting file is called "state-le-results.dta"
