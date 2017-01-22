
load('~/SH_HA_data.Rdata')
source("~/BlackWhiteMortalityGap/Code/2_smoothing_models/bayes_smoothing_src.R")

nrows <- length(levels(dtotal$state))

le.table <-  data.frame(state = factor(nrows), 
                        le.white = numeric(nrows), le.white.lcl = numeric(nrows), le.white.ucl = numeric(nrows),
                        le.black = numeric(nrows), le.black.lcl = numeric(nrows), le.black.ucl = numeric(nrows),
                        le.gap = numeric(nrows), le.gap.lcl = numeric(nrows), le.gap.ucl = numeric(nrows))
le.table$state <- levels(dtotal$state)

# Males 1990
system.time(
    for(state_i in setdiff(levels(dtotal$state), c("Idaho", "Montana", "North Dakota", "South Dakota", "Vermont"))) {
    for(race_i in c("White", "Black")) {
      ds_sub = subset_data(ds = dtotal, state = state_i, sex = "Male", race = race_i)
      data = jagsify_data(ds_sub)
      jags_model = run_smoothing_model(data)
      
      r1990 = clean_smoothing_results(data, jags_model, year=1990, n.posterior.samples=1000)
      #r2009 = clean_smoothing_results(data, jags_model, year=2009, n.posterior.samples=1000)
      
      #make a life table for each sample
      life.tables.1990 <- map(r1990, ~ life.table( .x, num.ages.in.group = "age_bin_size",
                                                   death.counts = "smoothed_deaths", 
                                                   population.counts = "population"))
      
      #estimate life expectancy for each sample
      assign(paste0("les.1990.", race_i, ".", state_i), map_dbl(life.tables.1990, ~ .x$e_x[1]))
      
    }
    le.table$le.white[le.table$state == state_i] <- quantile(get(paste0("les.1990.White.", state_i)), 0.5)
    le.table$le.black[le.table$state == state_i] <- quantile(get(paste0("les.1990.Black.", state_i)), 0.5)
    le.table$le.gap[le.table$state == state_i] <-   quantile(get(paste0("les.1990.White.", state_i)) - 
                                                               get(paste0("les.1990.Black.", state_i)), 0.5) 
    
    le.table$le.white.lcl[le.table$state == state_i] <- quantile(get(paste0("les.1990.White.", state_i)), 0.025)
    le.table$le.black.lcl[le.table$state == state_i] <- quantile(get(paste0("les.1990.Black.", state_i)), 0.025)
    le.table$le.gap.lcl[le.table$state == state_i] <-   quantile(get(paste0("les.1990.White.", state_i)) - 
                                                                   get(paste0("les.1990.Black.", state_i)), 0.025)
    
    le.table$le.white.ucl[le.table$state == state_i] <- quantile(get(paste0("les.1990.White.", state_i)), 0.975)
    le.table$le.black.ucl[le.table$state == state_i] <- quantile(get(paste0("les.1990.Black.", state_i)), 0.975)
    le.table$le.gap.ucl[le.table$state == state_i] <-   quantile(get(paste0("les.1990.White.", state_i)) -
                                                                   get(paste0("les.1990.Black.", state_i)), 0.975)
    
    print(state_i)
    }
  
)
le.table$sex = "Male"
le.table$year = "1990"

View(cbind(le.table[, 1], round(le.table[, 2:10], 1)))

SH.dat <- read.csv("~/SH_HA_supp_app_data.csv", header = T)
SH.dat$state <- SH.dat$State

compare.le <- merge(le.table, SH.dat, by = c("state", "sex", "year"))

ggplot(subset(compare.le, state!= "Wyoming"), aes(x = le.white.x, y =  le.white.y)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = -0.25, slope = 1, lty = 2)  

ggplot(subset(compare.le, state!= "Wyoming"), aes(x = le.black.x, y =  le.black.y)) + geom_point() + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = -0.25, slope = 1, lty = 2) 

ggplot(subset(compare.le, state!= "Wyoming"), aes(x = le.gap.x, y =  le.gap.y)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)

#to do:
#1. compare precision of the models
#2. change my LE calc to use 0.1 vs 0.09 for <1 to see if that acocunts for the diff between the calcs

save.image("~/SH_HA_compare_results.Rdata")
