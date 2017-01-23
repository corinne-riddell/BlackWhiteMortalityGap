
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

#eek - i can't get the regexpr() to work... this is me pulling out the ucl and lcl by hand. 
#bad programmer.
compare.le$le.gap.lcl.y <- c(6.5, -3.8, 4.4, 6.8, 6.4, 4.1, 7.2, 6.3, 8.6, 7.0, 
                             -18.9, 10.4, 6.3, 5.3, 6.2, 4.1, 7.2, -0.7, 7.8,
                             4.4, 8.9, 7.0, 5.9, 8.5, 5.6, 4.0, -5.2, 8.7, 1.0, 
                             7.7, 7.5, 6.4, 3.8, 5.0, 9.0, 4.9, 7.2, 6.6, 6.6, 
                             -1.1, 7.0, 13.3, 4.7, 4.4, 7.0, -1.0)

compare.le$le.gap.ucl.y <- c(7.3, 2.5, 6.2, 7.9, 7.0, 5.8, 8.7, 8.3, 9.3, 7.7, 
                             -2.0, 11.1, 7.3, 7.6, 7.8, 5.2, 8.0, 5.9, 8.6, 5.8, 
                             9.6, 9.1, 6.7, 9.5, 7.9, 6.1, 5.3, 9.6, 4.6, 8.2,
                             8.2, 7.1, 4.9, 7.9, 9.7, 8.0, 8.0, 7.4, 7.2, 5.9, 
                             7.7, 15.4, 6.5, 6.5, 8.4, 9.0)

ggplot(subset(compare.le, state!= "Wyoming"), aes(x = le.gap.lcl, y =  le.gap.lcl.y)) + 
  geom_point(col = "blue") + 
  geom_abline(intercept = 0, slope = 1) 

ggplot(subset(compare.le, state!= "Wyoming"), aes(x = le.gap.ucl, y =  le.gap.ucl.y)) + 
  geom_point(col = "red") + 
  geom_abline(intercept = 0, slope = 1) 

library(dplyr)

compare.le <- compare.le %>% mutate(diff.ucl = round(le.gap.ucl, 1) - le.gap.ucl.y,
                                    diff.lcl = round(le.gap.lcl, 1) - le.gap.lcl.y,
                                    diff.white.le = round(le.white.x, 1) - le.white.y,
                                    diff.black.le = round(le.black.x, 1) - le.black.y,
                                    diff.gap.le = round(le.gap.x, 1) - le.gap.y)

summary(compare.le$diff.ucl[compare.le$state != "Wyoming"])
summary(compare.le$diff.lcl[compare.le$state != "Wyoming"])

summary(compare.le$diff.white.le[compare.le$state != "Wyoming"])
summary(compare.le$diff.black.le[compare.le$state != "Wyoming"])
summary(compare.le$diff.gap.le[compare.le$state != "Wyoming"])

save.image("~/SH_HA_compare_results.Rdata")

#to do:
#2. change my LE calc to use 0.1 vs 0.09 for <1 to see if that acocunts for the diff between the calcs

