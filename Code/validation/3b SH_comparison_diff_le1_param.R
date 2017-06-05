#the purpose of this script is to change the LE function parameter ave.prop.le1.lived = 0.10 to see how that affects the LE for blacks and whites
#in the first five states.

load('~/SH_HA_data.Rdata')
source("~/BlackWhiteMortalityGap/Code/2_smoothing_models/bayes_smoothing_src.R")

nrows <- length(levels(dtotal$state))

le.table2 <-  data.frame(state = factor(nrows), 
                        le.white = numeric(nrows), le.white.lcl = numeric(nrows), le.white.ucl = numeric(nrows),
                        le.black = numeric(nrows), le.black.lcl = numeric(nrows), le.black.ucl = numeric(nrows),
                        le.gap = numeric(nrows), le.gap.lcl = numeric(nrows), le.gap.ucl = numeric(nrows))
le.table2$state <- levels(dtotal$state)

# Males 1990
system.time(
  for(state_i in setdiff(levels(dtotal$state), c("Idaho", "Montana", "North Dakota", "South Dakota", "Vermont"))[1:5]) {
    for(race_i in c("White", "Black")) {
      ds_sub = subset_data(ds = dtotal, state = state_i, sex = "Male", race = race_i)
      data = jagsify_data(ds_sub)
      jags_model = run_smoothing_model(data)
      
      r1990 = clean_smoothing_results(data, jags_model, year=1990, n.posterior.samples=1000)
      #r2009 = clean_smoothing_results(data, jags_model, year=2009, n.posterior.samples=1000)
      
      #make a life table for each sample
      life.tables.1990 <- map(r1990, ~ life.table( .x, num.ages.in.group = "age_bin_size",
                                                   death.counts = "smoothed_deaths", 
                                                   population.counts = "population", 
                                                   ave.prop.le1.lived = 0.10))
      
      #estimate life expectancy for each sample
      assign(paste0("les.1990.", race_i, ".", state_i, 2), map_dbl(life.tables.1990, ~ .x$e_x[1]))
      
    }
    le.table2$le.white[le.table2$state == state_i] <- quantile(get(paste0("les.1990.White.", state_i, 2)), 0.5)
    le.table2$le.black[le.table2$state == state_i] <- quantile(get(paste0("les.1990.Black.", state_i, 2)), 0.5)
    le.table2$le.gap[le.table2$state == state_i] <-   quantile(get(paste0("les.1990.White.", state_i, 2)) - 
                                                               get(paste0("les.1990.Black.", state_i, 2)), 0.5) 
    
    le.table2$le.white.lcl[le.table2$state == state_i] <- quantile(get(paste0("les.1990.White.", state_i, 2)), 0.025)
    le.table2$le.black.lcl[le.table2$state == state_i] <- quantile(get(paste0("les.1990.Black.", state_i, 2)), 0.025)
    le.table2$le.gap.lcl[le.table2$state == state_i] <-   quantile(get(paste0("les.1990.White.", state_i, 2)) - 
                                                                   get(paste0("les.1990.Black.", state_i, 2)), 0.025)
    
    le.table2$le.white.ucl[le.table2$state == state_i] <- quantile(get(paste0("les.1990.White.", state_i, 2)), 0.975)
    le.table2$le.black.ucl[le.table2$state == state_i] <- quantile(get(paste0("les.1990.Black.", state_i, 2)), 0.975)
    le.table2$le.gap.ucl[le.table2$state == state_i] <-   quantile(get(paste0("les.1990.White.", state_i, 2)) -
                                                                   get(paste0("les.1990.Black.", state_i, 2)), 0.975)
    
    print(state_i)
  }
  
)
le.table2$sex = "Male"
le.table2$year = "1990"

load("~/SH_HA_compare_results.Rdata")

compare.le2 <- merge(compare.le, le.table2, by = c("state", "sex", "year"))

ggplot(subset(compare.le2, state == setdiff(levels(dtotal$state), c("Idaho", "Montana", "North Dakota", "South Dakota", "Vermont"))[1:5]),
       aes(x = le.white.x, y =  le.white.y)) + 
  geom_point() +
  geom_point(aes(le.white.x, le.white), col = "red") +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = -0.25, slope = 1, lty = 2)  

ggplot(subset(compare.le2, state == setdiff(levels(dtotal$state), c("Idaho", "Montana", "North Dakota", "South Dakota", "Vermont"))[1:5]),
       aes(x = le.white.x, y =  le.white.y)) + 
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = -0.25, slope = 1, lty = 2) +
  geom_point(aes(le.black.x, le.black), col = "red") +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = -0.25, slope = 1, lty = 2)  


