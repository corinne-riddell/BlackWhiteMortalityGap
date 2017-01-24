
load('~/SH_HA_data.Rdata')
source("~/BlackWhiteMortalityGap/Code/2_smoothing_models/bayes_smoothing_src.R")

nrows <- 3*2*length(levels(dtotal$state))

le.table <-  data.frame(state = factor(nrows), 
                        le.white = numeric(nrows), le.white.lcl = numeric(nrows), le.white.ucl = numeric(nrows),
                        le.black = numeric(nrows), le.black.lcl = numeric(nrows), le.black.ucl = numeric(nrows),
                        le.gap = numeric(nrows), le.gap.lcl = numeric(nrows), le.gap.ucl = numeric(nrows)
                        )

le.table$state <- levels(dtotal$state)
le.table$year <- c(rep("1990", nrows/3), rep("2009", nrows/3), rep("difference", nrows/3))
le.table$sex <- c(rep("Male", nrows/6), rep("Female", nrows/6))

#1990 and 2009
system.time( #this took 7.5 hours on one core on the server.
  for(state_i in setdiff(levels(dtotal$state), c("Idaho", "Montana", "North Dakota", "South Dakota", "Vermont", "Wyoming"))) {
    for(sex_i in c("Male", "Female")) { 
      for(race_i in c("White", "Black")) {
        ds_sub = subset_data(ds = dtotal, state = state_i, sex = sex_i, race = race_i)
        data = jagsify_data(ds_sub)
        jags_model = run_smoothing_model(data)
        
        assign(paste0("r1990.", sex_i), clean_smoothing_results(data, jags_model, year = 1990, n.posterior.samples = 1000))
        assign(paste0("r2009.", sex_i), clean_smoothing_results(data, jags_model, year = 2009, n.posterior.samples = 1000))
        
        #make a life table for each sample
        
        assign(paste0("life.tables.1990.", sex_i), map(get(paste0("r1990.", sex_i)), ~ life.table( .x, num.ages.in.group = "age_bin_size",
                                                                                                   death.counts = "smoothed_deaths",
                                                                                                   population.counts = "population",
                                                                                                   ave.prop.le1.lived = 0.10)))
        
        assign(paste0("life.tables.2009.", sex_i), map(get(paste0("r2009.", sex_i)), ~ life.table( .x, num.ages.in.group = "age_bin_size",
                                                                                                   death.counts = "smoothed_deaths",
                                                                                                   population.counts = "population",
                                                                                                   ave.prop.le1.lived = 0.10)))              
        
        #estimate life expectancy for each sample
        assign(paste0("les.1990.", race_i, ".", state_i, ".", sex_i), map_dbl(get(paste0("life.tables.1990.", sex_i)), ~ .x$e_x[1]))
        assign(paste0("les.2009.", race_i, ".", state_i, ".", sex_i), map_dbl(get(paste0("life.tables.2009.", sex_i)), ~ .x$e_x[1]))
        
      }
    }
    #(median) LE estimates, LE gap, LE change over time, with 95% credible intervals
    
    for(time_i in c("1990", "2009")) {
      for(sex_i in c("Male", "Female")) {
        
        le.table$le.white[le.table$state == state_i & le.table$year == time_i & le.table$sex == sex_i] <- quantile(get(paste0("les.", time_i , ".White.", state_i, "." , sex_i)), 0.5)
        le.table$le.black[le.table$state == state_i & le.table$year == time_i & le.table$sex == sex_i] <- quantile(get(paste0("les.", time_i, ".Black.", state_i, "." , sex_i)), 0.5)
        le.table$le.gap[le.table$state == state_i & le.table$year == time_i & le.table$sex == sex_i] <-   quantile(get(paste0("les.", time_i, ".White.", state_i, "." , sex_i)) - 
                                                                                                                     get(paste0("les.", time_i, ".Black.", state_i, "." , sex_i)), 0.5)
        
        le.table$le.white.lcl[le.table$state == state_i & le.table$year == time_i & le.table$sex == sex_i] <- quantile(get(paste0("les.", time_i , ".White.", state_i, "." , sex_i)), 0.025)
        le.table$le.black.lcl[le.table$state == state_i & le.table$year == time_i & le.table$sex == sex_i] <- quantile(get(paste0("les.", time_i, ".Black.", state_i, "." , sex_i)), 0.025)
        le.table$le.gap.lcl[le.table$state == state_i & le.table$year == time_i & le.table$sex == sex_i] <-   quantile(get(paste0("les.", time_i, ".White.", state_i, "." , sex_i)) - 
                                                                                                                         get(paste0("les.", time_i, ".Black.", state_i, "." , sex_i)), 0.025) 
        
        le.table$le.white.ucl[le.table$state == state_i & le.table$year == time_i & le.table$sex == sex_i] <- quantile(get(paste0("les.", time_i , ".White.", state_i, "." , sex_i)), 0.975)
        le.table$le.black.ucl[le.table$state == state_i & le.table$year == time_i & le.table$sex == sex_i] <- quantile(get(paste0("les.", time_i, ".Black.", state_i, "." , sex_i)), 0.975)
        le.table$le.gap.ucl[le.table$state == state_i & le.table$year == time_i & le.table$sex == sex_i] <-   quantile(get(paste0("les.", time_i, ".White.", state_i, "." , sex_i)) - 
                                                                                                                         get(paste0("les.", time_i, ".Black.", state_i, "." , sex_i)), 0.975) 
      }
    }
    
    for(sex_i in c("Male", "Female")) {    
      le.table$le.white[le.table$state == state_i & le.table$year == "difference" & le.table$sex == sex_i] <-   quantile(get(paste0("les.2009.White.", state_i, "." , sex_i)) - 
                                                                                                                           get(paste0("les.1990.White.", state_i, "." ,sex_i)), 0.5)
      le.table$le.black[le.table$state == state_i & le.table$year == "difference" & le.table$sex == sex_i] <-   quantile(get(paste0("les.2009.Black.", state_i, "." , sex_i)) - 
                                                                                                                           get(paste0("les.1990.Black.", state_i, "." ,sex_i)), 0.5)
      
      le.table$le.gap[le.table$state == state_i & le.table$year == "difference" & le.table$sex == sex_i] <-   quantile((get(paste0("les.2009.White.", state_i, "." , sex_i)) - 
                                                                                                                          get(paste0("les.2009.Black.", state_i, "." ,sex_i))) -
                                                                                                                         (get(paste0("les.1990.White.", state_i, "." ,sex_i)) - 
                                                                                                                            get(paste0("les.1990.Black.", state_i, "." ,sex_i))), 0.5) 
      
      le.table$le.white.lcl[le.table$state == state_i & le.table$year == "difference" & le.table$sex == sex_i] <-   quantile(get(paste0("les.2009.White.", state_i, "." , sex_i)) - 
                                                                                                                               get(paste0("les.1990.White.", state_i, "." , sex_i)), 0.025)
      le.table$le.black.lcl[le.table$state == state_i & le.table$year == "difference" & le.table$sex == sex_i] <-   quantile(get(paste0("les.2009.Black.", state_i, "." , sex_i)) - 
                                                                                                                               get(paste0("les.1990.Black.", state_i, "." , sex_i)), 0.025)
      
      le.table$le.gap.lcl[le.table$state == state_i & le.table$year == "difference" & le.table$sex == sex_i] <-   quantile((get(paste0("les.2009.White.", state_i, "." , sex_i)) - 
                                                                                                                              get(paste0("les.2009.Black.", state_i, "." , sex_i))) -
                                                                                                                             (get(paste0("les.1990.White.", state_i, "." , sex_i)) - 
                                                                                                                                get(paste0("les.1990.Black.", state_i, "." , sex_i))), 0.025) 
      
      le.table$le.white.ucl[le.table$state == state_i & le.table$year == "difference" & le.table$sex == sex_i] <-   quantile(get(paste0("les.2009.White.", state_i, "." , sex_i)) - 
                                                                                                                               get(paste0("les.1990.White.", state_i, "." , sex_i)), 0.975)
      le.table$le.black.ucl[le.table$state == state_i & le.table$year == "difference" & le.table$sex == sex_i] <-   quantile(get(paste0("les.2009.Black.", state_i, "." , sex_i)) - 
                                                                                                                               get(paste0("les.1990.Black.", state_i, "." , sex_i)), 0.975)
      
      le.table$le.gap.ucl[le.table$state == state_i & le.table$year == "difference" & le.table$sex == sex_i] <-   quantile((get(paste0("les.2009.White.", state_i, "." , sex_i)) - 
                                                                                                                              get(paste0("les.2009.Black.", state_i, "." , sex_i))) -
                                                                                                                             (get(paste0("les.1990.White.", state_i, "." , sex_i)) - 
                                                                                                                                get(paste0("les.1990.Black.", state_i, "." , sex_i))), 0.975) 
    }
    
    print(state_i)
  }
  
)

View(cbind(le.table[, 1], round(le.table[, 2:10], 1)))

SH.dat <- read.csv("~/SH_HA_supp_app_data.csv", header = T)
SH.dat <- SH.dat %>% rename(state = State, 
                            le.black.original = le.black,
                            le.white.original = le.white,
                            le.gap.original = le.gap, 
                            le.gap.ci.original = le.gap.ci)

compare.le <- merge(le.table, SH.dat, by = c("state", "sex", "year"))

test <- strsplit(stringr::str_sub(compare.le$le.gap.ci.original, 2, -2), ",")
test2 <- matrix(unlist(test), ncol=2, byrow=TRUE)

compare.le$le.gap.lcl.original <- as.numeric(test2[ , 1])
compare.le$le.gap.ucl.original <- as.numeric(test2[ , 2])
rm(test, test2)

compare.le <- compare.le %>% mutate(le.gap.width.original = le.gap.ucl.original - le.gap.lcl.original,
                                    le.gap.width = le.gap.ucl - le.gap.lcl)

small.black.states = c("Hawaii", "Alaska", "Maine", "New Hampshire",
                         "Utah", "New Mexico")

ggplot(subset(compare.le, state!= "Wyoming" & year != "difference"), 
       aes(x = le.white.original, y = le.white)) +
  geom_point(aes(x = le.black.original, y = le.black, col = "blacks")) + 
  geom_point(aes(col = "whites")) +
  facet_grid(year~sex ) +
  geom_abline(intercept = 0, slope = 1) + 
  ylab("Life expectancy (years)") +
  xlab("Original life expectancy (years)") +
  ggtitle("Comparison of life expectancy between the original model and our model") +
  geom_text(data = subset(compare.le, state %in% small.black.states & year != "difference"),
            aes(x = le.black.original, y = le.black, label = state), size = 2.5)

ggplot(subset(compare.le, state!= "Wyoming" & year == "difference"), 
       aes(x = le.white.original, y = le.white)) +
  geom_point(aes(x = le.black.original, y = le.black, col = "blacks")) + 
  geom_point(aes(col = "whites")) +
  facet_grid(~sex ) +
  geom_abline(intercept = 0, slope = 1) + 
  ylab("Life expectancy change (years)") +
  xlab("Original life expectancy change (years)") +
  ggtitle("Comparison of life expectancy difference (1990 vs. 2009) between the original model and our model") + 
  geom_text(data = subset(compare.le, state %in% small.black.states & year == "difference"),
            aes(x = le.black.original, y = le.black, label = state), size = 2.5)

ggplot(subset(compare.le, state!= "Wyoming" & year != "difference"), 
       aes(x = le.gap.width.original, y = le.gap.width)) +
  geom_point() +
  facet_grid(year~sex ) +
  geom_abline(intercept = 0, slope = 1) + 
  ylab("Width of CI for LE gap") +
  xlab("Original width of CI for LE gap") +
  ggtitle("Comparison of CI width for LE gap between the original model and our model") +
  geom_text(data = subset(compare.le, state %in% small.black.states & year != "difference"),
            aes(x = le.gap.width.original, y = le.gap.width, label = state), size = 2.5)

ggplot(subset(compare.le, state!= "Wyoming" & year == "difference"), 
       aes(x = le.gap.width.original, y = le.gap.width)) +
  geom_point() +
  facet_grid(.~sex ) +
  geom_abline(intercept = 0, slope = 1) + 
  ylab("Width of CI for LE gap") +
  xlab("Original width of CI for LE gap") +
  ggtitle("Comparison of the change in the CI width for LE gap between the original model and our model") +
  geom_text(data = subset(compare.le, state %in% small.black.states & year == "difference"),
            aes(x = le.gap.width.original, y = le.gap.width, label = state), size = 2.5)

ggplot(subset(compare.le, state!= "Wyoming" & year != "difference"), 
       aes(x = le.gap.original, y = le.gap)) +
  geom_point() +
  facet_grid(year~sex ) +
  geom_abline(intercept = 0, slope = 1) + 
  ylab("Life expectancy gap (years)") +
  xlab("Original life expectancy gap (years)") +
  ggtitle("Comparison of life expectancy gap between the original model and our model") +
  geom_text(data = subset(compare.le, state %in% small.black.states & year != "difference"),
            aes(x = le.gap.original, y = le.gap, label = state), size = 2.5)

ggplot(subset(compare.le, state!= "Wyoming" & year == "difference"), 
       aes(x = le.gap.original, y = le.gap)) +
  geom_point() +
  facet_grid(.~sex ) +
  geom_abline(intercept = 0, slope = 1) + 
  ylab("Change in life expectancy gap (years)") +
  xlab("Original change in life expectancy gap (years)") +
  ggtitle("Comparison of change in the life expectancy gap between the original model and our model") +
  geom_text(data = subset(compare.le, state %in% small.black.states & year == "difference"),
            aes(x = le.gap.original, y = le.gap, label = state), size = 2.5)

save.image("~/SH_HA_compare_results.Rdata")
write.csv(compare.le, "~/BlackWhiteMortalityGap/Data/compare_LE.csv")
