entire.analysis <- function(state_i) {
  
  #load_libraries
  library(Rcpp)
  library(dplyr)
  library(purrr)
  library(ggplot2)
  library(stringr)
  library(rjags)
  library(R2jags) 
  library(devtools)
  library(jagstools)
  library(reshape2)
  library(parallel)
  library(gtools)
  library(bayesboot)
  library(coda)
  library(grid)
  library(gridExtra)
  library(parallel)
  
  #load_user_written_functions
  source("~/BlackWhiteMortalityGap/Code/2_smoothing_models/bayes_smoothing_functions.R")
  source('~/BlackWhiteMortalityGap/Code/life_expectancy_functions.R') 
  
  #load_tidied_data
  dat.clean = read.csv('~/dat_clean.csv') 
  
  #load US standard population
  std.pop <- read.csv("~/BlackWhiteMortalityGap/Data/US_Standard_Population_for_model.csv") %>% select(US_Standard_2000, age.weight, age_minbin)
  
  #fit_models
  #run time: 32 mins
  #fit the bayesian models and put the 1000 posterior samples into a data frame
  for(sex_i in c("Male", "Female")){
    print(sex_i)
    assign(paste0("r.White.", sex_i), run_analysis(dataset = dat.clean, state1 = state_i, sex1 = sex_i, race1 = "White"))
    assign(paste0("r.Black.", sex_i), run_analysis(dataset = dat.clean, state1 = state_i, sex1 = sex_i, race1 = "Black"))
  }
  
  r.White.Male$race <- "White"
  r.White.Female$race <- "White"
  r.Black.Male$race <- "Black"
  r.Black.Female$race <- "Black"
  r.All <- rbind(r.White.Male, r.White.Female, r.Black.Male, r.Black.Female)
  rm(r.White.Male, r.White.Female, r.Black.Male, r.Black.Female)
  
  #calculate age-standardized mortality rates (for each posterior sample)
  r.All <- merge(r.All, std.pop, by = "age_minbin")
  
  mortality.rates <- r.All %>% 
    group_by(post.samp, state.n, year.n, race.n, sex.n, COD) %>% 
    arrange(age_minbin) %>%
    summarise(age.std.mortality.rate = sum(age.weight*smoothed_rate), #the sum is across age bins
              rate.per.100k = age.std.mortality.rate*100000,
              race = first(race),
              sex = first(sex),
              state = first(state),
              year = first(year))
  
  mortality.rates <- add_Census_Division(mortality.rates, state)
  mortality.rates <- add_Census_Region(mortality.rates, state)
  mortality.rates$stabbrs <- state.abb[match(mortality.rates$state, state.name)]
  
  #calculate median, mean, and CI for age-standardized mortality rates (across samples) 
  mortality.rates.summary <- mortality.rates %>% group_by(state.n, year.n, race.n, sex.n, COD) %>%
    summarise(race = first(race),
              sex = first(sex),
              state = first(state),
              year = first(year),
              Census_Division = first(Census_Division),
              Census_Region = first(Census_Region),
              stabbrs = first(stabbrs),
              rate.per.100k_lcl = quantile(rate.per.100k, 0.025), 
              rate.per.100k_med = quantile(rate.per.100k, 0.5),
              rate.per.100k_ucl = quantile(rate.per.100k, 0.975),
              rate.per.100k_mean = mean(rate.per.100k))
  
  #compute black-white difference in mortality
  mortality.rates.white <- mortality.rates %>% filter(race == "White") %>% 
    ungroup() %>%
    select(sex, state, year, COD, post.samp, rate.per.100k) %>%
    rename(rate.per.100k.white = rate.per.100k)
  
  mortality.rates.black <- mortality.rates %>% filter(race == "Black") %>% 
    rename(rate.per.100k.black = rate.per.100k)  
  
  mortality.rates.wide <- merge(mortality.rates.black, mortality.rates.white, by = c("post.samp", "sex", "state", "year", "COD"))
  
  mortality.rates.wide <- mortality.rates.wide %>% mutate(rate.difference = rate.per.100k.black- rate.per.100k.white)
  
  mortality.rates.difference <- mortality.rates.wide %>% 
    group_by(state.n, year.n, sex.n, COD) %>%
    summarise(rate.difference_LCL = quantile(rate.difference, 0.025), #summarize across posterior samples (post.samp)
              rate.difference_med = quantile(rate.difference, 0.5),
              rate.difference_UCL = quantile(rate.difference, 0.975),
              rate.difference_mean = mean(rate.difference),
              Census_Division = first(Census_Division),
              Census_Region = first(Census_Region),
              stabbrs = first(stabbrs),
              sex = first(sex),
              state = first(state),
              year = first(year))
  
  r.All$smoothed_deaths <- r.All$smoothed_rate*r.All$population
  table(r.All$smoothed_deaths == 0) #check this -- if no deaths in an age-band (i.e., no death for any COD, then can't run the COD calculations for that stratum.)
  
  r.All <- r.All %>% group_by(post.samp, state, race.sex, year.n, age_minbin) %>%
    arrange(post.samp, state, race.sex, year.n, age_minbin)
  
  
  #RUN LIFE EXPECTANCY AND DECOMPOSITION CALCULATIONS
  #run time: 2.25 hours per state
  #run the life table and decomposition calculations for each of the 1000 posterior samples
  
  strata <- levels(droplevels(r.All$state.year.sex))
  n <- length(strata)
  
  le.calculations = vector(mode = "list", length = n)
  
  counter = 1
  
  for(stratum_i in strata){
    data_sub <- subset(r.All, state.year.sex == stratum_i)  
    
    le.calculations[[counter]]$stratum.id <- stratum_i
    le.calculations[[counter]]$calcs <- by(data = data_sub,
                                           INDICES = list(data_sub$post.samp),
                                           FUN = function(x) life_expectancy_and_gap(data = x))
    
    counter <- counter + 1
    
    print(stratum_i)
    rm(data_sub)
  }
  
  save(r.All, file = paste0("~/BW_results/r_All_", state_i, ".Rdata")) 
  levels.state.year.sex <- levels(droplevels(r.All$state.year.sex))
  rm(r.All)
  
  #calculate the difference in the COD contribution across the three eras.
  years <- c(1969, 1983, 1993, 2013)
  indices.female <- intersect(union_all(grep("1969", strata), grep("1983", strata), grep("1993", strata), grep("2013", strata)), grep("Female", strata))
  indices.male <- intersect(union_all(grep("1969", strata), grep("1983", strata), grep("1993", strata), grep("2013", strata)), grep("Male", strata))
  
  for(sex_i in c("Male", "Female")){
    indices <- indices.female
    if(sex_i == "Male"){
      indices <- indices.male
    }
    counter <- 1
    for(year_i in years){
      assign(paste0("cod.marginal.", year_i, ".", sex_i), map_df(le.calculations[[indices[counter]]]$calcs, ~.x$cod.marginal))  
      counter <- counter + 1
    }
  }
  
  cod.marginal.1969.Male$post.samp <- cod.marginal.1969.Female$post.samp <- rep(1:1000, each=6)
  cod.marginal.1983.Male$post.samp <- cod.marginal.1983.Female$post.samp <- rep(1:1000, each=6)
  cod.marginal.1993.Male$post.samp <- cod.marginal.1993.Female$post.samp <- rep(1:1000, each=6)
  cod.marginal.2013.Male$post.samp <- cod.marginal.2013.Female$post.samp <- rep(1:1000, each=6)
  
  cod.marginal.1969.Male <- cod.marginal.1969.Male %>% rename(C_x_1969 = C_x,
                                                              C_x_p_1969 = C_x_proportion,
                                                              total_Cx_1969 = total_Cx)
  cod.marginal.1969.Female <- cod.marginal.1969.Female %>% rename(C_x_1969 = C_x,
                                                                  C_x_p_1969 = C_x_proportion,
                                                                  total_Cx_1969 = total_Cx)
  
  cod.marginal.1983.Male <- cod.marginal.1983.Male %>% rename(C_x_1983 = C_x,
                                                              C_x_p_1983 = C_x_proportion,
                                                              total_Cx_1983 = total_Cx)
  cod.marginal.1983.Female <- cod.marginal.1983.Female %>% rename(C_x_1983 = C_x,
                                                                  C_x_p_1983 = C_x_proportion,
                                                                  total_Cx_1983 = total_Cx)
  
  cod.marginal.1993.Male <- cod.marginal.1993.Male %>% rename(C_x_1993 = C_x,
                                                              C_x_p_1993 = C_x_proportion,
                                                              total_Cx_1993 = total_Cx)
  cod.marginal.1993.Female <- cod.marginal.1993.Female %>% rename(C_x_1993 = C_x,
                                                                  C_x_p_1993 = C_x_proportion,
                                                                  total_Cx_1993 = total_Cx)
  
  cod.marginal.2013.Male <- cod.marginal.2013.Male %>% rename(C_x_2013 = C_x,
                                                              C_x_p_2013 = C_x_proportion,
                                                              total_Cx_2013 = total_Cx)
  cod.marginal.2013.Female <- cod.marginal.2013.Female %>% rename(C_x_2013 = C_x,
                                                                  C_x_p_2013 = C_x_proportion,
                                                                  total_Cx_2013 = total_Cx)
  
  cod.marginal.male <- merge(cod.marginal.1969.Male, cod.marginal.1983.Male, by = c("Cause.of.death", "post.samp"))
  cod.marginal.male <- merge(cod.marginal.male, cod.marginal.1993.Male, by = c("Cause.of.death", "post.samp"))
  cod.marginal.male <- merge(cod.marginal.male, cod.marginal.2013.Male, by = c("Cause.of.death", "post.samp"))
  
  cod.marginal.female <- merge(cod.marginal.1969.Female, cod.marginal.1983.Female, by = c("Cause.of.death", "post.samp"))
  cod.marginal.female <- merge(cod.marginal.female, cod.marginal.1993.Female, by = c("Cause.of.death", "post.samp"))
  cod.marginal.female <- merge(cod.marginal.female, cod.marginal.2013.Female, by = c("Cause.of.death", "post.samp"))
  
  rm(cod.marginal.2013.Male, cod.marginal.1993.Male, cod.marginal.1983.Male, cod.marginal.1969.Male,
     cod.marginal.2013.Female, cod.marginal.1993.Female, cod.marginal.1983.Female, cod.marginal.1969.Female)
  
  cod.marginal.male <- cod.marginal.male %>% mutate(C_x_era1_diff = C_x_1969 - C_x_1983,
                                                    C_x_era2_diff = C_x_1983 - C_x_1993,
                                                    C_x_era3_diff = C_x_1993 - C_x_2013,
                                                    tot_Cx_era1_diff = total_Cx_1969 - total_Cx_1983,
                                                    tot_Cx_era2_diff = total_Cx_1983 - total_Cx_1993,
                                                    tot_Cx_era3_diff = total_Cx_1993 - total_Cx_2013,
                                                    C_x_p_era1 = C_x_era1_diff/tot_Cx_era1_diff,
                                                    C_x_p_era2 = C_x_era2_diff/tot_Cx_era2_diff,
                                                    C_x_p_era3 = C_x_era3_diff/tot_Cx_era3_diff)
  
  cod.marginal.female <- cod.marginal.female %>% mutate(C_x_era1_diff = C_x_1969 - C_x_1983,
                                                        C_x_era2_diff = C_x_1983 - C_x_1993,
                                                        C_x_era3_diff = C_x_1993 - C_x_2013,
                                                        tot_Cx_era1_diff = total_Cx_1969 - total_Cx_1983,
                                                        tot_Cx_era2_diff = total_Cx_1983 - total_Cx_1993,
                                                        tot_Cx_era3_diff = total_Cx_1993 - total_Cx_2013,
                                                        C_x_p_era1 = C_x_era1_diff/tot_Cx_era1_diff,
                                                        C_x_p_era2 = C_x_era2_diff/tot_Cx_era2_diff,
                                                        C_x_p_era3 = C_x_era3_diff/tot_Cx_era3_diff)
  
  cod.marginal.male.summary <- cod.marginal.male %>% group_by(Cause.of.death) %>%
    summarise(
      Cx_era1_diff_lcl = quantile(C_x_era1_diff, 0.025),
      Cx_era1_diff_med = quantile(C_x_era1_diff, 0.5),
      Cx_era1_diff_ucl = quantile(C_x_era1_diff, 0.975),
      Cx_era1_diff_mean = mean(C_x_era1_diff),
      Cx_era2_diff_lcl = quantile(C_x_era2_diff, 0.025),
      Cx_era2_diff_med = quantile(C_x_era2_diff, 0.5),
      Cx_era2_diff_ucl = quantile(C_x_era2_diff, 0.975),
      Cx_era2_diff_mean = mean(C_x_era2_diff),
      Cx_era3_diff_lcl = quantile(C_x_era3_diff, 0.025),
      Cx_era3_diff_med = quantile(C_x_era3_diff, 0.5),
      Cx_era3_diff_ucl = quantile(C_x_era3_diff, 0.975),
      Cx_era3_diff_mean = mean(C_x_era3_diff),
      
      Cx_p_era1_diff_lcl = quantile(C_x_p_era1, 0.025),
      Cx_p_era1_diff_med = quantile(C_x_p_era1, 0.5),
      Cx_p_era1_diff_ucl = quantile(C_x_p_era1, 0.975),
      Cx_p_era1_diff_mean = mean(C_x_p_era1),
      Cx_p_era2_diff_lcl = quantile(C_x_p_era2, 0.025),
      Cx_p_era2_diff_med = quantile(C_x_p_era2, 0.5),
      Cx_p_era2_diff_ucl = quantile(C_x_p_era2, 0.975),
      Cx_p_era2_diff_mean = mean(C_x_p_era2),
      Cx_p_era3_diff_lcl = quantile(C_x_p_era3, 0.025),
      Cx_p_era3_diff_med = quantile(C_x_p_era3, 0.5),
      Cx_p_era3_diff_ucl = quantile(C_x_p_era3, 0.975),
      Cx_p_era3_diff_mean = mean(C_x_p_era3),
      
      tot_Cx_era1_diff_lcl = quantile(tot_Cx_era1_diff, 0.025),
      tot_Cx_era1_diff_med = quantile(tot_Cx_era1_diff, 0.5),
      tot_Cx_era1_diff_ucl = quantile(tot_Cx_era1_diff, 0.975),
      tot_Cx_era1_diff_mean = mean(tot_Cx_era1_diff),
      tot_Cx_era2_diff_lcl = quantile(tot_Cx_era2_diff, 0.025),
      tot_Cx_era2_diff_med = quantile(tot_Cx_era2_diff, 0.5),
      tot_Cx_era2_diff_ucl = quantile(tot_Cx_era2_diff, 0.975),
      tot_Cx_era2_diff_mean = mean(tot_Cx_era2_diff),
      tot_Cx_era3_diff_lcl = quantile(tot_Cx_era3_diff, 0.025),
      tot_Cx_era3_diff_med = quantile(tot_Cx_era3_diff, 0.5),
      tot_Cx_era3_diff_ucl = quantile(tot_Cx_era3_diff, 0.975),
      tot_Cx_era3_diff_mean = mean(tot_Cx_era3_diff))
  
  cod.marginal.female.summary <- cod.marginal.female %>% group_by(Cause.of.death) %>%
    summarise(
      Cx_era1_diff_lcl = quantile(C_x_era1_diff, 0.025),
      Cx_era1_diff_med = quantile(C_x_era1_diff, 0.5),
      Cx_era1_diff_ucl = quantile(C_x_era1_diff, 0.975),
      Cx_era1_diff_mean = mean(C_x_era1_diff),
      Cx_era2_diff_lcl = quantile(C_x_era2_diff, 0.025),
      Cx_era2_diff_med = quantile(C_x_era2_diff, 0.5),
      Cx_era2_diff_ucl = quantile(C_x_era2_diff, 0.975),
      Cx_era2_diff_mean = mean(C_x_era2_diff),
      Cx_era3_diff_lcl = quantile(C_x_era3_diff, 0.025),
      Cx_era3_diff_med = quantile(C_x_era3_diff, 0.5),
      Cx_era3_diff_ucl = quantile(C_x_era3_diff, 0.975),
      Cx_era3_diff_mean = mean(C_x_era3_diff),
      
      Cx_p_era1_diff_lcl = quantile(C_x_p_era1, 0.025),
      Cx_p_era1_diff_med = quantile(C_x_p_era1, 0.5),
      Cx_p_era1_diff_ucl = quantile(C_x_p_era1, 0.975),
      Cx_p_era1_diff_mean = mean(C_x_p_era1),
      Cx_p_era2_diff_lcl = quantile(C_x_p_era2, 0.025),
      Cx_p_era2_diff_med = quantile(C_x_p_era2, 0.5),
      Cx_p_era2_diff_ucl = quantile(C_x_p_era2, 0.975),
      Cx_p_era2_diff_mean = mean(C_x_p_era2),
      Cx_p_era3_diff_lcl = quantile(C_x_p_era3, 0.025),
      Cx_p_era3_diff_med = quantile(C_x_p_era3, 0.5),
      Cx_p_era3_diff_ucl = quantile(C_x_p_era3, 0.975),
      Cx_p_era3_diff_mean = mean(C_x_p_era3),
      
      tot_Cx_era1_diff_lcl = quantile(tot_Cx_era1_diff, 0.025),
      tot_Cx_era1_diff_med = quantile(tot_Cx_era1_diff, 0.5),
      tot_Cx_era1_diff_ucl = quantile(tot_Cx_era1_diff, 0.975),
      tot_Cx_era1_diff_mean = mean(tot_Cx_era1_diff),
      tot_Cx_era2_diff_lcl = quantile(tot_Cx_era2_diff, 0.025),
      tot_Cx_era2_diff_med = quantile(tot_Cx_era2_diff, 0.5),
      tot_Cx_era2_diff_ucl = quantile(tot_Cx_era2_diff, 0.975),
      tot_Cx_era2_diff_mean = mean(tot_Cx_era2_diff),
      tot_Cx_era3_diff_lcl = quantile(tot_Cx_era3_diff, 0.025),
      tot_Cx_era3_diff_med = quantile(tot_Cx_era3_diff, 0.5),
      tot_Cx_era3_diff_ucl = quantile(tot_Cx_era3_diff, 0.975),
      tot_Cx_era3_diff_mean = mean(tot_Cx_era3_diff))
  
  cod.marginal.male.summary$sex <- "Male"
  cod.marginal.female.summary$sex <- "Female"
  
  cod.marginal.summary <- rbind(cod.marginal.male.summary, cod.marginal.female.summary)
  
  cod.marginal.summary$state <- state_i
  
  #extract_life_expectancy_results
  BlackWhite <- data.frame(stratum.id = character(),
                           LE_white_lcl = numeric(), LE_white_med = numeric(), LE_white_ucl = numeric(), LE_white_mean = numeric(), 
                           LE_black_lcl = numeric(), LE_black_med = numeric(), LE_black_ucl = numeric(), LE_black_mean = numeric(), 
                           LE_wbgap_lcl = numeric(), LE_wbgap_med = numeric(), LE_wbgap_ucl = numeric(), LE_wbgap_mean = numeric(), 
                           stringsAsFactors = F)
  
  age_minbin <- le.calculations[[1]]$calcs[[1]]$age.decomp$Ages
  cods <- le.calculations[[1]]$calcs[[1]]$cod.marginal$Cause.of.death
  
  age.decomp.estimates <- data.frame(stratum.id = rep(levels.state.year.sex, 
                                                      each = length(age_minbin)),
                                     age_minbin = numeric(19*n),
                                     age_cont_yrs_lcl = numeric(19*n), 
                                     age_cont_yrs_med = numeric(19*n), 
                                     age_cont_yrs_ucl = numeric(19*n),
                                     age_cont_yrs_mean = numeric(19*n), 
                                     age_cont_prop_lcl = numeric(19*n), 
                                     age_cont_prop_med = numeric(19*n), 
                                     age_cont_prop_ucl = numeric(19*n),
                                     age_cont_prop_mean = numeric(19*n), 
                                     stringsAsFactors = F)
  
  cod.marginal.estimates <- data.frame(stratum.id = rep(levels.state.year.sex, 
                                                        each = length(cods)),
                                       COD = character(6*n),
                                       COD_cont_yrs_lcl = numeric(6*n), 
                                       COD_cont_yrs_med = numeric(6*n), 
                                       COD_cont_yrs_ucl = numeric(6*n),
                                       COD_cont_yrs_mean = numeric(6*n), 
                                       COD_cont_prop_lcl = numeric(6*n), 
                                       COD_cont_prop_med = numeric(6*n), 
                                       COD_cont_prop_ucl = numeric(6*n),
                                       COD_cont_prop_mean = numeric(6*n), 
                                       stringsAsFactors = F)
  
  age.cod.estimates <- data.frame(stratum.id = rep(levels.state.year.sex, 
                                                   each = length(cods)*length(age_minbin)),
                                  age_minbin = numeric(19*6*n),   
                                  COD = character(19*6*n),
                                  age_COD_cont_yrs_lcl = numeric(19*6*n), 
                                  age_COD_cont_yrs_med = numeric(19*6*n), 
                                  age_COD_cont_yrs_ucl = numeric(19*6*n),
                                  age_COD_cont_yrs_mean = numeric(19*6*n),
                                  age_COD_cont_prop_lcl = numeric(19*6*n), 
                                  age_COD_cont_prop_med = numeric(19*6*n), 
                                  age_COD_cont_prop_ucl = numeric(19*6*n), 
                                  age_COD_cont_prop_mean = numeric(19*6*n), 
                                  stringsAsFactors = F)
  
  
  for(i in 1:n) {
    
    BlackWhite[i, c("stratum.id")] <- le.calculations[[i]]$stratum.id  
    
    #LIFE EXPECTANCY EXTRACT RESULTS
    BlackWhite[i , c("LE_white_lcl", "LE_white_med", "LE_white_ucl")] <- quantile(map_dbl(le.calculations[[i]]$calcs,
                                                                                          ~.x$le.df$LE_White), 
                                                                                  c(0.025, 0.5, 0.975))
    BlackWhite[i , c("LE_black_lcl", "LE_black_med", "LE_black_ucl")] <- quantile(map_dbl(le.calculations[[i]]$calcs,
                                                                                          ~.x$le.df$LE_Black), 
                                                                                  c(0.025, 0.5, 0.975))
    BlackWhite[i , c("LE_wbgap_lcl", "LE_wbgap_med", "LE_wbgap_ucl")] <- quantile(map_dbl(le.calculations[[i]]$calcs,
                                                                                          ~.x$le.df$LE_WBgap), 
                                                                                  c(0.025, 0.5, 0.975))
    
    BlackWhite[i , c("LE_white_mean")] <- mean(map_dbl(le.calculations[[i]]$calcs, ~.x$le.df$LE_White))
    BlackWhite[i , c("LE_black_mean")] <- mean(map_dbl(le.calculations[[i]]$calcs, ~.x$le.df$LE_Black))
    BlackWhite[i , c("LE_wbgap_mean")] <- mean(map_dbl(le.calculations[[i]]$calcs, ~.x$le.df$LE_WBgap))    
    
    
    #AGE DECOMPOSITION EXTRACT RESULTS
    age.decomp.posteriors <- map_df(le.calculations[[i]]$calcs, ~ .x$age.decomp$C_x)
    age.decomp.posteriors$age_minbin <- age_minbin
    age.decomp.posteriors <- age.decomp.posteriors %>% tidyr::gather(`1`:`1000`, key = "post.samp", value = "C_x")
    
    age.decomp.estimates[age.decomp.estimates$stratum.id == le.calculations[[i]]$stratum.id, 
                         c("age_minbin", "age_cont_yrs_lcl", "age_cont_yrs_med", "age_cont_yrs_ucl", "age_cont_yrs_mean")] <- 
      age.decomp.posteriors %>% 
      group_by(age_minbin) %>%
      summarise(lcl_C_x = quantile(C_x, 0.025),
                median_C_x = quantile(C_x, 0.5), 
                ucl_C_x = quantile(C_x, 0.975),
                mean_C_x = mean(C_x))
    
    rm(age.decomp.posteriors)
    
    age.decomp.posteriors2 <- map_df(le.calculations[[i]]$calcs, ~ .x$age.decomp$C_x_proportion)
    age.decomp.posteriors2$age_minbin <- age_minbin
    age.decomp.posteriors2 <- age.decomp.posteriors2 %>% tidyr::gather(`1`:`1000`, key = "post.samp", value = "C_x_proportion")
    
    age.decomp.estimates[age.decomp.estimates$stratum.id == le.calculations[[i]]$stratum.id, 
                         c("age_minbin", "age_cont_prop_lcl", "age_cont_prop_med", "age_cont_prop_ucl", "age_cont_prop_mean")] <- 
      age.decomp.posteriors2 %>% 
      group_by(age_minbin) %>%
      summarise(lcl_C_x_proportion = quantile(C_x_proportion, 0.025),
                median_C_x_proportion = quantile(C_x_proportion, 0.5), 
                ucl_C_x_proportion = quantile(C_x_proportion, 0.975),
                mean_C_x_proportion = mean(C_x_proportion))
    
    rm(age.decomp.posteriors2)
    
    #COD DECOMPOSITION EXTRACT RESULTS
    cod.marginal.posteriors <- map_df(le.calculations[[i]]$calcs, ~ .x$cod.marginal$C_x)
    cod.marginal.posteriors$COD <- as.character(cods)
    cod.marginal.posteriors <- cod.marginal.posteriors %>% tidyr::gather(`1`:`1000`, key = "post.samp", value = "C_x")
    
    cod.marginal.estimates[cod.marginal.estimates$stratum.id == le.calculations[[i]]$stratum.id, 
                           c("COD", "COD_cont_yrs_lcl", "COD_cont_yrs_med", "COD_cont_yrs_ucl", "COD_cont_yrs_mean")] <- 
      cod.marginal.posteriors %>% 
      group_by(COD) %>%
      summarise(lcl_C_x = quantile(C_x, 0.025),
                median_C_x = quantile(C_x, 0.5), 
                ucl_C_x = quantile(C_x, 0.975),
                mean_C_x = mean(C_x))
    
    rm(cod.marginal.posteriors)
    
    cod.marginal.posteriors2 <- map_df(le.calculations[[i]]$calcs, ~ .x$cod.marginal$C_x_proportion)
    cod.marginal.posteriors2$COD <- as.character(cods)
    cod.marginal.posteriors2 <- cod.marginal.posteriors2 %>% tidyr::gather(`1`:`1000`, key = "post.samp", 
                                                                           value = "C_x_proportion")
    
    cod.marginal.estimates[cod.marginal.estimates$stratum.id == le.calculations[[i]]$stratum.id,
                           c("COD", "COD_cont_prop_lcl", "COD_cont_prop_med", "COD_cont_prop_ucl", "COD_cont_prop_mean")] <-
      cod.marginal.posteriors2 %>%
      group_by(COD) %>%
      summarise(lcl_C_x_proportion = quantile(C_x_proportion, 0.025),
                median_C_x_proportion = quantile(C_x_proportion, 0.5),
                ucl_C_x_proportion = quantile(C_x_proportion, 0.975),
                mean_C_x_proportion = mean(C_x_proportion))
    
    rm(cod.marginal.posteriors2)  
    
    #AGE-COD EXTRACT RESULTS
    age.cod.decomp.posteriors <- map_df(le.calculations[[i]]$calcs, ~ .x$cod.decomp$C_xi)
    age.cod.decomp.posteriors$age_minbin <- le.calculations[[i]]$calcs[[1]]$cod.decomp$Ages
    age.cod.decomp.posteriors$COD <- le.calculations[[i]]$calcs[[1]]$cod.decomp$Cause.of.death
    age.cod.decomp.posteriors <- age.cod.decomp.posteriors %>% tidyr::gather(`1`:`1000`, key = "post.samp", value = "C_xi")
    
    age.cod.estimates[age.cod.estimates$stratum.id == le.calculations[[i]]$stratum.id,
                      c("age_minbin", "COD", "age_COD_cont_yrs_lcl", "age_COD_cont_yrs_med", "age_COD_cont_yrs_ucl", "age_COD_cont_yrs_mean")] <-
      age.cod.decomp.posteriors %>%
      group_by(age_minbin, COD) %>%
      summarise(lcl_C_x = quantile(C_xi, 0.025),
                median_C_x = quantile(C_xi, 0.5),
                ucl_C_x = quantile(C_xi, 0.975),
                mean_C_x = mean(C_xi))
    
    rm(age.cod.decomp.posteriors)
    
    age.cod.decomp.posteriors2 <- map_df(le.calculations[[i]]$calcs, ~ .x$cod.decomp$C_xi_proportion)
    age.cod.decomp.posteriors2$age_minbin <- le.calculations[[i]]$calcs[[1]]$cod.decomp$Ages
    age.cod.decomp.posteriors2$COD <- le.calculations[[i]]$calcs[[1]]$cod.decomp$Cause.of.death
    age.cod.decomp.posteriors2 <- age.cod.decomp.posteriors2 %>% tidyr::gather(`1`:`1000`, key = "post.samp", 
                                                                               value = "C_xi_proportion")
    
    age.cod.estimates[age.cod.estimates$stratum.id == le.calculations[[i]]$stratum.id,
                      c("age_minbin", "COD", "age_COD_cont_prop_lcl", "age_COD_cont_prop_med", "age_COD_cont_prop_ucl", "age_COD_cont_prop_mean")] <-
      age.cod.decomp.posteriors2 %>%
      group_by(age_minbin, COD) %>%
      summarise(lcl_C_x_proportion = quantile(C_xi_proportion, 0.025),
                median_C_x_proportion = quantile(C_xi_proportion, 0.5),
                ucl_C_x_proportion = quantile(C_xi_proportion, 0.975),
                mean_C_x_proportion = mean(C_xi_proportion))
    
    rm(age.cod.decomp.posteriors2)
  }
  
  #save_results
  save(mortality.rates, mortality.rates.wide, le.calculations, cod.marginal.male, cod.marginal.female, file = paste0("~/BW_results/results_", state_i, ".Rdata")) 
  write.csv(x = BlackWhite, file = paste0("~/BlackWhiteMortalityGap/Results/BlackWhite_", state_i, ".csv"))
  write.csv(x = age.decomp.estimates, file = paste0("~/BlackWhiteMortalityGap/Results/age_decomp_", state_i, ".csv"))
  write.csv(x = cod.marginal.estimates, file = paste0("~/BlackWhiteMortalityGap/Results/cod_marginal_", state_i, ".csv"))
  write.csv(x = age.cod.estimates, file = paste0("~/BlackWhiteMortalityGap/Results/age_cod_", state_i, ".csv"))
  write.csv(x = mortality.rates.summary, file = paste0("~/BlackWhiteMortalityGap/Results/mortality_rates_", state_i, ".csv"))
  write.csv(x = mortality.rates.difference, file = paste0("~/BlackWhiteMortalityGap/Results/mortality_rates_diff_", state_i, ".csv"))
  write.csv(x = cod.marginal.summary, file = paste0("~/BlackWhiteMortalityGap/Results/cod_change_", state_i, ".csv"))
}