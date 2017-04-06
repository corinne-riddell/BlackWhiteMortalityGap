
#
# entire_analysis.R: All functions wrapped together for full analysis by state  
#               
#
# Written by Kathryn Morrison & Corinne Riddell, 2017 
#
#
# kt.morrison@mail.mcgill.ca | corinne.riddell@mail.mcgill.ca 
# Last revised: 06-04-2017 

        # 0. --------------------- PREAMBLE --------------------- 
prepare_environment = function(state_i)  {
        
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
  library(data.table)
  library(dtplyr)
  library(stringi)

name <- paste0('~/BlackWhiteMortalityGap/Data/data_by_state/dat_clean_', state_i, '.csv')
dat.clean <- read.csv(name) 
return(dat.clean)
}
  
        # 1. --------------------- BAYES SMOOTHING --------------------- 

bayes_smoothing_bystate = function(dat.clean, n.cods, seed) {        
  
  # previous time: 32 mins
  r_BM <- run_analysis(dataset = dat.clean, sex = 'Male', race = 'Black', n.cods = n.cods, seed = seed) 
  r_BF <- run_analysis(dataset = dat.clean, sex = 'Female', race = 'Black', n.cods = n.cods, seed = seed)   
  r_WM <- run_analysis(dataset = dat.clean, sex = 'Male', race = 'White', n.cods = n.cods, seed = seed)   
  r_WF <- run_analysis(dataset = dat.clean, sex = 'Female', race = 'White', n.cods = n.cods, seed = seed)  
  
  r.All = rbind(r_BM, r_BF, r_WM, r_WF)
  rm(r_BM, r_BF, r_WM, r_WF) 
  
  return(r.All)
}  
  
      # 3. --------------------- AGE-STANDARDIZED MORTALITY RATES --------------------- 
age_stdz_mortality_bystate = function(dat.clean, state_i, r.All) {
  
  std.pop <- read.csv("~/BlackWhiteMortalityGap/Data/US_Standard_Population_for_model.csv") %>% select(US_Standard_2000, age.weight, age_minbin)
  dat.clean.stdmortrate <- data.table(RID = dat.clean$RID, age_minbin = dat.clean$age_minbin, key = 'RID') 
  r.All.table = data.table(r.All, key='RID')
  r.All2 = merge(r.All.table, dat.clean.stdmortrate, by = "RID", all.x=TRUE, all.y=FALSE)
  
  r.All.merged = data.frame(merge(r.All2, std.pop, by='age_minbin')) 
  
  r.All.merged$race = substr(r.All.merged$RID, 8, 8)
  r.All.merged$sex = substr(r.All.merged$RID, 7, 7)
  r.All.merged$year = substr(r.All.merged$RID, 3,6)
  r.All.merged$cod = substr(r.All.merged$RID, start = (nchar(as.character(r.All.merged$RID)) - 2), stop= nchar(as.character(r.All.merged$RID))) 
   
  # Merging based on data.table instead of data.frame is way faster; no idea why (yet)
  # http://stackoverflow.com/questions/11146967/efficient-alternatives-to-merge-for-larger-data-frames-r
   
  mortality.rates <- r.All.merged %>%    #this isn't the fastest  ; approx 3 mins 
    group_by(post.samp, year, race, sex, cod) %>%  
    arrange(age_minbin) %>%
    summarise(age.std.mortality.rate = sum(age.weight*smoothed_rate), #the sum is across age bins
              rate.per.100k = age.std.mortality.rate*100000)
 
  mortality.rates.summary <- mortality.rates %>% group_by(year, race, sex, cod) %>%  
    summarise(rate.per.100k_lcl = quantile(rate.per.100k, 0.025), 
              rate.per.100k_med = quantile(rate.per.100k, 0.5),
              rate.per.100k_ucl = quantile(rate.per.100k, 0.975),
              rate.per.100k_mean = mean(rate.per.100k))
  
  mortality.rates.white <- mortality.rates[mortality.rates$race=='W', c('post.samp', 'year', 'sex', 'cod', 'rate.per.100k') ] 
  mortality.rates.black <- mortality.rates[mortality.rates$race=='B', c('post.samp', 'year', 'sex', 'cod', 'rate.per.100k') ] 
  
  names(mortality.rates.white)[names(mortality.rates.white) == 'rate.per.100k'] <- 'rate.per.100k.white'
  names(mortality.rates.black)[names(mortality.rates.black) == 'rate.per.100k'] <- 'rate.per.100k.black'
  
  mortality.rates.wide <- merge(mortality.rates.black, mortality.rates.white, by = c('post.samp', 'sex', 'year', 'cod')) 
    
  mortality.rates.wide$rate.difference <- mortality.rates.wide$rate.per.100k.black - mortality.rates.wide$rate.per.100k.white
  
  mortality.rates.difference <- mortality.rates.wide %>% 
    group_by(year, sex, cod) %>%
    summarise(rate.difference_LCL = quantile(rate.difference, 0.025), #summarize across posterior samples (post.samp)
              rate.difference_med = quantile(rate.difference, 0.5),
              rate.difference_UCL = quantile(rate.difference, 0.975),
              rate.difference_mean = mean(rate.difference))
 
  # instead, save to csv: mort.rate.difference, mort.rate.summary 
  write.csv(x = mortality.rates.summary, file = paste0("~/BlackWhiteMortalityGap/Results/mortality_rates_", state_i, ".csv"))
  write.csv(x = mortality.rates.difference, file = paste0("~/BlackWhiteMortalityGap/Results/mortality_rates_diff_", state_i, ".csv"))
  
}
  
      # 3. --------------------- RUN LIFE EXPECTANCY AND DECOMPOSITION CALCULATIONS --------------------- 
  # (previous time: 2.25 hours) 
    
life_expectancy_and_decomp = function(dat.clean, state_i, r.All) { 

  
  r.All$strata = paste0(state_i, '.', substr(r.All$RID, 3, 6), '.', ifelse(substr(r.All$RID, 7, 7) == 'M', 'Male', 'Female')) #substr(r.All$RID, 3, 7)
  stratas = unique(r.All$strata)
  
  n.post.samp = length(unique(r.All$post.samp)) 
  r.All$race = substr(r.All$RID, 8, 8)
  r.All = merge(data.table(r.All, key='RID'), 
                     data.table(dat.clean %>% select(age_minbin, year, sex, state, COD, RID), key='RID'), 
                     by='RID') 
  
  n <- length(stratas)

  le.calculations = vector(mode = "list", length = n)

  counter = 1
  for(stratum_i in stratas){ # stratum_i = stratas[1]
    data_sub <- subset(r.All, strata == stratum_i) # data = data_sub[data_sub$post.samp==1, ]

    le.calculations[[counter]]$stratum.id <- stratum_i
    system.time(le.calculations[[counter]]$calcs <- by(data = data_sub,
                                           INDICES = list(data_sub$post.samp),
                                           FUN = function(x) life_expectancy_and_gap(data = x)))

    counter <- counter + 1

    print(stratum_i)
  }
   return(le.calculations)
}

# 3. --------------------- RUN LIFE EXPECTANCY AND DECOMPOSITION CALCULATIONS PT II --------------------- 
 
#This still needs to be double checked to see if it works with the new code 
#CR: This doesn't work as it stands - relies on "strata" variable that isn't defined. 
life_expectancy_and_decomp_pt2 = function(le.calculations, r.All) {
  
  strata = unique(r.All$strata)
  #calculation the contour decomposition
  years <- c(1969, 1983, 1993, 2013)
  
  #save(r.All, file = paste0("~/BW_results/r_All_", state_i, ".Rdata")) 
  levels.state.year.sex <- strata
  #rm(r.All)
  
  #calculate the difference in the COD contribution across the three eras.
  indices.female <- intersect(union_all(grep("1969", strata), grep("1983", strata), grep("1993", strata), grep("2013", strata)), grep("F", strata))
  indices.male <- intersect(union_all(grep("1969", strata), grep("1983", strata), grep("1993", strata), grep("2013", strata)), grep("M", strata))
  
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
  #save(mortality.rates, mortality.rates.wide, le.calculations, cod.marginal.male, cod.marginal.female, file = paste0("~/BW_results/results_", state_i, ".Rdata")) 
  write.csv(x = BlackWhite, file = paste0("~/BlackWhiteMortalityGap/Results/BlackWhite_", state_i, ".csv"))
  write.csv(x = age.decomp.estimates, file = paste0("~/BlackWhiteMortalityGap/Results/age_decomp_", state_i, ".csv"))
  write.csv(x = cod.marginal.estimates, file = paste0("~/BlackWhiteMortalityGap/Results/cod_marginal_", state_i, ".csv"))
  write.csv(x = age.cod.estimates, file = paste0("~/BlackWhiteMortalityGap/Results/age_cod_", state_i, ".csv"))
  write.csv(x = cod.marginal.summary, file = paste0("~/BlackWhiteMortalityGap/Results/cod_change_", state_i, ".csv"))
}


