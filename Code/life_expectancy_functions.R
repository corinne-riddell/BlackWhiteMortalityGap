#dataset needs to be in order of age
#none of the population.counts can equal 0. If there are rows with population counts of 0 then first need 
#to run the collapse age brackets function.

#change names to something like col.name.death.counts or death.counts.in.data

life.table <- function(data, num.ages.in.group, death.counts, population.counts, ave.prop.lived = NA){
  
  data["R_x"] <- data[death.counts]/data[population.counts] #mortality rates
  
  if(is.na(ave.prop.lived) == T) {
    data["a_x"] <- 0.5 #average proportion of interval lived
    data[1, "a_x"] <- 0.09
  } else{
    data["a_x"] <- data[ave.prop.lived]
  }
  
  data["q_x"] <- data[num.ages.in.group]*data["R_x"]/(1 + (1 - data["a_x"])*data[num.ages.in.group]*data["R_x"]) 
  #probability of dying
  
  data[dim(data)[1], "q_x"] <- 1 #100% die in last age group
  
  data["p_x"] <- 1 - data["q_x"] #probability of survival
  
  data["l_x"] <- NA #l_x: is the number of individuals alive at the start of the interval
  data[1, "l_x"] <- 100000 
  
  for(i in 2:dim(data)[1]) {
    data[i, "l_x"] <- data[i - 1, "l_x"]*data[i-1, "p_x"]
  }
  
  data["d_x"] <- data["l_x"]*data["q_x"] #number of deaths
  data["L_x"] <- (data["l_x"] - data["d_x"])*data[num.ages.in.group] + data["d_x"]*data["a_x"]*data[num.ages.in.group] #person time in the age bracket
  data[dim(data)[1], "L_x"] <- data[dim(data)[1], "l_x"]/data[dim(data)[1], "R_x"] #different for last age bracket
  
  #now to calculate the total person time lived at or after age x, T_x:
  data["cumsum"] <- cumsum(data["L_x"])
  data["l.cumsum"] <- c(0, unlist(data[1:(dim(data)[1] - 1), "cumsum"]))
  data["T_x"] <- sum(data["L_x"]) - data["l.cumsum"]
  data["e_x"] <- data["T_x"]/data["l_x"]
  
  return(data)
  
} 

#takes the life table output from the first function -- need two life tables to perform the comparison
#relies on the names of the columns kept as-is in the life tables -- user should not change the column names!
#name.lt1 and name.lt2 are used to create variable names so these shouldn't have spaces in them and they can be 
#made up (they don't have to replicate the name of the life table object)
le_age_decomp <- function(life.table1, name.lt1 = 1, life.table2, name.lt2 = 2, age.groups) {

  life.table2["accumulated.lived.after"] <- c(unlist(life.table2[2:dim(life.table2)[1], "T_x"]), 0) #T_(x+n) in Auger's formula on pg 576 (step 1)
  
  life.table1["num.alive.next.interval"] <- c(unlist(life.table1[2:dim(life.table2)[1], "l_x"]), 0) #l_(x+n) 
  life.table2["num.alive.next.interval"] <- c(unlist(life.table2[2:dim(life.table2)[1], "l_x"]), 1) #--> end in 1 to fix calculation error of dividing by 0

  decomp.table <- data.frame("Ages" = life.table1[ ,age.groups], life.table1["e_x"], life.table2["e_x"],
                             "C_x" = rep(NA, dim(life.table1)[1]), "C_x_proportion" = rep(NA, dim(life.table1)[1]))
  
  names(decomp.table)[2] <- paste0("LE_", name.lt1)
  names(decomp.table)[3] <- paste0("LE_", name.lt2)
  
  decomp.table["C_x"] <- 
    (life.table1["l_x"]/life.table1[1, "l_x"])*((life.table2["L_x"]/life.table2["l_x"]) - (life.table1["L_x"]/life.table1["l_x"])) +
    ((life.table2["accumulated.lived.after"]/life.table2[1, "l_x"])*
     ((life.table1["l_x"]/life.table2["l_x"]) - (life.table1["num.alive.next.interval"]/life.table2["num.alive.next.interval"]))
     )
  
  decomp.table["C_x_proportion"] <- decomp.table["C_x"]/sum(decomp.table["C_x"])
  
  decomp.table["adds_to_gap"] <- ifelse(decomp.table$C_x > 0, T, F)

  return(decomp.table)
}


#cause of death table must be organized in a very specific way
#it needs to be sorted by COD, and then by age (within COD)
cause_of_death_decomp <- function(life.table1, life.table2, decomp.table, 
                                  cod.table, age.colname.cod.table, COD.colname.cod.table, 
                                  prop1.colname.cod.table, prop2.colname.cod.table) {
  
  C_x <- decomp.table[["C_x"]]
  prop2 <- cod.table[[prop2.colname.cod.table]]
  R_x2 <- life.table2[["R_x"]]
  prop1 <- cod.table[[prop1.colname.cod.table]]
  R_x1 <- life.table1[["R_x"]]
  
  C_xi = C_x*((prop2*R_x2 - prop1*R_x1)/(R_x2 - R_x1))
  
  COD.decomp.table <- data.frame("Ages" = cod.table[ , age.colname.cod.table], 
                                 cod.table[ , prop1.colname.cod.table],
                                 cod.table[ , prop2.colname.cod.table],
                                 "Cause.of.death" = cod.table[ , COD.colname.cod.table],
                                 "C_xi" = C_xi)
  
  COD.decomp.table["C_xi_proportion"] <- COD.decomp.table["C_xi"]/sum(COD.decomp.table["C_xi"])
  
  names(COD.decomp.table)[2] <- prop1.colname.cod.table
  names(COD.decomp.table)[3] <- prop2.colname.cod.table
  
  return(COD.decomp.table)
  }

#this function assumes the C_xi variable is names C_xi (not changed after running previous function)
#you must supply the names of the columns that correspond to the age group and cause of death variable.
make_dataset_cod_plot <- function(cod.decomp.table, age.groups, cause.of.death) {
  
  cod.decomp.table$sign <- ifelse(cod.decomp.table$C_xi < 0, "negative", "positive")
  cod.decomp.table$group <- interaction(cod.decomp.table$Ages, cod.decomp.table$sign, sep = ".")
  
  cod.decomp.table <- cod.decomp.table[order(cod.decomp.table[age.groups], 
                                             cod.decomp.table["sign"], 
                                             cod.decomp.table[cause.of.death]), ]
  
  cod.decomp.table$start <- 0
  cod.decomp.table$finish <- cod.decomp.table$C_xi
  
  cod.decomp.table$start2 <- 0
  cod.decomp.table$finish2 <- cod.decomp.table$C_xi_proportion
  
  updated <- NULL
  for (g in unique(cod.decomp.table$group)) {
    subset <- cod.decomp.table[cod.decomp.table$group == g, ]
    
    length.group <- dim(subset)[1]
    
    if(length.group > 1){
      for (i in 2:dim(subset)[1]) {
        subset$start[i] <- subset$finish[i - 1]
        subset$finish[i] <- subset$start[i] + subset$C_xi[i]
        
        subset$start2[i] <- subset$finish2[i - 1]
        subset$finish2[i] <- subset$start2[i] + subset$C_xi_proportion[i]
      }
    }
  
    updated <- rbind(updated, subset)

    }
  return(updated)
}

#contribution to the change in the gap
contribution.to.gap.change <- function(type.of.decomp, decomp.table1, decomp.table2) {
  if(type.of.decomp == "Age") {
    contribution_data <- data.frame("Ages" = decomp.table1[["Ages"]], 
                                    "Contribution.to.change" = decomp.table1[["C_x"]] - decomp.table2[["C_x"]])
  }
  
  if(type.of.decomp == "COD") {
     contribution_data <- data.frame("COD" = decomp.table1[["Cause.of.death"]], 
                                     "Contribution.to.change" = decomp.table1[["C_x_COD"]] - decomp.table2[["C_x_COD"]])
  }
  
  contribution_data[["Contrib.to.change.prop"]] <- contribution_data[["Contribution.to.change"]]/sum(contribution_data[["Contribution.to.change"]])
  contribution_data[["narrowed_gap"]] <- contribution_data[["Contribution.to.change"]] >= 0 
  
  return(contribution_data)
}

#############################
## Functions for the server #
#############################

#input: data.frame from kathryn's bayesian model 
#input is specific to one state, sex, year, but across all age groups and both races
#take this dataset and calculate LE for blacks and for whites and the gap (LE.gap = LE.white - LE.black)
#returns these three values.
life_expectancy_and_gap <- function(data) {
  library(sqldf)
  
  #aggegrate over COD within agegroup and race
  data.aggregated <- sqldf('select race, population, age_bin, year, sex, state, sum(smoothed_deaths) as total_smoothed_deaths from data group by race, age_bin')  
  
  #add num.ages.in.group
  data.aggregated$nx = 1*(data.aggregated$age_bin == 1) + 4*(data.aggregated$age_bin == 2) + 5*(data.aggregated$age_bin > 2)
  
  #calculate LE by race
  lt.black <- life.table(data = subset(data.aggregated, race == "Black"), num.ages.in.group = "nx" , death.counts = "total_smoothed_deaths", population.counts = "population")
  lt.white <- life.table(data = subset(data.aggregated, race == "White"), num.ages.in.group = "nx" , death.counts = "total_smoothed_deaths", population.counts = "population")
  
  return(data.frame("LE_Black" = lt.black$e_x[1], "LE_White" = lt.white$e_x[1], "LE_WBgap" = lt.white$e_x[1] - lt.black$e_x[1]))
}




#this function takes a list of the samples from the posterior distribution
#it then calculated the median ("med"), and 97.5th and 2.5th percentiles
#of the credible intervals as a function of the number of posterior draws.
#it returns a data.frame.
calc_running_med_CI_posterior <- function(list_empirical_posterior) {
  LE_distn <- data.frame("LE_Black" = rep(NA, length(list_empirical_posterior)), 
                         "LE_White" = rep(NA, length(list_empirical_posterior)),
                         "LE_WBgap" = rep(NA, length(list_empirical_posterior)),
                         "run_med_LEB" = rep(NA, length(list_empirical_posterior)), 
                         "run_med_LEW" = rep(NA, length(list_empirical_posterior)),
                         "run_med_LEG" = rep(NA, length(list_empirical_posterior)),
                         "run_975_LEB" = rep(NA, length(list_empirical_posterior)), 
                         "run_975_LEW" = rep(NA, length(list_empirical_posterior)),
                         "run_975_LEG" = rep(NA, length(list_empirical_posterior)),                       
                         "run_025_LEB" = rep(NA, length(list_empirical_posterior)), 
                         "run_025_LEW" = rep(NA, length(list_empirical_posterior)),
                         "run_025_LEG" = rep(NA, length(list_empirical_posterior))
  )
  
  for(i in 1:length(list_empirical_posterior)) {
    LE_distn[i, c("LE_Black", "LE_White", "LE_WBgap")] <- list_empirical_posterior[[i]]
    LE_distn$run_med_LEB[i] <- median(LE_distn$LE_Black[1:i])
    LE_distn$run_med_LEW[i] <- median(LE_distn$LE_White[1:i])
    LE_distn$run_med_LEG[i] <- median(LE_distn$LE_WBgap[1:i])
    
    LE_distn$run_975_LEB[i] <- quantile(LE_distn$LE_Black[1:i], 0.975)
    LE_distn$run_975_LEW[i] <- quantile(LE_distn$LE_White[1:i], 0.975)
    LE_distn$run_975_LEG[i] <- quantile(LE_distn$LE_WBgap[1:i], 0.975)
    
    LE_distn$run_025_LEB[i] <- quantile(LE_distn$LE_Black[1:i], 0.025)
    LE_distn$run_025_LEW[i] <- quantile(LE_distn$LE_White[1:i], 0.025)
    LE_distn$run_025_LEG[i] <- quantile(LE_distn$LE_WBgap[1:i], 0.025)
  }
  
  LE_distn$row <- 1:length(LE_distn$LE_Black)
  
  return(LE_distn)
}

#this function plots the credible intervals vs. # posterior draws
#and saves on the server in the black_white_mortality_project folder
plot_estimate_CI_vs_posterior <- function(LE_distn, file.name) {
  black.converge.plot <- ggplot(LE_distn, aes(x = row, y = run_med_LEB)) + geom_line() + 
    geom_line(aes(y = run_025_LEB), lty = 2) +
    geom_line(aes(y = run_975_LEB), lty = 2) +
    ylab("") + xlab("No. of posterior samples") + ggtitle("Convergence of Black Life Expectancy and 95% Credible Interval")
  
  white.converge.plot <- ggplot(LE_distn, aes(x = row, y = run_med_LEW)) + geom_line() + 
    geom_line(aes(y = run_025_LEW), lty = 2) +
    geom_line(aes(y = run_975_LEW), lty = 2) +
    ylab("") + xlab("No. of posterior samples") + ggtitle("Convergence of White Life Expectancy and 95% Credible Interval")
  
  gap.converge.plot <- ggplot(LE_distn, aes(x = row, y = run_med_LEG)) + geom_line() + 
    geom_line(aes(y = run_025_LEG), lty = 2) +
    geom_line(aes(y = run_975_LEG), lty = 2) +
    ylab("") + xlab("No. of posterior samples") + ggtitle("Convergence of Black-White Life Expectancy Gap and 95% Credible Interval")
  
  grob <- arrangeGrob(black.converge.plot, white.converge.plot, gap.converge.plot, nrow = 3)
  
  ggsave(grob, filename = paste0("~/black_white_mortality_project/", file.name, ".tiff"), width = 10, height = 15, units = "in", dpi = 100)
  
  return(list(white.converge.plot, black.converge.plot, gap.converge.plot))
}

##all of the steps together:
investigate_convergence <- function(year, state, sex, n_post_samp, graph_file_name) {
  mcmc_dist <- extract_mcmc_dist(year=year, state=state, sex=sex, n_post_samp=n_post_samp)
  saved_bayes = lapply(X = mcmc_dist, FUN=life_expectancy_and_gap)
  posterior.dataframe <- calc_running_med_CI_posterior(saved_bayes)
  plots <- plot_estimate_CI_vs_posterior(posterior.dataframe, graph_file_name)
  return(list(mcmc_dist, saved_bayes, posterior.dataframe, plots))
}