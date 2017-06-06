#dataset needs to be in order of age
#none of the population.counts can equal 0.

#change names to something like col.name.death.counts or death.counts.in.data

life.table <- function(data, sortable.age, num.ages.in.group, death.counts, population.counts, ave.prop.le1.lived = 0.09, return = "appended"){

  data <- dplyr::arrange_(data, sortable.age)
  
  data["R_x"] <- data[death.counts]/data[population.counts] #mortality rates
  data["a_x"] <- 0.5 #average proportion of interval lived
  data[1, "a_x"] <- ave.prop.le1.lived
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
  
  if(return == "appended") {
    return(data)
  } else if(return == "life expectancy at birth") {
    return(data[["e_x"]][1])
  } else if(return == "life table") {
    return(data[, c("sortable.age", "num.ages.in.group", "death.counts", "population.counts",
                    "R_x", "a_x", "q_x", "p_x", "l_x", "d_x", "L_x", "cumsum", "l.cumsum", "T_x", "e_x")])
  }
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
  COD.decomp.table["sign"] <- ifelse(COD.decomp.table["C_xi"] < 0, "negative", "positive")
  
  names(COD.decomp.table)[2] <- prop1.colname.cod.table
  names(COD.decomp.table)[3] <- prop2.colname.cod.table
  
  return(COD.decomp.table)
  }

#a general function that takes the decomposition table and adds variables to allow the user
#to easily plot the decomposition in a pleasing manner
make_pretty_decomp_plot <- function(decomp.table, strat.var.1, strat.var.2, strat.var.3, partition.bar.var, 
                                    sign.var, decomp.var, decomp.var.prop, type.of.data = NA) {
  if(is.na(type.of.data) == T | !(type.of.data %in% c("practice", "results"))){
    return(print("Must supply type of data as either practice or results."))
  }else if(type.of.data == "practice"){
    decomp.table["group"] <- interaction(decomp.table[[strat.var.1]], 
                                         decomp.table[[sign.var]], sep = ".")
    
    decomp.table <- decomp.table[order(decomp.table[[strat.var.1]], 
                                       decomp.table[[sign.var]], 
                                       decomp.table[[partition.bar.var]]), ]
  }else if(type.of.data == "results"){
    decomp.table["group"] <- interaction(decomp.table[[strat.var.1]], 
                                         decomp.table[[strat.var.2]], 
                                         decomp.table[[strat.var.3]], 
                                         decomp.table[[sign.var]], sep = ".")
    
    decomp.table <- decomp.table[order(decomp.table[[strat.var.1]], 
                                       decomp.table[[strat.var.2]], 
                                       decomp.table[[strat.var.3]], 
                                       decomp.table[[sign.var]], 
                                       decomp.table[[partition.bar.var]]), ]    
  }
  
  decomp.table$start <- 0
  decomp.table$finish <- decomp.table[[decomp.var]] #C_xi
  
  decomp.table$start2 <- 0
  decomp.table$finish2 <- decomp.table[[decomp.var.prop]] #C_xi_proportion
  
  updated <- NULL
  counter <- 1
  for (g in unique(decomp.table[["group"]])) {
    subset <- decomp.table[decomp.table[["group"]] == g, ]
    
    length.group <- dim(subset)[1]
    
    if(length.group > 1){
      for (i in 2:length.group) {
        subset$start[i] <- subset$finish[i - 1]
        subset$finish[i] <- subset$start[i] + subset[[decomp.var]][i]
        
        subset$start2[i] <- subset$finish2[i - 1]
        subset$finish2[i] <- subset$start2[i] + subset[[decomp.var.prop]][i]
      }
    }
    
    updated <- rbind(updated, subset)
    counter <- counter + 1
    print(counter)
  }
  
  return(updated)
}

#contribution to the change in the gap
contribution.to.gap.change <- function(type.of.decomp, decomp.table1, decomp.table2, type.of.data = NA) {
  if(type.of.decomp == "Age") {
    if(is.na(type.of.data) == T | !(type.of.data %in% c("practice", "results"))){
      return(print("Must supply type of data as either practice or results."))
    }else if(type.of.data == "practice"){
      contribution_data <- data.frame("Ages" = decomp.table1[["Ages"]], 
                                      "Contribution.to.change" = decomp.table1[["C_x"]] - decomp.table2[["C_x"]])
    }else if(type.of.data == "results"){
      contribution_data <- data.frame("Ages" = decomp.table1[["age_minbin"]], 
                                      "Contribution.to.change" = decomp.table1[["age_cont_yrs"]] - decomp.table2[["age_cont_yrs"]])
    }
    
  }
  
  if(type.of.decomp == "COD") {
    if(is.na(type.of.data) == T | !(type.of.data %in% c("practice", "results"))){
      return(print("Must supply type of data as either practice or results."))
    }else if(type.of.data == "practice"){
      contribution_data <- data.frame("COD" = decomp.table1[["Cause.of.death"]], 
                                      "Contribution.to.change" = decomp.table1[["C_x"]] - decomp.table2[["C_x"]])      
    }else if(type.of.data == "results"){
      contribution_data <- data.frame("COD" = decomp.table1[["COD"]], 
                                      "Contribution.to.change" = decomp.table1[["COD_cont_yrs"]] - decomp.table2[["COD_cont_yrs"]])
    }
    
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
  data.aggregated <- sqldf('select race, population, age_minbin, year, sex, state, sum(smoothed_deaths) as total_smoothed_deaths from data group by race, age_minbin')
  
  data <- merge(data, data.aggregated[, c("race", "age_minbin", "total_smoothed_deaths")], by = c("race", "age_minbin"))
  
  #add the cod proportions of all deaths
  data$cod_prop_smoothed_deaths <- data$smoothed_deaths/data$total_smoothed_deaths
  
  #make the cod table to be used in the cod decomposition
  ct.black <- data[data$race == "Black", c("age_minbin", "COD", "cod_prop_smoothed_deaths")]
  names(ct.black)[3] <- "prop.black"
  ct.white <- data[data$race == "White", c("age_minbin", "COD", "cod_prop_smoothed_deaths")]
  names(ct.white)[3] <- "prop.white"
  ct.both <- merge(ct.white, ct.black, by = c("age_minbin", "COD"))
  #ct.both <- ct.both[order(ct.both$COD, ct.both$age_minbin), ]
  ct.both <- dplyr::arrange_(ct.both, "COD", "age_minbin")
  
  #add num.ages.in.group
  data.aggregated$nx = 1*(data.aggregated$age_minbin == 0) + 4*(data.aggregated$age_minbin == 1) + 5*(data.aggregated$age_minbin > 1)
  
  #calculate LE by race
  lt.black <- life.table(data = subset(data.aggregated, race == "Black"),
                         num.ages.in.group = "nx" , death.counts = "total_smoothed_deaths",
                         population.counts = "population", ave.prop.le1.lived = 0.10,
                         sortable.age = "age_minbin")
  
  lt.white <- life.table(data = subset(data.aggregated, race == "White"),
                         num.ages.in.group = "nx" , death.counts = "total_smoothed_deaths",
                         population.counts = "population", ave.prop.le1.lived = 0.10,
                         sortable.age = "age_minbin")
  
  #calculate le_age_decomp
  age_decomp <- le_age_decomp(lt.black, "Black", lt.white, "White", "age_minbin")
  
  #calculate cod decomp
  if(!any(is.na(ct.both$prop.black)) & !any(is.na(ct.both$prop.white))) { #ensures that there are no ages with 0 deaths - for these COD prop == 0 and can't compute
    cod_decomp <- cause_of_death_decomp(lt.black, lt.white, age_decomp, ct.both, "age_minbin", "COD", "prop.black", "prop.white")
    cod_marginal <- make.cod.marginal(cod_decomp)
  } else {
    cod_decomp <- NA
    cod_marginal <- NA
  }
  
  
  #returns a list
  return(list(le.df = data.frame("LE_Black" = lt.black$e_x[1],
                                 "LE_White" = lt.white$e_x[1],
                                 "LE_WBgap" = lt.white$e_x[1] - lt.black$e_x[1]),
              age.decomp = age_decomp, cod.table = ct.both, cod.decomp = cod_decomp, cod.marginal = cod_marginal))
  
  # return(list(le.df = data.frame("LE_Black" = lt.black$e_x[1],
  #                                "LE_White" = lt.white$e_x[1],
  #                                "LE_WBgap" = lt.white$e_x[1] - lt.black$e_x[1])))
}

make.cod.marginal <- function(cod.decomp) {
  return(cod.decomp %>% 
           group_by(Cause.of.death) %>%
           summarise(C_x = sum(C_xi)) %>%
           mutate(total_Cx = sum(C_x), C_x_proportion = C_x/total_Cx)
  ) 
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
    LE_distn[i, c("LE_Black", "LE_White", "LE_WBgap")] <- list_empirical_posterior[[i]]$le.df

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
  LE_distn$CI_width_LEW <- LE_distn$run_975_LEW - LE_distn$run_025_LEW
  LE_distn$CI_width_LEB <- LE_distn$run_975_LEB - LE_distn$run_025_LEB
  LE_distn$CI_width_LEG <- LE_distn$run_975_LEG - LE_distn$run_025_LEG
  
  return(LE_distn)
}

#this function takes a list of the samples from the posterior distribution
#it then calculates the median and 97.5th and 2.5th percentiles
#of the credible intervals as a function of the number of posterior draws.
#for each of the age decomposition bins
calc_running_med_CI_age <- function(list_empirical_posterior) {
  age.decomp.distn <- data.frame(matrix(ncol = 6, nrow = 19*length(list_empirical_posterior)))
  names(age.decomp.distn) <- c("age_bin", "iteration", "C_x", "median", "lcl", "ucl")
  age.decomp.distn$age_bin <- rep(1:19, length(list_empirical_posterior))
  age.decomp.distn$iteration <- rep(1:length(list_empirical_posterior), each = 19)
  
  counter = 1
  for(i in 1:length(list_empirical_posterior)) {
    age.decomp.distn$C_x[counter:(counter + 19 - 1)] <- list_empirical_posterior[[i]]$age.decomp$C_x
    age.decomp.distn$median[counter:(counter + 19 - 1)] <- aggregate(C_x ~ age_bin, data = subset(age.decomp.distn, iteration %in% c(1:i)), FUN = median)$C_x 
    age.decomp.distn$lcl[counter:(counter + 19 - 1)] <- aggregate(C_x ~ age_bin, data = subset(age.decomp.distn, iteration %in% c(1:i)), probs = 0.025, FUN = quantile)$C_x 
    age.decomp.distn$ucl[counter:(counter + 19 - 1)] <- aggregate(C_x ~ age_bin, data = subset(age.decomp.distn, iteration %in% c(1:i)), probs = 0.975, FUN = quantile)$C_x
    counter = counter + 19
  }
  
  age.decomp.distn$CI_width <- age.decomp.distn$ucl - age.decomp.distn$lcl
  
  return(age.decomp.distn)
}

#this function takes a list of the samples from the posterior distribution
#it then calculates the median and 97.5th and 2.5th percentiles
#of the credible intervals as a function of the number of posterior draws.
#for each of cod age decomposition bins
calc_running_med_CI_cod <- function(list_empirical_posterior) {
  
  cod.decomp.distn <- data.frame(matrix(ncol = 6, nrow = 6*length(list_empirical_posterior)))
  names(cod.decomp.distn) <- c("cod", "iteration", "C_x", "median", "lcl", "ucl")
  cod.decomp.distn$cod <- rep(c("All other causes", "Cancers", "Cardiovascular", 
                                    "Communicable", "Injuries", "Non-communicable"), length(list_empirical_posterior))
  cod.decomp.distn$iteration <- rep(1:length(list_empirical_posterior), each = 6)
  
  counter = 1
  for(i in 1:length(list_empirical_posterior)) {
    codDecomp <- list_empirical_posterior[[i]]$cod.decomp
    codDecomp$cod <- codDecomp$Cause.of.death

    marginal.table <- sqldf('select cod, sum(C_xi) as C_x from codDecomp group by cod')  

    cod.decomp.distn$C_x[counter:(counter + 6 - 1)] <- marginal.table$C_x
    cod.decomp.distn$median[counter:(counter + 6 - 1)] <- aggregate(C_x ~ cod, data = subset(cod.decomp.distn, iteration %in% c(1:i)), FUN = median)$C_x 
    cod.decomp.distn$lcl[counter:(counter + 6 - 1)] <- aggregate(C_x ~ cod, data = subset(cod.decomp.distn, iteration %in% c(1:i)), probs = 0.025, FUN = quantile)$C_x 
    cod.decomp.distn$ucl[counter:(counter + 6 - 1)] <- aggregate(C_x ~ cod, data = subset(cod.decomp.distn, iteration %in% c(1:i)), probs = 0.975, FUN = quantile)$C_x
    counter = counter + 6
  }
  
  cod.decomp.distn$CI_width <- cod.decomp.distn$ucl - cod.decomp.distn$lcl
  
  return(cod.decomp.distn)
}

calc_running_med_CI_codage <- function(list_empirical_posterior) {
  age.cod.decomp.distn <- data.frame(matrix(ncol = 7, nrow = 19*6*length(list_empirical_posterior)))
  names(age.cod.decomp.distn) <- c("age", "cod", "C_x", "iteration", "median", "lcl", "ucl")
  age.cod.decomp.distn$cod <- factor(age.cod.decomp.distn$cod, levels = c("All other causes", "Cancers", "Cardiovascular", 
                                                                   "Communicable", "Injuries", "Non-communicable"))
  age.cod.decomp.distn$iteration <- rep(1:length(list_empirical_posterior), each = 19*6)  
  
  counter <- 1
  for(i in 1:length(list_empirical_posterior)) {
    age.cod.decomp.distn[counter:(counter + 19*6 - 1), c("age", "cod", "C_x")] <- list_empirical_posterior[[i]]$cod.decomp[, c("Ages", "Cause.of.death", "C_xi")]
    age.cod.decomp.distn$median[counter:(counter + 19*6 - 1)] <- aggregate(C_x ~ cod + age, 
                                                                           data = subset(age.cod.decomp.distn,
                                                                                         iteration %in% c(1:i)), 
                                                                           FUN = median)$C_x 
    age.cod.decomp.distn$lcl[counter:(counter + 19*6 - 1)] <- aggregate(C_x ~ cod + age, 
                                                                        data = subset(age.cod.decomp.distn,
                                                                                      iteration %in% c(1:i)), 
                                                                        probs = 0.025, FUN = quantile)$C_x 
    age.cod.decomp.distn$ucl[counter:(counter + 19*6 - 1)] <- aggregate(C_x ~ cod + age, 
                                                                        data = subset(age.cod.decomp.distn, 
                                                                                      iteration %in% c(1:i)), 
                                                                        probs = 0.975, FUN = quantile)$C_x
    counter = counter + 19*6
  }
  
  age.cod.decomp.distn$CI_width <- age.cod.decomp.distn$ucl - age.cod.decomp.distn$lcl
  
  return(age.cod.decomp.distn)
}

#this function plots the credible intervals vs. # posterior draws
#and saves on the server in the black_white_mortality_project folder
plot_estimate_CI_vs_posterior <- function(LE_distn, age_decomp, cod_decomp, codage_decomp, file.name, min, max) {
  black.converge.plot <- ggplot(LE_distn[min:max, ], aes(x = row, y = run_med_LEB)) + geom_line(col = "red") + 
    geom_line(aes(y = run_025_LEB), lty = 2, col = "red") +
    geom_line(aes(y = run_975_LEB), lty = 2, col = "red") +
    ylab("") + xlab("No. of posterior samples") + ggtitle("Convergence of Black Life Expectancy and 95% Credible Interval")
  
  white.converge.plot <- ggplot(LE_distn[min:max, ], aes(x = row, y = run_med_LEW)) + geom_line() + 
    geom_line(aes(y = run_025_LEW), lty = 2) +
    geom_line(aes(y = run_975_LEW), lty = 2) +
    ylab("") + xlab("No. of posterior samples") + ggtitle("Convergence of White Life Expectancy and 95% Credible Interval")
  
  both.width.plot <- ggplot(LE_distn[min:max, ], aes(x = row, y = CI_width_LEW)) + geom_line() + 
    geom_abline(intercept = LE_distn$CI_width_LEW[max], slope = 0, lty = 2) + 
    geom_line(aes(y = CI_width_LEB), col = "red") + 
    geom_abline(intercept = LE_distn$CI_width_LEB[max], slope = 0, lty = 2, col = "red")    
  
  gap.converge.plot <- ggplot(LE_distn[min:max, ], aes(x = row, y = run_med_LEG)) + geom_line(col = "blue") + 
    geom_line(aes(y = run_025_LEG), lty = 2, col = "blue") +
    geom_line(aes(y = run_975_LEG), lty = 2, col = "blue") +
    ylab("") + xlab("No. of posterior samples") + ggtitle("Convergence of Black-White Life Expectancy Gap and 95% Credible Interval")
 
  gap.width.plot <- ggplot(LE_distn[min:max, ], aes(x = row, y = CI_width_LEG)) + geom_line(col = "blue") + 
    geom_abline(intercept = LE_distn$CI_width_LEG[max], slope = 0, lty = 2, col = "blue") +
    ylab("") + xlab("No. of posterior samples") + ggtitle("Width of the 95% credible interval for the Gap")
  
  age.decomp.plot <- ggplot(subset(age_decomp, iteration %in% c(min:max)), aes(x = iteration, y = median)) + geom_line(col="green") + facet_wrap( ~ age_bin, scales = "free_y") +
    geom_line(aes(y = lcl), lty = 2, col="green") + geom_line(aes(y = ucl), lty = 2, col="green") + ylab("Contribution (years)") + xlab("No. of posterior samples") +
    ggtitle("Convergence of Age-Specific Contributions to the Gap and 95% Credible Interval")

  age.width.plot <- ggplot(subset(age_decomp, iteration %in% c(min:max)), aes(x = iteration, y = CI_width)) + 
    geom_line(col="green") + facet_wrap( ~ age_bin, scales = "free_y") +
    ylab("") + xlab("No. of posterior samples") +
    ggtitle("Width of the 95% credible interval for the age contribution")
  
  cod.decomp.plot <- ggplot(subset(cod_decomp, iteration %in% c(min:max)), aes(x = iteration, y = median)) + geom_line(col="purple") + facet_wrap( ~ cod, scales = "free_y") +
    geom_line(aes(y = lcl), lty = 2, col="purple") + geom_line(aes(y = ucl), lty = 2, col="purple") + ylab("Contribution (years)") + xlab("No. of posterior samples") +
    ggtitle("Convergence of COD-Specific Contributions to the Gap and 95% Credible Interval")    
  
  cod.width.plot <- ggplot(subset(cod_decomp, iteration %in% c(min:max)), aes(x = iteration, y = CI_width)) + 
    geom_line(col="purple") + facet_wrap( ~ cod, scales = "free_y") +
    ylab("") + xlab("No. of posterior samples") +
    ggtitle("Width of the 95% credible interval for the COD contribution")

  codage_decomp$both <- interaction(codage_decomp$age, codage_decomp$cod, sep = ".")
  
  codage.decomp.plot <- ggplot(subset(codage_decomp, iteration %in% c(min:max)), aes(x = iteration, y = median)) + geom_line(col="orange") + facet_wrap( ~ both, scales = "free_y", ncol = 5) +
    geom_line(aes(y = lcl), lty = 2, col="orange") + geom_line(aes(y = ucl), lty = 2, col="orange") + ylab("Contribution (years)") + xlab("No. of posterior samples") +
    ggtitle("Convergence of Age-COD Contributions to the Gap and 95% Credible Interval")    
  
  codage.width.plot <- ggplot(subset(codage_decomp, iteration %in% c(min:max)), aes(x = iteration, y = CI_width)) + 
    geom_line(col="orange") + facet_wrap( ~ both, scales = "free_y", ncol = 5) +
    ylab("") + xlab("No. of posterior samples") +
    ggtitle("Width of the 95% credible interval for the Age-COD contribution")
  
  grob <- arrangeGrob(black.converge.plot, white.converge.plot, both.width.plot, 
                      gap.converge.plot, gap.width.plot, age.decomp.plot, age.width.plot,
                      cod.decomp.plot, cod.width.plot, nrow = 9)
  
  grob2 <- arrangeGrob(codage.decomp.plot, codage.width.plot)
  
  ggsave(grob, filename = paste0("~/black_white_mortality_project/", file.name, ".tiff"), 
         width = 10, height = 47, units = "in", dpi = 100, limitsize = F)
  
  ggsave(grob2, filename = paste0("~/black_white_mortality_project/", file.name, "cod_age.tiff"), 
         width = 10, height = 66, units = "in", dpi = 100, limitsize = F)
  
  return(list(white.converge.plot, black.converge.plot, both.width.plot, gap.converge.plot, gap.width.plot, 
              age.decomp.plot, age.width.plot, cod.decomp.plot, cod.width.plot, codage.decomp.plot, codage.width.plot))
}

##all of the steps together:
investigate_convergence <- function(year, state, sex, n_post_samp, graph_file_name, min, max) {
  mcmc_dist <- extract_mcmc_dist(year=year, state=state, sex=sex, n_post_samp=n_post_samp)
  mcmc_dist <- mcmc_dist[1:(n_post_samp - 50)] #because for some reason it makes 950 not 1000.... need to ask KTM
  saved_bayes = lapply(X = mcmc_dist, FUN=life_expectancy_and_gap)
  posterior.df <- calc_running_med_CI_posterior(saved_bayes)
  posterior.df.age <- calc_running_med_CI_age(saved_bayes)
  posterior.df.cod <- calc_running_med_CI_cod(saved_bayes)
  posterior.df.codage <- calc_running_med_CI_codage(saved_bayes)
  
  plots <- plot_estimate_CI_vs_posterior(posterior.df, posterior.df.age, posterior.df.cod, posterior.df.codage, graph_file_name, min, max)
  return(list(mcmc_dist, saved_bayes, posterior.df, posterior.df.age, posterior.df.cod, posterior.df.codage, plots))
}

#use this one for now, since need to run the mcmc_dist() step separately to remove the NA rows for the last 50 list elements
investigate_convergence_rest <- function(mcmc_dist, graph_file_name, min, max) {
  saved_bayes = lapply(X = mcmc_dist, FUN=life_expectancy_and_gap)
  posterior.df <- calc_running_med_CI_posterior(saved_bayes)
  posterior.df.age <- calc_running_med_CI_age(saved_bayes)
  posterior.df.cod <- calc_running_med_CI_cod(saved_bayes)
  posterior.df.codage <- calc_running_med_CI_codage(saved_bayes)
  
  plots <- plot_estimate_CI_vs_posterior(posterior.df, posterior.df.age, posterior.df.cod, posterior.df.codage, graph_file_name, min, max)
  return(list(mcmc_dist, saved_bayes, posterior.df, posterior.df.age, posterior.df.cod, posterior.df.codage, plots))
}

add_Census_Division <- function(data, state) {
  data$Census_Division <- NA
  
  data$Census_Division[data$state %in% c("Connecticut", "Maine", "Massachusetts", 
                                                                     "New Hampshire", "Rhode Island", "Vermont")] <- "New England"
  
  data$Census_Division[data$state %in% c("New Jersey", "New York", "Pennsylvania")] <- "Mid-Atlantic"
  
  data$Census_Division[data$state %in% c("Illinois", "Indiana", "Michigan", "Ohio", 
                                                                     "Wisconsin")] <- "East North Central"
  
  data$Census_Division[data$state %in% c("Iowa", "Kansas", "Minnesota", "Missouri", 
                                                                     "Nebraska", "North Dakota", "South Dakota")] <- "West North Central"
  
  data$Census_Division[data$state %in% c("Delaware", "Florida", "Georgia", "Maryland", 
                                                                     "North Carolina", "South Carolina", 
                                                                     "Virginia", "Washington DC", "West Virginia")] <- "South Atlantic"
  
  data$Census_Division[data$state %in% c("Alabama", "Kentucky", "Mississippi", "Tennessee")] <- "East South Central"
  
  data$Census_Division[data$state %in% c("Arkansas", "Louisiana", "Oklahoma", "Texas")] <- "West South Central"       
  
  data$Census_Division[data$state %in% c("Arizona", "Colorado", "Idaho", "Montana", 
                                                                     "Nevada", "New Mexico", "Utah", "Wyoming")] <- "Mountain"
  
  data$Census_Division[data$state %in% c("Alaska", "California", "Hawaii", "Oregon","Washington")] <- "Pacific"
  
  data$Census_Division <- factor(data$Census_Division, levels = c("New England", "Mid-Atlantic", "East North Central", "West North Central", "South Atlantic", "East South Central", "West South Central", "Mountain", "Pacific"))
  
  return(data)
}

add_Census_Region <- function(data, state) {
  data$Census_Region <- NA
  data$Census_Region[data$state %in% c("Iowa", "Kansas", "Minnesota", "Missouri", 
                                       "Nebraska", "North Dakota", "South Dakota", 
                                       "Illinois", "Indiana", "Michigan", "Ohio", 
                                       "Wisconsin")] <- "Midwest"
  
  data$Census_Region[data$state %in% c("Connecticut", "Maine", "Massachusetts", 
                                       "New Hampshire", "Rhode Island", "Vermont",
                                       "New Jersey", "New York", "Pennsylvania")] <- "Northeast"
  
  data$Census_Region[data$state %in% c("Arizona", "Colorado", "Idaho", "Montana", 
                                       "Nevada", "New Mexico", "Utah", "Wyoming",
                                       "Alaska", "California", "Hawaii", "Oregon",
                                       "Washington")] <- "West"
  
  data$Census_Region[data$state %in% c("Delaware", "Florida", "Georgia", "Maryland", 
                                       "North Carolina", "South Carolina", 
                                       "Virginia", "Washington DC", "West Virginia",
                                       "Alabama", "Kentucky", "Mississippi", "Tennessee", 
                                       "Arkansas", "Louisiana", "Oklahoma", "Texas")] <- "South"
  
  data$Census_Region <- factor(data$Census_Region, levels = c("West", "Midwest", "South", "Northeast"))
  
  return(data)
}

#This function adds the variable "state.map.order" to the dataframe to be used to plot the facets to approximate the shape of the continental US
#DOES NOT INCLUDE ALASKA AND HAWAII!!
reorder.as.map <- function(dataset, state.var, state.abbrev.var) {
  # Create unique blank strip labels for empty facets
  bl = sapply(1:37, function(n) paste(rep(" ",n),collapse=""))
  
  dataset["state.map.order"] <- factor(dataset[[state.var]], 
                                       levels = c(bl[1:10], "Maine",
                                                  bl[11:19], "Vermont", "New Hampshire",
                                                  "Washington", "Idaho", "Montana", "North Dakota", "Minnesota", "Illinois", "Wisconsin", "Michigan", "New York", "Massachusetts", "Rhode Island",
                                                  "Oregon", "Nevada", "Wyoming", "South Dakota", "Iowa", "Indiana", "Ohio", "Pennsylvania", "New Jersey", "Connecticut", bl[20],
                                                  "California", "Utah", "Colorado", "Nebraska", "Missouri", "Kentucky", "West Virginia", "Virginia", "Maryland", "Delaware", bl[21],
                                                  bl[22], "Arizona", "New Mexico", "Kansas", "Arkansas", "Tennessee", "North Carolina", "South Carolina", "Washington DC", bl[23:24],
                                                  bl[25:27], "Oklahoma", "Louisiana", "Mississippi", "Alabama", "Georgia", bl[28:29],
                                                  bl[30:33], "Texas", bl[34:37], "Florida")) 
  
  dataset["stabbrs.map.order"] <- factor(dataset[[state.abbrev.var]], 
                                       levels = c(bl[1:10], "ME",
                                                  bl[11:19], "VT", "NH",
                                                  "WA", "ID", "MT", "ND", "MN", "IL", "WI", "MI", "NY", "MA", "RI",
                                                  "OR", "NV", "WY", "SD", "IA", "IN", "OH", "PA", "NJ", "CT", bl[20],
                                                  "CA", "UT", "CO", "NE", "MO", "KY", "WV", "VA", "MD", "DE", bl[21],
                                                  bl[22], "AZ", "NM", "KS", "AR", "TN", "NC", "SC", "DC", bl[23:24],
                                                  bl[25:27], "OK", "LA", "MS", "AL", "GA", bl[28:29],
                                                  bl[30:33], "TX", bl[34:37], "FL")) 
  
  return(dataset)
}


reorder.as.map2 <- function(dataset, state.var, state.abbrev.var) {
  # Create unique blank strip labels for empty facets
  bl = sapply(1:24, function(n) paste(rep(" ",n),collapse=""))
  
  dataset["state.map.order"] <- factor(dataset[[state.var]], 
                                       levels = c(#bl[1:10], "Maine",
                                                  #bl[11:19], "Vermont", "New Hampshire",
                                                  "Washington", bl[1], bl[2], bl[3], "Minnesota", "Illinois", "Wisconsin", "Michigan", "New York", "Massachusetts", "Rhode Island",
                                                  "Oregon", "Nevada", bl[4], bl[5], "Iowa", "Indiana", "Ohio", "Pennsylvania", "New Jersey", "Connecticut", bl[6],
                                                  "California", bl[7], "Colorado", "Nebraska", "Missouri", "Kentucky", "West Virginia", "Virginia", "Maryland", "Delaware", bl[8],
                                                  bl[9], "Arizona", "New Mexico", "Kansas", "Arkansas", "Tennessee", "North Carolina", "South Carolina", "Washington DC", bl[10:11],
                                                  bl[12:14], "Oklahoma", "Louisiana", "Mississippi", "Alabama", "Georgia", bl[15:17],
                                                  bl[18:20], "Texas", bl[21:24], "Florida")) 
  
  dataset["stabbrs.map.order"] <- factor(dataset[[state.abbrev.var]], 
                                         levels = c(#bl[1:10], "ME",
                                                    #bl[11:19], "VT", "NH",
                                                    "WA", bl[1], bl[2], bl[3], "MN", "IL", "WI", "MI", "NY", "MA", "RI",
                                                    "OR", "NV", bl[4], bl[5], "IA", "IN", "OH", "PA", "NJ", "CT", bl[6],
                                                    "CA", bl[7], "CO", "NE", "MO", "KY", "WV", "VA", "MD", "DE", bl[8],
                                                    bl[9], "AZ", "NM", "KS", "AR", "TN", "NC", "SC", "DC", bl[10:11],
                                                    bl[12:14], "OK", "LA", "MS", "AL", "GA", bl[15:17],
                                                    bl[18:20], "TX", bl[21:24], "FL")) 
  
  return(dataset)
}