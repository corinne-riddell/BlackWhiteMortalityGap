#dataset needs to be in order of age
#none of the population.counts can equal 0. If there are rows with population counts of 0 then first need 
#to run the collapse age brackets function.
life.table <- function(data, age.groups, num.ages.in.group, death.counts, population.counts, ave.prop.lived = NA){
  
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

le_age_decomp <- function(life.table1, life.table2, num.alive, interval.years.lived, accumulated.years.lived) {
  
  life.table2["accumulated.lived.after"] <- c(unlist(life.table2[2:dim(life.table2)[1], accumulated.years.lived]), 0) #T_(x+n) in Auger's formula on pg 576 (step 1)
  
  life.table1["num.alive.next.interval"] <- c(unlist(life.table1[2:dim(life.table2)[1], num.alive]), 0) #l_(x+n) 
  life.table2["num.alive.next.interval"] <- c(unlist(life.table2[2:dim(life.table2)[1], num.alive]), 1) #--> end in 1 to fix calculation error of dividing by 0

  decomp.table <- data.frame("Ages" = life.table1[ ,1], "Life_Expectancy_1" = life.table1["e_x"], "Life_Expectancy_2" = life.table2["e_x"],
                             "C_x" = rep(NA, dim(life.table1)[1]))
  
  names(decomp.table)[2] <- "Life_Expectancy_1"
  names(decomp.table)[3] <- "Life_Expectancy_2"
  
  decomp.table["C_x"] <- 
    (life.table1[num.alive]/life.table1[1, num.alive])*((life.table2[interval.years.lived]/life.table2[num.alive]) - (life.table1[interval.years.lived]/life.table1[num.alive])) +
    ((life.table2["accumulated.lived.after"]/life.table2[1, num.alive])*((life.table1[num.alive]/life.table2[num.alive]) - (life.table1["num.alive.next.interval"]/life.table2["num.alive.next.interval"])))

  return(decomp.table)
}

cause_of_death_decomp <- function() {
  
}
