#dataset needs to be in order of age
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

le_age_decomp <- function(data1, data2, num.alive, interval.years.lived, accumulated.years.lived) {
  
  data2["accumulated.lived.after"] <- c(unlist(data2[2:dim(data2)[1], accumulated.years.lived]), 0) #T_(x+n) in Auger's formula on pg 576 (step 1)
  
  data1["num.alive.next.interval"] <- c(unlist(data1[2:dim(data2)[1], num.alive]), 0) #l_(x+n) 
  data2["num.alive.next.interval"] <- c(unlist(data2[2:dim(data2)[1], num.alive]), 1) #--> end in 1 to fix calculation error of dividing by 0
  #this *only* holds when data2 is the one with the last term in the denominator
  
  data1["C_x"] <- 
    (data1[num.alive]/data1[1, num.alive])*((data2[interval.years.lived]/data2[num.alive]) - (data1[interval.years.lived]/data1[num.alive])) +
    ((data2["accumulated.lived.after"]/data2[1, num.alive])*((data1[num.alive]/data2[num.alive]) - (data1["num.alive.next.interval"]/data2["num.alive.next.interval"])))

  return(data1)
}

