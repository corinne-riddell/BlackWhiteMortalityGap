
life.table = function(data, num.ages.in.group, death.counts, population.counts, ave.prop.lived = NA){
  
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