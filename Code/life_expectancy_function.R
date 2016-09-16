load("Data/alabama_only.Rdata") 
#note that in order for this relative pathway to work, you must make your local version of BlackWhiteMortalityGap to be an RStudio "project".
#to do so, go to File>New Project>Existing Directory, and select the BlackWhiteMortalityGap as the r project directory.

#example data
alabama.by.strata <- 
  dat.clean.alabama %>% group_by(Year2, RaceSex, Age2) %>%
  arrange(RaceSex, Year2, Age2) %>%
  mutate(nx = 1*(Age2 == "<1 year") + 4*(Age2 == "1-4 years") + 5*(Age2 != "<1 year" & Age2 != "1-4 years")) %>%
  summarise(total_deaths5 = sum(Count_md5),
            Population = first(Population), 
            Race2 = first(Race2), 
            Sex2 = first(Sex2), 
            StateYearRaceSex = first(StateYearRaceSex),
            nx = first(nx))


#dataset needs to be in order of age
life.table <- function(data, calendar.years, age.groups, num.ages.in.group, death.counts, population.counts){
  
  data["R_x"] <- data[death.counts]/data[population.counts] #mortality rates
  data["a_x"] <- 0.5 #average proportion of interval lived
  data[1, "a_x"] <- 0.09
  data["q_x"] <- data[num.ages.in.group]*data["R_x"]/(1 + (1 - data["a_x"])*data[num.ages.in.group]*data["R_x"]) #probability of dying
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
  data["l.cumsum"] <- c(0, data[1:(dim(data)[1] - 1), "cumsum"])
  data["T_x"] <- sum(data["L_x"]) - data["l.cumsum"]
  data["e_x"] <- data["T_x"]/data["l_x"]
  
  return(data)
  
} 

lt <- life.table(data = subset(alabama.by.strata, StateYearRaceSex = "Alabama.1969.White.Male"), age.groups = "Age2", 
           num.ages.in.group = "nx", death.counts = "total_deaths5", population.counts = "Population")
