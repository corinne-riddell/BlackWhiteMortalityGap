load("/Users/corinneriddell/Dropbox/BlackWhiteGap/Data/alabama_only.Rdata")

#example data
alabama.by.strata <- 
  dat.clean.alabama %>% group_by(Year2, RaceSex, Age2) %>%
  arrange(RaceSex, Year2, Age2) %>%
  summarise(total_deaths5 = sum(Count_md5),
            Population = first(Population), 
            Race2 = first(Race2), 
            Sex2 = first(Sex2), 
            StateYearRaceSex = first(StateYearRaceSex))


life.table <- function(data, calendar.years, age.groups, num.ages.in.group, death.counts, population.counts){
  #Ave_Lived_by_Died = ifelse(data[age.groups]=="<1 year", 0.1, 0.5)
  
  temp.data <- data %>% mutate(age.spec.death.rate = data[death.counts]/data[population.counts])
  
  return(head(data[years]))
  
  
} 

life.table(alabama.by.strata, "Year2", "Age2", "total_deaths5", "Population")