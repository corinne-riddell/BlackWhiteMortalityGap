
for(i in c(14:27)){

  included.states <- c("Alabama", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
                       "Delaware", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
                       "Louisiana", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",   
                       "Missouri", "Nebraska", "Nevada", "New Jersey", "New Mexico", "New York",
                       "North Carolina", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
                       "South Carolina",  "Tennessee", "Texas", "Virginia", "Washington", "Washington DC",
                       "West Virginia", "Wisconsin") 
  
  source("./Code/entire_analysis.R")
  system.time(entire.analysis(state_i = included.states[i], chosen.seed = 123))
  rm(list = ls(all = TRUE))
}

