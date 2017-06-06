#In this file, we call the function to run the analysis on each state.

#Originally, I ran the analysis in three separate tmux (terminal multiplex) windows, hence the three files for running the models.
#This is a less sophisticated way of running the models in parallel, because when I tried to parallelize it within RStudio
#it would take over the server and use all the cores, even when I told it to only use three.

#NOTE: see the "NOTE" in 2_entire_analysis.R that specifies two lines of code within that file that you will need to change
#to prevent errors!

for(i in c(1:13)){
  
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

