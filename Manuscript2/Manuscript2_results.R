library(tidyverse)
#load("cod_contributions.Rdata")

#This file depends on first running "ageCOD_manuscript.Rmd" so that the data frames
#`women_cod_contributions` and `men_cod_contributions` are in memory.

# Abstract
# LE gap by year in each state
women_cod_contributions %>% 
  filter(year==1969 | year==2013) %>% 
  group_by(state, year) %>%
  summarize(le_gap = sum(age_COD_cont_yrs_mean))

men_cod_contributions %>% 
  filter(year==1969 | year==2013) %>% 
  group_by(state, year) %>%
  summarize(le_gap = sum(age_COD_cont_yrs_mean))

# contribution for young men
men_cod_contributions %>% 
  filter(year==2013, age_collapse == "<1 years" | age_collapse == "1-19 years" | age_collapse == "20-39 years") %>% 
  group_by(state, year) %>%
  summarize(le_gap = sum(age_COD_cont_yrs_mean))

# contribution for young women
women_cod_contributions %>% 
  filter(year==2013, age_collapse == "<1 years" | age_collapse == "1-19 years" | age_collapse == "20-39 years") %>% 
  group_by(state, year) %>%
  summarize(le_gap = sum(age_COD_cont_yrs_mean))

# CVD among those 40-64
men_cod_contributions %>% 
  filter(year==1969 | year == 2013, COD == "Cardiovascular", age_collapse == "40-64 years") %>% 
  group_by(state, year) %>%
  summarize(le_gap = sum(age_COD_cont_yrs_mean))

women_cod_contributions %>% 
  filter(year==1969 | year == 2013, COD == "Cardiovascular", age_collapse == "40-64 years") %>% 
  group_by(state, year) %>%
  summarize(le_gap = sum(age_COD_cont_yrs_mean))


# cancers among men
men_cod_contributions %>% 
  filter(year==1969 | year == 2013, COD == "Cancers") %>% 
  group_by(state, year) %>%
  summarize(le_gap = sum(age_COD_cont_yrs_mean))

# cancers among women
women_cod_contributions %>% 
  filter(year==1969 | year == 2013, COD == "Cancers") %>% 
  group_by(state, year) %>%
  summarize(le_gap = sum(age_COD_cont_yrs_mean))


## Main paper results

# LE gap by year in each state
women_cod_contributions %>% 
  filter(year==1969 | year==2013) %>% 
  group_by(state, year) %>%
  summarize(le_gap = sum(age_COD_cont_yrs_mean))

men_cod_contributions %>% 
  filter(year==1969 | year==2013) %>% 
  group_by(state, year) %>%
  summarize(le_gap = sum(age_COD_cont_yrs_mean))

# Contribution of infant mortality in women
women_cod_contributions %>% 
  filter(year==1969 | year==2013, age_collapse == "<1 year") %>% 
  group_by(state, year) %>%
  summarize(mean_le_gap = sum(age_COD_cont_yrs_mean))

# Contribution of infant mortality in men
men_cod_contributions %>% 
  filter(year==1969 | year==2013, age_collapse == "<1 year") %>% 
  group_by(state, year) %>%
  summarize(le_gap = sum(age_COD_cont_yrs_mean))


# CVD in Georgia for women
women_cod_contributions %>% 
  filter(year==1969 | year==2013, COD == "Cardiovascular") %>% 
  group_by(state, year) %>%
  summarize(le_gap = sum(age_COD_cont_yrs_mean))

# CVD among older women
women_cod_contributions %>% 
  filter(year==1969 | year==2013, age_collapse == "85+ years", COD == "Cardiovascular") %>% 
  group_by(state, year) %>%
  summarize(le_cont = sum(age_COD_cont_yrs_mean))

# Cancer contribution for women
women_cod_contributions %>% 
  filter(year==2013, COD == "Cancers") %>% group_by(state) %>%
  summarize(le_cont = sum(age_COD_cont_yrs_mean))

# Injury contribution among working-age women
women_cod_contributions %>% 
  filter(year==1969| year == 2013, COD == "Injuries", age_collapse == "40-64 years") %>% 
  group_by(state, year) %>% 
  summarize(le_cont = sum(age_COD_cont_yrs_mean))


# Contribution of infant mortality in men
men_cod_contributions %>% 
  filter(year==1969 | year==2013, age_collapse == "<1 year") %>% 
  group_by(state, year) %>%
  summarize(le_cont = sum(age_COD_cont_yrs_mean))

# Injuries in Illinois men
men_cod_contributions %>% 
  filter(COD == "Injuries", state=="Illinois") %>% 
  group_by(year) %>% 
  summarize(le_cont = sum(age_COD_cont_yrs_mean)) %>%
  print(n=45)
  
