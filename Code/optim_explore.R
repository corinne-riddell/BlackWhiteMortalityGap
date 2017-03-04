
dat.clean = read.csv('~/dat_clean.csv') 

included.states = c("Alabama", "Arizona", "Arkansas", "California", "Colorado", "Connecticut",
                     "Delaware", "Florida", "Georgia", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
                     "Louisiana", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",   
                     "Missouri", "Nebraska", "Nevada", "New Jersey", "New Mexico", "New York",
                     "North Carolina", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
                     "South Carolina",  "Tennessee", "Texas", "Virginia", "Washington", "Washington DC",
                     "West Virginia", "Wisconsin")  

state_abbrev = as.character(quote(c(AL, AZ, AR, CA, CO, CT, DE, FL, GA, IL, IN, IA, KS, KY, LA, 
                 MD, MA, MI, MN, MS, MO, NE, NV, NJ, NM, NY, NC, OH, OK, OR, 
                 PA, RI, SC, TN, TX, VA, WA, DC, WV, WI)))[-1]


state_id_match = data.frame(variable = c(state.name, 'Washington DC'), id = c(state.abb, 'DC'))

#state_id_match  = data.frame(variable = included.states, id = state_abbrev)
cod_id_match    = data.frame(variable = unique(dat.clean$COD), id = c('CAR', 'CAN', 'CDS', 'NDS', 'INJ', 'AOC') )
sex_id_match    = data.frame(variable = unique(dat.clean$sex), id = c('M', 'F'))
race_id_match   = data.frame(variable = unique(dat.clean$race), id = c('B', 'W'))

id_sex = substr(dat.clean$sex, 1, 1)
id_race = substr(dat.clean$race, 1, 1)
id_age = dat.clean$age.n
id_year = dat.clean$year
id_cod = cod_id_match$id[match(dat.clean$COD, cod_id_match$variable)]
id_state = state_id_match$id[match(dat.clean$state, state_id_match$variable)]

dat.clean$RID = paste0(id_state, id_year, id_sex, id_race, id_age, id_cod) 

head(dat.clean) 
length(unique(dat.clean$RID))
length(dat.clean$state.n) 

head(dat.clean)


setwd('/home/kathryn/BlackWhiteMortalityGap/Data/data_by_state')

for(i in 1:length(included.states)) { 
  name = paste0('dat_clean_', included.states[i], '.csv')
  ds_temp = dat.clean[dat.clean$state == included.states[i], ]
  write.csv(ds_temp, file=name)
}


