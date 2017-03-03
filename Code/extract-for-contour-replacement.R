
source("~/BlackWhiteMortalityGap/Contour-replacement/scripts/decomp_contour.R")
source("~/BlackWhiteMortalityGap/Contour-replacement/scripts/ex.R")

load("~/BW_results/results_New York.Rdata")
load("~/BW_results/r_All_New York.Rdata")

r.All.1 <- r.All %>% filter(post.samp == 1)

black <- r.All.1 %>% 
  filter(race == "Black" & sex == "Male" & year %in% c(1969, 1983)) %>% 
  group_by(year, age_minbin) %>%
  summarise(total_deaths = sum(smoothed_deaths),
            race = first(race), 
            population = first(population)) %>%
  mutate(mort.rate = total_deaths/population) %>%
  select(age_minbin, year, mort.rate) %>%
  tidyr::spread(year, mort.rate) %>%
  rename(Age = age_minbin, mx1 = `1969`, mx2=`1983`)

white <- r.All.1 %>% 
  filter(race == "White" & sex == "Male" & year %in% c(1969, 1983)) %>% 
  group_by(year, age_minbin) %>%
  summarise(total_deaths = sum(smoothed_deaths),
            race = first(race), 
            population = first(population)) %>%
  mutate(mort.rate = total_deaths/population) %>%
  select(age_minbin, year, mort.rate) %>%
  tidyr::spread(year, mort.rate) %>%
  rename(Age = age_minbin, mx1 = `1969`, mx2 = `1983`)

#this works
results <- decomp.contour(black, white, 
               ages = c(0, 15, 40, 65, 87),
               FUN = ex.per, sex = "m")

results <- decomp.contour(black, white, 
               ages = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 87),
               FUN = ex.per, sex = "m")

#now get it to work using my life expectancy function rather than the Max Planck one.

results <- decomp.contour(black, white, 
                          ages = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 87),
                          FUN = life.expectancy, )


#decomp.contour function
library(sqldf)
data.aggregated <- sqldf('select race, population, age_minbin, year, sex, state, sum(smoothed_deaths) as total_smoothed_deaths from r.All.1 group by race, age_minbin')

data.aggregated <- r.All.1 %>%  
  ungroup() %>%
  filter(sex == "Male" & year %in% c(1969, 1983)) %>% 
  select(race, population, age_minbin, year.n, year, sex, state, smoothed_deaths) %>%
  group_by(year.n, race, age_minbin) %>%
  summarise(total_smoothed_deaths = sum(smoothed_deaths), population = first(population), year = first(year))

data.aggregated$nx = 1*(data.aggregated$age_minbin == 0) + 4*(data.aggregated$age_minbin == 1) + 5*(data.aggregated$age_minbin > 1)

lt.black.1983 <- life.table(data = subset(data.aggregated, race == "Black" & year == 1983),
                       num.ages.in.group = "nx" , death.counts = "total_smoothed_deaths",
                       population.counts = "population", ave.prop.le1.lived = 0.10,
                       sortable.age = "age_minbin")
ltb2 <- lt.black.1983$e_x[1]

lt.black.1969 <- life.table(data = subset(data.aggregated, race == "Black" & year == 1969),
                            num.ages.in.group = "nx" , death.counts = "total_smoothed_deaths",
                            population.counts = "population", ave.prop.le1.lived = 0.10,
                            sortable.age = "age_minbin")
ltb1 <- lt.black.1969$e_x[1]

lt.white.1983 <- life.table(data = subset(data.aggregated, race == "White" & year == 1983),
                            num.ages.in.group = "nx" , death.counts = "total_smoothed_deaths",
                            population.counts = "population", ave.prop.le1.lived = 0.10,
                            sortable.age = "age_minbin")
ltw2 <- lt.white.1983$e_x[1]

lt.white.1969 <- life.table(data = subset(data.aggregated, race == "White" & year == 1969),
                            num.ages.in.group = "nx" , death.counts = "total_smoothed_deaths",
                            population.counts = "population", ave.prop.le1.lived = 0.10,
                            sortable.age = "age_minbin")
ltw1 <- lt.white.1969$e_x[1]

ltb2
ltw2

ltb1
ltw1

#recalculate le using mort rate for first age group in previous year
data = subset(data.aggregated, race == "Black" & year == 1983)
data$total_smoothed_deaths <- 

lt.black.1983 <- life.table(data = subset(data.aggregated, race == "Black" & year == 1983),
                            num.ages.in.group = "nx" , death.counts = "total_smoothed_deaths",
                            population.counts = "population", ave.prop.le1.lived = 0.10,
                            sortable.age = "age_minbin")
ltb2 <- lt.black.1983$e_x[1]

