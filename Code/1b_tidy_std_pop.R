std.pop <- read.csv("~/BlackWhiteMortalityGap/Data/US_Standard_Population.csv")

std.pop <- std.pop %>% select(Age, US_Standard_2000) %>% filter(Age != "Total")

std.pop <- std.pop %>% mutate(age.weight = US_Standard_2000/1000000)

std.pop <- std.pop %>% mutate(age_minbin = 0*(Age == "00 years") + 1*(Age == "01-04 years") + 5*(Age == "05-09 years") +
                                10*(Age == "10-14 years") + 15*(Age == "15-19 years") + 20*(Age == "20-24 years") +
                                25*(Age == "25-29 years") + 30*(Age == "30-34 years") + 35*(Age == "35-39 years") +
                                40*(Age == "40-44 years") + 45*(Age == "45-49 years") + 50*(Age == "50-54 years") +
                                55*(Age == "55-59 years") + 60*(Age == "60-64 years") + 65*(Age == "65-69 years") +
                                70*(Age == "70-74 years") + 75*(Age == "75-79 years") + 80*(Age == "80-84 years") +
                                85*(Age == "85+ years")) %>% select(US_Standard_2000, age.weight, age_minbin)

write.csv(std.pop, "~/BlackWhiteMortalityGap/Data/US_Standard_Population_for_model.csv")
