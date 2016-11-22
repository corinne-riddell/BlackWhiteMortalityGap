
not.suppressed <- foreign::read.dta("/Users/corinneriddell/Dropbox/VitalStats/notsuppressed_0610.dta")

not.suppressed$cod_cat[not.suppressed$COD_full == "In situ, benign or unknown behavior neoplasm"] <- "All other causes"

not.suppressed.grouped <- not.suppressed %>% 
  filter(agegrp > 1) %>%
  group_by(stabbrs, Year3, agegrp, Sex, Race, cod_cat) %>%
  summarise(total_deaths = sum(death), 
            region = first(region),
            division = first(division)) %>%
  mutate(COD2 = factor(cod_cat, levels = c("Cardiovascular", "Cancers", "Communicable", "Non-communicable", 
                                           "Injuries","All other causes")))


head(not.suppressed.grouped)

load(file = "/Users/corinneriddell/Dropbox/BlackWhiteGap/Data/main_datasets.Rdata")

years0610 <- dat.clean[dat.clean$Year3 >= 2006 & dat.clean$Year3 <= 2010 & dat.clean$Age > 1, ] 
years0610$agegrp <- years0610$Age

years0610 <- years0610 %>% mutate(stabbrs = factor(State, levels = c(0:50), 
                                                 labels = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC",
                                                            "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", 
                                                            "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", 
                                                            "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", 
                                                            "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", 
                                                            "VT", "VA", "WA", "WV", "WI", "WY")))

validation_data_0610 <- merge(years0610, not.suppressed.grouped, by = c("stabbrs", "Year3", "agegrp", "Sex", "Race", "COD2"))

validation_data_0610 <- validation_data_0610 %>% mutate(diff.count2 = Count - total_deaths)
summary(validation_data_0610$diff.count2)

validation_data_0610 <- validation_data_0610[, -c(33:44, 49)]
validation_data_0610 <- validation_data_0610[, -c(24:32)]

save(validation_data_0610, file = "/Users/corinneriddell/Dropbox/BlackWhiteGap/Data/validation_data_0610.Rdata")
