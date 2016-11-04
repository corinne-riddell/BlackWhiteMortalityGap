library(dplyr)
library(ggplot2)

#population heat maps
#goal: to explore population counts < 5000 to try and understand when the LE and SE calculations are meaningful.

#load the datasets
load(file = "/Users/corinneriddell/Dropbox/BlackWhiteGap/Data/Sep6_BWgap.Rdata")

#Pop_across_age is a variable in the dat2 dataset that listed the overall population for each age.
#create a dataset with one row per state, year, sex, race strata

max.pop <- max(dat2$Pop_across_age)

pop_data <- dat2 %>% 
  select(State2, Year3, Race2, Sex2, Age3, Pop_across_age) %>%
  filter(Age3 == 0) %>% #to remove duplicate rows by age 
  mutate(Pop_category = cut(Pop_across_age, breaks = c(0, 1000, 2500, 5000, 10000, 50000, max.pop + 1), include.lowest = T))

levels(pop_data$Pop_category) <- c("0 - 1,000","1,001 - 2,500","2,501 - 5,000","5,001 - 10,000","10,001 - 50,000", "50,001+")

plot <- ggplot(data = subset(pop_data, Race2 == "Black"), aes(x = Year3, y = State2, fill = Pop_category)) + 
  geom_tile(color = "white", size = 0.025) + 
  geom_tile() + 
  facet_grid(. ~ Sex2) +
  scale_fill_discrete(h = c(0, 200), guide = guide_legend(title = "Population size")) + 
  xlab("Year") + ylab("State") +
  ggtitle("Black population size by state over time") +
  theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_blank()) 

plot

ggsave(filename = "/Users/corinneriddell/Documents/repos/BlackWhiteMortalityGap/output/black_pop_heatmap", 
       plot = last_plot(), device = NULL,
       length = 10, width = 8, units = "in", dpi = 100)
