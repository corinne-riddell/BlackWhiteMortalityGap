library(dplyr)
library(ggplot2)
library(ggthemes)

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

heatmap.cod.prop <- dat.clean %>% 
  filter(COD2 == "Cancers") %>% 
  select(State2, Age3, Year3, Sex2, Race2, cod_prop_death5, Population)

pdf(file = "/Users/corinneriddell/Documents/repos/BlackWhiteMortalityGap/output/COD_na_heatmap.pdf")
ggplot(data = subset(heatmap.cod.prop, Race2 == "Black" & Age3 ==85), 
       aes(x = Year3, y = State2, fill = cod_prop_death5)) +
  geom_tile() +
  facet_wrap(~Sex2) + scale_fill_continuous(na.value = "red", guide = guide_legend(title = "COD %")) +
  xlab("Year") + ylab("State") +
  ggtitle("Proportion of deaths due to cancer in 85+ year old blacks over time") +
  ggthemes::theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_blank())   

ggplot(data = subset(heatmap.cod.prop, Race2 == "Black" & Age3 ==40), 
       aes(x = Year3, y = State2, fill = cod_prop_death5)) +
  geom_tile() +
  facet_wrap(~Sex2) + scale_fill_continuous(na.value = "red", guide = guide_legend(title = "COD %")) +
  xlab("Year") + ylab("State") +
  ggtitle("Proportion of deaths due to cancer in 40-44 year old blacks over time") +
  ggthemes::theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_blank())   

ggplot(data = subset(heatmap.cod.prop, Race2 == "Black" & Age3 ==5), 
       aes(x = Year3, y = State2, fill = cod_prop_death5)) +
  geom_tile() +
  facet_wrap(~Sex2) + scale_fill_continuous(na.value = "red", guide = guide_legend(title = "COD %")) +
  xlab("Year") + ylab("State") +
  ggtitle("Proportion of deaths due to cancer in 5-9 year old blacks over time") +
  ggthemes::theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_blank())

ggplot(data = subset(heatmap.cod.prop, Race2 == "White" & Age3 ==85), 
       aes(x = Year3, y = State2, fill = cod_prop_death5)) +
  geom_tile() +
  facet_wrap(~Sex2) + scale_fill_continuous(na.value = "red", guide = guide_legend(title = "COD %")) +
  xlab("Year") + ylab("State") +
  ggtitle("Proportion of deaths due to cancer in 85+ year old whites over time") +
  ggthemes::theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_blank())   

ggplot(data = subset(heatmap.cod.prop, Race2 == "White" & Age3 ==40), 
       aes(x = Year3, y = State2, fill = cod_prop_death5)) +
  geom_tile() +
  facet_wrap(~Sex2) + scale_fill_continuous(na.value = "red", guide = guide_legend(title = "COD %")) +
  xlab("Year") + ylab("State") +
  ggtitle("Proportion of deaths due to cancer in 40-44 year old whites over time") +
  ggthemes::theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_blank())  

ggplot(data = subset(heatmap.cod.prop, Race2 == "White" & Age3 ==5), 
       aes(x = Year3, y = State2, fill = cod_prop_death5)) +
  geom_tile() +
  facet_wrap(~Sex2) + scale_fill_continuous(na.value = "red", guide = guide_legend(title = "COD %")) +
  xlab("Year") + ylab("State") +
  ggtitle("Proportion of deaths due to cancer in 5-9 year old whites over time") +
  ggthemes::theme_tufte(base_family="Helvetica") +
  theme(axis.ticks=element_blank())

dev.off()
