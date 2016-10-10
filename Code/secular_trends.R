
setwd("~/Documents/BlackWhiteMortalityGap/Code")

source('4_smoothing_models_src.R') 


dat.clean.alabama$crude_new = dat.clean.alabama$Count/dat.clean.alabama$Population 

ds = dat.clean.alabama[dat.clean.alabama$Age==12, ] 

ggplot(subset(dat.clean.alabama, Race2=="White" & Sex2=="Male" & COD2=='All other causes' & Age==8), aes(x = Population, y=Count)) + geom_point(aes(col=crude_new))  

+ geom_smooth(col="black") + scale_y_continuous("Mortality Rate") + ggtitle("White Men in Alabama (all years)")



# mortality rate over time including missing data bounds 

ds = subset(dat.clean.alabama, Race2=="White" & Sex2=="Male" & COD2=='All other causes' & Age==1)
plot(ds$Year, ds$crude_new, type='l', ylim=c(0, 0.0005))
lines(ds$Year, ds$Count_md1/ds$Population, col='red')
lines(ds$Year, ds$Count_md9/ds$Population, col='blue') 

N = length(ds[,1])
ds$smoothed_rate = rep(NA, N) 

for(i in 2:N) {
  if( is.na(ds$crude_new[i])==T) {
    ds$smoothed_rate[i] = ds$crude_new[i-1]
    if( is.na(ds$smoothed_rate[i]) == T) { 
      ds$smoothed_rate[i] = ds$smoothed_rate[i-1] 
      } 
  }
}


lines(ds$Year, ds$smoothed_rate, col='purple')


# Population size by year, faceted by Agecat

g = ggplot(subset(dat.clean.alabama, Race2=="Black" & Sex2=="Male" & COD2=='Injuries'), aes(x = Year, y=Population)) + geom_point(aes(col=crude_new)) + facet_wrap( ~ Age2, scales='free') 

ggsave(g, filename='/Users/kathryn/Desktop/poptime.png', width=300, height=100, units=c('mm') )

