
setwd("~/Documents/BlackWhiteMortalityGap/Code")

source('4_smoothing_models_src.R') 

# smoothing by time only 
ds = subset_data(race='Black', sex='Male', cod='Injuries') 
#jags_model = jags_simple(ds$ds_jags) 
jags_model = jags_smooth_year(ds$ds_jags) 

smoothed_deaths = jagsresults(x=jags_model, params=c('mu'))  
mort_rate = jagsresults(x=jags_model, params=c('mort.rate'))

mean = 0.002
tau = 10000000
x = seq(-1,1, by=0.00001)
y = dnorm(x, mean=0.002, sd=1/sqrt(tau))
#plot(x,y, type='l', xlim = c(0, 0.05))
#


# Constructing a dataset for visualizations 

#r = data.frame(deaths=ds$ds$Count, smoothed_deaths=round(smoothed_deaths[,'50%'],0)) 
r = data.frame(agecat = rep(unique(dat.clean.alabama$Age2), each=45))
r$year = seq(1969, 2013)
N = length(r$year)
smoothed_rate = round(smoothed_deaths[,'50%'],0) /ds$ds$Population
r$mort_rate = ds$ds$Count/ds$ds$Population
r$rate_type = rep('crude_rate', N)
r$rate_type = ifelse( is.na(r$mort_rate)==TRUE, 'smoothed_rate', r$rate_type)

lower_mort_rate_bound = ifelse( is.na(r$mort_rate)==TRUE, 1/ds$ds$Population, NA)
upper_mort_rate_bound = ifelse( is.na(r$mort_rate)==TRUE, 9/ds$ds$Population, NA)
r$mort_rate = ifelse( is.na(r$mort_rate)==TRUE, smoothed_rate, r$mort_rate)

r2 = r
r2$rate_type = rep('lower_mort_rate_bound', N) 
r2$mort_rate = lower_mort_rate_bound 

r3 = r
r3$rate_type = rep('upper_mort_rate_bound', N) 
r3$mort_rate = upper_mort_rate_bound 

head(r) ; head(r2) ; head(r3)

r_group = rbind(r, r2, r3) 
class(r_group$year)

#r_group$group_type = c(rep('common', N), rep('solo1', N), rep('solo2', N))

r_group$group_type = c(rep('solo1', N), rep('solo2', N))
# Graphics: mortality rate by year faceted across age categories 

head(r_group) 

ggplot(r_group) + geom_line(aes(x=year, y=mort_rate, col=rate_type)) + facet_wrap( ~ agecat) 

g  = ggplot(r_group) + geom_line(aes(x=year, y=mort_rate, colour=rate_type, group=group_type)) + facet_wrap( ~ agecat, scales='free_y')  + theme(legend.position="top")  


ggplot(r_group) + geom_line(data=subset(r_group,group_type=='common'), aes(aes(x = year, y = mort_rate))) + 
  facet_wrap( ~ agecat, scales='free_y') + theme(legend.position="top")
 
 + 
  geom_point(aes(colour=rate_type), size=0.2) + facet_wrap( ~ agecat, scales='free_y') + theme(legend.position="top")



ggsave(g, filename='/Users/kathryn/Desktop/morttime.png', width=400, height=250, units=c('mm') )

#










r_group2 = r_group[r_group$agecat=='75-79 years', ]

ggplot(r_group2) + geom_line(aes(x=year, y=mort_rate, colour=rate_type)) + theme(legend.position="top")

# ARCHIVE 



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

