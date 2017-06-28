#random plots exploring the mortality rate
load("/Users/corinneriddell/Documents/repos/BlackWhiteGap/Data/alabama_only.Rdata")

ggplot(subset(dat.clean.alabama, Race2=="Black"), aes(x = Count, y=crude_new)) + geom_point(aes(col=Population)) + facet_wrap(~Age2, scales = "free")

ggplot(subset(dat.clean.alabama, Race2=="Black" & Sex2=="Male"), aes(x = Population, y=crude_new)) + 
  geom_point(aes(col=COD2)) + facet_wrap(~Age2, scales = "free") +
  geom_smooth(col="black") + scale_y_continuous("Mortality Rate") + ggtitle("Black Men in Alabama (all years)")

ggplot(subset(dat.clean.alabama, Race2=="Black" & Sex2=="Male"), aes(x = Population, y=crude_new)) + 
  geom_point(aes(col=Year)) + facet_wrap(~Age2, scales = "free") +
  geom_smooth(col="black") + scale_y_continuous("Mortality Rate") + ggtitle("Black Men in Alabama (all years)")


ggplot(subset(dat.clean.alabama, Race2=="White" & Sex2=="Male"), aes(x = Population, y=crude_new)) + 
  geom_point(aes(col=COD2)) + facet_wrap(~Age2, scales = "free") +
  geom_smooth(col="black") + scale_y_continuous("Mortality Rate") + ggtitle("White Men in Alabama (all years)")

ggplot(subset(dat.clean.alabama, Race2=="White" & Sex2=="Male"), aes(x = Population, y=crude_new)) + 
  geom_point(aes(col=Year)) + facet_wrap(~Age2, scales = "free") +
  geom_smooth(col="black") + scale_y_continuous("Mortality Rate") + ggtitle("White Men in Alabama (all years)")



mean.rate = c()
counts1 = unique(dat.clean.alabama$Count) ; length(counts)
counts = counts1[2:length(counts1)]

for(i in counts) {
  mean.rate = c(mean.rate, mean(dat.clean.alabama$crude_new[dat.clean.alabama$Count==i], na.rm=T))
}

plot(counts, mean.rate, pch=16, cex=0.5)

mean(dat.clean.alabama$crude_new[dat.clean.alabama$Count==10], na.rm = T)

mean(dat.clean.alabama$crude_new[dat.clean.alabama$Count>100 & dat.clean.alabama$Count<900], na.rm = T)

mean(dat.clean.alabama$crude_new[dat.clean.alabama$Count==1000], na.rm = T)

mean(dat.clean.alabama$crude_new[dat.clean.alabama$Count==1000], na.rm = T)