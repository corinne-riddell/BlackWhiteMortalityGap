
# Cleaning up the results imported from server 

r1 = read.csv('/Users/kathryn/Documents/BlackWhiteMortalityGap/Code/results_femalesNov4.csv') 

r2 = read.csv('/Users/kathryn/Documents/BlackWhiteMortalityGap/Code/results_malesNov4.csv')

r1$X = NULL ; r2$X = NULL 

r2$sex = rep('Male', length(r2$race)) 
head(r2)

r = rbind(r1, r2) 
head(r) ; tail(r) 


require(sqldf)

r2 = sqldf("select * from r order by state, sex, race, age_bin, year, COD ") 
head(r2) ; tail(r2)

write.csv(r2, 'smoothed_results.csv') 

head(dat.clean)

plot(r2$deaths, r2$smoothed_deaths, pch=16, cex=0.1)


# Quick figure to look at the results 

# black males by age group & cod, alabama 

ds1 = r2[r2$race=='Black' & r2$sex=='Male' & r2$state=='Alabama', ]
head(ds1)

ds2 = ds1[ ,c('deaths', 'smoothed_deaths', 'age_bin', 'year', 'cod')]

ds3 = melt(ds2, id.var=c('year','age_bin', 'cod')) 
head(ds3)


g = ggplot(data=ds3) + geom_point(aes(x=year, y=value, col=variable)) + facet_grid(age_bin ~ cod, scales='free')

g = ggpretty(g) 
ggsave(g, filename='test.png', width=18, height=30) 




ggpretty = function(g) { 
  g = g + 
    theme(legend.position="top", legend.title=element_blank(), 
          panel.background = element_rect(fill = '#F9F9FE',color='white'),
          axis.text.x = element_text(colour=c('black')),
          axis.title.x = element_text(colour = 'black', size=8),
          axis.title.y = element_text(colour = 'black', size=8, angle = 90, hjust = 0.5),
          strip.background = element_rect(fill="#F1F3FD"))
  return(g) 
}



