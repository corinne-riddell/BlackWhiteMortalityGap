##  program:  state-le-plots.r
##  task:     life expectancy plots
## 	input:	  state-le-results.dta					
##	output:		various figures
##  project:  state differences in black-white life expectancy
##  author:   sam harper \ 1aug2014

## users will need to specify working directory with life expectancy output
# setwd(".......")  

# setup
setwd("/Users/samharper/Dropbox/SRJ/Data/replication files")
library(ggplot2)
library(foreign)
library(gridExtra)
library(reshape2)
library(directlabels)
library(doBy)
library(proto)
    
# Grab results from decomposition
decomp <- read.dta("state-le-results.dta")

# subset to life expectancy at birth
tg <- subset(decomp,age=="00-00 years")

stabblist <- unique(tg$stabb)
stnamelist <- unique(tg$stname)
stnum <- unique(tg$stseer)
rnamelist <- unique(tg$regname)
rnum <- unique(tg$reg)
dnamelist <- unique(tg$divname)
dnum <- unique(tg$div)

# labels for state, region, division
tg$gender <- as.numeric(tg$gender)
tg$abb <- factor(tg$stseer,levels=c(stnum),labels=c(stabblist))
tg$name <- factor(tg$stseer,levels=c(stnum),labels=c(stnamelist))
tg$regn <- factor(tg$reg,levels=c(rnum),labels=c(rnamelist))
tg$regn <- factor(tg$reg,levels=c(1L,2L,3L,4L),labels=c("Northeast","Midwest","South","West"))
tg$divn <- factor(tg$div,levels=c(dnum),labels=c(dnamelist))
tg$divn <- factor(tg$div,levels=c(1L,2L,3L,4L,5L,6L,7L,8L,9L),labels=c("New England","Middle Atlantic","East North Central","West North Central","South Atlantic","East South Central","West South Central","Mountain","Pacific"))

### Need to just estimate a weighted average of each state-specific trend rather than using geom_smooth (too variable depending on scale)

# aggregate by division, year and gender
tg$lediffse <- abs(tg$lediffu - tg$lediffl)/(2*1.96) #standard err for gap in each state
tg$lediv <- (1/tg$lediffse^2) #inverse-variance weight for each state
tg$wg <- tg$lediff*tg$lediv   #weighted gap
divtrend <- summaryBy(lediff + wg + lediffse + lediv ~ year + divn + gender, FUN=sum, data=tg)  #calculate sums
divtrend$wa <- divtrend$wg.sum / divtrend$lediv.sum #divide by sum of weights

divtrend$year <- as.numeric(divtrend$year)
divtrend$year <- (divtrend$year+1989)

## Exhibit 1: Men
ggplot(subset(divtrend,gender==1), aes(x=year,y=wa, colour=divn)) + geom_line(size=1, aes(x=year, y=wa, colour=divn), show_guide=FALSE) + geom_dl(aes(label = divn),list("last.qp",hjust = -.1,cex=0.5)) + ylab("Black-white life expectancy difference (years)") + xlab("") + scale_x_continuous(limits=c(1990,2013)) + scale_y_continuous(limits=c(0,9),breaks=c(0,2,4,6,8)) + labs(title = "Men", colour="Division") + theme(axis.text.x = element_text(size = 16)) + theme(axis.title.y=element_text(size=16, angle=90)) + theme(axis.text.y = element_text(size = 16)) + scale_color_manual(values=c("#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd","#8c564b","#e377c2","#7f7f7f","#bcbd22"),guide="none") + theme_bw()

ggsave("men-div-weighted.pdf",width=6, height=5, dpi=600)


# Exhibit 2: Women
ggplot(subset(divtrend,gender==2), aes(x=year,y=wa, colour=divn)) + geom_line(size=1, aes(x=year, y=wa, colour=divn), show_guide=FALSE) + geom_dl(aes(label = divn),list("last.qp",hjust = -.1,cex=0.5)) + ylab("Black-white life expectancy difference (years)") + xlab("") + scale_x_continuous(limits=c(1990,2013)) + scale_y_continuous(limits=c(0,9),breaks=c(0,2,4,6,8)) + labs(title = "Women", colour="Division") + theme(axis.text.x = element_text(size = 16)) + theme(axis.title.y=element_text(size=16, angle=90)) + theme(axis.text.y = element_text(size = 16)) + scale_color_manual(values=c("#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd","#8c564b","#e377c2","#7f7f7f","#bcbd22"),guide="none") + theme_bw()

ggsave("women-div-weighted.pdf",width=6, height=5, dpi=600)


## Appendix Exhibit graphs

tg$year <- as.numeric(tg$year)
tg$year <- (tg$year+1989)

## Appendix Exhibit 3
## Plot for men, each state on a single graph with 95%CI
aapf1 <- ggplot(subset(tg,gender==1), aes(x=year, y=lediff))+ geom_ribbon(fill="lightblue", aes(ymin=lediffl,ymax=lediffu, alpha=0.5))  + geom_line(colour="dark blue") + facet_wrap(~name, ncol=8) + labs(title="Men") + theme_bw() + theme(legend.position="none") + geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed") + scale_y_continuous(limits=c(-10,20)) + xlab("") + ylab("black-white life expectancy difference (years)") + scale_x_continuous(breaks=c(1990,1998,2006)) + theme(axis.text.x = element_text(size = 8), strip.text.x = element_text(size=8))

ggsave("men-each-state.pdf", apf1, width=9, height=6.25, dpi=600)


## Appendix Exhibit 4
# Plot for women, each state on a single graph with 95%CI
apf2 <- ggplot(subset(tg,gender==2), aes(x=year, y=lediff))+ geom_ribbon(fill="lightblue", aes(ymin=lediffl,ymax=lediffu, alpha=0.5))  + geom_line(colour="dark blue") + facet_wrap(~name, ncol=8) + labs(title="Women") + theme_bw() + theme(legend.position="none") + geom_hline(aes(yintercept=0), colour="#990000", linetype="dashed") + xlab("") + scale_y_continuous(limits=c(-10,20)) + ylab("black-white life expectancy difference (years)") + scale_x_continuous(breaks=c(1990,1998,2006)) + theme(axis.text.x = element_text(size = 8), strip.text.x = element_text(size=8))

ggsave("women-each-state.pdf", apf2, width=9, height=6.25, dpi=600)




## Appendix Figure 6
## Plot for men, each state on a single graph with state label
fig1 <- ggplot(subset(tg,gender==1), aes(x=year, y=lediff, group=stname, colour=divn)) + geom_line(size=0.5) + geom_text(data = subset(tg,gender==1 & year == 2009 & (div==8 | (div>1 & div<5))), size=2.5, show_guide=FALSE, aes(label = abb),hjust=-0.1) + xlab("") + geom_text(data = subset(tg,gender==1 & year == 2009 & (div==1 | div==5)), size=2.5, show_guide=FALSE, aes(label = abb),hjust=-1.1) + geom_text(data = subset(tg,gender==1 & year == 2009 & (div==9 | (div>5 & div<8))), size=2.5, show_guide=FALSE, aes(label = abb),hjust=-2.1) + xlab("")+ ylab("Black-white life expectancy difference (years)") + scale_x_continuous(limits=c(1990,2011)) + coord_cartesian(ylim=c(-1.5,11)) + labs(title = "Men", colour="Census division") + geom_hline(aes(yintercept=0), linetype="dashed") + theme(axis.text.x = element_text(size = 16),axis.title.y=element_text(size=16, angle=90),axis.text.y = element_text(size = 16)) + theme_bw() + scale_color_manual(values=c("#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd","#8c564b","#e377c2","#7f7f7f","#bcbd22")) + theme(legend.position=c(0.9, 0.9),legend.key.height=unit(0.4, "cm"),legend.text = element_text(size = 7))


ggsave("men-states-stretched.png", fig1, width=6, height=8.5, dpi=600)
ggsave("men-states-stretched.pdf", fig1, width=6, height=8.5, dpi=600)




## Appendix Figure 7
## Plot for women, each state on a single graph with state label
fig2 <- ggplot(subset(tg,gender==2), aes(x=year, y=lediff, group=stname, colour=divn)) + geom_line(size=0.5) + geom_text(data = subset(tg,gender==2 & year == 2009 & (div==8 | (div>1 & div<5))), size=2.5, show_guide=FALSE, aes(label = abb),hjust=-0.1) + xlab("") + geom_text(data = subset(tg,gender==2 & year == 2009 & (div==1 | div==5)), size=2.5, show_guide=FALSE, aes(label = abb),hjust=-1.1) + geom_text(data = subset(tg,gender==2 & year == 2009 & (div==9 | (div>5 & div<8))), size=2.5, show_guide=FALSE, aes(label = abb),hjust=-2.1) + xlab("")+ ylab("Black-white life expectancy difference (years)") + scale_x_continuous(limits=c(1990,2011)) + coord_cartesian(ylim=c(-2,9)) + labs(title = "Women", colour="Census division") + geom_hline(aes(yintercept=0), linetype="dashed") + theme(axis.text.x = element_text(size = 16),axis.title.y=element_text(size=16, angle=90),axis.text.y = element_text(size = 16)) + theme_bw() + scale_color_manual(values=c("#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd","#8c564b","#e377c2","#7f7f7f","#bcbd22")) + theme(legend.position=c(0.9, 0.9),legend.key.height=unit(0.4, "cm"),legend.text = element_text(size = 7))


ggsave("women-states-stretched.png", fig2, width=6, height=8.5, dpi=600)
ggsave("women-states-stretched.pdf", fig2, width=6, height=8.5, dpi=600)


