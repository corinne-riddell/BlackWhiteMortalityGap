library(dplyr)
#explore the relationship between the CI estimates and CI width as a function of the number of draws of the posterior
#distribution.

#load("/Users/corinneriddell/Dropbox/BlackWhiteGap/Data/CI_vs_post_samples_results.RData") #LOCAL
load("~/black_white_mortality_project/CI_vs_post_samples_results_thinned.RData") #SERVER
# datasets <- c(georgia_2013_females[[3]], missouri_1985_females[[3]], utah_1969_females[[3]])
datasets <- c(georgia_2013_females_thinned[[3]], missouri_1985_females_thinned[[3]], utah_1969_females_thinned[[3]])

temp <- georgia_2013_females_thinned[[3]] 
temp <- temp %>% mutate(CI_width_LEW = run_975_LEW - run_025_LEW,
                        CI_width_LEB = run_975_LEB - run_025_LEB,
                        CI_width_LEG = run_975_LEG - run_025_LEG)
head(temp)
georgia_2013_females_thinned[[3]] <- temp

temp <- missouri_1985_females_thinned[[3]] 
temp <- temp %>% mutate(CI_width_LEW = run_975_LEW - run_025_LEW,
                        CI_width_LEB = run_975_LEB - run_025_LEB,
                        CI_width_LEG = run_975_LEG - run_025_LEG)
head(temp)
missouri_1985_females_thinned[[3]] <- temp

temp <- utah_1969_females_thinned[[3]] 
temp <- temp %>% mutate(CI_width_LEW = run_975_LEW - run_025_LEW,
                        CI_width_LEB = run_975_LEB - run_025_LEB,
                        CI_width_LEG = run_975_LEG - run_025_LEG)
head(temp)
utah_1969_females_thinned[[3]] <- temp

#Georgia CI widths
ggplot(georgia_2013_females_thinned[[3]][10:100, ], aes(x = row, y = CI_width_LEW)) + geom_line() + 
  geom_abline(intercept = georgia_2013_females_thinned[[3]]$CI_width_LEW[100], slope = 0, lty = 2) + 
  geom_line(aes(y = CI_width_LEB), col = "red") + 
  geom_abline(intercept = georgia_2013_females_thinned[[3]]$CI_width_LEB[100], slope = 0, lty = 2, col = "red")    

ggplot(georgia_2013_females[[3]], aes(x = row, y = CI_width_LEG)) + geom_line(col = "blue") + 
  geom_abline(intercept = georgia_2013_females[[3]]$CI_width_LEG[100], slope = 0, lty = 2, col = "blue") 

#Missouri CI widths
ggplot(missouri_1985_females[[3]], aes(x = row, y = CI_width_LEW)) + geom_line() + 
  geom_abline(intercept = missouri_1985_females[[3]]$CI_width_LEW[1000], slope = 0, lty = 2) + 
  geom_line(aes(y = CI_width_LEB), col = "red") + 
  geom_abline(intercept = missouri_1985_females[[3]]$CI_width_LEB[1000], slope = 0, lty = 2, col = "red")    

ggplot(missouri_1985_females[[3]], aes(x = row, y = CI_width_LEG)) + geom_line(col = "blue") + 
  geom_abline(intercept = missouri_1985_females[[3]]$CI_width_LEG[1000], slope = 0, lty = 2, col = "blue")

#Utah CI widths
ggplot(utah_1969_females[[3]], aes(x = row, y = CI_width_LEW)) + geom_line() + 
  geom_abline(intercept = utah_1969_females[[3]]$CI_width_LEW[1000], slope = 0, lty = 2) + 
  geom_line(aes(y = CI_width_LEB), col = "red") + 
  geom_abline(intercept = utah_1969_females[[3]]$CI_width_LEB[1000], slope = 0, lty = 2, col = "red")    

ggplot(utah_1969_females[[3]], aes(x = row, y = CI_width_LEG)) + geom_line(col = "blue") + 
  geom_abline(intercept = utah_1969_females[[3]]$CI_width_LEG[1000], slope = 0, lty = 2, col = "blue") 

#based on this output, KTM decided we should "thin" the runs by running 10000 and sampling every tenth one.