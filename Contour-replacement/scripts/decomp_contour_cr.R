strata.era1 <- r.All.1 %>% filter(state.year.sex %in% c(strata[1], strata[29])) #black and white females NY, 1969 and one post sample for now.

#c1 <- strata1 %>% filter(race == "Black")
#c2 <- strata1 %>% filter(race == "White")

decomp.contour.cr <- function(data) {
  
  data.sum <- strata %>% summarise_(year = first(year),
                                    race = first(race),
                                    total_smoothed_deaths = sum(smoothed_deaths),
                                    population = first(population),
                                    mort.rate = total_smoothed_deaths/population) %>%
    arrange_(year, race, age_minbin)
  #76 rows = 19 age groups * 2 races * 2 years
  
  data.sum$nx = 1*(data.sum$age_minbin == 0) + 4*(data.sum$age_minbin == 1) + 5*(data.sum$age_minbin > 1)
  
  # contour decomposition country1 and country 2
  # input arguments: 
  # c1, c2 - data for calculations, data frames. First column ages, second and third - rates
  # years - vector of years, eg c(1970,2000). if missed the first and last available year will be used
  # FUN - function to calculate statistics of interest
  #
  # (c) Dmitri A. Jdanov and Vladimir M. Shkolnikov
  # jdanov@demogr.mpg.de
  # last revised 30.11.2014
  
  Age = unique(data.sum$age_minbin)
  ages = c(0, 1, seq(from=5, to=85, by = 5), 86)
  agroups = cut(Age, ages, labels=FALSE, right = FALSE)
  na = max(agroups)

  
  # vectors of age-specific components of differences  
  dfAa = matrix(data = 0, nrow = na, ncol = 1)
  dfaA = matrix(0,na,1)
  dfab = matrix(0,na,1)
  dfba = matrix(0,na,1)
  dfBb = matrix(0,na,1)
  dfbB = matrix(0,na,1)
  dfAB = matrix(0,na,1)
  dfBA = matrix(0,na,1)
  
  # A->a->b->B  
  # vectors of rates
  end.year <- max(data.sum$year)
  begin.year <- min(data.sum$year)
  
  black.subset.end <- subset(data.sum, year == end.year & race == "Black")
  black.subset.begin <- subset(data.sum, year == begin.year & race == "Black")
  white.subset.end <- subset(data.sum, year == end.year & race == "White")
  white.subset.begin <- subset(data.sum, year == begin.year & race == "White")
  
  for (x in 1:na) {
      #fBA_1 = FUN(A, Age, ...)
      #ind <- agroups == x
      fBA_1 <- life.table(data = black.subset.end, sortable.age = "age_minbin", num.ages.in.group = "nx", 
                          death.counts = "total_smoothed_deaths", population.counts = "population", ave.prop.le1.lived = 0.1, return = "life expectancy at birth")
      index <- agroups == x
      
      #A[ind] = a[ind]
      #faA = FUN(A, Age, ...)
      #dfaA[x] = faA - fBA_1
      black.subset.end$total_smoothed_deaths[index] <- black.subset.begin$total_smoothed_deaths[index]
      faA <- life.table(data = black.subset.end, sortable.age = "age_minbin", num.ages.in.group = "nx", 
                        death.counts = "total_smoothed_deaths", population.counts = "population", ave.prop.le1.lived = 0.1, return = "life expectancy at birth")
      dfaA[x] = faA - fBA_1 
      
      #A[ind] = b[ind]
      #fbA = FUN(A, Age, ...)
      #dfba[x] = fbA - faA
      black.subset.end$total_smoothed_deaths[index] <- (white.subset.begin$total_smoothed_deaths[index]/white.subset.begin$population[index])/black.subset.end$population[index]
      fbA <- life.table(data = black.subset.end, sortable.age = "age_minbin", num.ages.in.group = "nx", 
                        death.counts = "total_smoothed_deaths", population.counts = "population", ave.prop.le1.lived = 0.1, return = "life expectancy at birth")
      dfba[x] = fbA - faA
      
      #A[ind] = B[ind]
      #fBA = FUN(A, Age, ...)
      #dfBb[x] = fBA - fbA
      black.subset.end$total_smoothed_deaths[index] <- (white.subset.end$total_smoothed_deaths[index]/white.subset.end$population[index])/black.subset.end$population[index]
      fBA <- life.table(data = black.subset.end, sortable.age = "age_minbin", num.ages.in.group = "nx", 
                        death.counts = "total_smoothed_deaths", population.counts = "population", ave.prop.le1.lived = 0.1, return = "life expectancy at birth")
      dfBb[x] = fBA - fbA
      
      dfAB[x] = fBA - fBA_1 #component of conventional decomposition (for control)
  }
    
  # B->b->a->A
  black.subset.end <- subset(data.sum, year == end.year & race == "Black")
  black.subset.begin <- subset(data.sum, year == begin.year & race == "Black")
  white.subset.end <- subset(data.sum, year == end.year & race == "White")
  white.subset.begin <- subset(data.sum, year == begin.year & race == "White")
  
  #something is wrong
  #stopped here
  for (x in 1:na) {
      #fAB_1 = FUN(B, Age,...)
      #ind <- agroups == x
      fAB_1 <- life.table(data = white.subset.end, sortable.age = "age_minbin", num.ages.in.group = "nx", 
                          death.counts = "total_smoothed_deaths", population.counts = "population", ave.prop.le1.lived = 0.1, return = "life expectancy at birth")
      index <- agroups == x
  
      # B[ind] = b[ind]
      # fbB = FUN(B, Age, ...)
      # dfbB[x] = fbB - fAB_1
      white.subset.end$total_smoothed_deaths[index] <- white.subset.begin$total_smoothed_deaths[index]
      fbB <- life.table(data = white.subset.end, sortable.age = "age_minbin", num.ages.in.group = "nx", 
                        death.counts = "total_smoothed_deaths", population.counts = "population", ave.prop.le1.lived = 0.1, return = "life expectancy at birth")
      dfbB[x] = fbB - fAB_1 
      
      # B[ind] = a[ind]
      # faB = FUN(B, Age, ...)
      # dfab[x] = faB - fbB
      white.subset.end$total_smoothed_deaths[index] <- (black.subset.begin$total_smoothed_deaths[index]/black.subset.begin$population[index])/white.subset.end$population[index] 
      faB <- life.table(data = white.subset.end, sortable.age = "age_minbin", num.ages.in.group = "nx", 
                        death.counts = "total_smoothed_deaths", population.counts = "population", ave.prop.le1.lived = 0.1, return = "life expectancy at birth")
      dfab[x] = faB - fbB
      
      # B[ind] = A[ind]
      # fAB = FUN(B, Age, ...)
      # dfAa[x] = fAB - faB
      white.subset.end$total_smoothed_deaths[index] <- (black.subset.end$total_smoothed_deaths[index]/black.subset.end$population[index])/white.subset.end$population[index]
      fAB <- life.table(data = white.subset.end, sortable.age = "age_minbin", num.ages.in.group = "nx", 
                        death.counts = "total_smoothed_deaths", population.counts = "population", ave.prop.le1.lived = 0.1, return = "life expectancy at birth")
      dfAa[x] = fAB - faB
      
      dfBA[x] = fAB - fAB_1
    }

  # define output matrices
  
  #initial conditions component  
  InitialEffect = (dfab - dfba) / 2
  #trend component
  TrendEffect = (dfAa - dfaA + dfbB - dfBb) / 2
  #conventional decomposition (=initial+trend) - to check the result
 
  CDecomp = (dfAB - dfBA) / 2

  
  dta = data.frame(cbind(InitialEffect, TrendEffect, CDecomp))
  rownames(dta) <- head(ages,-1)
  colnames(dta) <- c("initial","trend","conventional")

  return(dta)
}