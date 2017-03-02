# (c) Dmitri A. Jdanov and Vladimir M. Shkolnikov
# jdanov@demogr.mpg.de
# last revised 30.11.2014

cdecomp.sdmab<-function(fname1, fname2, bo = 1, years = c(1955, 1970), agerange = c(12,39)) {
  # SDMAB example
  # input arguments: file names, birth order, years, and age range
  # returns list with 2 components: $decomp - resuls of decomposition and $values - original values of SDMAB
  # cdecomp.sdmab(fname1, fname2, bo = 1, years = c(1955, 1970), agerange = c(12,39))
  # Example1: decomp.sdmab('CZEasfrVHbo.txt', 'RUSasfrVHbo.txt', 1) 
  #                 calculates (contour) decomposition of the 1970 cohort difference in the SDMAB
  #                 of first birth with split into initial condition (1955 cohort) and trend components
  #                 for CZE and RUS using data from the original HFD files (cohort ASFR by birth order) 
  # Example2: decomp.sdmab('CZEasfrVHbo.txt', 'RUSasfrVHbo.txt', bo = c(3:5), years = c(1950,1970)) 
  #                 same as Example 1 but for cohorts 1970 and 1950 and birth order 3+
  
  # prepare the data
  c1 = ASFR.select(fname1, years, bo, agerange)
  c2 = ASFR.select(fname2, years, bo, agerange)

  # decomposition
  r = decomp.contour(c1, c2, ages=c(12,20,25,30,35,40), FUN = sdmab)

  #calculate original SDMABs
  v = matrix(, 2, 2)
  
  for (i in 1:2) {
    v[1, i] = sdmab(c1[, i + 1], c1$Age)
    v[2, i] = sdmab(c2[, i + 1], c2$Age)
  }
  v = data.frame(v)
  rownames(v) = c(fname1, fname2)
  colnames(v) = years
  
  # output
  r = list(r, v)
  names(r) = c("decomp", "values")
  return(r)
}

sdmab<-function(fx, age) {
  #calculates standard deviatian in the mean age at birth (see the HFD Methods protocol for details)
  #input arguments: vectors of ASFR (fx) and ages (age, same dimension as fx)
  #usage: sdmab(fx, age)
  
  age = age + 0.5
  af = sum(age * (fx)) / sum(fx)
  sdmab = sum(age * age * (fx)) / sum(fx) - af * af
  return (sqrt(sdmab))
}

ASFR.select<-function(fname, years, bo, agerange) {
  # selects data from the HFD file with ASFR
  # input arguments: fname, birth order(s), age range
  # returns data frame with columns Age, mx1, mx2, 
  # where mx1 and mx2 are age specific fertility rates for years specified in vector years
  
  dta = readHFDfx(fname)
  dta = dta[dta$Age >= agerange[1] & dta$Age <= agerange[2], ]
  if (length(bo) > 1) {
    mx = rowSums(dta[, bo])
  }
  else {
    mx = dta[, bo]
  }
  dta = data.frame(cbind(dta$Age[dta[,1] == years[1]], mx[dta[,1] == years[1]], mx[dta[,1] == years[2]]))
  names(dta) <- c("Age","mx1", "mx2")
  return(dta)
}

readHFDfx<-function(fname) {
  #reads data from the standard HFD file with rates
  #input aguments: file name
  #returns data frame (column names same as in input file)
  #example: readHFD2cd('RUSasfrVHbo.txt')
  
  p = read.table(fname, header = T, skip = 2, colClasses = "character", strip.white=TRUE, na.string=".")
  p$Age[p$Age == '12-'] = 12
  p$Age[p$Age == '55+'] = 55;
  return(data.frame(apply(p, 2, as.numeric)))
}