# (c) Dmitri A. Jdanov and Vladimir M. Shkolnikov
# jdanov@demogr.mpg.de
# last revised 30.11.2014


cdecomp.ex<-function(fname1, fname2, years = c(1970,2009), sex = 'm') {
  # Life expectancy example
  # input arguments: file names, years
  # returns list with 2 components: $decomp - resuls of decomposition and $values - original values of e(0)
  # cdecomp.ex(fname1, fname2, years = c(1970, 2009))
  # Example: decomp.ex('.txt', 'RUSasfrVHbo.txt') 
  #                 calculates (contour) decomposition of the 2009 difference in the LE
  #                 with split into initial condition (of the year 1970) and trend components for JPN and USA
  #                 using data from the original HMD files (period life tables) 
  
  # prepare the data
  c1 = mx.select(fname1, years)
  c2 = mx.select(fname2, years)
  
  # decomposition
  r = decomp.contour(c1, c2, ages=c(0, 15, 40, 65, 80, 111), FUN = ex.per, sex = sex)
  
  #calculate original SDMABs
  v = matrix(, 2, 2)
  
  for (i in 1:2) {
    v[1, i] = ex.per(c1[, i + 1], c1$Age, sex)
    v[2, i] = ex.per(c2[, i + 1], c2$Age, sex)
  }
  v = data.frame(v)
  rownames(v) = c(fname1, fname2)
  colnames(v) = years
  
  # output
  r = list(r, v)
  names(r) = c("decomp", "values")
  return(r)
}

ex.per <- function(mx, age, sex = '') {
  # calculates life table from mx (period data, HMD method, w/o smoothing) 
  # usage: ex.per(mx, sex = 'm')
  # input arguments: mx - vector of mortality rates (one year age group), sex - "m" for males and "f" for females
  # output: life expectancy
  # 
  # Last revised: 28.11.2014
  
  last = length(mx)
  ax = diff(age) / 2
  if (sex == '') {
    cat('warning: sex==m')
    sex = 'm'
  }
  # a(0)
  if (mx[1] >= 0.107) {
    if (sex == 'm') {
      ax[1] = 0.33
    }
    else {
      ax[1] = 0.35
    }
  }
  else {
    if (sex == 'm') {
      ax[1] = 0.045 + 2.684 * mx[1]
    }
    else {
      ax[1] = 0.053 + 2.8 * mx[1]
    }
  }
  # last age
  ax = c(ax, 1 / mx[last])
  # q(x)
  qx = mx / (1 + (1 - ax) * mx)
  qx[is.na(qx)] = 1
  n = which.max(qx>1)
  if (n > 1) {
    qx = c(qx[1:(n - 1)], 1)
    ax = c(ax[1:(n - 1)], 0.5)
  }
  qx[last] = 1 
  ax[is.na(ax)] = 0.5
  qx[is.na(qx)] = 1
  ax[last] = 1 / mx[last]
  px = 1 - qx
  lx = c(1, cumprod(px))
  lx = head(lx, -1)
  dx = lx * qx
  dx[length(dx)] = lx[length(lx)]
  Lx = lx - (1 - ax) * dx
  Lx[last] = lx[last] * ax[last]
  # return e(0)
  return(sum(Lx))
}

mx.select<-function(fname, years) {
  # selects data from the HFD file with ASFR
  # input arguments: fname, years (vector)
  # returns data frame with columns Age, mx1, mx2, 
  # where mx1 and mx2 are age specific mortality rates for years specified in vector years
  
  dta = read.table(fname, header = T, skip = 2, colClasses = "character", strip.white=TRUE, na.string=".")
  dta$Age[dta$Age == '110+'] = 110;
  dta = data.frame(apply(dta, 2, as.numeric))
  dta = data.frame(cbind(dta$Age[dta[,1] == years[1]], dta$mx[dta[,1] == years[1]], dta$mx[dta[,1] == years[2]]))
  names(dta) <- c("Age","mx1", "mx2")
  return(dta)
}

