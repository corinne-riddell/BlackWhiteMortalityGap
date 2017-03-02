decomp.contour <- function(c1, c2, ages=c(0, 15, 40, 65, 80, 111), FUN = ex.per, ...) {
  
  # contour decomposition country1 and country 2
  # input arguments: 
  # c1, c2 - data for calculations, data frames. First column ages, second and third - rates
  # years - vector of years, eg c(1970,2000). if missed the first and last available year will be used
  # FUN - function to calculate statistics of interest
  #
  # (c) Dmitri A. Jdanov and Vladimir M. Shkolnikov
  # jdanov@demogr.mpg.de
  # last revised 30.11.2014
  
  Age = c1$Age
  agroups=cut(Age,ages,labels=FALSE, right = FALSE)
  na = max(agroups)

  
  # vectors of age-specific components of differences  
  dfAa = matrix(0,na,1)
  dfaA = matrix(0,na,1)
  dfab = matrix(0,na,1)
  dfba = matrix(0,na,1)
  dfBb = matrix(0,na,1)
  dfbB = matrix(0,na,1)
  dfAB = matrix(0,na,1)
  dfBA = matrix(0,na,1)
  
  # A->a->b->B  
  # vectors of rates
  A = c1$mx2
  a = c1$mx1
  B = c2$mx2
  b = c2$mx1
  for (x in 1:na) {
      fBA_1 = FUN(A, Age, ...)
      ind <- agroups == x
      
      A[ind] = a[ind]
      faA = FUN(A, Age, ...)
      dfaA[x] = faA - fBA_1
      
      A[ind] = b[ind]
      fbA = FUN(A, Age, ...)
      dfba[x] = fbA - faA
      
      A[ind] = B[ind]
      fBA = FUN(A, Age, ...)
      dfBb[x] = fBA - fbA
      
      dfAB[x] = fBA - fBA_1 #component of conventional decomposition (for control)
  }
    
  # B->b->a->A
  
  # vectors of rates
  A = c1$mx2
  a = c1$mx1
  B = c2$mx2
  b = c2$mx1
  for (x in 1:na) {
      fAB_1 = FUN(B, Age,...)
      ind <- agroups == x
  
      B[ind] = b[ind]
      fbB = FUN(B, Age, ...)
      dfbB[x] = fbB - fAB_1
  
      B[ind] = a[ind]
      faB = FUN(B, Age, ...)
      dfab[x] = faB - fbB
  
      B[ind] = A[ind]
      fAB = FUN(B, Age, ...)
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