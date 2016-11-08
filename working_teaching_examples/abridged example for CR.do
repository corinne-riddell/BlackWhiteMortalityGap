// simple abridged life table for Corinne
// 15sep2016 

// #1
// input data on age, population, deaths

* first 16 groups from Selvin, 1991 table 9-1.
clear
input age pop deaths
0 123342 1635
1 111520 64
2 109200 41
3 108749 22
4 105698 41
5 110548 55
6 106857 42
7 112184 58
8 116423 44
9 132952 52
10 134266 48
11 128938 60
12 125502 52
13 128212 82
14 132775 129
15 143600 23300
end

// #2
// quantities for complete life table

* define number of years in age interval
gen nx=1 

* average person-years contributed by those dying within interval
gen ax=0.1 if age==0
replace ax=0.5 if age>0

* mortality rate
gen Rx=deaths/pop
	
* probability of death
gen qx= Rx / (1 + (1-ax)*Rx)  // Selvin(9.4)
qui replace qx = 1 if age==15 // 1 for last interval
	
* conditional prob of survival
gen px=1-qx
	
* number alive at beginning of interval
gen lx = 100000 if age==0
qui replace lx = lx[_n-1] * px[_n-1] if age>0
	
/* Generate deaths by differencing the number of survivors and noting 
  that everyone dies in the end*/
gen dx = lx - lx[_n+1]
qui replace dx = lx if age==15
	
/* Compute person-years lived in each age group n for those who survive 
  the age group and n*ax for those who die */
gen Lx = n * (lx[_n+1] + (ax*dx))
qui replace Lx = lx/Rx if age==15
	
/* Accumulating from the bottom up is a bit tricky because Stata likes to sum 
  from the top down. You could sort the data from oldest to youngest, sum, 
  and then sort again. I will subtract the cumulative sum from the total. */
qui sum Lx
gen Tx = r(sum) - sum(Lx) + Lx
	
* Compute life expectancy (time lived after each age / survivors to that age)
gen ex = Tx/lx
format ex %3.1f

list age qx px dx Lx Tx ex, compress noobs

* create abridged age groups
recode age (0=0 "0-1y") (1/4 = 1 "1-4y") (5/9 = 2 "5-9y") ///
  (10/14 = 3 "10-14y") (15=4 "15+y"), gen(age5)

* conditional probabilites of death by 5-year age groups
table age5, c(sum qx)


// #3
// now collapse to 0-1, 1-4, 5-9, 10-14, 15+ age groups

collapse (sum) deaths pop, by(age5)

* define number of years in age interval
gen nx=1 if age5==0
qui replace nx=4 if age5==1
qui replace nx=5 if age5>1
tab nx age

* average person-years contributed by those dying within interval
gen ax=0.1 if age5==0
replace ax=0.5 if age5>0

* mortality rate
gen Rx=deaths/pop
	
* probability of death	
gen qx= (nx*Rx) / (1+nx*(1-ax)*Rx) // Selvin(9.32)
qui replace qx = 1 if age5==4
	
* conditional prob of survival
gen px=1-qx
	
* no alive at beginning of interval
gen lx = 100000 if age5==0
qui replace lx = lx[_n-1] * px[_n-1] if age5>0
	
/* Generate deaths by differencing the number of survivors and noting 
  that everyone dies in the end*/
gen dx = lx - lx[_n+1]
qui replace dx = lx if age5==4
format dx %6.1f
	
/* Compute person-years lived in each age group n for those who survive 
  the age group and n*ax for those who die */
gen Lx = nx * (lx[_n+1] + (ax*dx))
qui replace Lx = lx/Rx if age5==4
	
/* Accumulating from the bottom up is a bit tricky because Stata likes to sum 
  from the top down. You could sort the data from oldest to youngest, sum, 
  and then sort again. I will subtract the cumulative sum from the total. */
qui sum Lx
gen Tx = r(sum) - sum(Lx) + Lx
	
* Compute life expectancy (time lived after each age / survivors to that age)
gen ex = Tx/lx
format ex %3.1f

list age5 deaths qx dx Lx Tx ex, compress noobs
