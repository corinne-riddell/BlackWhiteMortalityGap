capture log close
log using state-le-change-nhisp, replace text

//  program:  	state-le-change-nhisp.do
//  task:		changes in black-white gap by state for non-hispanics       
//	input:		
//	output:		none
//  project:  	state black-white life expectancy
//  author:   	sam harper \ 1aug2014

//  #0
//  program setup

version 12
set linesize 80
clear all
macro drop _all

// #1
* grab results containing life expectancies
use "state-le-results-nhisp.dta", clear

* drop states with inadequate reporting for Hispanics, except for NY
drop if inlist(stfipsn,9,11,22,23,24,27,28,33,40,45,51)

* could potentially only drop those with inadequate reporting
* in 1990 or 2009
*drop if inlist(stfipsn,9,22,23,27,28,33,40,51)

* keep only life expectancy at birth and prepare for merging with pop data
keep if age==1
drop age
rename year year0
gen year=year0+1989

* merge with population estimates (total, not age specific)
merge 1:m stnum gender year using "state-le-pop.dta"

* keep matched states
keep if _merge==3
drop _merge

* generate total population by gender, year, race
egen tpopw=total(popw), by(gender year)
gen ppopw=popw/tpopw // %pop in each state for whites

egen tpopb=total(popb), by(gender year)
gen ppopb=popb/tpopb // %pop in each state for blacks


/*** Select out two years, reshape, check inequality trends ***/
keep if year==1990 | year==2009


// #2
// life expectancy changes for non-Hispanic blacks and whites

* table of overall life expectancy for each group
*LE for non-Hispanic whites
table gender year [fw=popw], c(mean lew) format(%3.1f)
*LE for non-Hispanic blacks
table gender year [fw=popb], c(mean leb) format(%3.1f)

* table of le in each state
*LE for men
table stname if year==1990 & gender==0, c(mean leb mean lew mean lediff ///
 	mean lediffl mean lediffu) format(%3.1f)
 
table stname if year==2009 & gender==0, c(mean leb mean lew mean lediff ///
 	mean lediffl mean lediffu) format(%3.1f)
	
* change in the gap for each state
table stname if gender==0, c(mean changeb mean changew mean change ///
	mean changel mean changeu) format(%3.1f)
 
 
*LE for women
table stname if year==1990 & gender==1, c(mean leb mean lew mean lediff ///
 	mean lediffl mean lediffu) format(%3.1f)
 
table stname if year==2009 & gender==1, c(mean leb mean lew mean lediff ///
 	mean lediffl mean lediffu) format(%3.1f)

* change in the gap for each state
table stname if gender==1, c(mean changeb mean changew mean change ///
	mean changel mean changeu) format(%3.1f)
	

	
// #3
// fixed effects meta-analysis by census division, with forest plots

* Select just last year
keep if year==2009

label define div 1 "New England" 2 "Middle Atlantic" 3 "East North Central" 4 "West North Central" 5 "South Atlantic" 6 "East South Central" ///
7 "West South Central" 8 "Mountain" 9 "Pacific", modify
label values div div

label var stname "State"
sort div stfips

metan change changel changeu if gender==0, by(div)  ///
	label(namevar=stname) lcols(stname) texts(200) ///
	effect("Years of life expectancy") nobox ///
	favours(Decreased black-white gap # Increased black-white gap) ///
	graphregion(fcolor(white) lcolor(white)) ///
	pointopt(msymbol(circle) mcolor(black) mfcolor(gray)) ///
	xlabel(-10,-8,-6,-4,-2,0,2,4,6,8,10) force xsize(6.5) ysize(8) ///
	diamopt(lcolor(gray) lwidth(vthin)) name(metamnh, replace) fixedi
	
metan change changel changeu if gender==1, by(div)  ///
	label(namevar=stname) lcols(stname) texts(200) ///
	effect("Years of life expectancy") nobox ///
	favours(Decreased black-white gap # Increased black-white gap) ///
	graphregion(fcolor(white) lcolor(white)) ///
	pointopt(msymbol(circle) mcolor(black) mfcolor(gray)) ///
	xlabel(-10,-8,-6,-4,-2,0,2,4,6,8,10) force xsize(6.5) ysize(8) ///
	diamopt(lcolor(gray) lwidth(vthin)) name(metawnh, replace) fixedi

log close
exit
