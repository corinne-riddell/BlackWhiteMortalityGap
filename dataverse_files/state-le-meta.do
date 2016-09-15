capture log close
log using state-le-change, replace text

//  program:  	state-le-meta.do
//  task:		changes in black-white gap by state        
//	input:		state-le-results.dta
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
use "state-le-results.dta", clear

* keep only life expectancy at birth and single year
keep if age==1 & year==1


* labels for forest plots
label define div 1 "New England" 2 "Middle Atlantic" 3 "East North Central" ///
	4 "West North Central" 5 "South Atlantic" 6 "East South Central" ///
	7 "West South Central" 8 "Mountain" 9 "Pacific", modify
label values div div

label var stname "State"

sort div stfips



// #2
// fixed effects meta-analysis by census division, with forest plots

*Appendix Exhibit 8
metan change changel changeu if gender==0, by(div)  ///
	label(namevar=stname) lcols(stname) texts(200) ///
	effect("Years of life expectancy") nobox ///
	favours(Decreased black-white gap # Increased black-white gap) ///
	graphregion(fcolor(white) lcolor(white)) ///
	pointopt(msymbol(circle) mcolor(black) mfcolor(gray)) ///
	xlabel(-10,-8,-6,-4,-2,0,2,4,6,8,10) force xsize(6.5) ysize(8) ///
	diamopt(lcolor(gray) lwidth(vthin)) name(metam, replace) fixedi

	
* Appendix Exhibit 9
metan change changel changeu if gender==1, by(div)  ///
	label(namevar=stname) lcols(stname) texts(200) ///
	effect("Years of life expectancy") nobox ///
	favours(Decreased black-white gap # Increased black-white gap) ///
	graphregion(fcolor(white) lcolor(white)) ///
	pointopt(msymbol(circle) mcolor(black) mfcolor(gray)) ///
	xlabel(-10,-8,-6,-4,-2,0,2,4,6,8,10) force xsize(6.5) ysize(8) ///
	diamopt(lcolor(gray) lwidth(vthin)) name(metaw, replace) fixedi


log close
exit
