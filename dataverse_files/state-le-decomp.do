capture log close
log using state-le-decomp, replace text

//  program:  	state-le-decomp.do
//  task:		decompose overall changes in black-white gap by state        
//	input:		state-le-data.dta, state-le-results
//	output:		state-le-pop.dta
//  project:  	state black-white life expectancy
//  author:   	sam harper \ 1aug2014

//  #0
//  program setup

version 12
set linesize 80
clear all
macro drop _all

// #1
// derive population estimates by state

* get state population data
use "state-le-data.dta", clear

* collapse over age to get total # deaths and pop by state/sex/race/year
collapse (sum) deaths pop, by(stnum sex race year)
label var deaths "deaths"
label var pop "population"
rename sex gender

* reshape wide to get pop and death estimates by race
reshape wide deaths pop, i(stnum gender year) j(race)
renvars deaths0 deaths1 pop0 pop1 / deathsw deathsb popw popb
label var deathsw "white deaths"
label var deathsb "black deaths"
label var popw "white pop"
label var popb "black pop"

saveold "state-le-pop.dta", replace


//#2
// calculate "national" life expectancy changes by race and gender

* grab results containing life expectancies
use "state-le-results.dta", clear

* keep only life expectancy at birth and prepare for merging with pop data
keep if age==1
drop age
rename year year0
gen year=year0+1989


* merge with population estimates (total pop, not age specific)
merge 1:m stnum gender year using "state-le-pop.dta"

* check for unmatched states (should be 13(ID),27(MT),35(ND),42(SD),46(VT)
tab stnum if _merge==2

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
keep st* reg* div* gender year lew leb lediff* change* popw popb ppopw ppopb

* table of overall life expectancy for each group
*LE for whites
table gender year [fw=popw], c(mean lew) format(%3.1f)
*LE for blacks
table gender year [fw=popb], c(mean leb) format(%3.1f)

* table of le in each state (Appendix Exhibit 1)
*LE for men
table stname if year==1990 & gender==0, c(mean leb mean lew mean lediff ///
 	mean lediffl mean lediffu) format(%3.1f)
 
table stname if year==2009 & gender==0, c(mean leb mean lew mean lediff ///
 	mean lediffl mean lediffu) format(%3.1f)
	
* change in the gap for each state
table stname if gender==0, c(mean changeb mean changewh mean change ///
	mean changel mean changeu) format(%3.1f)
 
 
*LE for women (Appendix Exhibit 2)
table stname if year==1990 & gender==1, c(mean leb mean lew mean lediff ///
 	mean lediffl mean lediffu) format(%3.1f)
 
table stname if year==2009 & gender==1, c(mean leb mean lew mean lediff ///
 	mean lediffl mean lediffu) format(%3.1f)

* change in the gap for each state
table stname if gender==1, c(mean changeb mean changewh mean change ///
	mean changel mean changeu) format(%3.1f)
 	

// #3
// decomposition at the national level

* reshape data for decomposition
reshape wide pop* ppop* le*, i(st* gender) j(year)

forvalues y = 1990(19)2009 {

	
	label var lew`y' "White LE `y'"	
	label var leb`y' "Black LE `y'"
	label var lediff`y' "B-W LE diff `y'"
	label var popw`y' "White pop `y'"
	label var popb`y' "Black pop `y'"
	label var ppopw`y' "% of white pop `y'"
	label var ppopb`y' "% of black pop `y'"
	}
	

/* Compute overall decomposition as in Jenkins 1995, p. 38 
	for total life expectancy gap */

* overall allocation and growth components
gen alloc = (lew2009-lew1990)*((ppopw2009+ppopw1990)/2) - ///
	(leb2009-leb1990)*((ppopb2009+ppopb1990)/2)

gen growth = (ppopw2009-ppopw1990)*((lew2009+lew1990)/2) - ///
 	(ppopb2009-ppopb1990)*((leb2009+leb1990)/2)

* total change in gap and overall decomposition
forvalues g=0/1 {
	foreach r of newlist b w {
		local sex : label gender `g'
		
		qui sum le`r'2009 if gender==`g' [fw=pop`r'2009]
		scalar le`r'2009g`g'=r(mean) // avg male le 2009
		scalar p`r'2009g`g'=r(sum_w) // total male pop 2009
		qui sum le`r'1990 if gender==`g' [fw=pop`r'1990]
		scalar le`r'1990g`g'=r(mean) // avg female le 1990
		scalar p`r'1990g`g'=r(sum_w) // total female pop 1990
		
		scalar le`r'grg`g'=ln(le`r'2009g`g' / le`r'1990g`g')/20 // avg le growth rate
		scalar pop`r'grg`g'=ln(p`r'2009g`g' / p`r'1990g`g')/20 // avg pop growth rate
		}
	disp " "
	disp "LE White " "`sex'" ", 1990: " %4.2f lew1990g`g' 
	disp "LE Black " "`sex'" ", 1990: " %4.2f leb1990g`g'
	disp "White-Black Gap " "`sex'" ", 1990: " %4.3f lew1990g`g'-leb1990g`g'
	disp "LE White " "`sex'" ", 2009: " %4.2f lew2009g`g' 
	disp "LE Black " "`sex'" ", 2009: " %4.2f leb2009g`g'
	disp "White-Black Gap " "`sex'" ", 2009: " %4.3f lew2009g`g'-leb2009g`g'
	disp " "
	disp "Change in LE White " "`sex'" ", 1990-2009: " ///
		%4.3f lew2009g`g'-lew1990g`g'
	disp "Avg LE growth rate for white " "`sex': " %4.3f lewgrg`g'*100 "%/yr"
	disp "Change in LE Black " "`sex'" ", 1990-2009: " ///
		%4.3f leb2009g`g'-leb1990g`g'
	disp "Avg LE growth rate for black " "`sex': " %4.3f lebgrg`g'*100 "%/yr"
	disp "Change in White-Black Gap " "`sex'" ", 1990-2009: " ///
		%4.3f (lew2009g`g'-leb2009g`g') - (lew1990g`g'-leb1990g`g')
	disp " "
		
	qui sum alloc if gender==`g'
	disp "Allocation effect for " "`sex': " %4.3f r(sum)
	disp "Allocation % of total change, " "`sex': " %4.2f ///
		r(sum)/((lew2009g`g'-leb2009g`g')-(lew1990g`g'-leb1990g`g'))*100 "%"
	qui sum growth if gender==`g'
	disp "Growth effect for " "`sex': " %4.3f r(sum)
	disp "Growth % of total change, " "`sex': " %4.2f ///
		r(sum)/((lew2009g`g'-leb2009g`g')-(lew1990g`g'-leb1990g`g'))*100 "%"
	}
	

encode stname, gen(state)

* observed LE and population annual growth rates
gen blegr=(ln(leb2009/leb1990)/20)*100
gen wlegr=(ln(lew2009/lew1990)/20)*100
gen bpopgr=(ln(popb2009/popb1990)/20)*100
gen wpopgr=(ln(popw2009/popw1990)/20)*100

gen bppopgr = (ln(ppopb2009/ppopb1990)/20)*100

label var blegr "Obs black LE growth rate (%/yr)"
label var wlegr "Obs white LE growth rate (%/yr)"
label var bpopgr "Obs black pop growth rate, (%/yr)"
label var wpopgr "Obs white pop growth rate, (%/yr)"

gen cleb2009g=leb2009
gen clew2009g=lew2009
gen cleb2009a=leb2009
gen clew2009a=lew2009
gen cpopb2009a=popb2009
gen cpopw2009a=popw2009

label var cleb2009g "Counterfactual black LE 2009: growth"
label var clew2009g "Counterfactual white LE 2009: growth"
label var cleb2009a "Counterfactual black LE 2009: alloc"
label var clew2009a "Counterfactual white LE 2009: alloc"
label var cpopb2009a "Counterfactual black pop 2009"
label var cpopw2009a "Counterfactual white pop 2009"


gen ngapc=(lew2009g0-leb2009g0) - (lew1990g0-leb1990g0) if gender==0
replace ngapc=(lew2009g1-leb2009g1) - (lew1990g1-leb1990g1) if gender==1
label var ngapc "National observed change in gap"
gen ncgapcg=.
label var ncgapcg "National counterfactual change in gap: growth"
gen ncgapca=.
label var ncgapca "National counterfactual change in gap: alloc"





// #4
// state-level decomposition

qui levelsof state, local(levels)
foreach s of local levels {
	forvalues g=0/1 {
		local st : label state `s'
		local sex : label gender `g'
			
		disp " "
		disp "********************************************"
		disp "State: " "`st' " " Gender: " "`sex'"
		disp " "
				
		* gen counterfactual national le using avg le growth
		
		// set counterfactual to observed value for all states for blacks
		qui replace cleb2009g=leb2009 
		
		// replace counterfactual for blacks for one state, by gender
		qui replace cleb2009g = leb1990*exp(lebgrg`g'*20) if gender==`g' & state==`s'
		
		// set counterfactual to observed value for all states for whites
		qui replace clew2009g=lew2009
		
		// replace counterfactual for whites for one state, by gender
		qui replace clew2009g = lew1990*exp(lewgrg`g'*20) if gender==`g' & state==`s'
		
		// now calculate national LE for blacks and whites, including the 
		// counterfactual value for the state replaced above
		qui sum cleb2009g if gender==`g' [aw=popb2009] // counterfactual le black 2009
		scalar cleb2009gg`g'=r(mean)
		qui sum clew2009g if gender==`g' [aw=popw2009] // counterfactual le white 2009
		scalar clew2009gg`g'=r(mean)
		disp "Observed national change in " "`sex'" " white-black gap, 1990-2009: " ///
			%4.3f (lew2009g`g'-leb2009g`g') - (lew1990g`g'-leb1990g`g')
		disp "Counterfactual analysis for growth"
		disp "Counterfactual change in " "`sex'" " white-black gap, 1990-2009: " ///
			%4.3f (clew2009gg`g'-cleb2009gg`g') - (lew1990g`g'-leb1990g`g')
		qui replace ncgapcg=(clew2009gg`g'-cleb2009gg`g')-(lew1990g`g'-leb1990g`g') ///
			if gender==`g' & state==`s'
			
		* gen counterfactual national le using avg pop growth
		
		// set counterfactual to observed value for all states for blacks
		qui replace cpopb2009a=popb2009
		
		// replace counterfactual with black pop for one state, assuming it had
		// experienced the same population growth rate as blacks nationwide
		qui replace cpopb2009a = round(popb1990*exp(popbgrg`g'*20),1) if gender==`g' & state==`s'
		
		// set counterfactual to observed value for all states for whites
		qui replace cpopw2009=popw2009
		
		// replace counterfactual with white pop for one state, assuming it had
		// experienced the same population growth rate as whites nationwide
		qui replace cpopw2009a = round(popw1990*exp(popwgrg`g'*20),1) if gender==`g' & state==`s'
		qui sum cleb2009a if gender==`g' [fw=cpopb2009] // counterfactual le black 2009
		scalar cleb2009ag`g'=r(mean)
		qui sum clew2009a if gender==`g' [fw=cpopw2009] // counterfactual le white 2009
		scalar clew2009ag`g'=r(mean)
		disp "Counterfactual analysis for allocation"
		disp "Counterfactual change in " "`sex'" " White-Black Gap, 1990-2009: " ///
			%4.3f (clew2009ag`g'-cleb2009ag`g') - (lew1990g`g'-leb1990g`g')
		qui replace ncgapca=(clew2009ag`g'-cleb2009ag`g')-(lew1990g`g'-leb1990g`g') ///
			if gender==`g' & state==`s'
		
		}
	}


* growth in LE for blacks
gen cleb= leb2009 - leb1990
label var cleb "Change in black life expectancy, 1990-2009"





// #5
// graph state-specific decomposition results

* Appendix Exhibit 12: decomposition results for men
graph twoway ///
	(scatter cleb ncgapcg, msize(vsmall) mcolor(black) mlab(stabb) ///
	mlabsize(tiny) mlabcolor(black) mlabpos(12) sort) ///
	(pcarrowi 12 -2.74 12 -2.76, lcolor(black) mcolor(black)) if gender==0, ///
	xline(-2.7622) ytitle("Change in black life expectancy, 1990-2009 (years)", size(vsmall)) ///
 	xtitle("Counterfactual changes in white-black life expectancy gap (years)", ///
	size(vsmall)) name(men, replace) title(Men, size(small)) scheme(sj) xsize(6.5) ///
	ysize(8) text(12 -2.74 "Observed change in gap (-2.74 years)", place(e) size(vsmall)) ///
	legend(off) xlabel(,labsize(vsmall)) ylabel(,labsize(vsmall))

* Appendix Exhibit 13: decomposition results for women
graph twoway ///
	(scatter cleb ncgapcg, msize(vsmall) mcolor(black) mlab(stabb) ///
	mlabsize(tiny) mlabcolor(black) mlabpos(12) sort) ///
	(pcarrowi 8 -1.63 8 -1.65, lcolor(black) mcolor(black)) if gender==1, ///
	xline(-1.651) ytitle("Change in black life expectancy, 1990-2009 (years)", size(vsmall)) ///
	xtitle("Counterfactual changes in white-black life expectancy gap (years)", ///
	size(vsmall)) name(women, replace) title(Women, size(small)) scheme(sj) xsize(6.5) ///
	ysize(8) text(8 -1.63 "Observed change in gap (-1.63 years)", place(e) size(vsmall)) ///
	legend(off) xlabel(,labsize(vsmall)) ylabel(,labsize(vsmall))


log close
exit
