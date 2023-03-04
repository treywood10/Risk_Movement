/*	*************************************************************/
/*     	File Name:	Geo_PKO_Data_Analysis	                    */
/*     	Date:   	August 3, 2022	                            */
/*      Author: 	Robert Lee Wood III				            */
/*      Purpose:	Data Analysis for Risk_Movement Chapter 	*/
/*      Input Files:Geo_PKO_nr.dta		     						*/
/*     	Output File: 						                    */	
/*	*************************************************************/


* Clear all *
clear all
frame reset


* Set global macro for working directory path *
global cd_path "/Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement"


* Set working directory *
cd ${cd_path}


* Import dataset *
use Make_Data/STATA_Prep/Geo_PKO_nr.dta, clear


*** Collapse HQ variables ***
gen hq = 0
replace hq = 1 if tcc_hq == 1
replace hq = 1 if sector_hq == 1
replace hq = 1 if mis_hq == 1
drop tcc_hq sector_hq mis_hq


*** Time since last violent action in cell ***

* Best *
gen best_dum = 0
replace best_dum = 1 if best > 0

bysort gid (year month): gen spell = (best_dum != 0)
bysort gid (year month): replace spell = sum(spell)
bysort gid spell (year month): gen wanted = _n-1 if spell > 0
bysort gid spell (year month): replace wanted = _n if spell == 0
rename wanted best_time

drop best_dum spell 


* OSV_total *
gen OSV_total_dum = 0
replace OSV_total_dum = 1 if OSV_total > 0

bysort gid (year month): gen spell = (OSV_total_dum != 0)
bysort gid (year month): replace spell = sum(spell)
bysort gid spell (year month): gen wanted = _n-1 if spell > 0
bysort gid spell (year month): replace wanted = _n if spell == 0
rename wanted OSV_total_time

drop OSV_total_dum spell 


* OSV_GOV *
gen OSV_GOV_dum = 0
replace OSV_GOV_dum = 1 if OSV_GOV > 0

bysort gid (year month): gen spell = (OSV_GOV_dum != 0)
bysort gid (year month): replace spell = sum(spell)
bysort gid spell (year month): gen wanted = _n-1 if spell > 0
bysort gid spell (year month): replace wanted = _n if spell == 0
rename wanted OSV_GOV_time

drop OSV_GOV_dum spell 


* OSV_Rebs *
gen OSV_Rebs_dum = 0
replace OSV_Rebs_dum = 1 if OSV_Rebs > 0

bysort gid (year month): gen spell = (OSV_Rebs_dum != 0)
bysort gid (year month): replace spell = sum(spell)
bysort gid spell (year month): gen wanted = _n-1 if spell > 0
bysort gid spell (year month): replace wanted = _n if spell == 0
rename wanted OSV_Rebs_time

drop OSV_Rebs_dum spell 


*** Lag Variables ***

* DV *
bysort gid mission (year month): gen lag_no_troops = no_troops[_n-1]


* Deaths *
bysort gid mission (year month): gen lag_OSV_GOV = OSV_GOV[_n-1]
bysort gid mission (year month): gen lag_OSV_Rebs = OSV_Rebs[_n-1]
bysort gid mission (year month): gen lag_OSV_total = OSV_total[_n-1]
bysort gid mission (year month): gen lag_best = best[_n-1]


* Make quality into millions *
rename quality v1 
gen quality = v1/1000
drop v1


* IVs *
local lags risk_ratio nlights_mean dist_unit dist_cap_hun neigh_troops_thou zone_de_confidence no_tcc mountains_prop total_land_area dist_cont_state dist_border_neigh dist_border_own hq days_to_urban Gov_watch drought_prop excluded_groups total_rain quality duration

foreach x in `lags' {
	bysort gid mission (year month): gen lag_`x' = `x'[_n-1]
	drop `x'
}



*** Label Variables ***
label var gid "PRIO-GRID Cell ID" 
label var month "Month" 
label var year "Year" 
label var mission "Mission"
label var COW "COW of Host State"
label var no_troops "Number of Troops in Cell"
label var lag_no_troops "Number of Troops in Cell (Lagged)"
label var lag_risk_ratio "Risk Ratio"
label var lag_nlights_mean "Night Lights"
label var lag_OSV_total "Total One Sided Violence"
label var lag_OSV_GOV "Government One Sided Violence"
label var lag_OSV_Rebs "Rebel One Sided Violence"
label var lag_dist_unit "Distance to Nearest Unit (Hundred km)"
label var lag_dist_cap_hun "Distance to Capital (Hundred km)"
label var lag_neigh_troops "Neighboring Troops"
label var lag_zone_de_confidence "Zone of Confidence"
label var lag_best "Battle Deaths"
label var lag_no_tcc "Number of TCCs in Cell"
label var lag_mountains_prop "Proportion of Mountainous Terrain"
label var lag_total_land_area "Land Area of Cell (Thou. Sq. km)"
label var lag_dist_cont_state "Distance to Contiguous State (Hundred km)"
label var lag_dist_border_neigh "Distance to Neighbor Border (Hundred km)"
label var lag_dist_border_own "Distance to Own Border (Hundred km)"
label var lag_hq "Headquarters"
label var lag_days_to_urban "Days to Urban Center"
label var best_time "Months Since Last Battle Death"
label var OSV_total_time "Months Since Last OSV Death"
label var OSV_GOV_time "Months Since Last Government OSV"
label var OSV_Rebs_time "Months Since Last Rebel OSV"
label var lag_Gov_watch "Government Overwatch"
label var lag_drought_prop "Proportion of Year in Drought"
label var lag_excluded_groups "Number of Excluded Ethnic Groups"
label var lag_total_rain "Year's Total Precipitation (mm)"
label var lag_quality "Troop Quality (Millions of Dollars)"
label var lag_duration "FC Duration"


*** Natural the neighboring troops variable ***
gen ln_lag_neigh_troops_thou = ln(lag_neigh_troops + 1)
drop neigh_troops lag_neigh_troops_thou
rename ln_lag_neigh_troops_thou lag_neigh_troops
label var lag_neigh_troops "Neighboring Troops (Thousands, Logged)"


*** Order Variables ***
order gid month year mission COW no_troops lag_no_troops lag_risk_ratio lag_nlights_mean lag_OSV_total ///
lag_OSV_GOV lag_OSV_Rebs lag_dist_unit lag_dist_cap_hun lag_mountains_prop lag_neigh_troops lag_zone_de_confidence lag_best ///
lag_no_tcc lag_mountains_prop  lag_total_land_area lag_dist_cont_state lag_dist_border_neigh ///
lag_dist_border_own lag_hq lag_days_to_urban best_time OSV_total_time OSV_GOV_time OSV_Rebs_time lag_Gov_watch


*** Drop varaibles *
drop OSV_GOV OSV_Rebs OSV_total 


*********************************
*** Mark UN Observer Missions ***
*********************************

// Observer missions based on their name and mandate 

gen observe = 0

// BINUB was a special political mission
replace observe = 1 if mission == "BINUB"

// MINUCI was a special political mission 
replace observe = 1 if mission == "MINUCI"

// MINUGUA was a small verification mission
replace observe = 1 if mission == "MINUGUA"

// Always an observer mission 
replace observe = 1 if mission == "MIPONUH"

// MONUA was always an observer mission 
replace observe = 1 if mission == "MONUA"

// MONUC was an observer mission, but had a rapidly increasing mandate
// that led to MONUSCO
// Began as an observer mission in 1999, but expanded in February 2000
replace observe = 1 if mission == "MONUC" & year == 1999
replace observe = 1 if mission == "MONUC" & year == 2000 & month < 2

// Always an observer mission 
replace observe = 1 if mission == "ONUSAL"

// Always an observer mission 
replace observe = 1 if mission == "UNOMSIL"

// UNAVEM II was an observer/small mission 
replace observe = 1 if mission == "UNAVEM II"

// UNIIMOG was an observer mission 
replace observe = 1 if mission == "UNIIMOG"

// Began as an observer mission, but had a military authorization beginning
// in February 1993
replace observe = 1 if mission == "UNIKOM" & year <= 1992
replace observe = 1 if mission == "UNIKOM" & year == 1993 & month < 2

// UNMIBH is an observer mission 
replace observe = 1 if mission == "UNMIBH"

// Always an observer mission 
replace observe = 1 if mission == "UNMIK"

// UNMIT was an observer mission 
replace observe = 1 if mission == "UNMIT"

// Always an observer mission 
replace observe = 1 if mission == "UNMOP"

// Always an observer mission
replace observe = 1 if mission == "UNMOT"

// Always an observer mission 
replace observe = 1 if mission == "UNOMIG"

// Always an observer mission
replace observe = 1 if mission == "UNOMIL"

// Always an observer mission 
replace observe = 1 if mission == "UNOMSIL"

// Always an observer mission 
replace observe = 1 if mission == "UNOMUR"

// Always an observer mission 
replace observe = 1 if mission == "UNPSG"

// Always an observer mission 
replace observe = 1 if mission == "UNSMIS"
label var observe "Observer Mission"


save Make_Data/STATA_Prep/Geo_PKO_pre_rand.dta, replace


**************************
*** Randomization Loop ***
**************************

cd ${cd_path}


use Make_Data/STATA_Prep/Geo_PKO_pre_rand.dta, clear

encode mission, gen(mission2)
levelsof mission2
local missions = r(levels)
save Make_Data/STATA_Prep/Geo_PKO_loop.dta, replace

foreach mis in `missions' {
	use Make_Data/STATA_Prep/Geo_PKO_loop.dta, clear
	keep if mission2 == `mis'
	save Make_Data/STATA_Prep/Random_Loops/r`mis'.dta, replace
	local i = 407
	
	while `i' <= 659 {
		use Make_Data/STATA_Prep/Random_Loops/r`mis'.dta, clear
		drop if yearmon != `i'
		
		if _N > 0 {
		
		* Random 1 *
		set seed 8739 // The numbers spell Trey
		bysort mission year month: complete_ra rand1_25 if no_troops == 0, prob(0.25) replace
		replace rand1_25 = 1 if rand1_25 == .
		bysort mission year month: complete_ra rand1_50 if no_troops == 0, prob(0.50) replace
		replace rand1_50 = 1 if rand1_50 == .

		* Random 2 *
		set seed 0722 // Trey's Birthday
		bysort mission year month: complete_ra rand2_25 if no_troops == 0, prob(0.25) replace
		replace rand2_25 = 1 if rand2_25 == .
		bysort mission year month: complete_ra rand2_50 if no_troops == 0, prob(0.50) replace
		replace rand2_50 = 1 if rand2_50 == .

		* Random 3 *
		set seed 1945 // Year the UN Was Founded
		bysort mission year month: complete_ra rand3_25 if no_troops == 0, prob(0.25) replace
		replace rand3_25 = 1 if rand3_25 == .
		bysort mission year month: complete_ra rand3_50 if no_troops == 0, prob(0.50) replace
		replace rand3_50 = 1 if rand3_50 == .

		* Random 4 *
		set seed 1948 // Year of First UN PKO
		bysort mission year month: complete_ra rand4_25 if no_troops == 0, prob(0.25) replace
		replace rand4_25 = 1 if rand4_25 == .
		bysort mission year month: complete_ra rand4_50 if no_troops == 0, prob(0.50) replace
		replace rand4_50 = 1 if rand4_50 == .

		* Random 5 *
		set seed 3079 // Last 4 Digits of My Phone Number
		bysort mission year month: complete_ra rand5_25 if no_troops == 0, prob(0.25) replace
		replace rand5_25 = 1 if rand5_25 == .
		bysort mission year month: complete_ra rand5_50 if no_troops == 0, prob(0.50) replace
		replace rand5_50 = 1 if rand5_50 == .

		* Random 6 *
		set seed 2021 // Year Project Began
		bysort mission year month: complete_ra rand6_25 if no_troops == 0, prob(0.25) replace
		replace rand6_25 = 1 if rand6_25 == .
		bysort mission year month: complete_ra rand6_50 if no_troops == 0, prob(0.50) replace
		replace rand6_50 = 1 if rand6_50 == .

		* Random 7 *
		set seed 1996 // Year Trey Was Born
		bysort mission year month: complete_ra rand7_25 if no_troops == 0, prob(0.25) replace
		replace rand7_25 = 1 if rand7_25 == .
		bysort mission year month: complete_ra rand7_50 if no_troops == 0, prob(0.50) replace
		replace rand7_50 = 1 if rand7_50 == .
		
		* Random 8 *
		set seed 7669 // Phone Spelling for My Favorite Headphones Brand, Sony
		bysort mission year month: complete_ra rand8_25 if no_troops == 0, prob(0.25) replace
		replace rand8_25 = 1 if rand8_25 == .
		bysort mission year month: complete_ra rand8_50 if no_troops == 0, prob(0.50) replace
		replace rand8_50 = 1 if rand8_50 == .
		
		* Random 9 *
		set seed 1955 // Year Covenant College was Founded
		bysort mission year month: complete_ra rand9_25 if no_troops == 0, prob(0.25) replace
		replace rand9_25 = 1 if rand9_25 == .
		bysort mission year month: complete_ra rand9_50 if no_troops == 0, prob(0.50) replace
		replace rand9_50 = 1 if rand9_50 == .

		* Random 10 *
		set seed 1865 // Year University Of Kentucky was Founded
		bysort mission year month: complete_ra rand10_25 if no_troops == 0, prob(0.25) replace
		replace rand10_25 = 1 if rand10_25 == .
		bysort mission year month: complete_ra rand10_50 if no_troops == 0, prob(0.50) replace
		replace rand10_50 = 1 if rand10_50 == .
		drop mission2
		duplicates drop
		save Make_Data/STATA_Prep/Random_Loops/Smaller_Datasets/r`mis'_`i'.dta, replace
		local i = `i' + 1
		}		
		else {
			local i = `i' + 1
		}
	}
}

* Append into one dataset *
cd "${cd_path}/Make_Data/STATA_Prep/Random_Loops/Smaller_Datasets"


! ls *.dta >filelist.txt

file open myfile using filelist.txt, read

file read myfile line
use `line'
save Geo_PKO_rand.dta, replace

file read myfile line
while r(eof)==0 { /* while you're not at the end of the file */
	append using `line'
	file read myfile line
}
file close myfile
save Geo_PKO_rand.dta, replace
drop yearmon 

****************************
*** Erase extra datasets ***
****************************

do ${cd_path}/Make_Data/STATA_Prep/Erases.do

******************
*** Final Save ***
******************

cd "${cd_path}/Data_Analysis"

save Geo_PKO.dta, replace





