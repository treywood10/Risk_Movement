/*	*************************************************************/
/*     	File Name:	Risk_Movement_DA		                    */
/*     	Date:   	September 7, 2022	                        */
/*      Author: 	Robert Lee Wood III				            */
/*      Purpose:	Data Analysis for Risk_Movement Chapter 	*/
/*      Input Files:Geo_PKO.dta		     						*/
/*     	Output File: 						                    */	
/*	*************************************************************/

* Clear all *
clear all
frame reset


* Set global macro for working directory path *
global cd_path "/Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement"


* Set working directory *
cd "${cd_path}"


*** Import Dataset ***
use Data_Analysis/Geo_PKO.dta, clear


* Randomly drew a seed of 1732 with `di round(runiform(1,9999))' *
set seed 1732


*** Global macros ***
global iv lag_risk_ratio lag_best best_time lag_duration
global host lag_nlights_mean lag_drought_prop lag_mountains_prop
global distances lag_dist_unit lag_dist_border_own lag_dist_cap_hun lag_days_to_urban
global mission lag_hq lag_zone_de_confidence lag_neigh_troops lag_quality


********************
*** Figure 3: DV ***
********************

* Run model for estimation sample *
nbreg no_troops $iv $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_best <= 200 & observe == 0, cluster(mission) 


* Include only observations from estiamted sample *
keep if e(sample)


* Send observations for DV histrogram *
save "Data_Analysis/R_Figures/DV_Hist.dta", replace


*** Import Dataset ***
use Data_Analysis/Geo_PKO.dta, clear


*************************************
*** Table 1: RR, Different Deaths ***
*************************************

* Battle Deaths
qui eststo m1: nbreg no_troops lag_risk_ratio lag_best best_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_best <= 200 & observe == 0, cluster(mission) 

* Total OSV
qui eststo m2: nbreg no_troops lag_risk_ratio lag_OSV_total OSV_total_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_total <= 200 & observe == 0, cluster(mission)  

* Rebel OSV
qui eststo m3: nbreg no_troops lag_risk_ratio lag_OSV_Rebs OSV_Rebs_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_Rebs <= 200 & observe == 0, cluster(mission)  

* Gov OSV
qui eststo m4: nbreg no_troops lag_risk_ratio lag_OSV_GOV OSV_GOV_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_GOV <= 200 & observe == 0, cluster(mission)  

esttab m1 m2 m3 m4 ///
using /Users/treywood/Dropbox/Projects/Active_Projects/Dissertation/Paper/RR_Troops.tex, replace ///
se(%6.3f) b(%6.3f) label nodep obslast nonotes ///
star(+ 0.10 * 0.05 ** 0.01) ///
title("Risk Ratio on Troops in Cell") ///
mtitles("Battle Deaths" "Total OSV" "Rebels OSV" "Gov OSV") ///
order(lag_risk_ratio lag_best best_time lag_OSV_total OSV_total_time lag_OSV_Rebs OSV_Rebs_time lag_OSV_GOV OSV_GOV_time lag_duration) ///
addnotes("Mission clustered standard errors in parentheses" "Dependent Variable is troop counts" "Randomly selected 25\% of grid-mission-month cells" "Restricted to 200 deaths and non-observer missions" "$+ p<0.10, * p<0.05, ** p<0.01$. Two-tailed test.")


**************************************
*** Table 2: RR X Different Deaths ***
**************************************

* Battle Deaths 
gen inter1 = lag_risk_ratio * lag_best
label var inter1 "Risk Ratio x Battle Deaths"
qui eststo m5: nbreg no_troops lag_risk_ratio lag_best inter1 best_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_best <= 200 & observe == 0, cluster(mission)  

* Total OSV 
gen inter2 = lag_risk_ratio * lag_OSV_total
label var inter2 "Risk Ratio x OSV Total"
qui eststo m6: nbreg no_troops lag_risk_ratio lag_OSV_total inter2 OSV_total_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_total <= 200 & observe == 0, cluster(mission)  

* Rebel OSV
gen inter3 = lag_risk_ratio * lag_OSV_Rebs
label var inter3 "Risk Ratio x OSV Rebs"
qui eststo m7: nbreg no_troops lag_risk_ratio lag_OSV_Rebs inter3 OSV_Rebs_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_Rebs <= 200 & observe == 0, cluster(mission)  

* Gov OSV
gen inter4 = lag_risk_ratio * lag_OSV_GOV
label var inter4 "Risk Ratio x OSV Gov"
qui eststo m8: nbreg no_troops lag_risk_ratio lag_OSV_GOV inter4 OSV_GOV_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_GOV <= 200 & observe == 0, cluster(mission)  


esttab m5 m6 m7 m8 ///
using /Users/treywood/Dropbox/Projects/Active_Projects/Dissertation/Paper/RR_Deaths.tex, replace ///
se(%6.3f) b(%6.3f) label nodep obslast nonotes ///
star(+ 0.10 * 0.05 ** 0.01) ///
title("Risk Ratio and Death Interactions") ///
mtitles("Battle Deaths" "Total OSV" "Rebels OSV" "Gov OSV") ///
order(lag_risk_ratio lag_best inter1 best_time lag_OSV_total inter2 OSV_total_time lag_OSV_Rebs inter3 OSV_Rebs_time lag_OSV_GOV inter4 OSV_GOV_time lag_duration) ///
addnotes("Mission clustered standard errors in parentheses" "Dependent Variable is troop counts" "Randomly selected 25\% of grid-mission-month cells" "Restricted to 200 deaths and non-observer missions" "$+ p<0.10, * p<0.05, ** p<0.01$. Two-tailed test.")

drop inter1 inter2 inter3 inter4


*********************************
*** Table 3: RR X Death Times ***
*********************************

* Battle Deaths 
gen inter1 = lag_risk_ratio * best_time
label var inter1 "Risk Ratio x Time Since Death"
eststo m9: nbreg no_troops lag_risk_ratio best_time inter1 lag_best lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_best <= 200 & observe == 0, cluster(mission)  

* Total OSV 
gen inter2 = lag_risk_ratio * OSV_total_time
label var inter2 "Risk Ratio x Time Since OSV Total"
qui eststo m10: nbreg no_troops lag_risk_ratio OSV_total_time inter2 lag_OSV_total lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_total <= 200 & observe == 0, cluster(mission)  

* Rebel OSV
gen inter3 = lag_risk_ratio * OSV_Rebs_time
label var inter3 "Risk Ratio x Time Since OSV Rebs"
qui eststo m11: nbreg no_troops lag_risk_ratio OSV_Rebs_time inter3 lag_OSV_Rebs lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_Rebs <= 200 & observe == 0, cluster(mission)  

* Gov OSV
gen inter4 = lag_risk_ratio * OSV_GOV_time
label var inter4 "Risk Ratio x Time Since OSV Gov"
qui eststo m12: nbreg no_troops lag_risk_ratio OSV_GOV_time inter4 lag_OSV_GOV lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_GOV <= 200 & observe == 0, cluster(mission)  


esttab m9 m10 m11 m12 ///
using /Users/treywood/Dropbox/Projects/Active_Projects/Dissertation/Paper/RR_Time.tex, replace /// 
se(%6.3f) b(%6.3f) label nodep obslast nonotes ///
star(+ 0.10 * 0.05 ** 0.01) ///
title("Risk Ratio and Time Since Violent Aciton Interactions") ///
mtitles("Battle Deaths" "Total OSV" "Rebels OSV" "Gov OSV") ///
order(lag_risk_ratio best_time inter1 lag_best OSV_total_time inter2 lag_OSV_total OSV_Rebs_time inter3 lag_OSV_Rebs OSV_GOV_time inter4 lag_OSV_GOV) ///
addnotes("Mission clustered standard errors in parentheses" "Dependent Variable is troop counts" "Randomly selected 25\% of grid-mission-month cells" "Restricted to 200 deaths and non-observer missions" "$+ p<0.10, * p<0.05, ** p<0.01$. Two-tailed test.")

drop inter1 inter2 inter3 inter4


*********************************
*** Table 4: RR X FC Duration ***
*********************************

* Battle Deaths 
gen inter1 = lag_risk_ratio * lag_duration
label var inter1 "Risk Ratio x FC Duration"
eststo m13: nbreg no_troops lag_risk_ratio lag_duration inter1 lag_best best_time $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_best <= 200 & observe == 0, cluster(mission)  

* Total OSV 
qui eststo m14: nbreg no_troops lag_risk_ratio lag_duration inter1 lag_OSV_total OSV_total_time $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_total <= 200 & observe == 0, cluster(mission)  

* Rebel OSV
qui eststo m15: nbreg no_troops lag_risk_ratio lag_duration inter1 lag_OSV_Rebs OSV_Rebs_time $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_Rebs <= 200 & observe == 0, cluster(mission)  

* Gov OSV
qui eststo m16: nbreg no_troops lag_risk_ratio lag_duration inter1 lag_OSV_GOV OSV_GOV_time $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_GOV <= 200 & observe == 0, cluster(mission)  


esttab m13 m14 m15 m16 ///
using /Users/treywood/Dropbox/Projects/Active_Projects/Dissertation/Paper/RR_FC_Dur.tex, replace /// 
se(%6.3f) b(%6.3f) label nodep obslast nonotes ///
star(+ 0.10 * 0.05 ** 0.01) ///
title("Risk Ratio and Force Commander Duration Interactions") ///
mtitles("Battle Deaths" "Total OSV" "Rebels OSV" "Gov OSV") ///
order(lag_risk_ratio lag_duration inter1 lag_best best_time lag_OSV_total OSV_total_time lag_OSV_Rebs OSV_Rebs_time lag_OSV_GOV OSV_GOV_time) ///
addnotes("Mission clustered standard errors in parentheses" "Dependent Variable is troop counts" "Randomly selected 25\% of grid-mission-month cells" "Restricted to 200 deaths and non-observer missions" "$+ p<0.10, * p<0.05, ** p<0.01$. Two-tailed test.")

drop inter1 



**********************************
*** RR, Different Deaths Plots ***
**********************************

* Battle Deaths
qui nbreg no_troops lag_risk_ratio lag_best best_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_best <= 200 & observe == 0, cluster(mission)  

drop if !e(sample)
save "Data_Analysis/R_Figures/data/RR_bd", replace

mat betas = e(b)
mat vcovs = e(V)

preserve 
svmat betas, names(matcol)
outsheet betas* in 1 using "Data_Analysis/R_Figures/betas/betas_RR_bd.txt", replace nolabel 
svmat vcovs, names(matcol)
outsheet vcovs* using "Data_Analysis/R_Figures/vcovs/vcovs_RR_bd.txt", replace nolabel 
restore 


*** Import Dataset ***
use Data_Analysis/Geo_PKO.dta, clear


* Total OSV
qui nbreg no_troops lag_risk_ratio lag_OSV_total OSV_total_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_total <= 200 & observe == 0, cluster(mission)  

drop if !e(sample)
save "Data_Analysis/R_Figures/data/RR_OSV", replace

mat betas = e(b)
mat vcovs = e(V)

preserve 
svmat betas, names(matcol)
outsheet betas* in 1 using "Data_Analysis/R_Figures/betas/betas_RR_OSV.txt", replace nolabel 
svmat vcovs, names(matcol)
outsheet vcovs* using "Data_Analysis/R_Figures/vcovs/vcovs_RR_OSV.txt", replace nolabel 
restore 

*** Import Dataset ***
use Data_Analysis/Geo_PKO.dta, clear


* Rebel OSV
qui nbreg no_troops lag_risk_ratio lag_OSV_Rebs OSV_Rebs_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_Rebs <= 200 & observe == 0, cluster(mission)  

drop if !e(sample)
save "Data_Analysis/R_Figures/data/RR_Rebs", replace

mat betas = e(b)
mat vcovs = e(V)

preserve 
svmat betas, names(matcol)
outsheet betas* in 1 using "Data_Analysis/R_Figures/betas/betas_RR_Rebs.txt", replace nolabel 
svmat vcovs, names(matcol)
outsheet vcovs* using "Data_Analysis/R_Figures/vcovs/vcovs_RR_Rebs.txt", replace nolabel 
restore 

*** Import Dataset ***
use Data_Analysis/Geo_PKO.dta, clear


* Gov OSV
qui nbreg no_troops lag_risk_ratio lag_OSV_GOV OSV_GOV_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_GOV <= 200 & observe == 0, cluster(mission) 

drop if !e(sample)
save "Data_Analysis/R_Figures/data/RR_Gov", replace

mat betas = e(b)
mat vcovs = e(V)

preserve 
svmat betas, names(matcol)
outsheet betas* in 1 using "Data_Analysis/R_Figures/betas/betas_RR_Gov.txt", replace nolabel 
svmat vcovs, names(matcol)
outsheet vcovs* using "Data_Analysis/R_Figures/vcovs/vcovs_RR_Gov.txt", replace nolabel 
restore 


 
***********************************
*** RR X Different Deaths Plots ***
***********************************

*** Import Dataset ***
use Data_Analysis/Geo_PKO.dta, clear


* Battle Deaths 
gen inter1 = lag_risk_ratio * lag_best
label var inter1 "Risk Ratio x Battle Deaths"
qui nbreg no_troops lag_risk_ratio lag_best inter1 best_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_best <= 200 & observe == 0, cluster(mission)  

drop if !e(sample)
save "Data_Analysis/R_Figures/data/RR_X_bd", replace

mat betas = e(b)
mat vcovs = e(V)

preserve 
svmat betas, names(matcol)
outsheet betas* in 1 using "Data_Analysis/R_Figures/betas/betas_RR_X_bd.txt", replace nolabel 
svmat vcovs, names(matcol)
outsheet vcovs* using "Data_Analysis/R_Figures/vcovs/vcovs_RR_X_bd.txt", replace nolabel 
restore

*** Import Dataset ***
use Data_Analysis/Geo_PKO.dta, clear


* Total OSV 
gen inter2 = lag_risk_ratio * lag_OSV_total
label var inter2 "Risk Ratio x OSV Total"
qui nbreg no_troops lag_risk_ratio lag_OSV_total inter2 OSV_total_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_total <= 200 & observe == 0, cluster(mission)  

drop if !e(sample)
save "Data_Analysis/R_Figures/data/RR_X_OSV", replace

mat betas = e(b)
mat vcovs = e(V)

preserve 
svmat betas, names(matcol)
outsheet betas* in 1 using "Data_Analysis/R_Figures/betas/betas_RR_X_OSV.txt", replace nolabel 
svmat vcovs, names(matcol)
outsheet vcovs* using "Data_Analysis/R_Figures/vcovs/vcovs_RR_X_OSV.txt", replace nolabel 
restore 

*** Import Dataset ***
use Data_Analysis/Geo_PKO.dta, clear


* Rebel OSV
gen inter3 = lag_risk_ratio * lag_OSV_Rebs
label var inter3 "Risk Ratio x OSV Rebs"
qui nbreg no_troops lag_risk_ratio lag_OSV_Rebs inter3 OSV_Rebs_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_Rebs <= 200 & observe == 0, cluster(mission)  

drop if !e(sample)
save "Data_Analysis/R_Figures/data/RR_X_Rebs", replace

mat betas = e(b)
mat vcovs = e(V)

preserve 
svmat betas, names(matcol)
outsheet betas* in 1 using "Data_Analysis/R_Figures/betas/betas_RR_X_Rebs.txt", replace nolabel 
svmat vcovs, names(matcol)
outsheet vcovs* using "Data_Analysis/R_Figures/vcovs/vcovs_RR_X_Rebs.txt", replace nolabel 
restore 

*** Import Dataset ***
use Data_Analysis/Geo_PKO.dta, clear


* Gov OSV
gen inter4 = lag_risk_ratio * lag_OSV_GOV
label var inter4 "Risk Ratio x OSV Gov"
qui nbreg no_troops lag_risk_ratio lag_OSV_GOV inter4 OSV_GOV_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_GOV <= 200 & observe == 0, cluster(mission)  

drop if !e(sample)
save "Data_Analysis/R_Figures/data/RR_X_Gov", replace

mat betas = e(b)
mat vcovs = e(V)

preserve 
svmat betas, names(matcol)
outsheet betas* in 1 using "Data_Analysis/R_Figures/betas/betas_RR_X_Gov.txt", replace nolabel 
svmat vcovs, names(matcol)
outsheet vcovs* using "Data_Analysis/R_Figures/vcovs/vcovs_RR_X_Gov.txt", replace nolabel 
restore 

*** Import Dataset ***
use Data_Analysis/Geo_PKO.dta, clear



***********************
*** RR X Time Plots ***
***********************

* Battle Deaths 
gen inter1 = lag_risk_ratio * best_time
label var inter1 "Risk Ratio x Time Since Death"
qui nbreg no_troops lag_risk_ratio best_time inter1 lag_best lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_best <= 200 & observe == 0, cluster(mission)  

drop if !e(sample)
save "Data_Analysis/R_Figures/data/RR_X_bd_t", replace

mat betas = e(b)
mat vcovs = e(V)

preserve 
svmat betas, names(matcol)
outsheet betas* in 1 using "Data_Analysis/R_Figures/betas/betas_RR_X_bd_t.txt", replace nolabel 
svmat vcovs, names(matcol)
outsheet vcovs* using "Data_Analysis/R_Figures/vcovs/vcovs_RR_X_bd_t.txt", replace nolabel 
restore

*** Import Dataset ***
use Data_Analysis/Geo_PKO.dta, clear


* Total OSV 
gen inter2 = lag_risk_ratio * OSV_total_time
label var inter2 "Risk Ratio x Time Since OSV Total"
qui nbreg no_troops lag_risk_ratio OSV_total_time inter2 lag_OSV_total lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_total <= 200 & observe == 0, cluster(mission)  

drop if !e(sample)
save "Data_Analysis/R_Figures/data/RR_X_OSV_t", replace

mat betas = e(b)
mat vcovs = e(V)

preserve 
svmat betas, names(matcol)
outsheet betas* in 1 using "Data_Analysis/R_Figures/betas/betas_RR_X_OSV_t.txt", replace nolabel 
svmat vcovs, names(matcol)
outsheet vcovs* using "Data_Analysis/R_Figures/vcovs/vcovs_RR_X_OSV_t.txt", replace nolabel 
restore 

*** Import Dataset ***
use Data_Analysis/Geo_PKO.dta, clear


* Rebel OSV
gen inter3 = lag_risk_ratio * OSV_Rebs_time
label var inter3 "Risk Ratio x Time Since OSV Rebs"
qui nbreg no_troops lag_risk_ratio OSV_Rebs_time inter3 lag_OSV_Rebs lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_Rebs <= 200 & observe == 0, cluster(mission)  

drop if !e(sample)
save "Data_Analysis/R_Figures/data/RR_X_Rebs_t", replace

mat betas = e(b)
mat vcovs = e(V)

preserve 
svmat betas, names(matcol)
outsheet betas* in 1 using "Data_Analysis/R_Figures/betas/betas_RR_X_Rebs_t.txt", replace nolabel 
svmat vcovs, names(matcol)
outsheet vcovs* using "Data_Analysis/R_Figures/vcovs/vcovs_RR_X_Rebs_t.txt", replace nolabel 
restore 

*** Import Dataset ***
use Data_Analysis/Geo_PKO.dta, clear

* Gov OSV
gen inter4 = lag_risk_ratio * OSV_GOV_time
label var inter4 "Risk Ratio x Time Since OSV Gov"
qui nbreg no_troops lag_risk_ratio OSV_GOV_time inter4 lag_OSV_GOV lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_GOV <= 200 & observe == 0, cluster(mission)  

drop if !e(sample)
save "Data_Analysis/R_Figures/data/RR_X_Gov_t", replace

mat betas = e(b)
mat vcovs = e(V)

preserve 
svmat betas, names(matcol)
outsheet betas* in 1 using "Data_Analysis/R_Figures/betas/betas_RR_X_Gov_t.txt", replace nolabel 
svmat vcovs, names(matcol)
outsheet vcovs* using "Data_Analysis/R_Figures/vcovs/vcovs_RR_X_Gov_t.txt", replace nolabel 
restore 

*** Import Dataset ***
use Data_Analysis/Geo_PKO.dta, clear


*************************
*** FC Duration Plots ***
*************************

* Battle Deaths 
qui nbreg no_troops lag_risk_ratio lag_duration lag_best best_time $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_best <= 200 & observe == 0, cluster(mission)  

drop if !e(sample)
save "Data_Analysis/R_Figures/data/RR_FC_bd", replace

mat betas = e(b)
mat vcovs = e(V)

preserve 
svmat betas, names(matcol)
outsheet betas* in 1 using "Data_Analysis/R_Figures/betas/betas_RR_FC_bd.txt", replace nolabel 
svmat vcovs, names(matcol)
outsheet vcovs* using "Data_Analysis/R_Figures/vcovs/vcovs_RR_FC_bd.txt", replace nolabel 
restore

*** Import Dataset ***
use Data_Analysis/Geo_PKO.dta, clear


* Total OSV 
qui nbreg no_troops lag_risk_ratio lag_duration lag_OSV_total OSV_total_time $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_total <= 200 & observe == 0, cluster(mission)  

drop if !e(sample)
save "Data_Analysis/R_Figures/data/RR_FC_OSV", replace

mat betas = e(b)
mat vcovs = e(V)

preserve 
svmat betas, names(matcol)
outsheet betas* in 1 using "Data_Analysis/R_Figures/betas/betas_RR_FC_OSV.txt", replace nolabel 
svmat vcovs, names(matcol)
outsheet vcovs* using "Data_Analysis/R_Figures/vcovs/vcovs_RR_FC_OSV.txt", replace nolabel 
restore 

*** Import Dataset ***
use Data_Analysis/Geo_PKO.dta, clear


* Rebel OSV
qui nbreg no_troops lag_risk_ratio lag_duration lag_OSV_Rebs OSV_Rebs_time $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_Rebs <= 200 & observe == 0, cluster(mission)  

drop if !e(sample)
save "Data_Analysis/R_Figures/data/RR_FC_Rebs", replace

mat betas = e(b)
mat vcovs = e(V)

preserve 
svmat betas, names(matcol)
outsheet betas* in 1 using "Data_Analysis/R_Figures/betas/betas_RR_FC_Rebs.txt", replace nolabel 
svmat vcovs, names(matcol)
outsheet vcovs* using "Data_Analysis/R_Figures/vcovs/vcovs_RR_FC_Rebs.txt", replace nolabel 
restore 

*** Import Dataset ***
use Data_Analysis/Geo_PKO.dta, clear

* Gov OSV
gen inter4 = lag_risk_ratio * lag_duration
label var inter4 "Risk Ratio x FC Duration"
qui nbreg no_troops lag_risk_ratio lag_duration lag_OSV_GOV OSV_GOV_time $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_GOV <= 200 & observe == 0, cluster(mission)  

drop if !e(sample)
save "Data_Analysis/R_Figures/data/RR_FC_Gov", replace

mat betas = e(b)
mat vcovs = e(V)

preserve 
svmat betas, names(matcol)
outsheet betas* in 1 using "Data_Analysis/R_Figures/betas/betas_RR_FC_Gov.txt", replace nolabel 
svmat vcovs, names(matcol)
outsheet vcovs* using "Data_Analysis/R_Figures/vcovs/vcovs_RR_FC_Gov.txt", replace nolabel 
restore 

*** Import Dataset ***
use Data_Analysis/Geo_PKO.dta, clear



******************************
*** RR X FC Duration Plots ***
******************************

* Battle Deaths 
gen inter1 = lag_risk_ratio * lag_duration
label var inter1 "Risk Ratio x FC Duration"
qui nbreg no_troops lag_risk_ratio lag_duration inter1 lag_best best_time $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_best <= 200 & observe == 0, cluster(mission)  

drop if !e(sample)
save "Data_Analysis/R_Figures/data/RR_X_FC_bd", replace

mat betas = e(b)
mat vcovs = e(V)

preserve 
svmat betas, names(matcol)
outsheet betas* in 1 using "Data_Analysis/R_Figures/betas/betas_RR_X_FC_bd.txt", replace nolabel 
svmat vcovs, names(matcol)
outsheet vcovs* using "Data_Analysis/R_Figures/vcovs/vcovs_RR_X_FC_bd.txt", replace nolabel 
restore

*** Import Dataset ***
use Data_Analysis/Geo_PKO.dta, clear


* Total OSV 
gen inter2 = lag_risk_ratio * lag_duration
label var inter2 "Risk Ratio x FC Duration"
qui nbreg no_troops lag_risk_ratio lag_duration inter2 lag_OSV_total OSV_total_time $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_total <= 200 & observe == 0, cluster(mission)  

drop if !e(sample)
save "Data_Analysis/R_Figures/data/RR_X_FC_OSV", replace

mat betas = e(b)
mat vcovs = e(V)

preserve 
svmat betas, names(matcol)
outsheet betas* in 1 using "Data_Analysis/R_Figures/betas/betas_RR_X_FC_OSV.txt", replace nolabel 
svmat vcovs, names(matcol)
outsheet vcovs* using "Data_Analysis/R_Figures/vcovs/vcovs_RR_X_FC_OSV.txt", replace nolabel 
restore 

*** Import Dataset ***
use Data_Analysis/Geo_PKO.dta, clear


* Rebel OSV
gen inter3 = lag_risk_ratio * lag_duration
label var inter3 "Risk Ratio x FC Duration"
qui nbreg no_troops lag_risk_ratio lag_duration inter3 lag_OSV_Rebs OSV_Rebs_time $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_Rebs <= 200 & observe == 0, cluster(mission)  

drop if !e(sample)
save "Data_Analysis/R_Figures/data/RR_X_FC_Rebs", replace

mat betas = e(b)
mat vcovs = e(V)

preserve 
svmat betas, names(matcol)
outsheet betas* in 1 using "Data_Analysis/R_Figures/betas/betas_RR_X_FC_Rebs.txt", replace nolabel 
svmat vcovs, names(matcol)
outsheet vcovs* using "Data_Analysis/R_Figures/vcovs/vcovs_RR_X_FC_Rebs.txt", replace nolabel 
restore 

*** Import Dataset ***
use Data_Analysis/Geo_PKO.dta, clear

* Gov OSV
gen inter4 = lag_risk_ratio * lag_duration
label var inter4 "Risk Ratio x FC Duration"
qui nbreg no_troops lag_risk_ratio lag_duration inter4 lag_OSV_GOV OSV_GOV_time $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_GOV <= 200 & observe == 0, cluster(mission)  

drop if !e(sample)
save "Data_Analysis/R_Figures/data/RR_X_FC_Gov", replace

mat betas = e(b)
mat vcovs = e(V)

preserve 
svmat betas, names(matcol)
outsheet betas* in 1 using "Data_Analysis/R_Figures/betas/betas_RR_X_FC_Gov.txt", replace nolabel 
svmat vcovs, names(matcol)
outsheet vcovs* using "Data_Analysis/R_Figures/vcovs/vcovs_RR_X_FC_Gov.txt", replace nolabel 
restore 

*** Import Dataset ***
use Data_Analysis/Geo_PKO.dta, clear



*********************************
**** Observer Mission Tables ****
*********************************

**** RR ****

* Battle Deaths
eststo m17: nbreg no_troops lag_risk_ratio lag_best best_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_best <= 200, cluster(mission) 

* Total OSV
eststo m18: nbreg no_troops lag_risk_ratio lag_OSV_total OSV_total_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_total <= 200, cluster(mission)  

* Rebel OSV
eststo m19: nbreg no_troops lag_risk_ratio lag_OSV_Rebs OSV_Rebs_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_Rebs <= 200, cluster(mission)  

* Gov OSV
eststo m20: nbreg no_troops lag_risk_ratio lag_OSV_GOV OSV_GOV_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_GOV <= 200, cluster(mission)  


**** RR X Battle Deaths ****

* Battle Deaths 
gen inter11 = lag_risk_ratio * lag_best
label var inter11 "Risk Ratio x Battle Deaths"
eststo m21: nbreg no_troops lag_risk_ratio lag_best inter11 best_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_best <= 200, cluster(mission)  

* Total OSV 
gen inter12 = lag_risk_ratio * lag_OSV_total
label var inter12 "Risk Ratio x OSV Total"
eststo m22: nbreg no_troops lag_risk_ratio lag_OSV_total inter12 OSV_total_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_total <= 200, cluster(mission)  

* Rebel OSV
gen inter13 = lag_risk_ratio * lag_OSV_Rebs
label var inter13 "Risk Ratio x OSV Rebs"
eststo m23: nbreg no_troops lag_risk_ratio lag_OSV_Rebs inter13 OSV_Rebs_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_Rebs <= 200, cluster(mission)  

* Gov OSV
gen inter14 = lag_risk_ratio * lag_OSV_GOV
label var inter14 "Risk Ratio x OSV Gov"
eststo m24: nbreg no_troops lag_risk_ratio lag_OSV_GOV inter14 OSV_GOV_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_GOV <= 200, cluster(mission)  


**** RR X Death Times ****

* Battle Deaths 
gen inter21 = lag_risk_ratio * best_time
label var inter21 "Risk Ratio x Time Since Death"
eststo m25: nbreg no_troops lag_risk_ratio best_time inter21 lag_best lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_best <= 200, cluster(mission)  

* Total OSV 
gen inter22 = lag_risk_ratio * OSV_total_time
label var inter22 "Risk Ratio x Time Since OSV Total"
qui eststo m26: nbreg no_troops lag_risk_ratio OSV_total_time inter22 lag_OSV_total lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_total <= 200, cluster(mission)  

* Rebel OSV
gen inter23 = lag_risk_ratio * OSV_Rebs_time
label var inter23 "Risk Ratio x Time Since OSV Rebs"
qui eststo m27: nbreg no_troops lag_risk_ratio OSV_Rebs_time inter23 lag_OSV_Rebs lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_Rebs <= 200, cluster(mission)  

* Gov OSV
gen inter24 = lag_risk_ratio * OSV_GOV_time
label var inter24 "Risk Ratio x Time Since OSV Gov"
qui eststo m28: nbreg no_troops lag_risk_ratio OSV_GOV_time inter24 lag_OSV_GOV lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_GOV <= 200, cluster(mission)  


**** RR x FC Duration ****

* Battle Deaths 
gen inter31 = lag_risk_ratio * lag_duration
label var inter31 "Risk Ratio x FC Duration"
eststo m29: nbreg no_troops lag_risk_ratio lag_duration inter31 lag_best best_time $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_best <= 200, cluster(mission)  

* Total OSV 
qui eststo m30: nbreg no_troops lag_risk_ratio lag_duration inter31 lag_OSV_total OSV_total_time $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_total <= 200, cluster(mission)  

* Rebel OSV
qui eststo m31: nbreg no_troops lag_risk_ratio lag_duration inter31 lag_OSV_Rebs OSV_Rebs_time $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_Rebs <= 200, cluster(mission)  

* Gov OSV
qui eststo m32: nbreg no_troops lag_risk_ratio lag_duration inter31 lag_OSV_GOV OSV_GOV_time $host $distances $mission lag_no_troops if rand1_25 == 1 & lag_OSV_GOV <= 200, cluster(mission)  


esttab m17 m18 m19 m20 m21 m22 m23 m24 ///
using /Users/treywood/Dropbox/Projects/Active_Projects/Dissertation/Paper/RR_Observers_p1.tex, replace /// 
se(%6.3f) b(%6.3f) label nodep obslast nonotes ///
star(+ 0.10 * 0.05 ** 0.01) ///
title("All Models Including Observer Missions, Part 1") ///
mtitles("Battle Deaths" "Total OSV" "Rebels OSV" "Gov OSV" "Battle Deaths" "Total OSV" "Rebels OSV" "Gov OSV") ///
order(lag_risk_ratio lag_best best_time lag_OSV_total OSV_total_time lag_OSV_Rebs OSV_Rebs_time lag_OSV_GOV OSV_GOV_time lag_duration inter*) ///
addnotes("Mission clustered standard errors in parentheses" "Dependent Variable is troop counts" "Randomly selected 25\% of grid-mission-month cells" "Restricted to 200 deaths" "$+ p<0.10, * p<0.05, ** p<0.01$. Two-tailed test.")

esttab m25 m26 m27 m28 m29 m30 m31 m32 ///
using /Users/treywood/Dropbox/Projects/Active_Projects/Dissertation/Paper/RR_Observers_p2.tex, replace /// 
se(%6.3f) b(%6.3f) label nodep obslast nonotes ///
star(+ 0.10 * 0.05 ** 0.01) ///
title("All Models Including Observer Missions, Part 2") ///
mtitles("Battle Deaths" "Total OSV" "Rebels OSV" "Gov OSV" "Battle Deaths" "Total OSV" "Rebels OSV" "Gov OSV") ///
order(lag_risk_ratio lag_best best_time lag_OSV_total OSV_total_time lag_OSV_Rebs OSV_Rebs_time lag_OSV_GOV OSV_GOV_time lag_duration inter*) ///
addnotes("Mission clustered standard errors in parentheses" "Dependent Variable is troop counts" "Randomly selected 25\% of grid-mission-month cells" "Restricted to 200 deaths" "$+ p<0.10, * p<0.05, ** p<0.01$. Two-tailed test.")

drop inter*


*********************************
**** All Battle Death Tables ****
*********************************

**** RR ****

* Battle Deaths
eststo m33: nbreg no_troops lag_risk_ratio lag_best best_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & observe == 0, cluster(mission) 

* Total OSV
eststo m34: nbreg no_troops lag_risk_ratio lag_OSV_total OSV_total_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & observe == 0, cluster(mission)  

* Rebel OSV
eststo m35: nbreg no_troops lag_risk_ratio lag_OSV_Rebs OSV_Rebs_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & observe == 0, cluster(mission)  

* Gov OSV
eststo m36: nbreg no_troops lag_risk_ratio lag_OSV_GOV OSV_GOV_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & observe == 0, cluster(mission)  


**** RR X Battle Deaths ****

* Battle Deaths 
gen inter11 = lag_risk_ratio * lag_best
label var inter11 "Risk Ratio x Battle Deaths"
eststo m37: nbreg no_troops lag_risk_ratio lag_best inter11 best_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & observe == 0, cluster(mission)  

* Total OSV 
gen inter12 = lag_risk_ratio * lag_OSV_total
label var inter12 "Risk Ratio x OSV Total"
eststo m38: nbreg no_troops lag_risk_ratio lag_OSV_total inter12 OSV_total_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & observe == 0, cluster(mission)  

* Rebel OSV
gen inter13 = lag_risk_ratio * lag_OSV_Rebs
label var inter13 "Risk Ratio x OSV Rebs"
eststo m39: nbreg no_troops lag_risk_ratio lag_OSV_Rebs inter13 OSV_Rebs_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & observe == 0, cluster(mission)  

* Gov OSV
gen inter14 = lag_risk_ratio * lag_OSV_GOV
label var inter14 "Risk Ratio x OSV Gov"
eststo m40: nbreg no_troops lag_risk_ratio lag_OSV_GOV inter14 OSV_GOV_time lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & observe == 0, cluster(mission)  


**** RR X Death Times ****

* Battle Deaths 
gen inter21 = lag_risk_ratio * best_time
label var inter21 "Risk Ratio x Time Since Death"
eststo m41: nbreg no_troops lag_risk_ratio best_time inter21 lag_best lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & observe == 0, cluster(mission)  

* Total OSV 
gen inter22 = lag_risk_ratio * OSV_total_time
label var inter22 "Risk Ratio x Time Since OSV Total"
qui eststo m42: nbreg no_troops lag_risk_ratio OSV_total_time inter22 lag_OSV_total lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & observe == 0, cluster(mission)  

* Rebel OSV
gen inter23 = lag_risk_ratio * OSV_Rebs_time
label var inter23 "Risk Ratio x Time Since OSV Rebs"
qui eststo m43: nbreg no_troops lag_risk_ratio OSV_Rebs_time inter23 lag_OSV_Rebs lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & observe == 0, cluster(mission)  

* Gov OSV
gen inter24 = lag_risk_ratio * OSV_GOV_time
label var inter24 "Risk Ratio x Time Since OSV Gov"
qui eststo m44: nbreg no_troops lag_risk_ratio OSV_GOV_time inter24 lag_OSV_GOV lag_duration $host $distances $mission lag_no_troops if rand1_25 == 1 & observe == 0, cluster(mission)  


**** RR x FC Duration ****

* Battle Deaths 
gen inter31 = lag_risk_ratio * lag_duration
label var inter31 "Risk Ratio x FC Duration"
eststo m45: nbreg no_troops lag_risk_ratio lag_duration inter31 lag_best best_time $host $distances $mission lag_no_troops if rand1_25 == 1 & observe == 0, cluster(mission)  

* Total OSV 
qui eststo m46: nbreg no_troops lag_risk_ratio lag_duration inter31 lag_OSV_total OSV_total_time $host $distances $mission lag_no_troops if rand1_25 == 1 & observe == 0, cluster(mission)  

* Rebel OSV
qui eststo m47: nbreg no_troops lag_risk_ratio lag_duration inter31 lag_OSV_Rebs OSV_Rebs_time $host $distances $mission lag_no_troops if rand1_25 == 1 & observe == 0, cluster(mission)  

* Gov OSV
qui eststo m48: nbreg no_troops lag_risk_ratio lag_duration inter31 lag_OSV_GOV OSV_GOV_time $host $distances $mission lag_no_troops if rand1_25 == 1 & observe == 0, cluster(mission)  


esttab m33 m34 m35 m36 m37 m38 m39 m40 ///
using /Users/treywood/Dropbox/Projects/Active_Projects/Dissertation/Paper/RR_All_Bds_p1.tex, replace /// 
se(%6.3f) b(%6.3f) label nodep obslast nonotes ///
star(+ 0.10 * 0.05 ** 0.01) ///
title("All Models without Death Restrictions, Part 1") ///
mtitles("Battle Deaths" "Total OSV" "Rebels OSV" "Gov OSV" "Battle Deaths" "Total OSV" "Rebels OSV" "Gov OSV") ///
order(lag_risk_ratio lag_best best_time lag_OSV_total OSV_total_time lag_OSV_Rebs OSV_Rebs_time lag_OSV_GOV OSV_GOV_time lag_duration inter*) ///
addnotes("Mission clustered standard errors in parentheses" "Dependent Variable is troop counts" "Randomly selected 25\% of grid-mission-month cells" "Restricted to non-observer missions" "$+ p<0.10, * p<0.05, ** p<0.01$. Two-tailed test.")

esttab m41 m42 m43 m44 m45 m46 m47 m48 ///
using /Users/treywood/Dropbox/Projects/Active_Projects/Dissertation/Paper/RR_All_Bds_p2.tex, replace /// 
se(%6.3f) b(%6.3f) label nodep obslast nonotes ///
star(+ 0.10 * 0.05 ** 0.01) ///
title("All Models without Death Restrictions, Part 2") ///
mtitles("Battle Deaths" "Total OSV" "Rebels OSV" "Gov OSV" "Battle Deaths" "Total OSV" "Rebels OSV" "Gov OSV") ///
order(lag_risk_ratio lag_best best_time lag_OSV_total OSV_total_time lag_OSV_Rebs OSV_Rebs_time lag_OSV_GOV OSV_GOV_time lag_duration inter*) ///
addnotes("Mission clustered standard errors in parentheses" "Dependent Variable is troop counts" "Randomly selected 25\% of grid-mission-month cells" "Restricted to non-observer missions" "$+ p<0.10, * p<0.05, ** p<0.01$. Two-tailed test.")

drop inter*

***********************
**** 50% of Cells  ****
***********************

**** RR ****

* Battle Deaths
eststo m49: nbreg no_troops lag_risk_ratio lag_best best_time lag_duration $host $distances $mission lag_no_troops if rand1_50 == 1 & lag_best <= 200 & observe == 0, cluster(mission)  

* Total OSV
eststo m50: nbreg no_troops lag_risk_ratio lag_OSV_total OSV_total_time lag_duration $host $distances $mission lag_no_troops if rand1_50 == 1 & lag_OSV_total <= 200 & observe == 0, cluster(mission)  

* Rebel OSV
eststo m51: nbreg no_troops lag_risk_ratio lag_OSV_Rebs OSV_Rebs_time lag_duration $host $distances $mission lag_no_troops if rand1_50 == 1 & lag_OSV_Rebs <= 200 & observe == 0, cluster(mission)  

* Gov OSV
eststo m52: nbreg no_troops lag_risk_ratio lag_OSV_GOV OSV_GOV_time lag_duration $host $distances $mission lag_no_troops if rand1_50 == 1 & lag_OSV_GOV <= 200 & observe == 0, cluster(mission)  


**** RR X Battle Deaths ****

* Battle Deaths 
gen inter11 = lag_risk_ratio * lag_best
label var inter11 "Risk Ratio x Battle Deaths"
eststo m53: nbreg no_troops lag_risk_ratio lag_best inter11 best_time lag_duration $host $distances $mission lag_no_troops if rand1_50 == 1 & lag_best <= 200 & observe == 0, cluster(mission)  

* Total OSV 
gen inter12 = lag_risk_ratio * lag_OSV_total
label var inter12 "Risk Ratio x OSV Total"
eststo m54: nbreg no_troops lag_risk_ratio lag_OSV_total inter12 OSV_total_time lag_duration $host $distances $mission lag_no_troops if rand1_50 == 1 & lag_OSV_total <= 200 & observe == 0, cluster(mission) diff

* Rebel OSV
gen inter13 = lag_risk_ratio * lag_OSV_Rebs
label var inter13 "Risk Ratio x OSV Rebs"
eststo m55: nbreg no_troops lag_risk_ratio lag_OSV_Rebs inter13 OSV_Rebs_time lag_duration $host $distances $mission lag_no_troops if rand1_50 == 1 & lag_OSV_Rebs <= 200 & observe == 0, cluster(mission)  

* Gov OSV
gen inter14 = lag_risk_ratio * lag_OSV_GOV
label var inter14 "Risk Ratio x OSV Gov"
eststo m56: nbreg no_troops lag_risk_ratio lag_OSV_GOV inter14 OSV_GOV_time lag_duration $host $distances $mission lag_no_troops if rand1_50 == 1 & lag_OSV_GOV <= 200 & observe == 0, cluster(mission)  


**** RR X Death Times ****

* Battle Deaths 
gen inter21 = lag_risk_ratio * best_time
label var inter21 "Risk Ratio x Time Since Death"
eststo m57: nbreg no_troops lag_risk_ratio best_time inter21 lag_best lag_duration $host $distances $mission lag_no_troops if rand1_50 == 1 & lag_best <= 200 & observe == 0, cluster(mission)  

* Total OSV 
gen inter22 = lag_risk_ratio * OSV_total_time
label var inter22 "Risk Ratio x Time Since OSV Total"
qui eststo m58: nbreg no_troops lag_risk_ratio OSV_total_time inter22 lag_OSV_total lag_duration $host $distances $mission lag_no_troops if rand1_50 == 1 & lag_OSV_total <= 200 & observe == 0, cluster(mission)  

* Rebel OSV
gen inter23 = lag_risk_ratio * OSV_Rebs_time
label var inter23 "Risk Ratio x Time Since OSV Rebs"
qui eststo m59: nbreg no_troops lag_risk_ratio OSV_Rebs_time inter23 lag_OSV_Rebs lag_duration $host $distances $mission lag_no_troops if rand1_50 == 1 & lag_OSV_Rebs <= 200 & observe == 0, cluster(mission)  

* Gov OSV
gen inter24 = lag_risk_ratio * OSV_GOV_time
label var inter24 "Risk Ratio x Time Since OSV Gov"
qui eststo m60: nbreg no_troops lag_risk_ratio OSV_GOV_time inter24 lag_OSV_GOV lag_duration $host $distances $mission lag_no_troops if rand1_50 == 1 & lag_OSV_GOV <= 200 & observe == 0, cluster(mission)  


**** RR x FC Duration ****

* Battle Deaths 
gen inter31 = lag_risk_ratio * lag_duration
label var inter31 "Risk Ratio x FC Duration"
eststo m61: nbreg no_troops lag_risk_ratio lag_duration inter31 lag_best best_time $host $distances $mission lag_no_troops if rand1_50 == 1 & lag_best <= 200 & observe == 0, cluster(mission)  

* Total OSV 
qui eststo m62: nbreg no_troops lag_risk_ratio lag_duration inter31 lag_OSV_total OSV_total_time $host $distances $mission lag_no_troops if rand1_50 == 1 & lag_OSV_total <= 200 & observe == 0, cluster(mission)  

* Rebel OSV
qui eststo m63: nbreg no_troops lag_risk_ratio lag_duration inter31 lag_OSV_Rebs OSV_Rebs_time $host $distances $mission lag_no_troops if rand1_50 == 1 & lag_OSV_Rebs <= 200 & observe == 0, cluster(mission)  

* Gov OSV
qui eststo m64: nbreg no_troops lag_risk_ratio lag_duration inter31 lag_OSV_GOV OSV_GOV_time $host $distances $mission lag_no_troops if rand1_50 == 1 & lag_OSV_GOV <= 200 & observe == 0, cluster(mission)  


esttab m49 m50 m51 m52 m53 m54 m55 m56 ///
using /Users/treywood/Dropbox/Projects/Active_Projects/Dissertation/Paper/RR_50p_p1.tex, replace /// 
se(%6.3f) b(%6.3f) label nodep obslast nonotes ///
star(+ 0.10 * 0.05 ** 0.01) ///
title("All Models with 50\% of Cells, Part 1") ///
mtitles("Battle Deaths" "Total OSV" "Rebels OSV" "Gov OSV" "Battle Deaths" "Total OSV" "Rebels OSV" "Gov OSV") ///
order(lag_risk_ratio lag_best best_time lag_OSV_total OSV_total_time lag_OSV_Rebs OSV_Rebs_time lag_OSV_GOV OSV_GOV_time lag_duration inter*) ///
addnotes("Mission clustered standard errors in parentheses" "Dependent Variable is troop counts" "Randomly selected 50\% of grid-mission-month cells" "Restricted to 200 deaths and non-observer missions" "$+ p<0.10, * p<0.05, ** p<0.01$. Two-tailed test.")

esttab m57 m58 m59 m60 m61 m62 m63 m64 ///
using /Users/treywood/Dropbox/Projects/Active_Projects/Dissertation/Paper/RR_50p_p2.tex, replace /// 
se(%6.3f) b(%6.3f) label nodep obslast nonotes ///
star(+ 0.10 * 0.05 ** 0.01) ///
title("All Models with 50\% of Cells, Part 2") ///
mtitles("Battle Deaths" "Total OSV" "Rebels OSV" "Gov OSV" "Battle Deaths" "Total OSV" "Rebels OSV" "Gov OSV") ///
order(lag_risk_ratio lag_best best_time lag_OSV_total OSV_total_time lag_OSV_Rebs OSV_Rebs_time lag_OSV_GOV OSV_GOV_time lag_duration inter*) ///
addnotes("Mission clustered standard errors in parentheses" "Dependent Variable is troop counts" "Randomly selected 50\% of grid-mission-month cells" "Restricted to 200 deaths and non-observer missions" "$+ p<0.10, * p<0.05, ** p<0.01$. Two-tailed test.")

drop inter*



***************************
**** Meta Analysis, 25 ****
***************************


* Model 1: Battle Deaths
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	replace rand`n'_25 = . if rand`n'_25 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] best_beta = _b[lag_best] best_se = _se[lag_best] dur_beta = _b[lag_duration] dur_se = _se[lag_duration] size = e(N), by(rand`n'_25) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio lag_best best_time lag_duration $host $distances $mission lag_no_troops if rand`n'_25 == 1 & lag_best <= 200 & observe == 0, cluster(mission) 
}
clear 

* Append smaller datasets
append using META/META_l_1_25 META/META_l_2_25 META/META_l_3_25 META/META_l_4_25 META/META_l_5_25 META/META_l_6_25 META/META_l_7_25 META/META_l_8_25 META/META_l_9_25 META/META_l_10_25
drop rand*
gen ID = _n
save "META/META_25_m1.dta", replace

* Erase *
local met META_l_1_25 META_l_2_25 META_l_3_25 META_l_4_25 META_l_5_25 META_l_6_25 META_l_7_25 META_l_8_25 META_l_9_25 META_l_10_25
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 2: Total OSV
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	replace rand`n'_25 = . if rand`n'_25 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] OSV_t_beta = _b[lag_OSV_total] OSV_t_se = _se[lag_OSV_total] dur_beta = _b[lag_duration] dur_se = _se[lag_duration] size = e(N), by(rand`n'_25) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio lag_OSV_total OSV_total_time lag_duration $host $distances $mission lag_no_troops if rand`n'_25 == 1 == 1 & lag_OSV_total <= 200 & observe == 0, cluster(mission)
}
clear 

* Append smaller datasets
append using META/META_l_1_25 META/META_l_2_25 META/META_l_3_25 META/META_l_4_25 META/META_l_5_25 META/META_l_6_25 META/META_l_7_25 META/META_l_8_25 META/META_l_9_25 META/META_l_10_25
drop rand*
gen ID = _n
save "META/META_25_m2.dta", replace

* Erase *
local met META_l_1_25 META_l_2_25 META_l_3_25 META_l_4_25 META_l_5_25 META_l_6_25 META_l_7_25 META_l_8_25 META_l_9_25 META_l_10_25
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 3: Rebel OSV
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	replace rand`n'_25 = . if rand`n'_25 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] OSV_r_beta = _b[lag_OSV_Rebs] OSV_r_se = _se[lag_OSV_Rebs] dur_beta = _b[lag_duration] dur_se = _se[lag_duration] size = e(N), by(rand`n'_25) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio lag_OSV_Rebs OSV_Rebs_time lag_duration $host $distances $mission lag_no_troops if rand`n'_25 == 1 == 1 & lag_OSV_Rebs <= 200 & observe == 0, cluster(mission) 
}
clear 

* Append smaller datasets
append using META/META_l_1_25 META/META_l_2_25 META/META_l_3_25 META/META_l_4_25 META/META_l_5_25 META/META_l_6_25 META/META_l_7_25 META/META_l_8_25 META/META_l_9_25 META/META_l_10_25
drop rand*
gen ID = _n
save "META/META_25_m3.dta", replace

* Erase *
local met META_l_1_25 META_l_2_25 META_l_3_25 META_l_4_25 META_l_5_25 META_l_6_25 META_l_7_25 META_l_8_25 META_l_9_25 META_l_10_25
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 4: Gov OSV
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	replace rand`n'_25 = . if rand`n'_25 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] OSV_g_beta = _b[lag_OSV_GOV] OSV_g_se = _se[lag_OSV_GOV] dur_beta = _b[lag_duration] dur_se = _se[lag_duration] size = e(N), by(rand`n'_25) saving(META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio lag_OSV_GOV OSV_GOV_time lag_duration $host $distances $mission lag_no_troops if rand`n'_25 == 1 & lag_OSV_GOV <= 200 & observe == 0, cluster(mission)  
}
clear 

* Append smaller datasets
append using META/META_l_1_25 META/META_l_2_25 META/META_l_3_25 META/META_l_4_25 META/META_l_5_25 META/META_l_6_25 META/META_l_7_25 META/META_l_8_25 META/META_l_9_25 META/META_l_10_25
drop rand*
gen ID = _n
save "META/META_25_m4.dta", replace

* Erase *
local met META_l_1_25 META_l_2_25 META_l_3_25 META_l_4_25 META_l_5_25 META_l_6_25 META_l_7_25 META_l_8_25 META_l_9_25 META_l_10_25
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 5: RR x Battle Deaths
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_25 != 1
	gen inter1 = lag_risk_ratio * lag_best
	replace rand`n'_25 = . if rand`n'_25 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] best_beta = _b[lag_best] best_se = _se[lag_best] inter_bd_beta = _b[inter1] inter_bd_se = _se[inter1] size = e(N), by(rand`n'_25) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio lag_best inter1 best_time lag_duration $host $distances $mission lag_no_troops if lag_best <= 200 & observe == 0, cluster(mission) iterate(1000)
}
clear 

* Append smaller datasets
append using META/META_l_1_25 META/META_l_2_25 META/META_l_3_25 META/META_l_4_25 META/META_l_5_25 META/META_l_6_25 META/META_l_7_25 META/META_l_8_25 META/META_l_9_25 META/META_l_10_25
drop rand*
gen ID = _n
save "META/META_25_m5.dta", replace

* Erase *
local met META_l_1_25 META_l_2_25 META_l_3_25 META_l_4_25 META_l_5_25 META_l_6_25 META_l_7_25 META_l_8_25 META_l_9_25 META_l_10_25
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 6: RR x OSV Total
gen inter2 = lag_risk_ratio * lag_OSV_total
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	replace rand`n'_25 = . if rand`n'_25 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] OSV_beta = _b[lag_OSV_total] OSV_se = _se[lag_OSV_total] inter_OSV_beta = _b[inter2] inter_OSV_se = _se[inter2] size = e(N), by(rand`n'_25) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio lag_OSV_total inter2 OSV_total_time lag_duration $host $distances $mission lag_no_troops if rand`n'_25 == 1 & lag_OSV_total <= 200 & observe == 0, cluster(mission)  
}
clear 

* Append smaller datasets
append using META/META_l_1_25 META/META_l_2_25 META/META_l_3_25 META/META_l_4_25 META/META_l_5_25 META/META_l_6_25 META/META_l_7_25 META/META_l_8_25 META/META_l_9_25 META/META_l_10_25
drop rand*
gen ID = _n
save "META/META_25_m6.dta", replace

* Erase *
local met META_l_1_25 META_l_2_25 META_l_3_25 META_l_4_25 META_l_5_25 META_l_6_25 META_l_7_25 META_l_8_25 META_l_9_25 META_l_10_25
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear



* Model 7: RR x OSV Rebs
gen inter3 = lag_risk_ratio * lag_OSV_Rebs
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	replace rand`n'_25 = . if rand`n'_25 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] Rebs_beta = _b[lag_OSV_Rebs] Rebs_se = _se[lag_OSV_Rebs] inter_Rebs_beta = _b[inter3] inter_Rebs_se = _se[inter3] size = e(N), by(rand`n'_25) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio lag_OSV_Rebs inter3 OSV_Rebs_time lag_duration $host $distances $mission lag_no_troops if lag_OSV_Rebs <= 200 & observe == 0, cluster(mission) diff
}
clear 

* Append smaller datasets
append using META/META_l_1_25 META/META_l_2_25 META/META_l_3_25 META/META_l_4_25 META/META_l_5_25 META/META_l_6_25 META/META_l_7_25 META/META_l_8_25 META/META_l_9_25 META/META_l_10_25
drop rand*
gen ID = _n
save "META/META_25_m7.dta", replace

* Erase *
local met META_l_1_25 META_l_2_25 META_l_3_25 META_l_4_25 META_l_5_25 META_l_6_25 META_l_7_25 META_l_8_25 META_l_9_25 META_l_10_25
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear



* Model 8: RR x OSV Gov
*gen inter4 = lag_risk_ratio * lag_OSV_GOV
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_25 != 1
	gen inter4 = lag_risk_ratio * lag_OSV_GOV
	replace rand`n'_25 = . if rand`n'_25 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] Gov_beta = _b[lag_OSV_GOV] Gov_se = _se[lag_OSV_GOV] inter_Gov_beta = _b[inter4] inter_Gov_se = _se[inter4] size = e(N), by(rand`n'_25) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio lag_OSV_GOV inter4 OSV_GOV_time lag_duration $host $distances $mission lag_no_troops if rand`n'_25 == 1 & lag_OSV_GOV <= 200 & observe == 0, cluster(mission) diff iterate(1000)
}
clear 

* Append smaller datasets
append using META/META_l_1_25 META/META_l_2_25 META/META_l_3_25 META/META_l_4_25 META/META_l_5_25 META/META_l_6_25 META/META_l_7_25 META/META_l_8_25 META/META_l_9_25 META/META_l_10_25
drop rand*
gen ID = _n
save "META/META_25_m8.dta", replace

* Erase *
local met META_l_1_25 META_l_2_25 META_l_3_25 META_l_4_25 META_l_5_25 META_l_6_25 META_l_7_25 META_l_8_25 META_l_9_25 META_l_10_25
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 9: RR x Best Time
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_25 != 1
	gen inter1 = lag_risk_ratio * best_time
	replace rand`n'_25 = . if rand`n'_25 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] best_t_beta = _b[best_time] best_t_se = _se[best_time] inter_best_t_beta = _b[inter1] inter_best_t_se = _se[inter1] size = e(N), by(rand`n'_25) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio best_time inter1 lag_best lag_duration $host $distances $mission lag_no_troops if rand`n'_25 == 1 & lag_best <= 200 & observe == 0, cluster(mission) iterate(1000)
}
clear 

* Append smaller datasets
append using META/META_l_1_25 META/META_l_2_25 META/META_l_3_25 META/META_l_4_25 META/META_l_5_25 META/META_l_6_25 META/META_l_7_25 META/META_l_8_25 META/META_l_9_25 META/META_l_10_25
drop rand*
gen ID = _n
save "META/META_25_m9.dta", replace

* Erase *
local met META_l_1_25 META_l_2_25 META_l_3_25 META_l_4_25 META_l_5_25 META_l_6_25 META_l_7_25 META_l_8_25 META_l_9_25 META_l_10_25
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 10: RR x OSV Total Time
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_25 != 1
	gen inter2 = lag_risk_ratio * OSV_total_time
	replace rand`n'_25 = . if rand`n'_25 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] OSV_t_beta = _b[OSV_total_time] OSV_t_se = _se[OSV_total_time] inter_OSV_t_beta = _b[inter2] inter_OSV_t_se = _se[inter2] size = e(N), by(rand`n'_25) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio OSV_total_time inter2 lag_OSV_total lag_duration $host $distances $mission lag_no_troops if rand`n'_25 == 1 & lag_OSV_total <= 200 & observe == 0, cluster(mission)  
}
clear 

* Append smaller datasets
append using META/META_l_1_25 META/META_l_2_25 META/META_l_3_25 META/META_l_4_25 META/META_l_5_25 META/META_l_6_25 META/META_l_7_25 META/META_l_8_25 META/META_l_9_25 META/META_l_10_25
drop rand*
gen ID = _n
save "META/META_25_m10.dta", replace

* Erase *
local met META_l_1_25 META_l_2_25 META_l_3_25 META_l_4_25 META_l_5_25 META_l_6_25 META_l_7_25 META_l_8_25 META_l_9_25 META_l_10_25
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 11: RR x OSV Rebs Time
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_25 != 1
	gen inter3 = lag_risk_ratio * OSV_Rebs_time
	replace rand`n'_25 = . if rand`n'_25 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] Rebs_t_beta = _b[OSV_Rebs_time] Rebs_t_se = _se[OSV_Rebs_time] inter_Rebs_t_beta = _b[inter3] inter_Rebs_t_se = _se[inter3] size = e(N), by(rand`n'_25) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio OSV_Rebs_time inter3 lag_OSV_Rebs lag_duration $host $distances $mission lag_no_troops if rand`n'_25 == 1 & lag_OSV_Rebs <= 200 & observe == 0, cluster(mission)  
}
clear 

* Append smaller datasets
append using META/META_l_1_25 META/META_l_2_25 META/META_l_3_25 META/META_l_4_25 META/META_l_5_25 META/META_l_6_25 META/META_l_7_25 META/META_l_8_25 META/META_l_9_25 META/META_l_10_25
drop rand*
gen ID = _n
save "META/META_25_m11.dta", replace

* Erase *
local met META_l_1_25 META_l_2_25 META_l_3_25 META_l_4_25 META_l_5_25 META_l_6_25 META_l_7_25 META_l_8_25 META_l_9_25 META_l_10_25
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 12: RR x OSV Gov Time
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_25 != 1
	gen inter4 = lag_risk_ratio * OSV_GOV_time
	replace rand`n'_25 = . if rand`n'_25 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] Gov_t_beta = _b[OSV_GOV_time] Gov_t_se = _se[OSV_GOV_time] inter_Gov_t_beta = _b[inter4] inter_Gov_t_se = _se[inter4] size = e(N), by(rand`n'_25) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio OSV_GOV_time inter4 lag_OSV_GOV lag_duration $host $distances $mission lag_no_troops if rand`n'_25 == 1 & lag_OSV_GOV <= 200 & observe == 0, cluster(mission)
}
clear 

* Append smaller datasets
append using META/META_l_1_25 META/META_l_2_25 META/META_l_3_25 META/META_l_4_25 META/META_l_5_25 META/META_l_6_25 META/META_l_7_25 META/META_l_8_25 META/META_l_9_25 META/META_l_10_25
drop rand*
gen ID = _n
save "META/META_25_m12.dta", replace

* Erase *
local met META_l_1_25 META_l_2_25 META_l_3_25 META_l_4_25 META_l_5_25 META_l_6_25 META_l_7_25 META_l_8_25 META_l_9_25 META_l_10_25
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 13: RR x FC Best
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_25 != 1
	gen inter1 = lag_risk_ratio * lag_duration
	replace rand`n'_25 = . if rand`n'_25 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] dur_beta = _b[lag_duration] dur_se = _se[lag_duration] inter_FC_beta = _b[inter1] inter_FC_se = _se[inter1] size = e(N), by(rand`n'_25) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio lag_duration inter1 lag_best best_time $host $distances $mission lag_no_troops if rand`n'_25 == 1 & lag_best <= 200 & observe == 0, cluster(mission)
}
clear 

* Append smaller datasets
append using META/META_l_1_25 META/META_l_2_25 META/META_l_3_25 META/META_l_4_25 META/META_l_5_25 META/META_l_6_25 META/META_l_7_25 META/META_l_8_25 META/META_l_9_25 META/META_l_10_25
drop rand*
gen ID = _n
save "META/META_25_m13.dta", replace

* Erase *
local met META_l_1_25 META_l_2_25 META_l_3_25 META_l_4_25 META_l_5_25 META_l_6_25 META_l_7_25 META_l_8_25 META_l_9_25 META_l_10_25
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 14: RR x FC OSV Total
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_25 != 1
	gen inter1 = lag_risk_ratio * lag_duration
	replace rand`n'_25 = . if rand`n'_25 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] dur_beta = _b[lag_duration] dur_se = _se[lag_duration] inter_FC_beta = _b[inter1] inter_FC_se = _se[inter1] size = e(N), by(rand`n'_25) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio lag_duration inter1 lag_OSV_total OSV_total_time $host $distances $mission lag_no_troops if rand`n'_25 == 1 & lag_OSV_total <= 200 & observe == 0, cluster(mission)  
}
clear 

* Append smaller datasets
append using META/META_l_1_25 META/META_l_2_25 META/META_l_3_25 META/META_l_4_25 META/META_l_5_25 META/META_l_6_25 META/META_l_7_25 META/META_l_8_25 META/META_l_9_25 META/META_l_10_25
drop rand*
gen ID = _n
save "META/META_25_m14.dta", replace

* Erase *
local met META_l_1_25 META_l_2_25 META_l_3_25 META_l_4_25 META_l_5_25 META_l_6_25 META_l_7_25 META_l_8_25 META_l_9_25 META_l_10_25
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 15: RR x FC OSV Rebs
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_25 != 1
	gen inter1 = lag_risk_ratio * lag_duration
	replace rand`n'_25 = . if rand`n'_25 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] dur_beta = _b[lag_duration] dur_se = _se[lag_duration] inter_FC_beta = _b[inter1] inter_FC_se = _se[inter1] size = e(N), by(rand`n'_25) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio lag_duration inter1 lag_OSV_Rebs OSV_Rebs_time $host $distances $mission lag_no_troops if rand`n'_25 == 1 & lag_OSV_Rebs <= 200 & observe == 0, cluster(mission)  
}
clear 

* Append smaller datasets
append using META/META_l_1_25 META/META_l_2_25 META/META_l_3_25 META/META_l_4_25 META/META_l_5_25 META/META_l_6_25 META/META_l_7_25 META/META_l_8_25 META/META_l_9_25 META/META_l_10_25
drop rand*
gen ID = _n
save "META/META_25_m15.dta", replace

* Erase *
local met META_l_1_25 META_l_2_25 META_l_3_25 META_l_4_25 META_l_5_25 META_l_6_25 META_l_7_25 META_l_8_25 META_l_9_25 META_l_10_25
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 16: RR x FC OSV Gov
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_25 != 1
	gen inter1 = lag_risk_ratio * lag_duration
	replace rand`n'_25 = . if rand`n'_25 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] dur_beta = _b[lag_duration] dur_se = _se[lag_duration] inter_FC_beta = _b[inter1] inter_FC_se = _se[inter1] size = e(N), by(rand`n'_25) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio lag_duration inter1 lag_OSV_GOV OSV_GOV_time $host $distances $mission lag_no_troops if rand`n'_25 == 1 & lag_OSV_GOV <= 200 & observe == 0, cluster(mission)  
}
clear 

* Append smaller datasets
append using META/META_l_1_25 META/META_l_2_25 META/META_l_3_25 META/META_l_4_25 META/META_l_5_25 META/META_l_6_25 META/META_l_7_25 META/META_l_8_25 META/META_l_9_25 META/META_l_10_25
drop rand*
gen ID = _n
save "META/META_25_m16.dta", replace

* Erase *
local met META_l_1_25 META_l_2_25 META_l_3_25 META_l_4_25 META_l_5_25 META_l_6_25 META_l_7_25 META_l_8_25 META_l_9_25 META_l_10_25
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


***************************
**** Meta Analysis, 50 ****
***************************

* Import dataset *
use Geo_PKO.dta, clear


* Model 1: Battle Deaths
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_50 != 1
	replace rand`n'_50 = . if rand`n'_50 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] best_beta = _b[lag_best] best_se = _se[lag_best] dur_beta = _b[lag_duration] dur_se = _se[lag_duration] size = e(N), by(rand`n'_50) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio lag_best best_time lag_duration $host $distances $mission lag_no_troops if rand`n'_50 == 1 & lag_best <= 200 & observe == 0, cluster(mission) diff
}
clear 

* Append smaller datasets
append using META/META_l_1_50 META/META_l_2_50 META/META_l_3_50 META/META_l_4_50 META/META_l_5_50 META/META_l_6_50 META/META_l_7_50 META/META_l_8_50 META/META_l_9_50 META/META_l_10_50
drop rand*
gen ID = _n
save "META/META_50_m1.dta", replace

* Erase *
local met META_l_1_50 META_l_2_50 META_l_3_50 META_l_4_50 META_l_5_50 META_l_6_50 META_l_7_50 META_l_8_50 META_l_9_50 META_l_10_50
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 2: Total OSV
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_50 != 1
	replace rand`n'_50 = . if rand`n'_50 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] OSV_t_beta = _b[lag_OSV_total] OSV_t_se = _se[lag_OSV_total] dur_beta = _b[lag_duration] dur_se = _se[lag_duration] size = e(N), by(rand`n'_50) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio lag_OSV_total OSV_total_time lag_duration $host $distances $mission lag_no_troops if rand`n'_50 == 1 == 1 & lag_OSV_total <= 200 & observe == 0, cluster(mission)
}
clear 

* Append smaller datasets
append using META/META_l_1_50 META/META_l_2_50 META/META_l_3_50 META/META_l_4_50 META/META_l_5_50 META/META_l_6_50 META/META_l_7_50 META/META_l_8_50 META/META_l_9_50 META/META_l_10_50
drop rand*
gen ID = _n
save "META/META_50_m2.dta", replace

* Erase *
local met META_l_1_50 META_l_2_50 META_l_3_50 META_l_4_50 META_l_5_50 META_l_6_50 META_l_7_50 META_l_8_50 META_l_9_50 META_l_10_50
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 3: Rebel OSV
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_50 != 1
	replace rand`n'_50 = . if rand`n'_50 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] OSV_r_beta = _b[lag_OSV_Rebs] OSV_r_se = _se[lag_OSV_Rebs] dur_beta = _b[lag_duration] dur_se = _se[lag_duration] size = e(N), by(rand`n'_50) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio lag_OSV_Rebs OSV_Rebs_time lag_duration $host $distances $mission lag_no_troops if rand`n'_50 == 1 == 1 & lag_OSV_Rebs <= 200 & observe == 0, cluster(mission) 
}
clear 

* Append smaller datasets
append using META/META_l_1_50 META/META_l_2_50 META/META_l_3_50 META/META_l_4_50 META/META_l_5_50 META/META_l_6_50 META/META_l_7_50 META/META_l_8_50 META/META_l_9_50 META/META_l_10_50
drop rand*
gen ID = _n
save "META/META_50_m3.dta", replace

* Erase *
local met META_l_1_50 META_l_2_50 META_l_3_50 META_l_4_50 META_l_5_50 META_l_6_50 META_l_7_50 META_l_8_50 META_l_9_50 META_l_10_50
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 4: Gov OSV
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_50 != 1
	replace rand`n'_50 = . if rand`n'_50 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] OSV_g_beta = _b[lag_OSV_GOV] OSV_g_se = _se[lag_OSV_GOV] dur_beta = _b[lag_duration] dur_se = _se[lag_duration] size = e(N), by(rand`n'_50) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio lag_OSV_GOV OSV_GOV_time lag_duration $host $distances $mission lag_no_troops if lag_OSV_GOV <= 200 & observe == 0, cluster(mission) iterate(1000) diff
}
clear 

* Append smaller datasets
append using META/META_l_1_50 META/META_l_2_50 META/META_l_3_50 META/META_l_4_50 META/META_l_5_50 META/META_l_6_50 META/META_l_7_50 META/META_l_8_50 META/META_l_9_50 META/META_l_10_50
drop rand*
gen ID = _n
save "META/META_50_m4.dta", replace

* Erase *
local met META_l_1_50 META_l_2_50 META_l_3_50 META_l_4_50 META_l_5_50 META_l_6_50 META_l_7_50 META_l_8_50 META_l_9_50 META_l_10_50
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 5: RR x Battle Deaths
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_50 != 1
	gen inter1 = lag_risk_ratio * lag_best
	replace rand`n'_50 = . if rand`n'_50 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] best_beta = _b[lag_best] best_se = _se[lag_best] inter_bd_beta = _b[inter1] inter_bd_se = _se[inter1] size = e(N), by(rand`n'_50) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio lag_best inter1 best_time lag_duration $host $distances $mission lag_no_troops if lag_best <= 200 & observe == 0, cluster(mission) iterate(1000) diff
}
clear 

* Append smaller datasets
append using META/META_l_1_50 META/META_l_2_50 META/META_l_3_50 META/META_l_4_50 META/META_l_5_50 META/META_l_6_50 META/META_l_7_50 META/META_l_8_50 META/META_l_9_50 META/META_l_10_50
drop rand*
gen ID = _n
save "META/META_50_m5.dta", replace

* Erase *
local met META_l_1_50 META_l_2_50 META_l_3_50 META_l_4_50 META_l_5_50 META_l_6_50 META_l_7_50 META_l_8_50 META_l_9_50 META_l_10_50
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 6: RR x OSV Total
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_50 != 1
	gen inter2 = lag_risk_ratio * lag_OSV_total
	replace rand`n'_50 = . if rand`n'_50 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] OSV_beta = _b[lag_OSV_total] OSV_se = _se[lag_OSV_total] inter_OSV_beta = _b[inter2] inter_OSV_se = _se[inter2] size = e(N), by(rand`n'_50) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio lag_OSV_total inter2 OSV_total_time lag_duration $host $distances $mission lag_no_troops if lag_OSV_total <= 200 & observe == 0, cluster(mission) iterate(1000) diff
}
clear 

* Append smaller datasets
append using META/META_l_1_50 META/META_l_2_50 META/META_l_3_50 META/META_l_4_50 META/META_l_5_50 META/META_l_6_50 META/META_l_7_50 META/META_l_8_50 META/META_l_9_50 META/META_l_10_50
drop rand*
gen ID = _n
save "META/META_50_m6.dta", replace

* Erase *
local met META_l_1_50 META_l_2_50 META_l_3_50 META_l_4_50 META_l_5_50 META_l_6_50 META_l_7_50 META_l_8_50 META_l_9_50 META_l_10_50
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear



* Model 7: RR x OSV Rebs
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_50 != 1
	gen inter3 = lag_risk_ratio * lag_OSV_Rebs
	replace rand`n'_50 = . if rand`n'_50 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] Rebs_beta = _b[lag_OSV_Rebs] Rebs_se = _se[lag_OSV_Rebs] inter_Rebs_beta = _b[inter3] inter_Rebs_se = _se[inter3] size = e(N), by(rand`n'_50) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio lag_OSV_Rebs inter3 OSV_Rebs_time lag_duration $host $distances $mission lag_no_troops if lag_OSV_Rebs <= 200 & observe == 0, cluster(mission) iterate(1000) diff
}
clear 

* Append smaller datasets
append using META/META_l_1_50 META/META_l_2_50 META/META_l_3_50 META/META_l_4_50 META/META_l_5_50 META/META_l_6_50 META/META_l_7_50 META/META_l_8_50 META/META_l_9_50 META/META_l_10_50
drop rand*
gen ID = _n
save "META/META_50_m7.dta", replace

* Erase *
local met META_l_1_50 META_l_2_50 META_l_3_50 META_l_4_50 META_l_5_50 META_l_6_50 META_l_7_50 META_l_8_50 META_l_9_50 META_l_10_50
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear



* Model 8: RR x OSV Gov
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_50 != 1
	gen inter4 = lag_risk_ratio * lag_OSV_GOV
	replace rand`n'_50 = . if rand`n'_50 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] Gov_beta = _b[lag_OSV_GOV] Gov_se = _se[lag_OSV_GOV] inter_Gov_beta = _b[inter4] inter_Gov_se = _se[inter4] size = e(N), by(rand`n'_50) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio lag_OSV_GOV inter4 OSV_GOV_time lag_duration $host $distances $mission lag_no_troops if lag_OSV_GOV <= 200 & observe == 0, cluster(mission) iterate(1000) diff
}
clear 

* Append smaller datasets
append using META/META_l_1_50 META/META_l_2_50 META/META_l_3_50 META/META_l_4_50 META/META_l_5_50 META/META_l_6_50 META/META_l_7_50 META/META_l_8_50 META/META_l_9_50 META/META_l_10_50
drop rand*
gen ID = _n
save "META/META_50_m8.dta", replace

* Erase *
local met META_l_1_50 META_l_2_50 META_l_3_50 META_l_4_50 META_l_5_50 META_l_6_50 META_l_7_50 META_l_8_50 META_l_9_50 META_l_10_50
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 9: RR x Best Time
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_50 != 1
	gen inter1 = lag_risk_ratio * best_time
	replace rand`n'_50 = . if rand`n'_50 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] best_t_beta = _b[best_time] best_t_se = _se[best_time] inter_best_t_beta = _b[inter1] inter_best_t_se = _se[inter1] size = e(N), by(rand`n'_50) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio best_time inter1 lag_best lag_duration $host $distances $mission lag_no_troops if lag_best <= 200 & observe == 0, cluster(mission) iterate(1000) diff
}
clear 

* Append smaller datasets
append using META/META_l_1_50 META/META_l_2_50 META/META_l_3_50 META/META_l_4_50 META/META_l_5_50 META/META_l_6_50 META/META_l_7_50 META/META_l_8_50 META/META_l_9_50 META/META_l_10_50
drop rand*
gen ID = _n
save "META/META_50_m9.dta", replace

* Erase *
local met META_l_1_50 META_l_2_50 META_l_3_50 META_l_4_50 META_l_5_50 META_l_6_50 META_l_7_50 META_l_8_50 META_l_9_50 META_l_10_50
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 10: RR x OSV Total Time
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_50 != 1
	gen inter2 = lag_risk_ratio * OSV_total_time
	replace rand`n'_50 = . if rand`n'_50 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] OSV_t_beta = _b[OSV_total_time] OSV_t_se = _se[OSV_total_time] inter_OSV_t_beta = _b[inter2] inter_OSV_t_se = _se[inter2] size = e(N), by(rand`n'_50) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio OSV_total_time inter2 lag_OSV_total lag_duration $host $distances $mission lag_no_troops if lag_OSV_total <= 200 & observe == 0, cluster(mission) iterate(1000) diff
}
clear 

* Append smaller datasets
append using META/META_l_1_50 META/META_l_2_50 META/META_l_3_50 META/META_l_4_50 META/META_l_5_50 META/META_l_6_50 META/META_l_7_50 META/META_l_8_50 META/META_l_9_50 META/META_l_10_50
drop rand*
gen ID = _n
save "META/META_50_m10.dta", replace

* Erase *
local met META_l_1_50 META_l_2_50 META_l_3_50 META_l_4_50 META_l_5_50 META_l_6_50 META_l_7_50 META_l_8_50 META_l_9_50 META_l_10_50
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 11: RR x OSV Rebs Time
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_50 != 1
	gen inter3 = lag_risk_ratio * OSV_Rebs_time
	replace rand`n'_50 = . if rand`n'_50 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] Rebs_t_beta = _b[OSV_Rebs_time] Rebs_t_se = _se[OSV_Rebs_time] inter_Rebs_t_beta = _b[inter3] inter_Rebs_t_se = _se[inter3] size = e(N), by(rand`n'_50) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio OSV_Rebs_time inter3 lag_OSV_Rebs lag_duration $host $distances $mission lag_no_troops if lag_OSV_Rebs <= 200 & observe == 0, cluster(mission) iterate(1000) diff 
}
clear 

* Append smaller datasets
append using META/META_l_1_50 META/META_l_2_50 META/META_l_3_50 META/META_l_4_50 META/META_l_5_50 META/META_l_6_50 META/META_l_7_50 META/META_l_8_50 META/META_l_9_50 META/META_l_10_50
drop rand*
gen ID = _n
save "META/META_50_m11.dta", replace

* Erase *
local met META_l_1_50 META_l_2_50 META_l_3_50 META_l_4_50 META_l_5_50 META_l_6_50 META_l_7_50 META_l_8_50 META_l_9_50 META_l_10_50
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 12: RR x OSV Gov Time
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_50 != 1
	gen inter4 = lag_risk_ratio * OSV_GOV_time
	replace rand`n'_50 = . if rand`n'_50 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] Gov_t_beta = _b[OSV_GOV_time] Gov_t_se = _se[OSV_GOV_time] inter_Gov_t_beta = _b[inter4] inter_Gov_t_se = _se[inter4] size = e(N), by(rand`n'_50) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio OSV_GOV_time inter4 lag_OSV_GOV lag_duration $host $distances $mission lag_no_troops if lag_OSV_GOV <= 200 & observe == 0, cluster(mission) iterate(1000) diff
}
clear 

* Append smaller datasets
append using META/META_l_1_50 META/META_l_2_50 META/META_l_3_50 META/META_l_4_50 META/META_l_5_50 META/META_l_6_50 META/META_l_7_50 META/META_l_8_50 META/META_l_9_50 META/META_l_10_50
drop rand*
gen ID = _n
save "META/META_50_m12.dta", replace

* Erase *
local met META_l_1_50 META_l_2_50 META_l_3_50 META_l_4_50 META_l_5_50 META_l_6_50 META_l_7_50 META_l_8_50 META_l_9_50 META_l_10_50
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 13: RR x FC Best
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_50 != 1
	gen inter1 = lag_risk_ratio * lag_duration
	replace rand`n'_50 = . if rand`n'_50 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] dur_beta = _b[lag_duration] dur_se = _se[lag_duration] inter_FC_beta = _b[inter1] inter_FC_se = _se[inter1] size = e(N), by(rand`n'_50) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio lag_duration inter1 lag_best best_time $host $distances $mission lag_no_troops if lag_best <= 200 & observe == 0, cluster(mission) iterate(1000) diff
}
clear 

* Append smaller datasets
append using META/META_l_1_50 META/META_l_2_50 META/META_l_3_50 META/META_l_4_50 META/META_l_5_50 META/META_l_6_50 META/META_l_7_50 META/META_l_8_50 META/META_l_9_50 META/META_l_10_50
drop rand*
gen ID = _n
save "META/META_50_m13.dta", replace

* Erase *
local met META_l_1_50 META_l_2_50 META_l_3_50 META_l_4_50 META_l_5_50 META_l_6_50 META_l_7_50 META_l_8_50 META_l_9_50 META_l_10_50
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 14: RR x FC OSV Total
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_50 != 1
	gen inter1 = lag_risk_ratio * lag_duration
	replace rand`n'_50 = . if rand`n'_50 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] dur_beta = _b[lag_duration] dur_se = _se[lag_duration] inter_FC_beta = _b[inter1] inter_FC_se = _se[inter1] size = e(N), by(rand`n'_50) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio lag_duration inter1 lag_OSV_total OSV_total_time $host $distances $mission lag_no_troops if lag_OSV_total <= 200 & observe == 0, cluster(mission) iterate(1000) diff
}
clear 

* Append smaller datasets
append using META/META_l_1_50 META/META_l_2_50 META/META_l_3_50 META/META_l_4_50 META/META_l_5_50 META/META_l_6_50 META/META_l_7_50 META/META_l_8_50 META/META_l_9_50 META/META_l_10_50
drop rand*
gen ID = _n
save "META/META_50_m14.dta", replace

* Erase *
local met META_l_1_50 META_l_2_50 META_l_3_50 META_l_4_50 META_l_5_50 META_l_6_50 META_l_7_50 META_l_8_50 META_l_9_50 META_l_10_50
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 15: RR x FC OSV Rebs
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_50 != 1
	gen inter1 = lag_risk_ratio * lag_duration
	replace rand`n'_50 = . if rand`n'_50 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] dur_beta = _b[lag_duration] dur_se = _se[lag_duration] inter_FC_beta = _b[inter1] inter_FC_se = _se[inter1] size = e(N), by(rand`n'_50) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio lag_duration inter1 lag_OSV_Rebs OSV_Rebs_time $host $distances $mission lag_no_troops if lag_OSV_Rebs <= 200 & observe == 0, cluster(mission) iterate(1000) diff 
}
clear 

* Append smaller datasets
append using META/META_l_1_50 META/META_l_2_50 META/META_l_3_50 META/META_l_4_50 META/META_l_5_50 META/META_l_6_50 META/META_l_7_50 META/META_l_8_50 META/META_l_9_50 META/META_l_10_50
drop rand*
gen ID = _n
save "META/META_50_m50.dta", replace

* Erase *
local met META_l_1_50 META_l_2_50 META_l_3_50 META_l_4_50 META_l_5_50 META_l_6_50 META_l_7_50 META_l_8_50 META_l_9_50 META_l_10_50
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


* Model 16: RR x FC OSV Gov
local num 1 2 3 4 5 6 7 8 9 10
foreach n of numlist `num' {
	use Geo_PKO.dta, clear
	drop if rand`n'_50 != 1
	gen inter1 = lag_risk_ratio * lag_duration
	replace rand`n'_50 = . if rand`n'_50 == 0
	statsby risk_beta = _b[lag_risk_ratio] risk_se = _se[lag_risk_ratio] dur_beta = _b[lag_duration] dur_se = _se[lag_duration] inter_FC_beta = _b[inter1] inter_FC_se = _se[inter1] size = e(N), by(rand`n'_50) saving(Data_Analysis/META/META_l_`n'_25, replace): nbreg no_troops lag_risk_ratio lag_duration inter1 lag_OSV_GOV OSV_GOV_time $host $distances $mission lag_no_troops if lag_OSV_GOV <= 200 & observe == 0, cluster(mission) iterate(1000) diff
}
clear 

* Append smaller datasets
append using META/META_l_1_50 META/META_l_2_50 META/META_l_3_50 META/META_l_4_50 META/META_l_5_50 META/META_l_6_50 META/META_l_7_50 META/META_l_8_50 META/META_l_9_50 META/META_l_10_50
drop rand*
gen ID = _n
save "META/META_50_m16.dta", replace

* Erase *
local met META_l_1_50 META_l_2_50 META_l_3_50 META_l_4_50 META_l_5_50 META_l_6_50 META_l_7_50 META_l_8_50 META_l_9_50 META_l_10_50
foreach x in `met' {
	erase /Users/treywood/Dropbox/Projects/Active_Projects/Risk_Movement/Data/Data_Analysis/META/`x'.dta
}


* Import dataset *
use Geo_PKO.dta, clear


****************************
**** Meta Analysis, 25% ****
****************************

*** Model 1 ***

* Import dataset *
use Data_Analysis/META/META_25_m1.dta, clear 


* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)
beep

* Best *
meta set best_beta best_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* FC Duration *
meta set dur_beta dur_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 2 ***

* Import dataset *
use Data_Analysis/META/META_25_m2.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Total OSV *
meta set OSV_t_beta OSV_t_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* FC Duration *
meta set dur_beta dur_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 3 ***

* Import dataset *
use Data_Analysis/META/META_25_m3.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Rebel OSV *
meta set OSV_r_beta OSV_r_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* FC Duration *
meta set dur_beta dur_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 4 ***

* Import dataset *
use Data_Analysis/META/META_25_m4.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Gov OSV *
meta set OSV_g_beta OSV_g_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* FC Duration *
meta set dur_beta dur_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 5 ***

* Import dataset *
use Data_Analysis/META/META_25_m5.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Best *
meta set best_beta best_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Interaction *
meta set inter_bd_beta inter_bd_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 6 ***

* Import dataset *
use Data_Analysis/META/META_25_m6.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* OSV Total  *
meta set OSV_beta OSV_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Interaction *
meta set inter_OSV_beta inter_OSV_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 7 ***

* Import dataset *
use Data_Analysis/META/META_25_m7.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* OSV Total  *
meta set Rebs_beta Rebs_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Interaction *
meta set inter_Rebs_beta inter_Rebs_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 8 ***

* Import dataset *
use Data_Analysis/META/META_25_m8.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Gov Total  *
meta set Gov_beta Gov_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Interaction *
meta set inter_Gov_beta inter_Gov_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 9 ***

* Import dataset *
use Data_Analysis/META/META_25_m9.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Gov Total  *
meta set best_t_beta best_t_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Interaction *
meta set inter_best_t_beta inter_best_t_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 10 ***

* Import dataset *
use Data_Analysis/META/META_25_m10.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Gov Total  *
meta set OSV_t_beta OSV_t_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Interaction *
meta set inter_OSV_t_beta inter_OSV_t_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 11 ***

* Import dataset *
use Data_Analysis/META/META_25_m11.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Gov Total  *
meta set Rebs_t_beta Rebs_t_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Interaction *
meta set inter_Rebs_t_beta inter_Rebs_t_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 12 ***

* Import dataset *
use Data_Analysis/META/META_25_m12.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Gov Total  *
meta set Gov_t_beta Gov_t_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Interaction *
meta set inter_Gov_t_beta inter_Gov_t_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 13 ***

* Import dataset *
use Data_Analysis/META/META_25_m13.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* FC Dur  *
meta set dur_beta dur_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Interaction *
meta set inter_FC_beta inter_FC_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 14 ***

* Import dataset *
use Data_Analysis/META/META_25_m14.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* FC Dur  *
meta set dur_beta dur_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Interaction *
meta set inter_FC_beta inter_FC_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 15 ***

* Import dataset *
use Data_Analysis/META/META_25_m15.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* FC Dur  *
meta set dur_beta dur_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Interaction *
meta set inter_FC_beta inter_FC_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 16 ***

* Import dataset *
use Data_Analysis/META/META_25_m16.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* FC Dur  *
meta set dur_beta dur_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Interaction *
meta set inter_FC_beta inter_FC_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)


****************************
**** Meta Analysis, 50% ****
****************************

*** Model 1 ***

* Import dataset *
use Data_Analysis/META/META_50_m1.dta, clear 


* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)
beep

* Best *
meta set best_beta best_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* FC Duration *
meta set dur_beta dur_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 2 ***

* Import dataset *
use Data_Analysis/META/META_50_m2.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Total OSV *
meta set OSV_t_beta OSV_t_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* FC Duration *
meta set dur_beta dur_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 3 ***

* Import dataset *
use Data_Analysis/META/META_50_m3.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Rebel OSV *
meta set OSV_r_beta OSV_r_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* FC Duration *
meta set dur_beta dur_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 4 ***

* Import dataset *
use Data_Analysis/META/META_50_m4.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Gov OSV *
meta set OSV_g_beta OSV_g_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* FC Duration *
meta set dur_beta dur_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 5 ***

* Import dataset *
use Data_Analysis/META/META_50_m5.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Best *
meta set best_beta best_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Interaction *
meta set inter_bd_beta inter_bd_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 6 ***

* Import dataset *
use Data_Analysis/META/META_50_m6.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* OSV Total  *
meta set OSV_beta OSV_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Interaction *
meta set inter_OSV_beta inter_OSV_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 7 ***

* Import dataset *
use Data_Analysis/META/META_50_m7.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* OSV Total  *
meta set Rebs_beta Rebs_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Interaction *
meta set inter_Rebs_beta inter_Rebs_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 8 ***

* Import dataset *
use Data_Analysis/META/META_50_m8.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Gov Total  *
meta set Gov_beta Gov_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Interaction *
meta set inter_Gov_beta inter_Gov_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 9 ***

* Import dataset *
use Data_Analysis/META/META_50_m9.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Gov Total  *
meta set best_t_beta best_t_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Interaction *
meta set inter_best_t_beta inter_best_t_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 10 ***

* Import dataset *
use Data_Analysis/META/META_50_m10.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Gov Total  *
meta set OSV_t_beta OSV_t_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Interaction *
meta set inter_OSV_t_beta inter_OSV_t_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 11 ***

* Import dataset *
use Data_Analysis/META/META_50_m11.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Gov Total  *
meta set Rebs_t_beta Rebs_t_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Interaction *
meta set inter_Rebs_t_beta inter_Rebs_t_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 12 ***

* Import dataset *
use Data_Analysis/META/META_50_m12.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Gov Total  *
meta set Gov_t_beta Gov_t_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Interaction *
meta set inter_Gov_t_beta inter_Gov_t_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 13 ***

* Import dataset *
use Data_Analysis/META/META_50_m13.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* FC Dur  *
meta set dur_beta dur_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Interaction *
meta set inter_FC_beta inter_FC_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 14 ***

* Import dataset *
use Data_Analysis/META/META_50_m14.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* FC Dur  *
meta set dur_beta dur_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Interaction *
meta set inter_FC_beta inter_FC_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 15 ***

* Import dataset *
use Data_Analysis/META/META_50_m15.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* FC Dur  *
meta set dur_beta dur_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Interaction *
meta set inter_FC_beta inter_FC_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)



*** Model 16 ***

* Import dataset *
use Data_Analysis/META/META_50_m16.dta, clear 

* RR *
meta set risk_beta risk_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* FC Dur  *
meta set dur_beta dur_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)

* Interaction *
meta set inter_FC_beta inter_FC_se, common studylabel(ID) studysize(size)

* Get overall effect
meta summarize, common(invvariance)
