** BASED ON THE ONE PREPARED BY BY ANA VAZ AND SABINA ALKIRE AT WWW.OPHI.ORG.UK //
** FOR THE CALCULATION OF THE WOMEN'S EMPOWERMENT IN AGRICULTURE INDEX OF USAID.
** THERE ARE TWO FILES YOU NEED TO MAKE THE INDEX; THIS ONE (DATAPREP) AND WEAI CALCULATING. 
** LAST UPDATE BY WEAI TEAM: HAZEL MALAPIT / MARCH 2015
** LAST UPDATE BY AUTHORS: MARTINA OCCELLI / FEBRUARY 2022
** For any question, please contact mo386@cornell.edu
 

cd "D:\" // Change directory
clear all
set more off
cap log close


*** OPEN DATA FILE  
**************************************************

use "modifieddata\all_indicators_1.1.dta", clear //


**************************************************
*******   FIVE DOMAINS EMPOWERMENT (5DE)   *******
**************************************************

// So far all_indicators were defined so 1 identifies adequate. //
// Now we transform indicators so 1 identifies inadequate. //

foreach var in  feelinputdecagr raiprod_any jown_count jrightanyagr credjanydec_any incdec_count groupmember_any speakpublic_any npoor_z105 leisuretime {
	rename `var' `var'_ndepr
	gen `var'=1 if `var'_ndepr==0
	replace `var'=0 if `var'_ndepr==1
	}

*We are now starting with 0-1 variables where 1 means that the person is inadequate in that indicator. 

gen weight=1 // Note: =1 if unweighted; otherwise, assign variable containing individual sampling weights

save "modifieddata\all_depr_indicators_1.1.dta", replace

// CONSTRUCTING A LOOP FOR EACH COUNTRY. //

*forvalues c=1(1)2 { //NOTE: add * at beginning of this line for single-country calculation

*preserve //NOTE: add * at beginning of this line for single-country calculation

*keep if country==`c' //NOTE: add * at beginning of this line for single-country calculation
	

	
*****************************************************************************
********  Create a local variable with all your indicators varlist_emp ******
*****************************************************************************

#delimit;
local varlist_emp feelinputdecagr raiprod_any jown_count jrightanyagr credjanydec_any incdec_count groupmember_any speakpublic_any npoor_z105 leisuretime;

gen sample1=(feelinputdecagr~=. & raiprod_any~=. & jown_count~=. & jrightanyagr~=.& credjanydec_any~=. & incdec_count~=. & groupmember_any~=. & speakpublic_any~=. & npoor_z105~=. & leisuretime~=.);
#delimit cr

*****************************************************************************
**** Define the weights. Weights sum to 1 (not to the number of indicators)**
*****************************************************************************
** Create a loop for the variables with the same weight *********************
*****************************************************************************

*We now create the indicators’ weights.*

foreach var in feelinputdecagr raiprod_any{
	gen w_`var'=1/10
	}
foreach var in jown_count jrightanyagr credjanydec_any {
	gen w_`var'=1/15
	}
foreach var in incdec_count {
	gen w_`var'=1/5
	}
foreach var in groupmember_any speakpublic_any {
	gen w_`var'=1/10
	}
foreach var in npoor_z105 leisuretime{
	gen w_`var'=1/10
	}

	
*******************************************************************
*********     Define the weighted inadequacy g0* matrix       ****
*******************************************************************

// WE FOCUSED ON THE MEASURE OF INADEQUACIES (DISEMPOWERMENT). //

foreach var in `varlist_emp'{
	gen wg0_`var'= `var'*w_`var'
}

******************************************************************************
*********** Compute the frequency of missing values for indicator ************
******************************************************************************

foreach var in `varlist_emp' {
gen `var'_miss=1 if `var'==.
replace `var'_miss=0 if `var'!=.
}

sum *_miss

********************************************************************************
*************   Define the (weighted) inadequacy count vector "ci" ************
********************************************************************************

egen ci=rsum(wg0_*)

label variable ci "Inadequacy Count"

egen n_missing=rowmiss(wg0_*)
label variable n_missing "Number of missing variables by individual"
gen missing=(n_missing>0)
label variable missing "Individual with missing variables"

*** Check sample drop due to missing values
tab missing
*drop if missing

***********************************************************************
***** Create the identification vector (inadequate/adequate) ***********
***** and compute individual average of inadequacy    *****************
***********************************************************************

egen total_w=total(weight) if missing==0

// FIRST, WE COMPUTED THE DISEMPOWERMENT IN AGRICULTURE INDEX (DAI). //
// AFTERWARDS, WE COMPUTE THE EMPOWERMENT IN AGRICULTURE INDEX (HERE CALLED EAI): EAI = 1 - DAI. //

*These are now percentages - this creates DAI by each percentage. 

forvalues x=1(1)100 { // FOR EACH POSSIBLE CUTOFF X BETWEEN 1% AND 100% //
gen ch_`x'p=(ci>float(`x'/100))	// WE CREATE A VARIABLE THAT IDENTIFIES THE DISEMPOWERED INDIVIDUALS (THOSE WHO HAVE AN INADEQUACY SCORE HIGHER THE X%). //
replace ch_`x'p=. if missing==1
gen a_`x'p=(ci) if ch_`x'p==1 // WE COMPUTE THE INDIVIDUAL INADEQUACY OF THOSE WHO ARE DISEMPOWERED. //
replace a_`x'p=. if missing==1
egen DAI_`x'p= total(ci*ch_`x'p*weight/total_w) // WE COMPUTE THE DISEMPOWERMENT INDEX (FOR EACH POSSIBLE CUTOFF X) //
gen EAI_`x'p=1-DAI_`x'p // THEN, WE OBTAIN THE EMPOWERMENT INDEX. //
label var ch_`x'p "Condition of disempowerment  k=`x'%"
label var a_`x'p "Individual Average inadequacy  k=`x'"
label var DAI_`x'p "National Disempowerment Index k=`x'%"
label var EAI_`x'p "Combined Empowerment Index k=`x'%"

}

// PLEASE NOTE THAT THESE ARE NOT YET THE 5DE. SO FAR WE ARE STILL LOOKING AT WOMEN AND MEN TOGETHER AND WE HAVE NOT YET DEFINED THE CUTOFF WE WANT TO USE. //

summarize ch_* a_* DAI_* EAI_* [aw=weight]

************************************************************************
******* Compute raw headcounts        **********************************
************************************************************************

foreach var in `varlist_emp' {
gen `var'_raw=(`var')
replace `var'_raw=. if missing==1
}

su *_raw  [iw=weight]

***********************************************************************************
*********** Compute Censored headcount by subgroups (gender or region etc)   ******
***********************************************************************************

// NOW WE DEFINE THE CUTOFF THAT WE WANT TO USE AND WE START LOOKING AT WOMEN AND MEN SEPARATELY //

* Please define in the first line your cutoff, the example shows k=20 is 20% of the variables
* In the second line replace with the name of the categorical variable (the variable name by which censored headcount is to be generated for the variables)
* that represents the different subgroups. 
* The subgroup variable must be coded in consecutive natural numbers starting in 1

pause
gen nation=`c' //NOTE: add * at beginning of this line for single-country calculation

local k=20
*decode sex, gen(n)
*encode n, gen (gender)
gen gender=sex

local r="gender"

foreach var in `varlist_emp' {
gen `var'_CH_`k'p=(`var'==1 & ch_`k'==1)
replace `var'_CH_`k'p=. if missing==1
}

summarize *_CH_`k'p [iw=weight]

*****************************************************************************************
*****************************************************************************************
**** Define decomposition rule (country, sex)
**** We keep the information of the weighted population before reducing the sample to only 
**** those cases with information in all the indicators considered

egen total_b = total(weight)
label var total_b "Total Population Before Sample Drop"
egen pop_shr_before = total(weight/total_b), by(`r')
label var pop_shr_before "Weighted Population Share of Each `r' before Sample Reduction"
gen temp=1 // We generate this variable for counting observations
egen sample_r_before = total(temp), by(`r')
label var sample_r_before "Sample Size of each `r' before Sample Reduction"

egen pop_shr_after = total(weight/total_w) if miss==0, by(`r')
label var pop_shr_after "Weighted Population Share of Each `r' after Sample Reduction"
egen sample_r_after = total(temp) if missing==0, by(`r')
label var sample_r_after  "Sample Size of Each `r' after Sample Reduction"
gen sample_lost_ratio= sample_r_after/sample_r_before
label var sample_lost_ratio  "Relative size of the final sample after reduction in each `r'"



************************************************************************************
**** Collapsing ********************************************************************
* So far, our database has individual level data, if we want to aggregate
* at any level, we use the command “collapse”. Collapse calculates weighted 
* averages at the  level defined by the user (gender), if the option "by(gender)"
* is not specified, the observations are aggregated at the national level.
* Before collapse, save your results using the following command
*************************************************************************************

save "modifieddata\individual_indices_`c'_1.1.dta", replace  // SAVES, FOR EACH COUNTRY, A DATASET WITH INDIVIDUAL DATA. //
// THIS DATASET INCLUDES INDIVIDUAL INADEQUACY COUNT, VARIABLES THAT IDENTIFY DISEMPOWERED FOR EACH CUTOFF AND VALUE OF DAI AND EAI FOR EACH CUTOFF. //
// PLEASE REMEMBER THAT DAI AND EAI WERE COMPUTED CONSIDERING WOMEN AND MEN TOGETHER. //
 
* You can use also the commands preserve before the command “collapse” and restore just after
* preserve

// NOW WE COMPUTE RELEVANT VARIABLES BY GENDER. //

egen pop_shr = total(weight/total_w) if miss==0, by(`r')

* collapse
* The following command will "collapse" our individual results according to the subgroup previously defined. 
//pause
collapse nation ch_* a_* *_CH_`k'p *_raw w_* EAI_* *_miss missing DAI_* pop_shr* sample_r_* sample_lost_ratio [aw=weight],by(`r')

* You have already calculated the national DAI. With the following lines you will calculate the 
* DAI for every region using the formulation M0=H*A obtained after collapsing the dataset.

// ATTENTION: DAI AND EAI REFER TO NATIONAL FIGURES. M0 AND EA REFER TO GENDER FIGURES. //

forvalues x=1(1)100 {
gen M0_`x'p=ch_`x'p*a_`x'p
label var M0_`x'p "Population Subgroup DAI k=`x'%"
gen EA_`x'p=1-M0_`x'p
label var EA_`x'p "Population Subgroup EAI k=`x'%"
ren ch_`x'p H_`x'p
label var H_`x'p "Population Subgroup Multidimensional Headcount Ratio k=`x'%"
ren a_`x'p A_`x'p
label var A_`x'p "Population Subgroup Average Inadequacy k=`x'%"
label var DAI_`x'p "National DAI k=`x'%"
}

foreach var in `varlist_emp' {
gen `var'_cont_`k'_EAI=((`var'_CH_`k'p* w_`var')/ EA_`k'p)
label var `var'_cont_`k'_EAI "Decomposed Contribution of `var' to the total Empowerment k=`k'"

gen `var'_cont_`k'_DAI=((`var'_CH_`k'p* w_`var')/ M0_`k'p)
label var `var'_cont_`k'_DAI "Decomposed Contribution of `var' to the total Disempowerment k=`k'"

label var  `var'_CH_`k'p  "Decomposed Censored Headcount `var' k=`k'"
label var  `var'_raw  "Decomposed Raw Headcount `var'"
label var  `var'_miss  "Decomposed Missing values `var'"
}

label variable pop_shr "Population Share"
gen cont_group_`k'=M0_`k'p/DAI_`k'p*pop_shr
label variable cont_group_`k' "Decomposed Contribution"

gen cont_subgroup_DAI_`k'=M0_`k'p/DAI_`k'p*pop_shr_after
label variable cont_subgroup_DAI_`k' "Population Subgroup Contribution to DAI"

gen cont_subgroup_EAI_`k'=EA_`k'p/EAI_`k'p*pop_shr_after
label variable cont_subgroup_EAI_`k' "Population Subgroup Contribution to EAI"

capture decode `r', gen(level)
drop `r'

gen gender=_n
label define gender_lab 1 "Male" 2 "Female"
label values gender gender_lab

save "modifieddata\results_`c'_`r'_1.1.dta", replace
// FOR EACH COUNTRY, SAVES A DATASET WITH THE RELEVANT EMPOWERMENT FIGURES FOR EACH GENDER. //
// THE DATASETS INCLUDE THE DISEMPOWERMENT FIGURES FOR ALL CUTOFFS BETWEEN 1% AND 100%. WHEN EXTRACTING THE INFO WE FOCUS ON THE RELEVANT CUTOFF. //
// PLEASE SEE BELOW HOW TO EXTRACT RELEVANT INFORMATION FOR CUTOFF 20%. //

//collapse *_cont [iw=weight],by(`r')

restore  //NOTE: add stars for single-country calculation
}  		//NOTE: add stars for single-country calculation

clear

*exit
/*
*** EXTRACT TABLES

// HOW TO EXTRACT RELEVANT INFO. EXAMPLE FOR COUNTRY 1 WITH CUTOFF 20% //

use "H:\OPHI\WEI\Do-files\results_1_gender.dta", clear // Example for country = 1 //

browse H_20p A_20p M0_20p EA_20p if gender==2 // DISEMPOWERED HEADCOUNT (H_20p), AVERAGE INADEQUACY SHARE (A_20p), 5 DOMAINS DISEMPOWERMENT INDEX (M0_20p) AND 5 DOMAINS EMPOWERMENT INDEX (EA_20P) FOR THE SAMPLE OF WOMEN. //
browse H_20p A_20p M0_20p EA_20p if gender==1

browse *_CH_20p if gender==2  // INDICATORS CENSORED HEADCOUNTS FOR WOMEN. //
browse *cont_20_DAI if gender==2  // INDICATORS CONTRIBUTION TO DISEMPOWERMENT FOR WOMEN. //
browse *_CH_20p if gender==1
browse *cont_20_DAI if gender==1

*/
*********************************************
*******   GENDER PARITY INDEX (GPI)   *******
*********************************************

use "modifieddata\all_depr_indicators_1.1.dta", clear

** Focus on male and female households

sort hhid sex
bys hhid: gen i=_n
bys hhid: egen n=max(i)

tab hh_type n, miss
drop if n==1

*****************************************************************************
********  Create a local variable with all your indicators varlist_emp ******
*****************************************************************************

#delimit;
local varlist_5do feelinputdecagr raiprod_any jown_count jrightanyagr credjanydec_any incdec_count groupmember_any speakpublic_any npoor_z105 leisuretime;

gen sample5do=(feelinputdecagr~=. & raiprod_any~=. & jown_count~=. & jrightanyagr~=.& credjanydec_any~=. & incdec_count~=. & groupmember_any~=. & speakpublic_any~=. & npoor_z105~=. & leisuretime~=.);
#delimit cr

******************************
**** Define the weights.  ****
******************************

foreach var in feelinputdecagr raiprod_any{
	gen w_`var'=1/10
	}
foreach var in jown_count jrightanyagr credjanydec_any {
	gen w_`var'=1/15
	}
foreach var in incdec_count {
	gen w_`var'=1/5
	}
foreach var in groupmember_any speakpublic_any {
	gen w_`var'=1/10
	}
foreach var in npoor_z105 leisuretime{
	gen w_`var'=1/10
	}

	
**********************************************************
*********     Define the weigted inadequacy g0*      ****
**********************************************************

foreach var in `varlist_5do'{
	gen wg0_`var'= `var'*w_`var'
	}

********************************************************************************
*************   Define the (weighted) inadequacy count vector "ci" ************
********************************************************************************

egen ci=rsum(wg0_*)
replace ci = . if sample5do==0

label variable ci "Inadequacy Count without Parity"

********************************************
*** Compute censored inadequacy scores  ***
********************************************

bys hhid: gen w_ci_id=ci if sex==2 
bys hhid: gen m_ci_id=ci if sex==1 
bys hhid: egen W_ci=max(w_ci_id)
bys hhid: egen M_ci=max(m_ci_id)
drop w_ci_id m_ci_id

bys hhid: gen float W_cen_ci=W_ci
bys hhid:replace W_cen_ci=0.20 if W_cen_ci<=0.2 & W_cen_ci!=.
bys hhid: gen M_cen_ci=M_ci
bys hhid:replace M_cen_ci=0.20 if M_cen_ci<=0.20 & M_cen_ci!=.

******************************************************
*** Identify inadequate in terms of gender parity  ***
******************************************************

bys hhid: gen ci_above=(W_cen_ci>M_cen_ci) 
bys hhid: replace ci_above=. if W_cen_ci==.|M_cen_ci==.
label var ci_above "Equals 1 if individual lives in MF hh where the depr score of the woman is higher than the man - EI 1"

bys country: sum ci_above
bys country: sum ci_above [aw=weight]

************************************
*** Compute Gender Parity Index  ***
************************************

** Full sample

gen female=(sex==2 & ci_above!=.)
bys country: egen women_n=total(female)
bys country: egen women_wt=total(female*weight)
drop female

* Verification
bys country: gen women_i=(sex==2 & M_cen_ci!=. & W_cen_ci!=.)
bys country: egen women_wt2=total(women_i*weight)
bys country: tab women_wt women_wt2, miss
drop women_i women_wt2

** Headcount ratio of inadequate women

gen inadequate=(ci_above==1 & sex==2)
bys country: egen float inadequate_n = total(inadequate)
gen H=inadequate_n/women_n // Considering unweighted sample //
bys country: egen float inadequate_wt = total(inadequate*weight) 
gen H_wt=inadequate_wt/women_wt // Considering weighted sample //


*Verification
bys country: gen inadequate_i=(M_cen_ci<W_cen_ci & sex==2 & M_cen_ci!=. & W_cen_ci!=.)
bys country: egen inadequate_wt2=total(inadequate_i*weight)
bys country: tab inadequate_wt inadequate_wt2, miss
drop inadequate_i inadequate_wt2


** Computation of normalized gap

qui gen ci_gap=(W_cen_ci-M_cen_ci)/(1-M_cen_ci) if ci_above==1 & sex==2 


bys country: egen float ci_gap_sum = total(ci_gap*weight)
bys country: gen ci_average=ci_gap_sum/inadequate_wt


** Computation of GPI

bys country: gen H_GPI=inadequate_wt/women_wt
bys country: gen P1=H_GPI*ci_average
bys country: gen GPI=1-P1

**************************
*** Summarize results  ***
**************************

bys country: sum H_GPI ci_average P1 GPI 
bys country: count if sex==2
bys country: tab women_n women_wt

save "modifieddata\results_GPI_1.1.dta", replace

log close
