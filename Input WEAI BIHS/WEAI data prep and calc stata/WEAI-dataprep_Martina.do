** BASED ON THE ONE PREPARED BY BY ANA VAZ AND SABINA ALKIRE AT WWW.OPHI.ORG.UK //
** FOR THE CALCULATION OF THE WOMEN'S EMPOWERMENT IN AGRICULTURE INDEX OF USAID.
** THERE ARE TWO FILES YOU NEED TO MAKE THE INDEX; THIS ONE (DATAPREP) AND WEAI CALCULATING. 
** LAST UPDATE BY WEAI TEAM: HAZEL MALAPIT / MARCH 2015
** LAST UPDATE BY AUTHORS: MARTINA OCCELLI / FEBRUARY 2022
** For any question, please contact mo386@cornell.edu

cd "D:\" // Change directory
capture log close
clear all
set more off
set mem 100m // Allocating Stata memory for the computation - recent version of Stata do not require this anymore

***  PREPARATION OF DATASET ( TO BE REPEATED FOR EVERY ROUND) ***  

use "WEAI input_Round3\prod_inc_loan_leis_domain_18M.dta", clear // Insert your data file

* Production and Income Domains: Modules WE2 and WE5 *

qui recode we2_02_* we2_03_* (98=.)	// code as missing: 98=decision not made
qui recode we2_01* (98=.) // code as missing: 98=missing

***we2.01***

foreach x in 1 2 31 32 33 4 5 6 {
	gen partact_`x'=(we2_01_`x'==1)
	replace partact_`x'=. if we2_01_`x'==.
	} 	
egen partact=rowtotal(partact_*), missing
label var partact "Number of activities in which individual participates"
egen partactagr=rowtotal(partact_1 partact_2 partact_31 partact_32 partact_33 partact_6), missing
label var partactagr "Number of agricultural activities in which individual participates"
	
***we2.02, we2.03***

*Adequate if respondent has at least some decisionmaking power
foreach x in 1 2 31 32 33 4 5 6  {
	gen inputdec_`x'=(we2_02_`x'>2) if partact_`x'==1
	replace inputdec_`x'=. if we2_02_`x'==. & partact_`x'==1
	}

label var inputdec_1 "Has some input in decisions regarding food crop farming"
label var inputdec_2 "Has some input in decisions regarding cash crop farming"
label var inputdec_31 "Has some input in decisions regarding large livestock raising"
label var inputdec_32 "Has some input in decisions regarding small livestock raising"
label var inputdec_33 "Has some input in decisions regarding poultry raising"
label var inputdec_4 "Has some input in decisions regarding non-farm activity"
label var inputdec_5 "Has some input in decisions regarding wage & salary employment"
label var inputdec_6 "Has some input in decisions regarding fishing"

foreach x in 1 2 31 32 33 4 5 6 {
	gen incomedec_`x'=(we2_03_`x'>2) if partact_`x'==1
	replace incomedec_`x'=. if we2_03_`x'==. & partact_`x'==1
	}

label var incomedec_1 "Has some input in decisions regarding income from food crop farming"
label var incomedec_2 "Has some input in decisions regarding income from cash crop farming"
label var incomedec_31 "Has some input in decisions regarding income from large livestock raising"
label var incomedec_32 "Has some input in decisions regarding income from small livestock raising"
label var incomedec_33 "Has some input in decisions regarding income from poultry raising"
label var incomedec_4 "Has some input in decisions regarding income from non-farm activity"
label var incomedec_5 "Has some input in decisions regarding income from wage & salary employment"
label var incomedec_6 "Has some input in decisions regarding income from fishing"

***we5.01, we5.02***

qui recode we5a_01* we5a_02* (98=.)
foreach x in a b c d e f g {
	gen skip_`x'=((we5a_01a_`x'==1) | (we5a_01b_`x'==1) | (we5a_01c_`x'==1) ) // up to 3 decisionmakers mentioned in Bangladesh module of WEAI 
	*Adequate if feel can make decisions to a medium extent (we5_02) 
	*or actually makes decision	(we5a_01)
	gen feelmakedec_`x'=(we5a_02_`x'>2)
	replace feelmakedec_`x'=1 if skip_`x'==1
	replace feelmakedec_`x'=. if skip_`x'!=1 & we5a_02_`x'==.
	replace feelmakedec_`x'=. if we5a_01a_`x'==. & we5a_01b_`x'==. & we5a_01c_`x'==. & we5a_02_`x'==.
	}
drop skip*	

label var feelmakedec_a "Feels can make decisions regarding getting inputs for agricultural production"
label var feelmakedec_b "Feels can make decisions regarding types of crops to grow"
label var feelmakedec_c "Feels can make decisions regarding taking crops to the market"
label var feelmakedec_d "Feels can make decisions regarding livestock raising"
label var feelmakedec_e "Feels can make decisions regarding own wage or salary employment"
label var feelmakedec_f "Feels can make decisions regarding major household expenditures"
label var feelmakedec_g "Feels can make decisions regarding minor household expenditures"

* Aggregation *
*INPUT IN PRODUCTIVE DECISIONS: adequate if there is AT LEAST TWO domains in which individual has some input in decisions, 
*or makes the decision, or feels he/she could make it if he/she wanted

egen feelinputdecagr_sum=rowtotal(feelmakedec_a-feelmakedec_d inputdec_1 inputdec_2 inputdec_31 inputdec_32 inputdec_33 inputdec_6), missing 
gen feelinputdecagr=(feelinputdecagr_sum>1)
replace feelinputdecagr=. if feelinputdecagr_sum==.
label var feelinputdecagr_sum "No. agr. domains individual has some input in decisions or feels can make decisions"
label var feelinputdecagr "Has some input in decisions or feels can make decisions in AT LEAST TWO domains"

*CONTROL OVER USE OF INCOME: adequate if there is AT LEAST ONE domain in which individual has some input in income decisions or feels she/he can make decisions regarding wage, employment and minor hh 
*expenditures; as long the only domain in which the individual feels that he/she makes decisions IS NOT minor household expenditures

egen incomedec_sum=rowtotal(incomedec_1 incomedec_2 incomedec_31 incomedec_32 incomedec_33 incomedec_4 incomedec_5 incomedec_6 feelmakedec_e feelmakedec_f feelmakedec_g), missing
gen incdec_count=(incomedec_sum>0)
replace incdec_count=0 if incdec_count==1 & incomedec_sum==1 & feelmakedec_g==1
replace incdec_count=. if incomedec_sum==.
label var incomedec_sum "No. domains individual has some input in income decisions or feels can make decisions"
label var incdec_count "Has some input in income dec or feels can make dec AND not only minor hh expend"

*drop partact_* inputdec_1-incomedec_6 feelmakedec_a-feelmakedec_m // If you are not interested in these variable you can drop them	

***we5ba - we5bd *** 

qui recode we5ba* (98=.)

gen rai_a_04 = 1 if we5ba_04_1 == 1

qui recode we5bb* (98=.)

gen rai_b_04 = 1 if we5bb_04_1 == 1

qui recode we5bc* (98=.)

gen rai_c_04 = 1 if we5bc_04_1 == 1

label var rai_a_04 "RAI in types of crops to grow and sale in market" 
label var rai_b_04 "RAI in taking crops to the market"
label var rai_c_04 "RAI above 1 in livestock raising"

*AGGREGATION 

** AUTONOMY IN PRODUCTION: adequate if RAI>1 in AT LEAST ONE domain/activity linked to production
egen raiprod_any=rowmax(rai_a_04 rai_b_04 rai_c_04)
replace raiprod_any=0 if raiprod_any==. & partactagr==0
label var raiprod_any "Has positive RAI in at least one production activity"

summarize raiprod_any

***we3.07***
foreach x in a b c d e {
	gen creditaccess_`x'=(we3d_17`x'>=1 & we3d_17`x'<=3)
	replace creditaccess_`x'=. if we3d_17`x'==. | we3d_17`x'==97
	}
egen creditaccess=rowtotal(creditaccess_*), missing
label var creditaccess "No. of credit sources that the hh uses”

***we3.08, we3.09***
foreach y in a b c d e {
	*Self or joint decide to borrow
	gen creditselfjointborrow_`y'=(we3d_18a_`y'==1 | we3d_18b_`y'==1 | we3d_18c_`y'==1) if creditaccess_`y'==1
	replace creditselfjointborrow_`y'=. if we3d_18a_`y'==. & we3d_18b_`y'==. & we3d_18c_`y'==. & creditaccess_`y'==1
	
	*Self or joint decide how to use
	gen creditselfjointuse_`y'=(we3d_19a_`y'==1 | we3d_19b_`y'==1 | we3d_19c_`y'==1) if creditaccess_`y'==1
	replace creditselfjointuse_`y'=. if we3d_19a_`y'==. & we3d_19b_`y'==. & we3d_19c_`y'==. & creditaccess_`y'==1
	
	*Self or joint makes AT LEAST ONE decision regarding credit
	egen creditselfjointanydec_`y'=rowmax(creditselfjointborrow_`y' creditselfjointuse_`y')

	}

foreach x in borrow use {
	label var creditselfjoint`x'_a "Jointly made decision about `x' credit from NGO"
	label var creditselfjoint`x'_b " Jointly made decision about `x' credit from informal lender"
	label var creditselfjoint`x'_c " Jointly made decision about `x' credit from formal lender"
	label var creditselfjoint`x'_d " Jointly made decision about `x' credit from friends & relatives"
	label var creditselfjoint`x'_e " Jointly made decision about `x' credit from other group-based"
	}

label var creditselfjointanydec_a "Jointly made AT LEAST ONE decision regarding credit from NGO"
label var creditselfjointanydec_b "Jointly made AT LEAST ONE decision regarding credit from informal lender"
label var creditselfjointanydec_c "Jointly made AT LEAST ONE decision regarding credit from formal lender"
label var creditselfjointanydec_d "Jointly made AT LEAST ONE decision regarding credit from friends & relatives"
label var creditselfjointanydec_e "Jointly made AT LEAST ONE decision regarding credit from other group-based"

*AGGREGATION
*ACCESS TO AND DECISIONS ON CREDIT: Adequate if self/selfjoint makes dec regarding AT LEAST ONE source of credit AND has at least one source of credit
foreach x in anydec {
	egen creditselfjoint`x'any=rowmax(creditselfjoint`x'_*)
	replace creditselfjoint`x'any=0 if creditaccess==0
	rename creditselfjoint`x'any credj`x'_any 
	}

label var credjanydec_any "Jointly makes AT LEAST ONE decision regarding AT LEAST ONE source of credit"

* Leadership Domain: Module WE4

qui recode we4* (97 98=.) 


***we4.01 - we4.03***
*empowered if comfortable speaking in public
foreach x of num 1/3 {
	gen speakpublic_`x'=(we4_0`x'>1)
	replace speakpublic_`x'=. if we4_0`x'==.
	}

*AGGREGATION
*SPEAK IN PUBLIC: Adequate if comfortable speaking in public in AT LEAST ONE context
egen speakpublic_any=rowmax(speakpublic_1 speakpublic_2 speakpublic_3)

***we4.07***
foreach x in a b c d e f g h i j {
	gen groupmember_`x'=(we4_08`x'==1)
	replace groupmember_`x'=. if we4_08`x'==.
	gen nogroup_`x'=(we4_07`x'==2 | we4_07`x'==.)
	}

*AGGREGATION
*GROUP MEMBERSHIP: Adequate if individual is part of AT LEAST ONE group
egen groupmember_any=rowmax(groupmember_*)
replace groupmember_any=0 if groupmember_any==. /*Inadequate if no groups in community*/

* Time Domain: Module WE6

***we6b.12***
*LEISURE TIME: Adequate if does not express any level of dissatisfaction with the amount of leisure time available
gen leisuretime=(we6b_12>4)
replace leisuretime=. if we6b_12==.

rename wa06 hh_type
rename wa05 sex
rename a01 hhid
rename wa10 enum_sex
rename wa12 int_alone


save "weai_main_component_18M.dta" // save file, changing suffix according to gender and year

keep feelinputdecagr raiprod_any credjanydec_any incdec_count groupmember_any speakpublic_any leisuretime hhid sex hh_type enum_sex int_alone

save "input_rai_credit_group_leisure_18M.dta" // save file, changing suffix according to gender and year

* POVERTY MEASURE * 

***we6a***

** Create time poverty measure ***
// Open dataset with time use information //

use "D:\WEAI_input Round 2\time_hour_15M.dta", clear // IMPORTANT: Change to your time data file

rename a01 hhid 

*Define work (w/ commuting/travelling) 
qui gen w=(wact=="E" | wact=="F" | wact=="G" | wact=="J" | wact=="K" | ///
		   wact=="L" | wact=="M" | wact=="N" | wact=="P")
drop if w==0

*Calculate total time spent working as primary and secondary activity
egen w_hour = rowtotal(we6a01_*)

***Define poverty lines

*10 hr/day = 40 X 15 minutes
qui gen z10=40
*10.5 hr/day = 42 X 15 minutes
qui gen z105=42
*11 hr/day = 44 X 15 minutes
qui gen z11=44

keep hhid wact w_hour z10 z105 z11

reshape wide w_hour z10 z105 z11, i(hhid) j(wact) string // to sum for rows in each working activity

foreach x in E F G J K L M N P {
	egen tot_hour=rowtotal(w_hour*)
	}

gen poor_z1 = 1 if tot_hour >= z10E
replace poor_z1 = 0 if tot_hour < z10E
gen poor_z2 = 1 if tot_hour >= z105E
replace poor_z2 = 0 if tot_hour < z105E
gen poor_z3 = 1 if tot_hour >= z11E
replace poor_z3 = 0 if tot_hour < z11E

tabulate poor_z3

save "time_poverty_15M.dta"

keep hhid tot_hour poor_z1 poor_z2 poor_z3

save "input_time_15M.dta"

log close

