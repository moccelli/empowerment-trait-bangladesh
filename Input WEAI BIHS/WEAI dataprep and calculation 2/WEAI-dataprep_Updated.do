** DO FILE HAS BEEN PREPARED BY ANA VAZ AND SABINA ALKIRE AT WWW.OPHI.ORG.UK //
** FOR THE CALCULATION OF THE WOMEN'S EMPOWERMENT IN AGRICULTURE INDEX OF USAID.
** THERE ARE TWO FILES YOU NEED TO MAKE THE INDEX; THIS ONE (DATAPREP) AND WEAI CALCULATING. 
** LAST UPDATE: HAZEL MALAPIT / MARCH 2015

cd "D:\Users\\`c(username)'\Dropbox\WEAI_Pilot\Public Release" // IMPORTANT: Change directory
capture log close
clear all
set more off
log using "logs\WEAI_dataprep_1.1.txt", text replace 
set mem 100m 

*******************************
***  PREPARATION OF DATASET ***  
*******************************

use "clean data\ind_all_1.1_clean.dta", clear // IMPORTANT: Change to your .dta file
renvars, subst(_p1) // harmonize varnames across pilot types

********************************************************
*** Production and Income Domains: Modules G2 and G5 *** 
********************************************************

qui recode g2_02_* g2_03_* (98=.)	// code as missing: 98=decision not made
qui recode g2_01* (98=.) // code as missing: 98=missing

***g2.01***

foreach x in a b c d e f {
	gen partact_`x'=(g2_01_`x'==1)
	replace partact_`x'=. if g2_01_`x'==.
	} 	
egen partact=rowtotal(partact_*), missing
label var partact "Number of activities in which individual participates"
egen partactagr=rowtotal(partact_a partact_b partact_c partact_f), missing
label var partactagr "Number of agricultural activities in which individual participates"
	
***g2.02, g2.03***

*Adequate if respondent has at least some decisionmaking power
foreach x in a b c d e f {
	gen inputdec_`x'=(g2_02_`x'>2) if partact_`x'==1
	replace inputdec_`x'=. if g2_02_`x'==. & partact_`x'==1
	}

label var inputdec_a "Has some input in decisions regarding food crop farming"
label var inputdec_b "Has some input in decisions regarding cash crop farming"
label var inputdec_c "Has some input in decisions regarding livestock raising"
label var inputdec_d "Has some input in decisions regarding non-farm activity"
label var inputdec_e "Has some input in decisions regarding wage & salary employment"
label var inputdec_f "Has some input in decisions regarding fishing"

foreach x in a b c d e f {
	gen incomedec_`x'=(g2_03_`x'>2) if partact_`x'==1
	replace incomedec_`x'=. if g2_03_`x'==. & partact_`x'==1
	}

label var incomedec_a "Has some input in decisions regarding income from food crop farming"
label var incomedec_b "Has some input in decisions regarding income from cash crop farming"
label var incomedec_c "Has some input in decisions regarding income from livestock raising"
label var incomedec_d "Has some input in decisions regarding income from non-farm activity"
label var incomedec_e "Has some input in decisions regarding income from wage & salary employment"
label var incomedec_f "Has some input in decisions regarding income from fishing"

***g5.01, g5.02***

qui recode g5a_01* g5a_02* (98=.)
foreach x in a b c d e f g {
	gen skip_`x'=((g5a_01a_`x'==1) | (g5a_01b_`x'==1) | (g5a_01c_`x'==1) ) /*up to 3 decisionmakers mentioned */ 
	*Adequate if feel can make decisions to a medium extent (g5_02) 
	*or actually makes decision	(g5a_01)
	gen feelmakedec_`x'=(g5a_02_`x'>2)
	replace feelmakedec_`x'=1 if skip_`x'==1
	replace feelmakedec_`x'=. if skip_`x'!=1 & g5a_02_`x'==.
	replace feelmakedec_`x'=. if g5a_01a_`x'==. & g5a_01b_`x'==. & g5a_01c_`x'==. & g5a_02_`x'==.
	}
drop skip*	

label var feelmakedec_a "Feels can make decisions regarding getting inputs for agricultural production"
label var feelmakedec_b "Feels can make decisions regarding types of crops to grow"
label var feelmakedec_c "Feels can make decisions regarding taking crops to the market"
label var feelmakedec_d "Feels can make decisions regarding livestock raising"
label var feelmakedec_e "Feels can make decisions regarding own wage or salary employment"
label var feelmakedec_f "Feels can make decisions regarding major household expenditures"
label var feelmakedec_g "Feels can make decisions regarding minor household expenditures"


*AGGREGATION
*INPUT IN PRODUCTIVE DECISIONS: adequate if there is AT LEAST TWO domains in which individual has some input in decisions, 
*or makes the decision, or feels he/she could make it if he/she wanted
egen feelinputdecagr_sum=rowtotal(feelmakedec_a-feelmakedec_d inputdec_a inputdec_b inputdec_c inputdec_f), missing 
gen feelinputdecagr=(feelinputdecagr_sum>1)
replace feelinputdecagr=. if feelinputdecagr_sum==.
label var feelinputdecagr_sum "No. agr. domains individual has some input in decisions or feels can make decisions"
label var feelinputdecagr "Has some input in decisions or feels can make decisions in AT LEAST TWO domains"

*CONTROL OVER USE OF INCOME: adequate if there is AT LEAST ONE domain in which individual has some input in income decisions or feels she/he can make decisions regarding wage, employment and minor hh 
*expenditures; as long the only domain in which the individual feels that he/she makes decisions IS NOT minor household expenditures
egen incomedec_sum=rowtotal(incomedec_a incomedec_b incomedec_c incomedec_d incomedec_e incomedec_f feelmakedec_e feelmakedec_f feelmakedec_g), missing
gen incdec_count=(incomedec_sum>0)
replace incdec_count=0 if incdec_count==1 & incomedec_sum==1 & feelmakedec_g==1
replace incdec_count=. if incomedec_sum==.
label var incomedec_sum "No. domains individual has some input in income decisions or feels can make decisions"
label var incdec_count "Has some input in income dec or feels can make dec AND not only minor hh expend"

*drop partact_* inputdec_1-incomedec_6 feelmakedec_a-feelmakedec_m	

***g5.03 - g5.05***

qui recode g5b* (98=.)

foreach x in a b c d {
	gen rai_`x'=-2*g5b_03_`x'-g5b_04_`x'+3*g5b_05_`x'
	gen raiabove_`x'=( rai_`x'>1)
	replace raiabove_`x'=. if rai_`x'==.
	}
	

label var raiabove_a "RAI above 1 regarding getting inputs for consumption and sale at market"
label var raiabove_b "RAI above 1 regarding types of crops to grow"
label var raiabove_c "RAI above 1 regarding taking crops to the market"
label var raiabove_d "RAI above 1 regarding livestock raising"


*AGGREGATION 

** AUTONOMY IN PRODUCTION: adequate if RAI>1 in AT LEAST ONE domain/activity linked to production
egen raiprod_any=rowmax(raiabove_a raiabove_b raiabove_c raiabove_d)
replace raiprod_any=1 if raiprod_any==. & partactagr==0
label var raiprod_any "Has RAI above one in at least on production activity"



***********************************
*** Resources Domain: Module G3 ***
***********************************

qui recode g3a_0* g3b_0* (98=.)

***g3.01***

foreach x in a b c d e f g h i j k l m n {
	gen own_`x'=(g3a_01a_`x'==1 & g3a_01b_`x'!=0)
	replace own_`x'=. if g3a_01a_`x'==. 
	}
label var own_a "Household owns agricultural land"
label var own_b "Household owns large livestock"
label var own_c "Household owns small livestock"
label var own_d "Household owns chickens, ducks, turkeys, pigeons"
label var own_e "Household owns agricultural fish pond or fishing equipment"
label var own_f "Household owns farm equipment (non-mechanized)"
label var own_g "Household owns farm equipment (mechanized)"
label var own_h "Household owns non-farm business equipment"
label var own_i "Household owns house (or other structures)"
label var own_j "Household owns large consumer durables (fridge, TV)"
label var own_k "Household owns small consumer durables (radio, cookware)"
label var own_l "Household owns cell phone"
label var own_m "Household owns non-agricultural land"
label var own_n "Household owns means of transportation"


*Aggregation
*Sum types of assets hh owns
egen own_sum=rowtotal(own_a-own_n), missing
egen ownagr_sum=rowtotal(own_a-own_g), missing

label var own_sum "No. of types of assets household owns"
label var ownagr_sum "No. of types of agricultural assets household owns"


***g3.02 - g3.06***
foreach x in a b c d e f g h i j k l m n{
	*Self or joint own most
	gen selfjointown_`x'=(g3a_02a_`x'==1 | g3a_02b_`x'==1 | g3a_02c_`x'==1) if own_`x'==1
	replace selfjointown_`x'=. if g3a_02a_`x'==. & g3a_02b_`x'==. & g3a_02c_`x'==. & own_`x'==1
	
	*Self or joint decide to sell
	gen selfjointsell_`x'=(g3a_03a_`x'==1 | g3a_03b_`x'==1 | g3a_03c_`x'==1) if own_`x'==1
	replace selfjointsell_`x'=. if g3a_03a_`x'==. & g3a_03b_`x'==. & g3a_03c_`x'==. & own_`x'==1
	
	*Self or joint decide to give away
	gen selfjointgive_`x'=(g3a_04a_`x'==1 | g3a_04b_`x'==1 | g3a_04c_`x'==1) if own_`x'==1
	replace selfjointgive_`x'=. if g3a_04a_`x'==. & g3a_04b_`x'==. & g3a_04c_`x'==. & own_`x'==1
	
	*Self or joint mortgage or rent	
	gen selfjointrent_`x'=(g3a_05a_`x'==1 | g3a_05b_`x'==1 | g3a_05c_`x'==1) if own_`x'==1
	replace selfjointrent_`x'=. if g3a_05a_`x'==. & g3a_05b_`x'==. & g3a_05c_`x'==. & own_`x'==1
	
	*Self or joint buy
	gen selfjointbuy_`x'=(g3a_06a_`x'==1 | g3a_06b_`x'==1 | g3a_06c_`x'==1) if own_`x'==1
	replace selfjointbuy_`x'=. if g3a_06a_`x'==. & g3a_06b_`x'==. & g3a_06c_`x'==.  & own_`x'==1


	*Rights
	**Makes AT LEAST ONE type of decision
	egen selfjointrightany_`x'=rowmax(selfjointsell_`x' selfjointgive_`x' selfjointrent_`x' selfjointbuy_`x')
	replace selfjointrightany_`x'=. if own_`x'==.
	}
	
**Labels
foreach x in own{
	label var selfjoint`x'_a "Jointly `x's most of agricultural land"
	label var selfjoint`x'_b "Jointly `x's most of large livestock"
	label var selfjoint`x'_c "Jointly `x's most of small livestock"
	label var selfjoint`x'_d "Jointly `x's most of chickens, turkeys, ducks"
	label var selfjoint`x'_e "Jointly `x's most of fish pond or fishing equipment"
	label var selfjoint`x'_f "Jointly `x's most of farm equipment (non-mechanized)"
	label var selfjoint`x'_g "Jointly `x's most of farm equipment (mechanized)"
	label var selfjoint`x'_h "Jointly `x's most of non-farm business equipment"
	label var selfjoint`x'_i "Jointly `x's most of the house (or other structures)"
	label var selfjoint`x'_j "Jointly `x's most of large consumer durables"
	label var selfjoint`x'_k "Jointly `x's most of small consumer durables"
	label var selfjoint`x'_l "Jointly `x's most of cell phone"
	label var selfjoint`x'_m "Jointly `x's most of non-agricultural land"
	label var selfjoint`x'_n "Jointly `x's most of means of transportation "
}
foreach x in sell give rent buy{
	label var selfjoint`x'_a "Jointly can `x' agricultural land"
	label var selfjoint`x'_b "Jointly can `x' large livestock"
	label var selfjoint`x'_c "Jointly can `x' small livestock"
	label var selfjoint`x'_d "Jointly can `x' chickens, turkeys, ducks"
	label var selfjoint`x'_e "Jointly can `x' fish pond or fishing equipment"
	label var selfjoint`x'_f "Jointly can `x' farm equipment (non-mechanized)"
	label var selfjoint`x'_g "Jointly can `x' farm equipment (mechanized)"
	label var selfjoint`x'_h "Jointly can `x' non-farm business equipment"
	label var selfjoint`x'_i "Jointly can `x' the house (or other structures)"
	label var selfjoint`x'_j "Jointly can `x' large consumer durables"
	label var selfjoint`x'_k "Jointly can `x' small consumer durables"
	label var selfjoint`x'_l "Jointly can `x' cell phone"
	label var selfjoint`x'_m "Jointly can `x' non-agricultural land"
	label var selfjoint`x'_n "Jointly can `x' means of transportation "
}

label var selfjointrightany_a "Jointly has AT LEAST ONE right over agricultural land"
label var selfjointrightany_b "Jointly has AT LEAST ONE right over large livestock"
label var selfjointrightany_c "Jointly has AT LEAST ONE right over small livestock"
label var selfjointrightany_d "Jointly has AT LEAST ONE right over chickens, turkeys, ducks"
label var selfjointrightany_e "Jointly has AT LEAST ONE right over fishing equipment"
label var selfjointrightany_f "Jointly has AT LEAST ONE right over farm equipment (non-mechanized)"
label var selfjointrightany_g "Jointly has AT LEAST ONE right over farm equipment (mechanized)"
label var selfjointrightany_h "Jointly has AT LEAST ONE right over non-farm business equipment"
label var selfjointrightany_i "Jointly has AT LEAST ONE right over house (or other structures)"
label var selfjointrightany_j "Jointly has AT LEAST ONE right over large consumer durables"
label var selfjointrightany_k "Jointly has AT LEAST ONE right over small consumer durables"
label var selfjointrightany_l "Jointly has AT LEAST ONE right over cell phone"
label var selfjointrightany_m "Jointly has AT LEAST ONE right over non agricultural land"
label var selfjointrightany_n "Jointly as AT LEAST ONE right over means of transportation"

*AGGREGATION
*OWNERSHIP: Adequate if selfjoint owns AT LEAST two small assets (chicken, farming equipment non-mechanized, and small consumer durables)  OR one large asset (all the other). 
	* This is the same to say: empowered if owns AT LEAST one assets and that asset is not a small asset.
	* Inadequate if lives in a household that owns no assets
foreach x in own{
	egen selfjoint`x'sum=rowtotal(selfjoint`x'_*), missing
	egen j`x'count=rowmax(selfjoint`x'_*)
	replace j`x'count=0 if j`x'count==1 & selfjoint`x'sum==1 &(selfjointown_d==1|selfjointown_f==1|selfjointown_k==1)
	replace j`x'count=0 if own_sum==0

	rename j`x'count j`x'_count
	rename selfjoint`x'sum selfjoint`x'_sum
	
	}

*PURCHASE, SALE OR TRANSFER OF ASSETS: Adequate if selfjoint has AT LEAST ONE type of right
*over AT LEAST ONE type of asset as long as it is not chicken nor farming equipment non-mechanized.
*Inadequate if living in households with no assets are automatically adequate
		
foreach x in rightany{
	*Agricultural assets
	egen selfjoint`x'agrsum=rowtotal(selfjoint`x'_a selfjoint`x'_b selfjoint`x'_c selfjoint`x'_d selfjoint`x'_e selfjoint`x'_f selfjoint`x'_g), missing
	egen selfjoint`x'agrcount=rowmax(selfjoint`x'_a selfjoint`x'_b selfjoint`x'_c selfjoint`x'_d selfjoint`x'_e selfjoint`x'_f selfjoint`x'_g)
	replace selfjoint`x'agrcount=0 if selfjoint`x'agrcount==1 & selfjoint`x'agrsum==1 & (selfjoint`x'_d==1|selfjoint`x'_f==1)
	replace selfjoint`x'agrcount=0 if ownagr_sum==0
		
	rename selfjoint`x'agrsum selfjoint`x'agr_sum
	rename selfjoint`x'agrcount j`x'agr
	
	}

label var jrightanyagr "Jointly has AT LEAST ONE right in AT LEAST ONE agricultural asset the hh owns"


***g3.07***
foreach x in a b c d e {
	gen creditaccess_`x'=(g3b_07_`x'>=1 & g3b_07_`x'<=3)
	replace creditaccess_`x'=. if g3b_07_`x'==. | g3b_07_`x'==97
	}
egen creditaccess=rowtotal(creditaccess_*), missing
label var creditaccess "No. of credit sources that the hh uses”

***g3.08, g3.09***
foreach y in a b c d e {
	*Self or joint decide to borrow
	gen creditselfjointborrow_`y'=(g3b_08a_`y'==1 | g3b_08b_`y'==1 | g3b_08c_`y'==1) if creditaccess_`y'==1
	replace creditselfjointborrow_`y'=. if g3b_08a_`y'==. & g3b_08b_`y'==. & g3b_08c_`y'==. & creditaccess_`y'==1
	
	*Self or joint decide how to use
	gen creditselfjointuse_`y'=(g3b_09a_`y'==1 | g3b_09b_`y'==1 | g3b_09c_`y'==1) if creditaccess_`y'==1
	replace creditselfjointuse_`y'=. if g3b_09a_`y'==. & g3b_09b_`y'==. & g3b_09c_`y'==. & creditaccess_`y'==1
	
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



***********************************
***Leadership Domain: Module G4 ***
***********************************

qui recode g4* (97 98=.) 


***g4.01 - g4.03***
*empowered if comfortable speaking in public
foreach x of num 1/3 {
	gen speakpublic_`x'=(g4a_0`x'>1)
	replace speakpublic_`x'=. if g4a_0`x'==.
	}

*AGGREGATION
*SPEAK IN PUBLIC: Adequate if comfortable speaking in public in AT LEAST ONE context
egen speakpublic_any=rowmax(speakpublic_1 speakpublic_2 speakpublic_3)

***g4.05***
foreach x in a b c d e f g h i j {
	*Active group member - ***Pilot 1.1 data uses g4b_04_`x' whereas this is g4.05 in the 1.1 Qx***
	gen groupmember_`x'=(g4b_04_`x'==1)
	replace groupmember_`x'=. if g4b_04_`x'==.
	gen nogroup_`x'=(g4b_03_`x'==2 | g4b_03_`x'==.)
	}

*AGGREGATION
*GROUP MEMBERSHIP: Adequate if individual is part of AT LEAST ONE group
egen groupmember_any=rowmax(groupmember_*)
replace groupmember_any=0 if groupmember_any==. /*Inadequate if no groups in community*/


******************************
*** Time Domain: Module G6 ***
******************************

***g6.02***
*LEISURE TIME: Adequate if does not express any level of dissatisfaction with the amount of leisure time available
gen leisuretime=(g6_02>4)
replace leisuretime=. if g6_02==.

rename a05 mid
rename g1_03 sex
rename a01 hhid
save "modifieddata\all_indicators_1.1.dta", replace


***g6.01a***

** Create time poverty measure ***


// Open dataset with time use information //

use "clean data\ind_time.dta", clear // IMPORTANT: Change to your time data file
renvars, subst(_p1) // harmonize varnames across pilot types
rename a05 mid
rename a01 hhid 

merge m:1 country hhid mid using "modifieddata\all_indicators_1.1.dta", keepusing(sex)
drop if _m==2


*Define work (w/ commuting/travelling) 
qui gen w=(act=="E" | act=="F" | act=="G" | act=="J" | act=="K" | ///
		   act=="L" | act=="M" | act=="N" | act=="P")
drop if w==0

*Calculate total time spent working as primary and secondary activity
collapse (sum) time_1 time_2 (mean) sex, by(country hhid mid) 
gen work=time_1 + (.5*time_2)


***Define poverty lines
*10 hr/day
qui gen z10=10*60
*10.5 hr/day
qui gen z105=10.5*60
*11 hr/day
qui gen z11=11*60
*75 hr/week
qui gen z75=(75/7)*60

foreach x of var z*{
	qui gen H_`x'=.
	foreach y in 1 2{
		qui gen H_`x'_`y'=.
		}
	}
	
foreach x of var z*{
	qui gen poor_`x'=(work>`x')
	foreach y in 1 2 3{
		*Headcount
		qui sum poor_`x' if country==`y'
		local q=r(sum)
		qui sum work if country==`y'
		local n=r(N)
		qui replace H_`x'=`q'/`n' if country==`y'
		foreach z in 1 2{
			*Headcount
			qui sum poor_`x' if country==`y' & sex==`z'
			local q=r(sum)
			qui sum work if country==`y' & sex==`z'
			qui replace H_`x'_`z'=`q'/r(N) if country==`y'
			}
		}
	}


	
foreach y in 1 2 3{
	foreach x in 10 105 11 75 {
		qui sum H_z`x' if country==`y'
		local overall=r(mean)
		qui sum H_z`x'_1 if country==`y'
		local men=r(mean)
		qui sum H_z`x'_2 if country==`y'
		local women=r(mean)
*		post stats3 (5) (`y') (`x') (`overall') (`men') (`women')
		}
	}

save "modifieddata\time measure_1.1.dta", replace 


// Merge time poverty measure with all indicators dataset //

use "modifieddata\all_indicators_1.1.dta", clear

merge 1:1 country hhid mid using "modifieddata\time measure_1.1.dta", keepusing(work poor_z10 poor_z105 poor_z75 poor_z11)

foreach x in 10 105 75 11 {
	gen npoor_z`x'=1-poor_z`x'
	}

drop _merge 
save "modifieddata\all_indicators_1.1.dta", replace 

log close

