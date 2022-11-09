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

***********************************************************
*********** 2015 & 2018  (ROUND 2 AND 3) ******************
***********************************************************

* SPECIFIC MODULE ON RESOURCE DOMAIN: Module WE3A *

use "WEAI input_Round3\resource_domain_18F.dta", clear // Insert your data file on resource domain for every round and every gender
use "WEAI input_Round3\resource_domain_18M.dta", clear
use "WEAI_input Round 2\resource_domain_15F.dta", clear
use "WEAI_input Round 2\resource_domain_15M.dta", clear

reshape wide we3a_lbl we3a01a we3a01b we3a02a we3a02b we3a02c we3a03a we3a03b we3a03c we3a04a we3a04b we3a04c we3a05a we3a05b we3a05c we3a06a we3a06b we3a06c, i(a01) j(we3a) string   // reshape the format of the data into wide

qui recode we3a0* (98=.)

***we3a***

foreach x in A B C D E F G H I J K L M N {
	gen own_`x'=(we3a01a`x' ==1 & we3a01b`x' !=0)
	replace own_`x'=. if we3a01a`x'==. 
	}
	
label var own_A "Household owns agricultural land"
label var own_B "Household owns large livestock"
label var own_C "Household owns small livestock"
label var own_D "Household owns chickens, ducks, turkeys, pigeons"
label var own_E "Household owns agricultural fish pond or fishing equipment"
label var own_F "Household owns farm equipment (non-mechanized)"
label var own_G "Household owns farm equipment (mechanized)"
label var own_H "Household owns non-farm business equipment"
label var own_I "Household owns house (or other structures)"
label var own_J "Household owns large consumer durables (fridge, TV)"
label var own_K "Household owns small consumer durables (radio, cookware)"
label var own_L "Household owns cell phone"
label var own_M "Household owns non-agricultural land"
label var own_N "Household owns means of transportation"


*Aggregation
*Sum types of assets hh owns
egen own_sum=rowtotal(own_A-own_N), missing
egen ownagr_sum=rowtotal(own_A-own_G), missing

label var own_sum "No. of types of assets household owns"
label var ownagr_sum "No. of types of agricultural assets household owns"


***we3.02 - we3.06***
foreach x in A B C D E F G H I J K L M N {
	*Self or joint own most
	gen selfjointown_`x'=(we3a02a`x'==1 | we3a02b`x'==1 | we3a02c`x'==1) if own_`x'==1
	replace selfjointown_`x'=. if we3a02a`x' ==. & we3a02b`x'==. & we3a02c`x'==. & own_`x'==1
	
	*Self or joint decide to sell
	gen selfjointsell_`x'=(we3a03a`x'==1 | we3a03b`x'==1 | we3a03c`x'==1) if own_`x'==1
	replace selfjointsell_`x'=. if we3a03a`x'==. & we3a03b`x'==. & we3a03c`x'==. & own_`x'==1
	
	*Self or joint decide to give away
	gen selfjointgive_`x'=(we3a04a`x'==1 | we3a04b`x'==1 | we3a04c`x'==1) if own_`x'==1
	replace selfjointgive_`x'=. if we3a04a`x'==. & we3a04b`x'==. & we3a04c`x'==. & own_`x'==1
	
	*Self or joint mortgage or rent	
	gen selfjointrent_`x'=(we3a05a`x'==1 | we3a05b`x'==1 | we3a05c`x'==1) if own_`x'==1
	replace selfjointrent_`x'=. if we3a05a`x'==. & we3a05b`x'==. & we3a05c`x'==. & own_`x'==1
	
	*Self or joint buy
	gen selfjointbuy_`x'=(we3a06a`x'==1 | we3a06b`x'==1 | we3a06c`x'==1) if own_`x'==1
	replace selfjointbuy_`x'=. if we3a06a`x'==. & we3a06b`x'==. & we3a06c`x'==.  & own_`x'==1


	*Rights
	**Makes AT LEAST ONE type of decision
	egen selfjointrightany_`x'=rowmax(selfjointsell_`x' selfjointgive_`x' selfjointrent_`x' selfjointbuy_`x')
	replace selfjointrightany_`x'=. if own_`x'==.
	}
	
**Labels
foreach x in own{
	label var selfjoint`x'_A "Jointly `x's most of agricultural land"
	label var selfjoint`x'_B "Jointly `x's most of large livestock"
	label var selfjoint`x'_C "Jointly `x's most of small livestock"
	label var selfjoint`x'_D "Jointly `x's most of chickens, turkeys, ducks"
	label var selfjoint`x'_E "Jointly `x's most of fish pond or fishing equipment"
	label var selfjoint`x'_F "Jointly `x's most of farm equipment (non-mechanized)"
	label var selfjoint`x'_G "Jointly `x's most of farm equipment (mechanized)"
	label var selfjoint`x'_H "Jointly `x's most of non-farm business equipment"
	label var selfjoint`x'_I "Jointly `x's most of the house (or other structures)"
	label var selfjoint`x'_J "Jointly `x's most of large consumer durables"
	label var selfjoint`x'_K "Jointly `x's most of small consumer durables"
	label var selfjoint`x'_L "Jointly `x's most of cell phone"
	label var selfjoint`x'_M "Jointly `x's most of non-agricultural land"
	label var selfjoint`x'_N "Jointly `x's most of means of transportation "
}
foreach x in sell give rent buy{
	label var selfjoint`x'_A "Jointly can `x' agricultural land"
	label var selfjoint`x'_B "Jointly can `x' large livestock"
	label var selfjoint`x'_C "Jointly can `x' small livestock"
	label var selfjoint`x'_D "Jointly can `x' chickens, turkeys, ducks"
	label var selfjoint`x'_E "Jointly can `x' fish pond or fishing equipment"
	label var selfjoint`x'_F "Jointly can `x' farm equipment (non-mechanized)"
	label var selfjoint`x'_G "Jointly can `x' farm equipment (mechanized)"
	label var selfjoint`x'_H "Jointly can `x' non-farm business equipment"
	label var selfjoint`x'_I "Jointly can `x' the house (or other structures)"
	label var selfjoint`x'_J "Jointly can `x' large consumer durables"
	label var selfjoint`x'_K "Jointly can `x' small consumer durables"
	label var selfjoint`x'_L "Jointly can `x' cell phone"
	label var selfjoint`x'_M "Jointly can `x' non-agricultural land"
	label var selfjoint`x'_N "Jointly can `x' means of transportation "
}

label var selfjointrightany_A "Jointly has AT LEAST ONE right over agricultural land"
label var selfjointrightany_B "Jointly has AT LEAST ONE right over large livestock"
label var selfjointrightany_C "Jointly has AT LEAST ONE right over small livestock"
label var selfjointrightany_D "Jointly has AT LEAST ONE right over chickens, turkeys, ducks"
label var selfjointrightany_E "Jointly has AT LEAST ONE right over fishing equipment"
label var selfjointrightany_F "Jointly has AT LEAST ONE right over farm equipment (non-mechanized)"
label var selfjointrightany_G "Jointly has AT LEAST ONE right over farm equipment (mechanized)"
label var selfjointrightany_H "Jointly has AT LEAST ONE right over non-farm business equipment"
label var selfjointrightany_I "Jointly has AT LEAST ONE right over house (or other structures)"
label var selfjointrightany_J "Jointly has AT LEAST ONE right over large consumer durables"
label var selfjointrightany_K "Jointly has AT LEAST ONE right over small consumer durables"
label var selfjointrightany_L "Jointly has AT LEAST ONE right over cell phone"
label var selfjointrightany_M "Jointly has AT LEAST ONE right over non agricultural land"
label var selfjointrightany_N "Jointly as AT LEAST ONE right over means of transportation"

*AGGREGATION
*OWNERSHIP: Adequate if selfjoint owns AT LEAST two small assets (chicken, farming equipment non-mechanized, and small consumer durables)  OR one large asset (all the other). 
	* This is the same to say: empowered if owns AT LEAST one assets and that asset is not a small asset.
	* Inadequate if lives in a household that owns no assets
foreach x in own{
	egen selfjoint`x'sum=rowtotal(selfjoint`x'_*), missing
	egen j`x'count=rowmax(selfjoint`x'_*)
	replace j`x'count=0 if j`x'count==1 & selfjoint`x'sum==1 &(selfjointown_D==1|selfjointown_F==1|selfjointown_K==1)
	replace j`x'count=0 if own_sum==0

	rename j`x'count j`x'_count
	rename selfjoint`x'sum selfjoint`x'_sum
	
	}

*PURCHASE, SALE OR TRANSFER OF ASSETS: Adequate if selfjoint has AT LEAST ONE type of right
*over AT LEAST ONE type of asset as long as it is not chicken nor farming equipment non-mechanized.
*Inadequate if living in households with no assets are automatically adequate
		
foreach x in rightany{
	*Agricultural assets
	egen selfjoint`x'agrsum=rowtotal(selfjoint`x'_A selfjoint`x'_B selfjoint`x'_C selfjoint`x'_D selfjoint`x'_E selfjoint`x'_F selfjoint`x'_G), missing
	egen selfjoint`x'agrcount=rowmax(selfjoint`x'_A selfjoint`x'_B selfjoint`x'_C selfjoint`x'_D selfjoint`x'_E selfjoint`x'_F selfjoint`x'_G)
	replace selfjoint`x'agrcount=0 if selfjoint`x'agrcount==1 & selfjoint`x'agrsum==1 & (selfjoint`x'_D==1|selfjoint`x'_F==1)
	replace selfjoint`x'agrcount=0 if ownagr_sum==0
		
	rename selfjoint`x'agrsum selfjoint`x'agr_sum
	rename selfjoint`x'agrcount j`x'agr
	}

label var jrightanyagr "Jointly has AT LEAST ONE right in AT LEAST ONE agricultural asset the hh owns"

*** Save the dataset for resource domain *** // each time keep only gender and year of interest

save resource_domain_18F_transformed.dta
save resource_domain_18M_transformed.dta
save resource_domain_15F_transformed.dta
save resource_domain_15M_transformed.dta
save resource_domain_11F_transformed.dta
save resource_domain_11M_transformed.dta


*** Save aside the variables useful for WEAI calculation ***

keep a01 jown_count jrightanyagr

save var_resource_15M.dta // Change denomination for every gender and year of reference


*********************************************
*********** 2011 (ROUND 1) ******************
*********************************************

use "WEAI_input_Round1\resource_domain_11F.dta", clear // In the dataset of 2011, data are already in wide format
use "WEAI_input_Round1\resource_domain_11M.dta", clear // In the dataset of 2011, data are already in wide format

qui recode wc* (98=.)

***wc01a wc01b***

foreach x in a b c d e f g h i j k l m n {
	gen own_`x'=(wc01a_`x' ==1 & wc01b_`x' !=0)
	replace own_`x'=. if wc01a_`x'==. 
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


***we3.02 - we3.06***

foreach x in a b c d e f g h i j k l m n {
	*Self or joint own most
	gen selfjointown_`x'=(wc02_`x'==1) if own_`x'==1
	replace selfjointown_`x'=. if wc02_`x' ==. & own_`x'==1
	
	*Self or joint decide to sell
	gen selfjointsell_`x'=(wc03_`x'==1) if own_`x'==1
	replace selfjointsell_`x'=. if wc03_`x'==. & own_`x'==1
	
	*Self or joint decide to give away
	gen selfjointgive_`x'=(wc04_`x'==1) if own_`x'==1
	replace selfjointgive_`x'=. if wc04_`x'==. & own_`x'==1
	
	*Self or joint mortgage or rent	
	gen selfjointrent_`x'=(wc05_`x'==1) if own_`x'==1
	replace selfjointrent_`x'=. if wc05_`x'==. & own_`x'==1
	
	*Self or joint buy
	gen selfjointbuy_`x'=(wc06_`x'==1) if own_`x'==1
	replace selfjointbuy_`x'=. if wc06_`x'==. & own_`x'==1


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

*** Save the dataset for resource domain *** // each time keep only gender and year of interest

save resource_domain_11F_transformed.dta
save resource_domain_11M_transformed.dta


*** Save aside the variables useful for WEAI calculation ***

keep wa01 jown_count jrightanyagr

save var_resource_11M.dta // Change denomination for every gender and year of reference