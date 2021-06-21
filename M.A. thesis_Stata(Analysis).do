/*
################################################################################
################################################################################
                                Analysis Part
################################################################################
################################################################################
*/

local list2  "emp revt"
local list3  "emp revt xrd capx"
local list4  "xrdin capxin lev levc"
local list5  "xrd xrdin comb_rdcap capx capxin ppent lev levc"

cd "C:\Users\user\Desktop"
use "pooled_firm.dta", clear



***** Replace missing xrd observations into 0 for those firms with missingrate=1
*(missingrate=0 means all of xrd observations of a specific firm are missing or zero)
replace charat3_xrd = 0 if missingrate == 1
replace chabr_xrd = 0 if missingrate == 1
replace charat3_capx = 0 if missingrate_capx == 1
replace chabr_capx = 0 if missingrate_capx == 1

* To use as a dummy variable which has a value of 1 if non-xrd firm later, 
* replace missingrate = 0 if missingrate!=1
replace missingrate = 0 if missingrate!=1
drop if missingrate ==0 & chabr_xrd==.
replace missingrate_capx = 0 if missingrate_capx!=1
drop if missingrate_capx ==0 & chabr_capx==.



***** Specify RHS variable 
gen RHS_var = quan_ptb07

replace RHS_var = 0 if RHS_var<9
replace RHS_var = 1 if RHS_var>=9

keep if (charat3_wtot_emp!=. & chabr_wtot_emp!=. & chara_wtot_emp!=. ///
	   & lag_chara_wtot_emp!=. & mean_wtot_emp!=. & lag_mean_emp!=.) /// 
       | (charat3_wtot_revt!=. & chabr_wtot_revt!=. & chara_wtot_revt!=. ///
	   & lag_chara_wtot_revt!=. & mean_wtot_revt!=. & lag_mean_revt!=.)




***** Generate Recession dummy
gen Rec = 0
replace Rec = 1 if a==1981
drop if a<1977 | a>1985
drop if a==1980 | a==1982
/* (3 /(-1)/ Recession /(-1)/ 3)
replace Rec = 1 if a==2008
drop if a<2004 | a>2012 
drop if a==2007 | a==2009

replace Rec = 1 if a==2001
drop if a<1997 | a>2005
drop if a==2000 | a==2002

replace Rec = 1 if a==1990
drop if a<1986 | a>1994
drop if a==1989 | a==1991

replace Rec = 1 if a==1981
drop if a<1977 | a>1985
drop if a==1980 | a==1982
*/


***** Normalize investment-related explanatory variables by the difference 
*     between 90th percentile and 10th percentile

*** (1) Capital investment related(flow and stock variables) 
local listpp "charat3_capx charat3_ppent"
foreach var of varlist `listpp'{
local p10 = "`var'"+"_10"
local p90 = "`var'"+"_90"
local norm = "`var'"+"_norm"
bysort year : egen `p10' = pctile(`var'), p(10)
bysort year : egen `p90' = pctile(`var'), p(90)
gen `norm' = `p90'-`p10'
replace `var' = `var'/`norm'
}
*** (2) R&D investment related(flow and stock variables) 
local listpp "charat3_xrd"
foreach var of varlist `listpp'{
local p10 = "`var'"+"_10"
local p90 = "`var'"+"_90"
local norm = "`var'"+"_norm"
* derive 90th percentile and 10th percentile for only xrd existing observations
bysort year xrd_rat_zero : egen `p10' = pctile(`var'), p(10)
bysort year xrd_rat_zero : egen `p90' = pctile(`var'), p(90)
gen `norm' = `p90'-`p10'
replace `norm' = 1 if `norm'==0
replace `var' = `var'/`norm'
}
local listpp "charat3_comb_rdcap"
foreach var of varlist `listpp'{
local p10 = "`var'"+"_10"
local p90 = "`var'"+"_90"
local norm = "`var'"+"_norm"
* derive 90th percentile and 10th percentile for only xrd existing observations
bysort year rdcap_rat_zero : egen `p10' = pctile(`var'), p(10)
bysort year rdcap_rat_zero : egen `p90' = pctile(`var'), p(90)
gen `norm' = `p90'-`p10'
replace `norm' = 1 if `norm'==0
replace `var' = `var'/`norm'
}



********************************Regression**************************************
*** Wtot value change analysis
local listw "emp revt"
foreach var of varlist `listw'{

local charat3_wtot_obj = "charat3_wtot_"+"`var'"
local chabr_wtot_obj = "chabr_wtot_"+"`var'"
local chara_wtot_obj = "chara_wtot_"+"`var'"

local lag_chara_wtot_obj = "lag_chara_wtot_"+"`var'"

local mean_wtot = "mean_wtot_"+"`var'"
local lag_mean = "lag_mean_"+"`var'"

/*pooling period*/

reghdfe `charat3_wtot_obj' c.RHS_var#c.Rec RHS_var `lag_chara_wtot_obj' `lag_mean' ///
	debt_assets age at che , absorb (sic year) vce(cluster year)
reghdfe `chabr_wtot_obj'   c.RHS_var#c.Rec RHS_var `lag_chara_wtot_obj' `lag_mean' ///
	debt_assets age at che ,  absorb (sic year) vce(cluster year)
reghdfe `chara_wtot_obj'   c.RHS_var#c.Rec RHS_var `lag_chara_wtot_obj' `mean_wtot' ///
	debt_assets age at che , absorb (sic year) vce(cluster year)
}



*** Original value change analysis
local lista "xrd capx ppent comb_rdcap"
foreach var of varlist `lista'{
local charat3_obj = "charat3_"+"`var'"
local chabr_obj = "chabr_"+"`var'"
local chara_obj = "chara_"+"`var'"

local charat3_rat_obj = "charat3_rat_"+"`var'"
local chabr_rat_obj = "chabr_rat_"+"`var'"
local chara_rat_obj = "chara_rat_"+"`var'"
local chara2_rat_obj = "chara2_rat_"+"`var'"

local lag1 = "lag1_"+"`var'"
local lag2 = "lag2_"+"`var'"
local lag_chara_obj = "lag_chara_"+"`var'"
local lag_chara_rat_obj = "lag_chara_rat_"+"`var'"

reghdfe `charat3_rat_obj' c.RHS_var#c.Rec RHS_var `lag_chara_rat_obj' `lag1' ///
	debt_assets age at che , absorb (sic year) vce(cluster year)
reghdfe `chabr_rat_obj'   c.RHS_var#c.Rec RHS_var `lag_chara_rat_obj' `lag1' ///
	debt_assets age at che , absorb (sic year) vce(cluster year)
reghdfe `chara_rat_obj'   c.RHS_var#c.Rec RHS_var `lag_chara_rat_obj' `lag2' ///
	debt_assets age at che , absorb (sic year) vce(cluster year)
reghdfe `chara2_rat_obj'  c.RHS_var#c.Rec RHS_var `lag_chara_rat_obj' `lag1' ///
	debt_assets age at che , absorb (sic year) vce(cluster year)
}


*** Channel analysis
local listw "emp revt"
foreach var of varlist `listw'{
local charat3_wtot_obj = "charat3_wtot_"+"`var'"
local lag_charat3_wtot_obj = "lag_charat3_wtot_"+"`var'"
local lag_mean = "lag_mean_"+"`var'"

reghdfe `charat3_wtot_obj' c.RHS_var#c.Rec#c.charat3_xrd c.RHS_var#c.Rec#i.missingrate ///
							c.RHS_var#c.Rec RHS_var `lag_chara_wtot_obj' `lag_mean' ///
							debt_assets age at che , absorb (sic year) vce(cluster year)
}
