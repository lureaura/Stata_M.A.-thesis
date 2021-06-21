
/*
################################################################################
################################################################################
                              Data Setting Part
################################################################################
################################################################################
*/


cd "C:\Users\user\Desktop"
/*
################################################################################
1. Data file merging
################################################################################
*/

/*
1-1) Data setting for 'Financial ratios.dta' from monthly to yearly data
   by averaging
################################################################################
*/
use "Financial ratios.dta" ,clear
sort gvkey public_date
drop adate qdate
gen year = year(public_date)

collapse (mean) capei bm evm pe_op_basic pe_op_dil ps dpr aftret_invcapx ///
debt_assets de_ratio rd_sale ptb divyield , by(gvkey year)

save "Ratios_y.dta", replace




/*
1-2) Merge Accounting data(Firm data_y.dta) and Financial Ratio data('Ratios_y.dta')
   And generate combined 'Firm total_y.dta'
################################################################################
*/
use "Firm data_y.dta", clear
replace fyear = year(datadate) if fyear==.
rename fyear year

keep if datafmt == "STD"
drop if indfmt == "FS"

***** Remove double observations
duplicates report gvkey year
/*
--------------------------------------
   copies | observations       surplus
----------+---------------------------
        1 |       513937             0
        2 |          188            94
--------------------------------------
*/
*Drop one of which current asset(act) is missing
bysort gvkey year : egen dup_count = count(year)
drop if dup_count ==2 & act==.
duplicates report gvkey year
/*
--------------------------------------
   copies | observations       surplus
----------+---------------------------
        1 |       514027             0
--------------------------------------
*/

drop datadate consol popsrc datafmt curcd indfmt dup_count


***** Generate Age variable by setting the first observation-year of a firm as age 0
bysort gvkey : egen age = min(year)
replace age = year-age

order gvkey year age conm acctchg acctstd fyr fyrc pddur apdedate fdate pdate ///
 exchg costat fic idbflag loc busdesc city county ein naics sic dlrsn dldte ///
 ipodate mkvalt tfva tfvl at act lt lct bast ceq ceql capx xrd che ch chech ///
 dlc dltt ppegt ppent re teq seq rveqt wcap ip ipv ivst icapt intan intano /// 
 gdwl invch invfg invo invrm invt invwip cogs ebit ebitda gp ni opiti revt /// 
 sale xad xlr xstf xstfo xstfws xopr xt govgr


***** Merge with Financial Ratio data
merge 1:1 gvkey year using "Ratios_y.dta"
/*
    Result                           # of obs.
    -----------------------------------------
    not matched                       315,932
        from master                   299,205  (_merge==1)
        from using                     16,727  (_merge==2)

    matched                           214,822  (_merge==3)
    -----------------------------------------
*/
rename _merge merge1
order merge1, after(gvkey)
sort gvkey year

label variable capei "Shillers Cyclically Adjusted P/E Ratio"
label variable bm "Book/Market"
label variable evm "Enterprise Value Multiple"
label variable ps "Price/Sales"
label variable dpr "Dividend Payout Ratio"
label variable aftret_invcapx "After-tax Return on Invested Capital"
label variable debt_assets "Total Debt/Total Assets"
label variable de_ratio "Total Debt/Equity"
label variable rd_sale "Research and Development/Sales"
label variable divyield "Dividend Yield"
label variable ptb "price/book"

save "Firm total_y.dta", replace



/*
1-3) Merging USPTO data and citation data and create additional related variables
   And construct 'USPTO.dta'
################################################################################
*/
use "basic_uspto.dta", clear
merge 1:1 wku using "cit_stock.dta", keep(1 3) nogenerate

/*
    Result                           # of obs.
    -----------------------------------------
    not matched                     3,525,658
        from master                   424,925  (_merge==1)
        from using                  3,100,733  (_merge==2)

    matched                         1,446,313  (_merge==3)
    -----------------------------------------
	_m==1 => no-citation patent
	_m==2 => Cited at least once but no gvkey matched(individual, non-public firm, government patent)
	_m==3 => Cited at least once and gvkey matched
*/

bysort gvkey year: egen patent = count(wku)
bysort gvkey year: egen citation_tot = total(stock_total)
bysort gvkey year: egen citation_3year = total(stock_3year)
bysort gvkey year: egen selfcitation_tot = total(selfstock_total)
bysort gvkey year: egen selfcitation_3year = total(selfstock_3year)
keep year gyear gvkey patent citation_tot citation_3year selfcitation_tot selfcitation_3year

duplicates drop gvkey year, force
save "USPTO.dta", replace




/*
1-4) Merging Firm total data and USPTO patent data
   And construct 'Firm_USPTO_y.dta'
################################################################################
*/
use "Firm total_y.dta", clear
destring gvkey, replace

merge 1:1 gvkey year using "USPTO.dta"
/*
    Result                           # of obs.
    -----------------------------------------
    not matched                       494,317
        from master                   471,507  (_merge==1)
        from using                     22,810  (_merge==2)

    matched                            59,247  (_merge==3)
    -----------------------------------------
*/
rename _merge merge2

save "Firm_USPTO_y.dta", replace





/*
################################################################################
2. Data cleaning and Variable creation procedure
################################################################################
*/

use "Firm_USPTO_y.dta", clear
duplicates report gvkey year
/*
--------------------------------------
   copies | observations       surplus
----------+---------------------------
        1 |       553564             0
--------------------------------------
*/


***** Adjust year for errorneous year cases
tab year

replace year = 1975 if year==2975
replace year = 1986 if year==2986
replace year = 1987 if year==2987
replace year = 1978 if year==7978
replace year = 1973 if year==9173
replace year = 1975 if year==9175
replace year = 1976 if year==9176

drop at_fn capx_fn cogs_fn dltt_fn emp_fn icapt_fn invt_fn ip_fn ni_fn ppegt_fn ///
 ppent_fn revt_fn re_fn sale_fn xlr_fn xstf_fn xt_fn bast_dc capx_dc ceq_dc ceql_dc ///
 ch_dc che_dc chech_dc dlc_dc dltt_dc ebit_dc ebitda_dc gdwl_dc emp_dc govgr_dc ///
 icapt_dc intan_dc intano_dc invch_dc invfg_dc invt_dc invrm_dc invwip_dc ip_dc ///
 ipv_dc ivst_dc lct_dc lt_dc ni_dc ppegt_dc ppent_dc re_dc rveqt_dc seq_dc /// 
 tfva_dc tfvl_dc xad_dc xlr_dc xstf_dc xstfo_dc

drop if merge1!=3
drop merge1
xtset gvkey year, yearly



***** SIC 2-digit code and Industry(1-digit) variable generating 
destring sic, replace
gen work = string(sic, "%10.0g")
gen sic_2d = real(substr(work, 1, length(work) - 2)) 
drop work
order sic_2d, after(sic)
label variable sic_2d "2-digit SIC code"

gen industry = "Mining" if sic_2d >=10 & sic_2d<=14
replace industry = "Construction" if sic_2d >=15 & sic_2d<=17
replace industry = "Manufacturing" if sic_2d >=20 & sic_2d<=39
replace industry = "Transportation & Public Utilities" if sic_2d >=40 & sic_2d<=49
replace industry = "Wholesale Trade" if sic_2d >=50 & sic_2d<=51
replace industry = "Retail Trade" if sic_2d >=52 & sic_2d<=59
replace industry = "Finance, Insurance, & Real Estate" if sic_2d >=60 & sic_2d<=67
replace industry = "Services" if sic_2d >=70 & sic_2d<=89
replace industry = "Public Administration" if sic_2d >=91 & sic_2d<=98
replace industry = "Nonclassifiable Establishments" if sic_2d ==99
order industry, after(sic_2d)

***** Exclude farm-related firms (sic_2d: 01~09)
drop if sic_2d >=1 & sic_2d <=9 

***** Encode industry variable and save it as 'ind'
encode industry, generate(ind)
order ind industry, after(sic_2d)
drop merge2


***** Find xrd-investing firms which is defined as firms with at least 
*     one positive xrd from 2-year before to present year
xtset gvkey year, yearly
sort gvkey year
bysort gvkey : egen count = count(gvkey)

gen indxrd = 1 if /// 
( ( (L2.xrd>0 & L2.xrd!=.) | (L1.xrd>0 & L1.xrd!=.) | (xrd>0 & xrd!=.) ) ///
& gvkey ==L.gvkey ) | ( (xrd>0 & xrd!=.) & gvkey != L.gvkey)

* Count total number of positive years of xrd for each firm
bysort gvkey : egen cxrd = total(indxrd)
* Derive the difference between the total number of each firm observation and
* the number of positive xrd years of that firm
gen dif = count-cxrd
* The ratio of the number of non-xrd-missing observations 
*       to the total number of firm-specific observations 
*  -> used when determine zero-xrd firms later(missingrate==1) 
gen missingrate = dif/count
order xrd count indxrd cxrd dif missingrate,after(year)
*keep if missingrate<1
* From original 214,822 firm-year to 102,006 firm-year observations (47.5% remained) 


***** Generate time index from 1970
gen t = year-1970+1
order t, after(year)

xtset gvkey year, yearly
sort gvkey year

***** Generate avg_ps,ptb for previous 3-year variable for each year
local list1 "ps ptb"
foreach var of varlist `list1'{
local name1 = "avg3_"+"`var'"
bysort gvkey : gen `name1' = (L3.`var'+L2.`var'+L.`var')/3
order `name1' , after(`var')
}

***** Generate quan_avg_ps,ptb(10) for each year
* avg_ps,ptb are available after 1980 
foreach var of varlist `list1'{
local n1 = "quan_avg_"+"`var'"
local x1 = "avg3_"+"`var'"
sort year `x1'
bysort year : egen `n1' = xtile(`x1'), nq(10)
order `n1', after(`x1')
}

sort gvkey year

***** Create additional leverage, operational efficiency, R&D intensity variables 
gen levc = lct/act
gen lev = lt/at
gen op_eff = gp/revt
gen xrdin = xrd/revt

save "Hershbein_temp.dta", replace




/*
################################################################################
3. Create Growth-related dependent variables and data pooling
   for DID regression following Hershbein and Kahn, 2018
################################################################################
*/

local list1 "ps ptb"
local list2  "xrd xrdin emp revt gp intano ppent capx xad"
forvalues x = 1975/2014{
use "Hershbein_temp.dta", clear
cd C:\Users\user\Desktop\DIDID


* To generate file name according to a specified year
gen a = `x'
local file_y = string(a)
* Setting a starting year time index (each starting year's t=0)(eg. 2007: t=38->0)
gen sta = a-1970+1
replace t=t-sta


xtset gvkey year, yearly
sort gvkey year


***** Generate RHS_var(dependent variable) for a specific year(eg. 2007)
*** Remove 'quan_avg_' missing data
foreach var of varlist `list1'{
local x1 = "quan_avg_"+"`var'"
drop if `x1'==.
}


*** 'quan_avg_' term -> quan07 
foreach var of varlist `list1'{
local quan_obj07 = "quan_"+"`var'"+"07"
local quan_avg_obj = "quan_avg_"+"`var'"

gen `quan_obj07' = `quan_avg_obj'
replace `quan_obj07' = . if t!=0
* generate RHS_var for after the starting year
replace `quan_obj07' = L.`quan_obj07' if t>=1 & `quan_obj07'==. & gvkey == L.gvkey  
sort gvkey year
* generate RHS_var for before the starting year
gen negq = -year
order negq, after(year)
xtset gvkey negq, yearly
sort gvkey negq
replace `quan_obj07' = L.`quan_obj07' if t<0 & `quan_obj07'==. & gvkey == L.gvkey  
drop negq

xtset gvkey year, yearly
sort gvkey year
}


***** Generate growth ratio terms(rat_obj) and lag_obj terms
sort gvkey year
foreach var of varlist `list2'{
local n1 = "rat_"+"`var'"
local n2 = "lag_"+"`var'"
bysort gvkey: gen `n1' = (`var'-L.`var')/L.`var'
bysort gvkey: gen `n2' = L.`var'
}



***** Generate 'market share of each firm within total/sic/sic_2d' 
foreach var of varlist `list2'{
local tot1_obj = "tot1_"+"`var'"
local wtot_obj = "wtot_"+"`var'"
bysort year : egen `tot1_obj' = total(`var')
gen `wtot_obj' = `var'/`tot1_obj'

local tot_sic_obj = "tot_sic_"+"`var'"
local wtot_sic_obj = "wtot_sic_"+"`var'"
bysort year sic: egen `tot_sic_obj' = total(`var')
gen `wtot_sic_obj' = `var'/`tot_sic_obj'

local tot_sic2_obj = "tot_sic2_"+"`var'"
local wtot_sic2_obj = "wtot_sic2_"+"`var'"
bysort year sic_2d: egen `tot_sic2_obj' = total(`var')
gen `wtot_sic2_obj' = `var'/`tot_sic2_obj'
}



***** Check default firms - by last reported year
bysort gvkey : egen last_year = max(year)
order last_year, after(year)
gen default = 1 if last_year<(a+4) & costat=="I"

***** Remove non-full sample(sample which does not exist for all of time window)
drop if t>4 | t<-3
bysort gvkey : egen num_obs = count(gvkey)
order num_obs, after(gvkey)
sort gvkey year 

***** Check starting firms - by year and age 
bysort gvkey : egen first_year = min(year)
order first_year, after(year)
gen starting = 1 if first_year>(a-3) & (year-age)>(a-3)




***** Dependent variables generating : for variables(sales, employment) themselves
***** (charat3_ : before 3-year to after 3-year)
***** (chabr_ : before 3-year to present 2-year)
***** (chara_ : present 2-year to after 3-year compared to present 2-year)
***** (chara2_ : present 2-year to after 3-year compared to before 3-year)
sort gvkey year
*** mean changes
*** (-3 / 0-1 / 3) time window
foreach var of varlist `list2'{
local charat3_obj = "charat3_"+"`var'"
bysort gvkey: gen `charat3_obj' = ((F4.`var'+F3.`var'+F2.`var')- ///
	(L3.`var'+L2.`var'+L1.`var'))/(L3.`var'+L2.`var'+L1.`var')
local chabr_obj = "chabr_"+"`var'"
bysort gvkey: gen `chabr_obj' = ((`var'+F1.`var')/2- ///
	(L3.`var'+L2.`var'+L1.`var')/3)/((L3.`var'+L2.`var'+L1.`var')/3)
local chara_obj = "chara_"+"`var'"
bysort gvkey: gen `chara_obj' = -((`var'+F1.`var')/2- ///
	(F4.`var'+F3.`var'+F2.`var')/3)/((`var'+F1.`var')/2)
local chara2_obj = "chara2_"+"`var'"
bysort gvkey: gen `chara2_obj' = -((`var'+F1.`var')/2- ///
	(F4.`var'+F3.`var'+F2.`var')/3)/((L3.`var'+L2.`var'+L1.`var')/3)
}




***** Dependent variables generating : for market share of variables(sales, employment)
***** (charat3_ : before 3-year to after 3-year)
***** (chabr_ : before 3-year to present 2-year)
***** (chara_ : present 2-year to after 3-year compared to present 2-year)
sort gvkey year
*** mean changes
*** (-3 / 0-1 / 3) time window
foreach var of varlist `list2'{

local wtot_obj = "wtot_"+"`var'"
local charat3_wtot_obj = "charat3_wtot_"+"`var'"
bysort gvkey: gen `charat3_wtot_obj' = ((F4.`wtot_obj'+F3.`wtot_obj'+F2.`wtot_obj')/3 ///
	-(L3.`wtot_obj'+L2.`wtot_obj'+L1.`wtot_obj')/3)
local chabr_wtot_obj = "chabr_wtot_"+"`var'"
bysort gvkey: gen `chabr_wtot_obj' = ((`wtot_obj'+F1.`wtot_obj')/2 ///
	-(L3.`wtot_obj'+L2.`wtot_obj'+L1.`wtot_obj')/3)
local chara_wtot_obj = "chara_wtot_"+"`var'"
bysort gvkey: gen `chara_wtot_obj' = -((`wtot_obj'+F1.`wtot_obj')/2 ///
	-(F4.`wtot_obj'+F3.`wtot_obj'+F2.`wtot_obj')/3)

local wtot_sic_obj = "wtot_sic_"+"`var'"
local charat3_wtot_sic_obj = "charat3_wtot_sic_"+"`var'"
bysort gvkey: gen `charat3_wtot_sic_obj' = ///
	((F4.`wtot_sic_obj'+F3.`wtot_sic_obj'+F2.`wtot_sic_obj')/3 ///
	-(L3.`wtot_sic_obj'+L2.`wtot_sic_obj'+L1.`wtot_sic_obj')/3)
local chabr_wtot_sic_obj = "chabr_wtot_sic_"+"`var'"
bysort gvkey: gen `chabr_wtot_sic_obj' = ///
	((`wtot_sic_obj'+F1.`wtot_sic_obj')/2 ///
	-(L3.`wtot_sic_obj'+L2.`wtot_sic_obj'+L1.`wtot_sic_obj')/3)
local chara_wtot_sic_obj = "chara_wtot_sic_"+"`var'"
bysort gvkey: gen `chara_wtot_sic_obj' = ///
	-((`wtot_sic_obj'+F1.`wtot_sic_obj')/2 ///
	-(F4.`wtot_sic_obj'+F3.`wtot_sic_obj'+F2.`wtot_sic_obj')/3)

local wtot_sic2_obj = "wtot_sic2_"+"`var'"
local charat3_wtot_sic2_obj = "charat3_wtot_sic2_"+"`var'"
bysort gvkey: gen `charat3_wtot_sic2_obj' = ///
	((F4.`wtot_sic2_obj'+F3.`wtot_sic2_obj'+F2.`wtot_sic2_obj')/3 ///
	-(L3.`wtot_sic2_obj'+L2.`wtot_sic2_obj'+L1.`wtot_sic2_obj')/3)
local chabr_wtot_sic2_obj = "chabr_wtot_sic2_"+"`var'"
bysort gvkey: gen `chabr_wtot_sic2_obj' = ///
	((`wtot_sic2_obj'+F1.`wtot_sic2_obj')/2 ///
	-(L3.`wtot_sic2_obj'+L2.`wtot_sic2_obj'+L1.`wtot_sic2_obj')/3)
local chara_wtot_sic2_obj = "chara_wtot_sic2_"+"`var'"
bysort gvkey: gen `chara_wtot_sic2_obj' = ///
	-((`wtot_sic2_obj'+F1.`wtot_sic2_obj')/2 ///
	-(F4.`wtot_sic2_obj'+F3.`wtot_sic2_obj'+F2.`wtot_sic2_obj')/3)
}



* t = 1,2,3,4,5,6,7,8,9 (5가 기준 연도 a)
replace t = t+5

save `file_y'.dta, replace
}


use "1975.dta", clear
forvalues x=1976/2014{
	append using "`x'.dta"
}

order a gvkey t
sort a gvkey t


save "pooled_firm.dta", replace
