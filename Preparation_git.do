************************************************************************************
* Describes and prepares the ICU Covid data for further analyses
* Requires (in data_dir) "DAAG COVID13_2021-08-05.dta"
* Creates (in work_dir): Preparation.log, Deleteme_Prepared_data.dta
* Creates (in Stata): Histograms of continuous comorbidity variables
* For brevity, I refer to non-covid respiratory infections as "flu"
* Takes about 7s to run
* Deleteme_Prepared_data.dta MUST BE DELETED after analyses have run
* David Carslake, July 2024
************************************************************************************
* Data and working directories must be specified

*----------
* Settings:
*----------
* Define the directories:
local data_dir = "REDACTED"
local work_dir = "REDACTED"
* Define what admission dates to include (fsa is for the flu sensitivity analysis):
local first_cov_start = date("05/02/2020","DMY")
local last_cov_start = date("01/08/2021","DMY")
local first_flu_start = date("01/02/2018","DMY")
local last_flu_start = date("31/08/2019","DMY")
local first_fsa_start = date("01/02/2020","DMY")
local last_fsa_start = date("30/06/2021","DMY")
* Should those with fate "not yet known" be censored ("censor") or excluded ("exclude")?
local nya_treatment = "censor"

*---------------
* Preliminaries:
*---------------
set more off
clear all
cd "`work_dir'"
use "`data_dir'\DAAG_COVID13_2021-08-05.dta", clear
save Deleteme_Prepared_data.dta, replace
order *, alpha
capture log close 
log using "Preparation.log", replace
display c(current_date)+", "+c(current_time)

*----------------------------------------
* Process variables defining the outcome:
*----------------------------------------
* Define, provisionally, whether follow-up ended with the event or with censoring:
generate Outcome = .
replace Outcome = 0 if last_dis_n=="Survived"
replace Outcome = 1 if last_dis_n=="Died"
if "`nya_treatment'"=="censor" replace Outcome = 0 if last_dis_n=="Not yet available"
if "`nya_treatment'"=="exclude" display `"Patients with outcome "Not yet available" will be excluded"'
* Put dates into Stata format:
generate ICU_start = date(daicu,"YMD")
format %d ICU_start
generate ICU_end = date(statusdate,"DMY")
format %d ICU_end
* Calculate follow-up time:
generate FUT = ICU_end-ICU_start
* Censor all follow-up at 30d:
tabulate Outcome if FUT>30 & FUT<., missing
replace Outcome = 0 if Outcome==1 & FUT>30 & FUT<.
replace FUT=30 if FUT>30 & FUT<.
* Assume that anyone discharged at <30d survived to 30d (but nya at <30d remain censored if not excluded):
replace FUT = 30 if last_dis_n=="Survived" & FUT>=0 & FUT<30
* Give a follow-up time of 0.5 to those who died (or were nya) on the day of admission (those who were discharged on day of admission have FUT=30):
replace FUT = 0.5 if FUT==0

*-----------------------------------------------------------------
* Process variables defining the admission condition and exposure:
*-----------------------------------------------------------------
* Make a single variable defining the admission condition:
tabulate flu covid19, missing
generate Condition = ""
replace Condition = "cov" if covid19=="Yes" & flu=="No"
replace Condition = "flu" if covid19=="No" & flu=="Yes"
* Make an alternative definition excluding thos with flu whose first reason for admission was bacterial pneumonia:
count if Condition=="flu" & desc1=="Bacterial pneumonia"
generate Condition2 = Condition
replace Condition2 = "" if Condition=="flu" & desc1=="Bacterial pneumonia"
* Check that everyone has one condition but never both:
count if Condition!="flu" & Condition!="cov"
assert r(N)==0
tabulate Condition
* Process the exposure, BMI:
generate BMI=bmi
replace BMI = wkg/(hcm/100)^2 if missing(BMI)
generate BMIcat = .
replace BMIcat = 0 if BMI < 18.5
replace BMIcat = 1 if BMI >=18.5 & BMI <25
replace BMIcat = 2 if BMI >=25.0 & BMI <30
replace BMIcat = 3 if BMI >=30.0 & BMI <35
replace BMIcat = 4 if BMI >=35.0 & BMI <40
replace BMIcat = 5 if BMI >=40.0 & BMI!=.
label define BMIcat_lbl 0 "UW" 1 "NW" 2 "OW" 3 "Ob1" 4 "Ob2" 5 "Ob3" 
label values BMIcat BMIcat_lbl 
tabulate BMIcat
* Note whether the weight and height used in BMI were estimated:
rename wkgest Weight_est
rename hcmest Height_est
tabulate Weight_est Height_est if !missing(BMI), missing
generate BMI_est = .
replace BMI_est = 1 if !missing(BMI) & Weight_est==0 & Height_est==0
replace BMI_est = 2 if !missing(BMI) & Weight_est==1 & Height_est==0
replace BMI_est = 3 if !missing(BMI) & Weight_est==0 & Height_est==1
replace BMI_est = 4 if !missing(BMI) & Weight_est==1 & Height_est==1
label define BMI_est_lbl 1 "Neither" 2 "Weight only" 3 "Height only" 4 "Both"
label values BMI_est BMI_est_lbl 
tabulate BMI_est
* Make condition-specific Z scores of BMI:
* (Note that these Z-scores are relative to the entire sample, not just the people included in the analysis)
generate zBMI = .
foreach condition in "cov" "flu"{
	display "Doing z-scores for condition `condition'"
	summarize BMI if Condition=="`condition'"
	replace zBMI = (BMI-r(mean))/r(sd) if Condition=="`condition'"
}

*------------------------------------------
* Process the socio-demographic covariates:
*------------------------------------------
* Sex, age, ethnicity, deprivation:
generate Male = cond(sex=="M",1,cond(sex=="F",0,.))
rename calage Age
mkspline "Age_cspl" = Age, cubic nknots(5) displayknots
generate Agecat = .
replace Agecat = 1 if Age>=16 & Age<35
replace Agecat = 2 if Age>=35 & Age<50
replace Agecat = 3 if Age>=50 & Age<60
replace Agecat = 4 if Age>=60 & Age<70
replace Agecat = 5 if Age>=70 & Age<80
replace Agecat = 6 if Age>=80 & Age<.
generate Ethnicity = .
replace Ethnicity = 1 if ethng=="Asian"
replace Ethnicity = 2 if ethng=="Black"
replace Ethnicity = 3 if ethng=="White"
replace Ethnicity = 4 if ethng=="Other" | ethng=="Mixed"
label define Ethnicity_lbl 1 "Asian" 2 "Black" 3 "White" 4 "Other"
label values Ethnicity Ethnicity_lbl
* (Ethnicity "Not stated" remains missing)
generate Deprivation = substr(imd2019,1,1)
destring Deprivation, replace
* Regions of admission (9 levels for descriptives):
generate Region9 = .
replace Region9 = 0 if region=="London"
replace Region9 = 1 if region=="East Of England"
replace Region9 = 2 if region=="Midlands"
replace Region9 = 3 if region=="North East And Yorkshire"
replace Region9 = 4 if region=="North West"
replace Region9 = 5 if region=="South East"
replace Region9 = 6 if region=="South West"
replace Region9 = 7 if region=="Wales"
replace Region9 = 8 if region=="Northern Ireland"
label define Region9_lbl 0 "London" 1 "East" 2 "Midlands" 3 "North East & Yorkshire" 4 "North West" 5 "South East" 6 "South West" 7 "Wales" 8 "Northern Ireland"
label values Region9 Region9_lbl
tabulate Region9, missing
* Regions of admission (6 levels for analysis):
generate Region6 = .
replace Region6 = 0 if region=="London"
replace Region6 = 1 if region=="East Of England" | region=="Midlands"
replace Region6 = 2 if region=="North East And Yorkshire" | region=="North West"
replace Region6 = 3 if region=="South East" | region=="South West"
replace Region6 = 4 if region=="Wales"
replace Region6 = 5 if region=="Northern Ireland"
label define Region6_lbl 0 "London" 1 "East England & Midlands" 2 "North East & Yorkshire & North West" 3 "South East & South West" 4 "Wales" 5 "Northern Ireland"
label values Region6 Region6_lbl
tabulate Region6, missing
* Periods of admission (approx 3 months, for analysis, condition-specific):
generate Period = .
replace Period = 1 if Condition=="cov" & ICU_start>=date("20200205","YMD") & ICU_start<=date("20200430","YMD")
replace Period = 2 if Condition=="cov" & ICU_start>=date("20200501","YMD") & ICU_start<=date("20200731","YMD")
replace Period = 3 if Condition=="cov" & ICU_start>=date("20200801","YMD") & ICU_start<=date("20201031","YMD")
replace Period = 4 if Condition=="cov" & ICU_start>=date("20201101","YMD") & ICU_start<=date("20210131","YMD")
replace Period = 5 if Condition=="cov" & ICU_start>=date("20210201","YMD") & ICU_start<=date("20210430","YMD")
replace Period = 6 if Condition=="cov" & ICU_start>=date("20210501","YMD") & ICU_start<=date("20210801","YMD")
replace Period = 1 if Condition=="flu" & ICU_start>=date("20180201","YMD") & ICU_start<=date("20180430","YMD")
replace Period = 2 if Condition=="flu" & ICU_start>=date("20180501","YMD") & ICU_start<=date("20180731","YMD")
replace Period = 3 if Condition=="flu" & ICU_start>=date("20180801","YMD") & ICU_start<=date("20181031","YMD")
replace Period = 4 if Condition=="flu" & ICU_start>=date("20181101","YMD") & ICU_start<=date("20190131","YMD")
replace Period = 5 if Condition=="flu" & ICU_start>=date("20190201","YMD") & ICU_start<=date("20190430","YMD")
replace Period = 6 if Condition=="flu" & ICU_start>=date("20190501","YMD") & ICU_start<=date("20190831","YMD")
* Sensitivity analysis: Flu during the covid period:
replace Period = 0 if Condition=="flu" & ICU_start>=date("20200201","YMD") & ICU_start<=date("20210630","YMD")
* Months of admission (for description):
generate Month = .
replace Month = 1 if ICU_start>=date("20180201","YMD") & ICU_start<=date("20180228","YMD")
replace Month = 2 if ICU_start>=date("20180301","YMD") & ICU_start<=date("20180331","YMD")
replace Month = 3 if ICU_start>=date("20180401","YMD") & ICU_start<=date("20180430","YMD")
replace Month = 4 if ICU_start>=date("20180501","YMD") & ICU_start<=date("20180531","YMD")
replace Month = 5 if ICU_start>=date("20180601","YMD") & ICU_start<=date("20180630","YMD")
replace Month = 6 if ICU_start>=date("20180701","YMD") & ICU_start<=date("20180731","YMD")
replace Month = 7 if ICU_start>=date("20180801","YMD") & ICU_start<=date("20180831","YMD")
replace Month = 8 if ICU_start>=date("20180901","YMD") & ICU_start<=date("20180930","YMD")
replace Month = 9 if ICU_start>=date("20181001","YMD") & ICU_start<=date("20181031","YMD")
replace Month = 10 if ICU_start>=date("20181101","YMD") & ICU_start<=date("20181130","YMD")
replace Month = 11 if ICU_start>=date("20181201","YMD") & ICU_start<=date("20181231","YMD")
replace Month = 12 if ICU_start>=date("20190101","YMD") & ICU_start<=date("20190131","YMD")
replace Month = 13 if ICU_start>=date("20190201","YMD") & ICU_start<=date("20190228","YMD")
replace Month = 14 if ICU_start>=date("20190301","YMD") & ICU_start<=date("20190331","YMD")
replace Month = 15 if ICU_start>=date("20190401","YMD") & ICU_start<=date("20190430","YMD")
replace Month = 16 if ICU_start>=date("20190501","YMD") & ICU_start<=date("20190531","YMD")
replace Month = 17 if ICU_start>=date("20190601","YMD") & ICU_start<=date("20190630","YMD")
replace Month = 18 if ICU_start>=date("20190701","YMD") & ICU_start<=date("20190731","YMD")
replace Month = 19 if ICU_start>=date("20190801","YMD") & ICU_start<=date("20190831","YMD")
replace Month = 25 if ICU_start>=date("20200201","YMD") & ICU_start<=date("20200229","YMD")
replace Month = 26 if ICU_start>=date("20200301","YMD") & ICU_start<=date("20200331","YMD")
replace Month = 27 if ICU_start>=date("20200401","YMD") & ICU_start<=date("20200430","YMD")
replace Month = 28 if ICU_start>=date("20200501","YMD") & ICU_start<=date("20200531","YMD")
replace Month = 29 if ICU_start>=date("20200601","YMD") & ICU_start<=date("20200630","YMD")
replace Month = 30 if ICU_start>=date("20200701","YMD") & ICU_start<=date("20200731","YMD")
replace Month = 31 if ICU_start>=date("20200801","YMD") & ICU_start<=date("20200831","YMD")
replace Month = 32 if ICU_start>=date("20200901","YMD") & ICU_start<=date("20200930","YMD")
replace Month = 33 if ICU_start>=date("20201001","YMD") & ICU_start<=date("20201031","YMD")
replace Month = 34 if ICU_start>=date("20201101","YMD") & ICU_start<=date("20201130","YMD")
replace Month = 35 if ICU_start>=date("20201201","YMD") & ICU_start<=date("20201231","YMD")
replace Month = 36 if ICU_start>=date("20210101","YMD") & ICU_start<=date("20210131","YMD")
replace Month = 37 if ICU_start>=date("20210201","YMD") & ICU_start<=date("20210228","YMD")
replace Month = 38 if ICU_start>=date("20210301","YMD") & ICU_start<=date("20210331","YMD")
replace Month = 39 if ICU_start>=date("20210401","YMD") & ICU_start<=date("20210430","YMD")
replace Month = 40 if ICU_start>=date("20210501","YMD") & ICU_start<=date("20210531","YMD")
replace Month = 41 if ICU_start>=date("20210601","YMD") & ICU_start<=date("20210630","YMD")
replace Month = 42 if ICU_start>=date("20210701","YMD") & ICU_start<=date("20210731","YMD")
replace Month = 43 if ICU_start>=date("20210801","YMD") & ICU_start<=date("20210831","YMD")
tabulate Condition Month, missing
local flu_months = `"1 "Feb '18" 2 "Mar '18" 3 "Apr '18" 4 "May '18" 5 "Jun '18" 6 "Jul '18" 7 "Aug '18" 8 "Sep '18" 9 "Oct '18" 10 "Nov '18" 11 "Dec '18" 12 "Jan '19" 13 "Feb '19" 14 "Mar '19" 15 "Apr '19" 16 "May '19" 17 "Jun '19" 18 "Jul '19" 19 "Aug '19""'
local cov_months = `"25 "Feb '20" 26 "Mar '20" 27 "Apr '20" 28 "May '20" 29 "Jun '20" 30 "Jul '20" 31 "Aug '20" 32 "Sep '20" 33 "Oct '20" 34 "Nov '20" 35 "Dec '20" 36 "Jan '21" 37 "Feb '21" 38 "Mar '21" 39 "Apr '21" 40 "May '21" 41 "Jun '21" 42 "Jul '21" 43 "Aug '21""'
label define Month_lbl `flu_months' `cov_months'
label values Month Month_lbl

*-----------------------------------
* Process the comorbidity variables:
*-----------------------------------
* Past severe illness (APACHE2 past medical history weight):
generate CM_apm = cond(ap2pmhwt==0,0,1)
tabulate CM_apm, missing
* Dependency prior to admission (2 categories):
generate CM_dep = . 
replace CM_dep = 0 if dep_cat=="Able to live without assistance in daily activities"
replace CM_dep = 1 if dep_cat=="Some (minor/major) assistance with daily activities" | dep_cat=="Total assistance with all daily activities"
label define CM_dep_lbl 0 "Independent" 1 "Some or total dependency" 
label values CM_dep CM_dep_lbl
tabulate CM_dep dep_cat
* Cardiovascular disease (past medical history):
generate CM_cvd = cond(pmh_vscd=="Yes",1,cond(pmh_vscd=="No",0,.))
tabulate CM_cvd pmh_vscd, missing
* Respiratory disease (past medical history):
generate CM_res = cond(pmh_resp=="Yes",1,cond(pmh_resp=="No",0,.))
tabulate CM_res pmh_res, missing
* Liver disease (past medical history):
generate CM_liv = cond(pmh_liver=="Yes",1,cond(pmh_liver=="No",0,.))
tabulate CM_liv pmh_liver, missing
* End stage kidney disease (past medical history):
generate CM_kid = cond(pmh_crrx=="Yes",1,cond(pmh_crrx=="No",0,.))
tabulate CM_kid pmh_crrx, missing
* Metastatic disease (past medical history):
generate CM_met = cond(pmh_meta=="Yes",1,cond(pmh_meta=="No",0,.))
tabulate CM_met pmh_meta, missing
* Haematological disease (leukemia/lymphoma, past medical history):
generate CM_hae = cond(pmh_haem=="Yes",1,cond(pmh_haem=="No",0,.))
tabulate CM_hae pmh_haem, missing
* Immunocompromised (past medical history):
generate CM_imm = cond(pmh_immun=="Yes",1,cond(pmh_immun=="No",0,.))
tabulate CM_imm pmh_immun, missing
* APACHE II composite score for illness severity in ICU:
rename ap2score CM_apa
summarize CM_apa
histogram CM_apa, freq name(hist_apa)
* ICNARC's composite acute physiological severity score in ICU:
rename imscore CM_pss
histogram CM_pss, freq name(hist_CM_pss)
* PaO2/FiO2 ratio:
rename pf CM_pfr
summarize CM_pfr
histogram CM_pfr, freq name(hist_pfr)
* Number of days advanced respiratory support:
rename au_arsd CM_ars
summarize CM_ars
histogram CM_ars, freq name(hist_ars)
* 30-day binary mortality (variable already calculated):
tabulate Outcome, missing
*-----------------------------------------------------------------------------
* The following comorbidities were calculated before but don't seem to be used:
* Needed ventilation once in ICU:
generate CM_ven = cond(vent=="Yes",1,cond(vent=="No",0,.))
tabulate CM_ven vent, missing
* Dichotomised APACHE II:
generate CM_apache2bin = cond(CM_apa==0,0,cond(CM_apa>0&CM_apa<.,1,.))
tabulate CM_apa CM_apache2bin, missing
* Dependency prior to admission (3 categories):
generate CM_dep3 = . 
replace CM_dep3 = 0 if dep_cat=="Able to live without assistance in daily activities"
replace CM_dep3 = 1 if dep_cat=="Some (minor/major) assistance with daily activities"
replace CM_dep3 = 2 if dep_cat=="Total assistance with all daily activities"
label define CM_dep3_lbl 0 "Independent" 1 "Some dependency" 2 "Total dependency" 
label values CM_dep3 CM_dep3_lbl
tabulate CM_dep3 dep_cat

*----------------------------
* Create inclusion variables:
*----------------------------
* (Initially, Useme=1 indicates inclusion for the main analysis, Useme=2 is a mutually exclusive group of flu patients during covid used in a sensitivity analysis)
* (Descriptives and other sensitivity analyses use subsets of the main analysis set (Useme1=1))
* Select people who were admitted within the condition-specific study periods:
generate Useme = 0
replace Useme = 1 if Condition=="cov" & (ICU_start>=`first_cov_start' & ICU_start<=`last_cov_start')
replace Useme = 1 if Condition=="flu" & (ICU_start>=`first_flu_start' & ICU_start<=`last_flu_start')
replace Useme = 2 if Condition=="flu" & (ICU_start>=`first_fsa_start' & ICU_start<=`last_fsa_start')
tabulate Useme Condition , missing
foreach condition in "cov" "flu"{
	* Exclude those who are under 16 (missing age will be excluded later):
	replace Useme = 0 if Condition=="`condition'" & Age<16
	tabulate Useme if Condition=="`condition'", missing
	* Exclude those with missing or implausible data on outcome or follow-up time:
	replace Useme = 0 if Condition=="`condition'" & missing(Outcome)
	replace Useme = 0 if Condition=="`condition'" & missing(FUT)
	replace Useme = 0 if Condition=="`condition'" & FUT<0
	tabulate Useme if Condition=="`condition'", missing
	* Exclude those with missing data on the exposure:
	replace Useme = 0 if Condition=="`condition'" & missing(BMI)
	tabulate Useme if Condition=="`condition'", missing
	* Exclude those with missing data on any adjustment variable:
	local cvs = "Male Age Ethnicity Deprivation Region6 Period"
	foreach cv of local cvs{
		display "`cv'"
		replace Useme = 0 if Condition=="`condition'" & missing(`cv')
	}
	tabulate Useme if Condition=="`condition'", missing
}
* Inclusion variable for the main analyses:
generate Useme1 = cond(Useme==1,1,0)
tabulate Useme1 Condition
* Inclusion variable for the sensitivity analysis of flu during the pandemic:
generate Useme2 = cond(Useme==2,1,0)
tabulate Useme2 Condition
* Inclusion variable for the sensitivity analysis excluding estimated BMI:
generate Useme3 = cond(Useme==1 & BMI_est==1,1,0)
tabulate Useme3 Condition
* Inclusion variable for a sensitivity analysis excluding those with comorbidities:
* (This excludes many more (45%) in flu than in cov (18%), and many of these variables are plausible mediators of a BMI effect)
* (It is thus extremely vulnerable to bias and was omitted from the published paper)
generate Useme4 = cond(Useme==1 & max(CM_apm, CM_dep, CM_cvd, CM_res, CM_liv, CM_kid, CM_met, CM_hae, CM_imm)==0,1,0)
tabulate Useme4 Condition
* Inclusion variable for the sensitivity analysis excluding those with some or total dependency:
* (Dependency is not such a likely mediator of a BMI effect on mortality)
generate Useme5 = cond(Useme==1 & CM_dep==0,1,0)
tabulate Useme5 Condition
drop Useme

*------------
* Wrap it up:
*------------
compress
save Deleteme_Prepared_data.dta, replace
display c(current_date)+", "+c(current_time)
log close







