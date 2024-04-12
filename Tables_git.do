************************************************************************************************************************
* Generates data for tables in the BMI-covid paper using ICNARC data
* Requires (in work_dir): Deleteme_Prepared_data.dta
* Creates (in work_dir): Tables.log, T1.dta, T2toT4.dta, TS1toTS9.dta
* Creates (in work_dir): Plots_1_***_*.gph (survivor and hazard functions for various conditions and inclusion criteria)
* Creates (in work_dir): NA_*_***_*_obs.gph (Nelson-Aalen plots for various conditions and inclusion criteria)
* Takes about 14 minutes to run
* David Carslake, April 2024
************************************************************************************************************************
* Working directory must be specified

*----------
* Settings:
*----------
* Define the directories:
local work_dir = "REDACTED"
* Distribution to use for parametric survival analyses (weibull or gompertz):
local psa_distribution = "gompertz"

*---------------
* Preliminaries:
*---------------
set more off
clear all
cd "`work_dir'"
use "`work_dir'\Deleteme_Prepared_data.dta", clear
save delme.dta, replace
order *, alpha
capture log close 
log using "Tables.log", replace
display c(current_date)+", "+c(current_time)

*------------------------------------
* Table 1. Descriptives by condition:
*------------------------------------
* Make a place to store the results:
tempname memhold_T1
postfile `memhold_T1' str3 Condition Usemevbl str16 Variable str3 Type Mean SD Ntot using "T1.dta", replace
* List the descriptive variables by type:
local continuous_vbls = "Age BMI CM_apa CM_pss CM_pfr CM_ars"
local categorical_vbls = "Agecat Ethnicity Deprivation Region9 BMIcat"
local binary_vbls = "Male CM_apm CM_dep CM_cvd CM_res CM_liv CM_kid CM_met CM_hae CM_imm Outcome"
* Loop through combinations of Condition and Usemevbl:
foreach condition_useme in cov_1 flu_1 flu_2{
	tokenize "`condition_useme'", parse("_")
	local condition = "`1'"
	local usemevbl = `3'
	* Loop through the variables:
	foreach vbl in `continuous_vbls' `categorical_vbls' `binary_vbls'{
		display "Doing condition `condition' with Useme`usemevbl'==1 and variable `vbl'"
		* Define the type of variable:
		if regexm(" `continuous_vbls' "," `vbl' ")==1 local type = "con"
		if regexm(" `binary_vbls' "," `vbl' ")==1 local type = "bin"
		if regexm(" `categorical_vbls' "," `vbl' ")==1 local type = "cat"
		* Get the total sample size:
		count if Condition=="`condition'" & Useme`usemevbl'==1 & !missing(`vbl')
		local Ntot = r(N)
		* Generate other descriptive stats according to the variable type:
		if "`type'"=="con"{
			summarize `vbl' if Condition=="`condition'" & Useme`usemevbl'==1 & !missing(`vbl')
			post `memhold_T1' ("`condition'") (`usemevbl') ("`vbl'") ("`type'") (r(mean)) (r(sd)) (`Ntot') 	
		}
		if "`type'"=="bin"{
			summarize `vbl' if Condition=="`condition'" & Useme`usemevbl'==1 & !missing(`vbl')
			post `memhold_T1' ("`condition'") (`usemevbl') ("`vbl'") ("`type'") (r(mean)) (.) (`Ntot') 	
		}
		if "`type'"=="cat"{
			levelsof(`vbl'), local(levels)
			foreach level of local levels{
				count if Condition=="`condition'" & Useme`usemevbl'==1 & `vbl'==`level'
				local mean = r(N)/`Ntot'
				post `memhold_T1' ("`condition'") (`usemevbl') ("`vbl'_`level'") ("`type'") (`mean') (.) (`Ntot') 	
			}
		}
	}
}
postclose `memhold_T1'

*---------------------------------------
* Tables 2-4, S10 and X1. Mortality~BMI:
*---------------------------------------
* Make a place to store the results:
tempname memhold_T2toT4
postfile `memhold_T2toT4' str10 Conditionvbl str3 Condition Usemevbl str8 Stratification Splitperiod str8 Exposure Beta SE N NCadmin NCother D Dlater Pph Pint using "T2toT4.dta", replace
* stset the data:
stset FUT, failure(Outcome) id(recordid)
* Define the adjustment set (except for period & region):
*local adjustment = "c.Male c.Age i.Ethnicity i.Deprivation"
local adjustment = "c.Male c.Age_cspl1 c.Age_cspl2 c.Age_cspl3 c.Age_cspl4 i.Ethnicity i.Deprivation"
* Define combinations of Conditionvbl-Condition-Usemevbl (ccu):
* (Conditionvbl 2 excludes bacterial pneumonia from the definition of flu)
foreach ccu in 1_cov_1 1_flu_1 1_flu_2 2_flu_1 1_cov_3 1_flu_3 1_cov_4 1_flu_4{
	tokenize "`ccu'", parse("_")
	local conditionvbl = cond(`1'==1,"Condition",cond(`1'==2,"Condition2","ERROR"))
	local condition = "`3'"
	local usemevbl = `5'
	display "Doing `conditionvbl'==`condition' and Useme`usemevbl'==1"
	* 1. Continuous BMI, unstratified:
	streg zBMI `adjustment' i.Region6 i.Period if `conditionvbl'=="`condition'" & Useme`usemevbl'==1, distribution(`psa_distribution')
	display "Numbers experiencing event or censored before 30d and overall in all periods and regions:"
	count if `conditionvbl'=="`condition'" & Useme`usemevbl'==1 & (ICU_end-ICU_start)>30
	local NCadmin = r(N)
	count if `conditionvbl'=="`condition'" & Useme`usemevbl'==1 & (ICU_end-ICU_start)<=30 & last_dis_n=="Not yet available"
	local NCother = r(N)
	count if `conditionvbl'=="`condition'" & Useme`usemevbl'==1 & (ICU_end-ICU_start)>30 & last_dis_n=="Died"
	local Dlater = r(N)
	summarize Outcome if `conditionvbl'=="`condition'" & Useme`usemevbl'==1
	local N = r(N)
	assert e(N)==`N'
	local meanY = r(mean)
	local D = round(`meanY'*`N',1)
	if `N'<5 post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("None") (.) ("zBMI") (.a) (.a) (`N') (`NCadmin') (`NCother') (`D') (`Dlater') (.) (.) 
	else if `meanY'==0 post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("None") (.) ("zBMI") (.b) (.b) (`N') (`NCadmin') (`NCother') (`D') (`Dlater') (.) (.)
	else if `meanY'==1 post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("None") (.) ("zBMI") (.c) (.c) (`N') (`NCadmin') (`NCother') (`D') (`Dlater') (.) (.)
	else post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("None") (.) ("zBMI") (_b[zBMI]) (_se[zBMI]) (`N') (`NCadmin') (`NCother') (`D') (`Dlater') (.) (.)
	if "`ccu'"=="1_cov_1" | "`ccu'"=="1_flu_1" | "`ccu'"=="1_flu_2"{
		stcurve, hazard yscale(range(0 0.02)) name(ht_`ccu'_fit, replace)
		stcurve, survival yscale(range(0 1)) name(St_`ccu'_fit, replace)
		sts graph if `conditionvbl'=="`condition'" & Useme`usemevbl'==1, ci hazard yscale(range(0 0.02)) name(ht_`ccu'_obs, replace) legend(off)
		sts graph if `conditionvbl'=="`condition'" & Useme`usemevbl'==1, ci survival yscale(range(0 1)) name(St_`ccu'_obs, replace) legend(off)
		graph combine ht_`ccu'_fit St_`ccu'_fit ht_`ccu'_obs St_`ccu'_obs, rows(2) name(Plots_`ccu', replace) saving(Plots_`ccu'.gph, replace)
		graph drop ht_`ccu'_fit St_`ccu'_fit ht_`ccu'_obs St_`ccu'_obs
	}
	* 2. Continuous BMI, split at median time to death to test PH:
	summarize _t if `conditionvbl'=="`condition'" & Useme`usemevbl'==1 & _d==1, detail
	local median = r(p50)
	display "Median time to death for `conditionvbl'==`condition' and Useme`usemevbl'==1 is `median' days"
	stsplit Splitperiod, at(0 `median')
	streg i.Splitperiod#c.zBMI `adjustment' i.Region6 i.Period if `conditionvbl'=="`condition'" & Useme`usemevbl'==1, distribution(`psa_distribution')
	test 0b.Splitperiod#c.zBMI = `median'.Splitperiod#c.zBMI
	local Pph = r(p)
	summarize Outcome if `conditionvbl'=="`condition'" & Useme`usemevbl'==1 & Splitperiod==0
	local N = r(N)
	local meanY = r(mean)
	local D = round(`meanY'*`N',1)
	if `N'<5 post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("None") (0) ("zBMI") (.a) (.a) (`N') (.) (.) (`D') (.) (`Pph') (.)
	else if `meanY'==0 post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("None") (0) ("zBMI") (.b) (.b) (`N') (.) (.) (`D') (.) (`Pph') (.)
	else if `meanY'==1 post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("None") (0) ("zBMI") (.c) (.c) (`N') (.) (.) (`D') (.) (`Pph') (.)
	else post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("None") (0) ("zBMI") (_b[0b.Splitperiod#c.zBMI]) (_se[0b.Splitperiod#c.zBMI]) (`N') (.) (.) (`D') (.) (`Pph') (.)
	summarize Outcome if `conditionvbl'=="`condition'" & Useme`usemevbl'==1 & Splitperiod==`median'
	local N = r(N)
	local meanY = r(mean)
	local D = round(`meanY'*`N',1)
	if `N'<5 post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("None") (`median') ("zBMI") (.a) (.a) (`N') (.) (.) (`D') (.) (`Pph') (.)
	else if `meanY'==0 post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("None") (`median') ("zBMI") (.b) (.b) (`N') (.) (.) (`D') (.) (`Pph') (.)
	else if `meanY'==1 post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("None") (`median') ("zBMI") (.c) (.c) (`N') (.) (.) (`D') (.) (`Pph') (.)
	else post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("None") (`median') ("zBMI") (_b[`median'.Splitperiod#c.zBMI]) (_se[`median'.Splitperiod#c.zBMI]) (`N') (.) (.) (`D') (.) (`Pph') (.)
	drop Splitperiod
	stjoin
	* 3. Categorical BMI, unstratified:
	streg ib1.BMIcat `adjustment' i.Region6 i.Period if `conditionvbl'=="`condition'" & Useme`usemevbl'==1, distribution(`psa_distribution')
	foreach cat in "0" "1" "2" "3" "4" "5"{
		post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("None") (.) ("BMIcat`cat'") (_b[`cat'.BMIcat]) (_se[`cat'.BMIcat]) (e(N)) (.) (.) (.) (.) (.) (.)
	}
	sts graph if `conditionvbl'=="`condition'" & Useme`usemevbl'==1, by(BMIcat) na yscale(log) xscale(log) name(NA_`ccu'_obs, replace) saving(NA_`ccu'_obs.gph, replace)
	* Stratified analyses aren't applied to the sensitivity analyses:
	if "`conditionvbl'"=="Condition" & `usemevbl'==1{
		* 4. Continuous BMI, stratification by Period:
		streg i.Period i.Period#(c.zBMI `adjustment' i.Region6) if `conditionvbl'=="`condition'" & Useme`usemevbl'==1, distribution(`psa_distribution') anc(i.Period)
		test 1b.Period#c.zBMI = 2.Period#c.zBMI = 3.Period#c.zBMI = 4.Period#c.zBMI = 5.Period#c.zBMI = 6.Period#c.zBMI
		local Pint = r(p)
		foreach period in "1" "2" "3" "4" "5" "6"{
			display "Numbers experiencing event or censored before 30d and overall in period `period':"
			
			count if `conditionvbl'=="`condition'" & Useme`usemevbl'==1 & Period==`period' & (ICU_end-ICU_start)>30
			local NCadmin = r(N)
			count if `conditionvbl'=="`condition'" & Useme`usemevbl'==1 & Period==`period' & (ICU_end-ICU_start)<=30 & last_dis_n=="Not yet available"
			local NCother = r(N)
			count if `conditionvbl'=="`condition'" & Useme`usemevbl'==1 & Period==`period' & (ICU_end-ICU_start)>30 & last_dis_n=="Died"
			local Dlater = r(N)

			summarize Outcome if `conditionvbl'=="`condition'" & Useme`usemevbl'==1 & Period==`period'
			local N = r(N)
			local meanY = r(mean)
			local D = round(`meanY'*`N',1)
			if `N'<5 post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("Period`period'") (.) ("zBMI") (.a) (.a) (`N') (`NCadmin') (`NCother') (`D') (`Dlater') (.) (`Pint')
			else if `meanY'==0 post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("Period`period'") (.) ("zBMI") (.b) (.b) (`N') (`NCadmin') (`NCother') (`D') (`Dlater') (.) (`Pint')
			else if `meanY'==1 post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("Period`period'") (.) ("zBMI") (.c) (.c) (`N') (`NCadmin') (`NCother') (`D') (`Dlater') (.) (`Pint')
			else post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("Period`period'") (.) ("zBMI") (_b[`period'.Period#zBMI]) (_se[`period'.Period#zBMI]) (`N') (`NCadmin') (`NCother') (`D') (`Dlater') (.) (`Pint')
		}
		* 5. Categorical BMI, stratification by Period:
		streg i.Period i.Period#(ib1.BMIcat `adjustment' i.Region6) if `conditionvbl'=="`condition'" & Useme`usemevbl'==1, distribution(`psa_distribution') anc(i.Period)
		foreach cat in "0" "1" "2" "3" "4" "5"{
			foreach period in "1" "2" "3" "4" "5" "6"{
				* (Nb: Although coefficients for, and interacting with, reference values are given "b" and "o" suffixes in e(b), Stata will recognise the unsuffixed names)
				summarize Outcome if `conditionvbl'=="`condition'" & Useme`usemevbl'==1 & Period==`period' & BMIcat==`cat'
				local N = r(N)
				local meanY = r(mean)
				local D = round(`meanY'*`N',1)
				if `N'<5 post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("Period`period'") (.) ("BMIcat`cat'") (.a) (.a) (`N') (.) (.) (`D') (.) (.) (.)
				else if `meanY'==0 post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("Period`period'") (.) ("BMIcat`cat'") (.b) (.b) (`N') (.) (.) (`D') (.) (.) (.)
				else if `meanY'==1 post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("Period`period'") (.) ("BMIcat`cat'") (.c) (.c) (`N') (.) (.) (`D') (.) (.) (.)
				else post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("Period`period'") (.) ("BMIcat`cat'") (_b[`period'.Period#`cat'.BMIcat]) (_se[`period'.Period#`cat'.BMIcat]) (`N') (.) (.) (`D') (.) (.) (.)
			}
		}
		* 6. Continuous BMI, stratification by Region6:
		streg i.Region6 i.Region6#(c.zBMI `adjustment' i.Period) if `conditionvbl'=="`condition'" & Useme`usemevbl'==1, distribution(`psa_distribution') anc(i.Region6)
		test 0b.Region6#c.zBMI = 1.Region6#c.zBMI = 2.Region6#c.zBMI = 3.Region6#c.zBMI = 4.Region6#c.zBMI = 5.Region6#c.zBMI
		local Pint = r(p)
		foreach region6 in "0" "1" "2" "3" "4" "5"{
			display "Numbers experiencing event or censored before 30d and overall in region `region':"
			
			count if `conditionvbl'=="`condition'" & Useme`usemevbl'==1 & Region6==`region6' & (ICU_end-ICU_start)>30
			local NCadmin = r(N)
			count if `conditionvbl'=="`condition'" & Useme`usemevbl'==1 & Region6==`region6' & (ICU_end-ICU_start)<=30 & last_dis_n=="Not yet available"
			local NCother = r(N)
			count if `conditionvbl'=="`condition'" & Useme`usemevbl'==1 & Region6==`region6' & (ICU_end-ICU_start)>30 & last_dis_n=="Died"
			local Dlater = r(N)
			
			summarize Outcome if `conditionvbl'=="`condition'" & Useme`usemevbl'==1 & Region6==`region6'
			local N = r(N)
			local meanY = r(mean)
			local D = round(`meanY'*`N',1)
			if `N'<5 post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("Region6`region6'") (.) ("zBMI") (.a) (.a) (`N') (`NCadmin') (`NCother') (`D') (`Dlater') (.) (`Pint')
			else if `meanY'==0 post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("Region6`region6'") (.) ("zBMI") (.b) (.b) (`N') (`NCadmin') (`NCother') (`D') (`Dlater') (.) (`Pint')
			else if `meanY'==1 post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("Region6`region6'") (.) ("zBMI") (.c) (.c) (`N') (`NCadmin') (`NCother') (`D') (`Dlater') (.) (`Pint')
			else post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("Region6`region6'") (.) ("zBMI") (_b[`region6'.Region6#zBMI]) (_se[`region6'.Region6#zBMI]) (`N') (`NCadmin') (`NCother') (`D') (`Dlater') (.) (`Pint')
		}
		* 7. Categorical BMI, stratification by Region6:
		streg i.Region6 i.Region6#(ib1.BMIcat `adjustment' i.Period) if `conditionvbl'=="`condition'" & Useme`usemevbl'==1, distribution(`psa_distribution') anc(i.Region6)
		foreach cat in "0" "1" "2" "3" "4" "5"{
			foreach region6 in "0" "1" "2" "3" "4" "5"{
				summarize Outcome if `conditionvbl'=="`condition'" & Useme`usemevbl'==1 & Region6==`region6' & BMIcat==`cat'
				local N = r(N)
				local meanY = r(mean)
				local D = round(`meanY'*`N',1)
				if `N'<5 post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("Region6`region6'") (.) ("BMIcat`cat'") (.a) (.a) (`N') (.) (.) (`D') (.) (.) (.)
				else if `meanY'==0 post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("Region6`region6'") (.) ("BMIcat`cat'") (.b) (.b) (`N') (.) (.) (`D') (.) (.) (.)
				else if `meanY'==1 post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("Region6`region6'") (.) ("BMIcat`cat'") (.c) (.c) (`N') (.) (.) (`D') (.) (.) (.)
				else post `memhold_T2toT4' ("`conditionvbl'") ("`condition'") (`usemevbl') ("Region6`region6'") (.) ("BMIcat`cat'") (_b[`region6'.Region6#`cat'.BMIcat]) (_se[`region6'.Region6#`cat'.BMIcat]) (`N') (.) (.) (`D') (.) (.) (.)
			}
		}
	}
}
postclose `memhold_T2toT4'	

*---------------------------------------------------------------------------------------------------------------------------------
* Tables S1-S9 (confounder/selector associations with exposure and outcome, with and without stratification by period and region):
*---------------------------------------------------------------------------------------------------------------------------------
* Make a place to store the results:
tempname memhold_TS1toTS9
postfile `memhold_TS1toTS9' str7 Statification Stratlevel str3 Condition str16 Variable str3 Type str4 Outcome Beta SE Ntot Ncat Dtot Dcat Phet1 Phet2 using "TS1toTS9.dta", replace
* (Phet1 compares coefficients for flu and covid, Phet2 compares stratification groups)
* List the potential confounders/selectors by type:
local continuous_vbls = "CM_apa CM_pss CM_pfr CM_ars Deprivation"
local binary_vbls = "CM_apm CM_dep CM_cvd CM_res CM_liv CM_kid CM_met CM_hae CM_imm Ethnicity_1 Ethnicity_2 Ethnicity_3 Ethnicity_4 "
* Make some indicator variables from categorical variables:
tabulate Ethnicity, generate(Ethnicity_)
* stset the data:
stset FUT, failure(Outcome) id(recordid)
* Define the adjustment set:
local adjustment = "c.Male c.Age_cspl1 c.Age_cspl2 c.Age_cspl3 c.Age_cspl4"
* Make a binary indicator for Condition:
generate IsCovid = cond(Condition=="cov",1,cond(Condition=="flu",0,.))
* Loop through the outcomes and variables:
foreach outcome in BMI Mortality{
	foreach vbl in `continuous_vbls' `binary_vbls'{
		display "Doing outcome `outcome' and variable `vbl'"
		* Define the type of variable and add prefixes:
		if regexm(" `continuous_vbls' "," `vbl' ")==1 local type = "con"
		if regexm(" `binary_vbls' "," `vbl' ")==1 local type = "bin"	
		* 1. No stratification:
		if "`outcome'"=="BMI" regress BMI ibn.IsCovid ibn.IsCovid#(c.`vbl' `adjustment') if Useme1==1, noconstant
		if "`outcome'"=="Mortality" streg ibn.IsCovid ibn.IsCovid#(c.`vbl' `adjustment') if Useme1==1, noconstant distribution(`psa_distribution') anc(IsCovid)
		test 0.IsCovid#c.`vbl' = 1.IsCovid#c.`vbl'
		local Phet1 = r(p)
		forvalues iscovid = 0/1{
			local condition = word("flu cov",`iscovid'+1)
			count if Useme1==1 & IsCovid==`iscovid' & !missing(`vbl')
			local Ntot = r(N)
			count if Useme1==1 & IsCovid==`iscovid' & !missing(`vbl') & `vbl'==1
			local Ncat = cond("`type'"=="bin",r(N),.)
			count if Useme1==1 & IsCovid==`iscovid' & !missing(`vbl') & Outcome==1
			local Dtot = cond("`outcome'"=="Mortality",r(N),.)
			count if Useme1==1 & IsCovid==`iscovid' & !missing(`vbl') & `vbl'==1 & Outcome==1
			local Dcat = cond("`outcome'"=="Mortality" & "`type'"=="bin",r(N),.)
			* Don't post estimates if there are <5 people in a category (identifiable) or if there's no variation in the mortality outcome:
			if (`Ntot'<5 | `Ncat'<5) post `memhold_TS1toTS9' ("None") (.) ("`condition'") ("`vbl'") ("`type'") ("`outcome'") (.a) (.a) (`Ntot') (`Ncat') (`Dtot') (`Dcat') (`Phet1') (.)
			else if "`outcome'"=="Mortality" & (`Dtot'==0 | `Dcat'==0) post `memhold_TS1toTS9' ("None") (.) ("`condition'") ("`vbl'") ("`type'") ("`outcome'") (.b) (.b) (`Ntot') (`Ncat') (`Dtot') (`Dcat') (`Phet1') (.)
			else if "`outcome'"=="Mortality" & (`Dtot'==`Ntot' | ("`type'"=="bin" & (`Dcat'==`Ncat'))) post `memhold_TS1toTS9' ("None") (.) ("`condition'") ("`vbl'") ("`type'") ("`outcome'") (.c) (.c) (`Ntot') (`Ncat') (`Dtot') (`Dcat') (`Phet1') (.)
			else post `memhold_TS1toTS9' ("None") (.) ("`condition'") ("`vbl'") ("`type'") ("`outcome'") (_b[`iscovid'.IsCovid#c.`vbl']) (_se[`iscovid'.IsCovid#c.`vbl']) (`Ntot') (`Ncat') (`Dtot') (`Dcat') (`Phet1') (.)
		}
		* 2. Stratification by Period:
		levelsof Period if Useme1==1, local(slevels)
		if "`outcome'"=="BMI" regress BMI ibn.Period#ibn.IsCovid ibn.Period#ibn.IsCovid#(c.`vbl' `adjustment') if Useme1==1, noconstant
		if "`outcome'"=="Mortality" streg ibn.Period#ibn.IsCovid ibn.Period#ibn.IsCovid#(c.`vbl' `adjustment') if Useme1==1, noconstant distribution(`psa_distribution') anc(IsCovid#Period)
		forvalues iscovid = 0/1{
			local condition = word("flu cov",`iscovid'+1)
			test 1.Period#`iscovid'.IsCovid#c.`vbl' = 2.Period#`iscovid'.IsCovid#c.`vbl' = 3.Period#`iscovid'.IsCovid#c.`vbl' = 4.Period#`iscovid'.IsCovid#c.`vbl' = 5.Period#`iscovid'.IsCovid#c.`vbl' = 6.Period#`iscovid'.IsCovid#c.`vbl'
			local Phet2 = r(p)
			foreach slevel of local slevels{
				count if Useme1==1 & Period==`slevel' & Condition=="`condition'" & !missing(`vbl')
				local Ntot = r(N)
				count if Useme1==1 & Period==`slevel' & Condition=="`condition'" & !missing(`vbl') & `vbl'==1
				local Ncat = cond("`type'"=="bin",r(N),.)
				count if Useme1==1 & Period==`slevel' & Condition=="`condition'" & !missing(`vbl') & Outcome==1
				local Dtot = cond("`outcome'"=="Mortality",r(N),.)
				count if Useme1==1 & Period==`slevel' & Condition=="`condition'" & !missing(`vbl') & `vbl'==1 & Outcome==1
				local Dcat = cond("`outcome'"=="Mortality" & "`type'"=="bin",r(N),.)
				* Don't post estimates if there are <5 people in a category (identifiable) or if there's no variation in the mortality outcome:
				if (`Ntot'<5 | `Ncat'<5) post `memhold_TS1toTS9' ("Period") (`slevel') ("`condition'") ("`vbl'") ("`type'") ("`outcome'") (.a) (.a) (`Ntot') (`Ncat') (`Dtot') (`Dcat') (.) (`Phet2')
				else if "`outcome'"=="Mortality" & (`Dtot'==0 | `Dcat'==0) post `memhold_TS1toTS9' ("Period") (`slevel') ("`condition'") ("`vbl'") ("`type'") ("`outcome'") (.b) (.b) (`Ntot') (`Ncat') (`Dtot') (`Dcat') (.) (`Phet2')
				else if "`outcome'"=="Mortality" & (`Dtot'==`Ntot' | ("`type'"=="bin" & (`Dcat'==`Ncat'))) post `memhold_TS1toTS9' ("Period") (`slevel') ("`condition'") ("`vbl'") ("`type'") ("`outcome'") (.c) (.c) (`Ntot') (`Ncat') (`Dtot') (`Dcat') (.) (`Phet2')
				else post `memhold_TS1toTS9' ("Period") (`slevel') ("`condition'") ("`vbl'") ("`type'") ("`outcome'") (_b[`slevel'.Period#`iscovid'.IsCovid#c.`vbl']) (_se[`slevel'.Period#`iscovid'.IsCovid#c.`vbl']) (`Ntot') (`Ncat') (`Dtot') (`Dcat') (.) (`Phet2')
			} 
		}
		* 3. Stratification by Region6:
		levelsof Region6 if Useme1==1, local(slevels)
		if "`outcome'"=="BMI" regress BMI ibn.Region6#ibn.IsCovid ibn.Region6#ibn.IsCovid#(c.`vbl' `adjustment') if Useme1==1, noconstant
		if "`outcome'"=="Mortality" streg ibn.Region6#ibn.IsCovid ibn.Region6#ibn.IsCovid#(c.`vbl' `adjustment') if Useme1==1, noconstant distribution(`psa_distribution') anc(IsCovid#Region6)
		forvalues iscovid = 0/1{
			local condition = word("flu cov",`iscovid'+1)
			test 0.Region6#`iscovid'.IsCovid#c.`vbl' = 1.Region6#`iscovid'.IsCovid#c.`vbl' = 2.Region6#`iscovid'.IsCovid#c.`vbl' = 3.Region6#`iscovid'.IsCovid#c.`vbl' = 4.Region6#`iscovid'.IsCovid#c.`vbl' = 5.Region6#`iscovid'.IsCovid#c.`vbl'
			local Phet2 = r(p)
			foreach slevel of local slevels{
				count if Useme1==1 & Region6==`slevel' & Condition=="`condition'" & !missing(`vbl')
				local Ntot = r(N)
				count if Useme1==1 & Region6==`slevel' & Condition=="`condition'" & !missing(`vbl') & `vbl'==1
				local Ncat = cond("`type'"=="bin",r(N),.)
				count if Useme1==1 & Region6==`slevel' & Condition=="`condition'" & !missing(`vbl') & Outcome==1
				local Dtot = cond("`outcome'"=="Mortality",r(N),.)
				count if Useme1==1 & Region6==`slevel' & Condition=="`condition'" & !missing(`vbl') & `vbl'==1 & Outcome==1
				local Dcat = cond("`outcome'"=="Mortality" & "`type'"=="bin",r(N),.)
				* Don't post estimates if there are <5 people in a category (identifiable) or if there's no variation in the mortality outcome:
				if (`Ntot'<5 | `Ncat'<5) post `memhold_TS1toTS9' ("Region6") (`slevel') ("`condition'") ("`vbl'") ("`type'") ("`outcome'") (.a) (.a) (`Ntot') (`Ncat') (`Dtot') (`Dcat') (.) (`Phet2')
				else if "`outcome'"=="Mortality" & (`Dtot'==0 | `Dcat'==0) post `memhold_TS1toTS9' ("Region6") (`slevel') ("`condition'") ("`vbl'") ("`type'") ("`outcome'") (.b) (.b) (`Ntot') (`Ncat') (`Dtot') (`Dcat') (.) (`Phet2')
				else if "`outcome'"=="Mortality" & (`Dtot'==`Ntot' | ("`type'"=="bin" & (`Dcat'==`Ncat'))) post `memhold_TS1toTS9' ("Region6") (`slevel') ("`condition'") ("`vbl'") ("`type'") ("`outcome'") (.c) (.c) (`Ntot') (`Ncat') (`Dtot') (`Dcat') (.) (`Phet2')
				else post `memhold_TS1toTS9' ("Region6") (`slevel') ("`condition'") ("`vbl'") ("`type'") ("`outcome'") (_b[`slevel'.Region6#`iscovid'.IsCovid#c.`vbl']) (_se[`slevel'.Region6#`iscovid'.IsCovid#c.`vbl']) (`Ntot') (`Ncat') (`Dtot') (`Dcat') (.) (`Phet2')
			} 
		}
	}
}
postclose `memhold_TS1toTS9'	

*------------
* Wrap it up:
*------------
display c(current_date)+", "+c(current_time)
log close
