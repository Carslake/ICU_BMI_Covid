************************************************************************************************************************
* Draws figures for the BMI-covid paper using ICNARC data
* Figure 2 is created separately as a Powerpoint file
* Requires (in work_dir): Deleteme_Prepared_data.dta
* Creates  (in work_dir): Figures.log, F1.pdf, FS1.pdf-FS13.pdf, F3.pdf, FS14.pdf-FS15.pdf
* Takes about 11 mins to run
* David Carslake, July 2024
************************************************************************************************************************
* Working directory must be specified

*----------
* Settings:
*----------
* Define the directories:
local work_dir = "REDACTED"
* The means and SD used to standardise BMI (from Preparation.log):
local Mean_BMI_cov = 30.84966
local SD_BMI_cov = 7.627248
local Mean_BMI_flu = 27.12243
local SD_BMI_flu = 7.517702
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
log using "Figures.log", replace
display c(current_date)+", "+c(current_time)

*------------------------------------------
* Figure 1 (admissions by month & region6):
*------------------------------------------
set graphics off
local general_options_month = `"frequency width(1) scheme(s2mono) graphregion(color(white)) lwidth(0) ylabel(0 2000 4000 6000 8000) xsize(3.2) ysize(2.5) ytitle("Frequency", size(medlarge))"'
local p1_options = `"start(23.999999) xscale(range(23.5, 44.5)) xtitle("Date admitted to ICU with COVID-19", size(medlarge)) title("COVID-19 patients", size(medlarge) position(11))"'
local p1_xlabels = `" xlabel(24 "1 Jan 2020" 30 "1 Jul 2020" 36 "1 Jan 2021" 42 "1 Jul 2021") xmticks(24(1)44, tlength(*0.5))"'
histogram Month if Useme1==1 & Condition=="cov", `general_options_month' `p1_options' `p1_xlabels' name(p1, replace) 
local p2_options = `"start(0.999999) xscale(range(-0.5, 20.5)) xtitle("Date admitted to ICU with non-COVID-19", size(medlarge)) title("Non-COVID-19 patients, before pandemic", size(medlarge) position(11))"'
local p2_xlabels = `"xlabel(0 "1 Jan 2018" 6 "1 Jul 2018" 12 "1 Jan 2019" 18 "1 Jul 2019") xmticks(0(1)20, tlength(*0.5))"'
histogram Month if Useme1==1 & Condition=="flu", `general_options_month' `p2_options' `p2_xlabels' name(p2, replace) 
local p3_options = `"start(23.999999) xscale(range(23.5, 44.5)) xtitle("Date admitted to ICU with non-COVID-19", size(medlarge)) title("Non-COVID-19 patients, during pandemic", size(medlarge) position(11))"'
local p3_xlabels = `" xlabel(24 "1 Jan 2020" 30 "1 Jul 2020" 36 "1 Jan 2021" 42 "1 Jul 2021") xmticks(24(1)44, tlength(*0.5))"'
histogram Month if Useme2==1 & Condition=="flu", `general_options_month' `p3_options' `p3_xlabels' name(p3, replace) 
local general_options_region6 = `"frequency discrete scheme(s2mono) graphregion(color(white)) lwidth(0) ylabel(0 2000 4000 6000 8000 10000 "10000") barwidth(0.5) xlabel(0 "London" 1 "E/M England" 2 "N England" 3 "S England" 4 "Wales" 5 "N Ireland", labsize(small))  xsize(3.2) ysize(2.5) ytitle("Frequency", size(medlarge))"'
histogram Region6 if Useme1==1 & Condition=="cov", `general_options_region6' name(p4, replace) xtitle("Region admitted to ICU with COVID-19", size(medlarge)) title("COVID-19 patients", size(medlarge) position(11))
histogram Region6 if Useme1==1 & Condition=="flu", `general_options_region6' name(p5, replace) xtitle("Region admitted to ICU with non-COVID-19", size(medlarge)) title("Non-COVID-19 patients, before pandemic", size(medlarge) position(11)) 
histogram Region6 if Useme2==1 & Condition=="flu", `general_options_region6' name(p6, replace) xtitle("Region admitted to ICU with non-COVID-19", size(medlarge)) title("Non-COVID-19 patients, during pandemic", size(medlarge) position(11)) 
graph combine p1 p2 p3, rows(1) xsize(9.6) ysize(2.625) title("Admissions by month",position(11) size(medium)) graphregion(color(white)) name(p7, replace) imargin(zero) iscale(0.75)
graph combine p4 p5 p6, rows(1) xsize(9.6) ysize(2.625) title("Admissions by region",position(11) size(medium)) graphregion(color(white)) name(p8, replace) imargin(zero) iscale(0.75)
graph combine p7 p8, rows(2) xsize(9.6) ysize(5.25) graphregion(color(white)) imargin(zero) iscale(0.75) name(F1)
graph drop p1 p2 p3 p4 p5 p6 p7 p8

*----------------------------------------------------------------------------
* Figures S1-S5 (Descriptives by Month) and S6-S13 (Descriptives by Region6):
*----------------------------------------------------------------------------
* Define some options for all graphs by month:
local opts_all_m = `"scheme(s2mono) graphregion(color(white)) xsize(5) ysize(1.7)"'
local opts_cov_m = `"xscale(range(25.5, 43.5)) xtitle("Month admitted to ICU with COVID-19",size(medium)) xlabel(26(3)41,valuelabel tlength(0.2cm) tlwidth(medthick) labgap(*0.2)) xmticks(25(1)43, tlength(0.15cm)) name(p_cov_m, replace)"'
local opts_flu_m = `"xscale(range(0.5, 19.5)) xtitle("Month admitted to ICU with non-COVID-19",size(medium)) xlabel(1(3)19,valuelabel tlength(0.2cm) tlwidth(medthick) labgap(*0.2)) xmticks(1(1)19, tlength(0.15cm)) name(p_flu_m, replace)"'
* Define some options for all graphs by region:
local opts_all_r = `"scheme(s2mono) graphregion(color(white)) xsize(3.5) ysize(1.7) xscale(range(-0.5, 5.5)) xlabel(0 "London" 1 "E/M England" 2 "N England" 3 "S England" 4 "Wales" 5 "N Ireland", labsize(small))"'
local opts_cov_r = `"xtitle("Region admitted to ICU with COVID-19",size(medium)) name(p_cov_r, replace)"'
local opts_flu_r = `"xtitle("Region admitted to ICU with non-COVID-19",size(medium)) name(p_flu_r, replace)"'
* List the variables to be plotted and loop through them:
local continuous_vbls = "Age BMI CM_apa CM_pfr CM_ars CM_pss"
local binary_vbls = "Male Outcome"
local categorical_vbls = "Agecat Ethnicity Deprivation BMIcat CM_dep3 BMI_est"
local comorbidity_vbls = "CM_apm CM_cvd CM_hae CM_imm CM_liv CM_met CM_kid CM_res"
local all_vbls = "`continuous_vbls' `binary_vbls' `categorical_vbls' Comorbidities"
* Define some properties for the categorical variable levels:
local cat_plot_colours = "gray blue red orange brown emerald magenta dkgreen"
local cat_plot_symbols = "O D T S Oh Dh Th Sh"
local cat_plot_lpatterns = "solid shortdash dot dash solid shortdash dot dash"
* Loop through all variables:
foreach vbl of local all_vbls{
	display "Doing variable `vbl'"
	* Define titles for each variable:
	if "`vbl'"=="Age" local title = "Age at admission"
	if "`vbl'"=="Agecat" local title = "Age category (years) at admission"
	if "`vbl'"=="BMI" local title = "Body mass index"
	if "`vbl'"=="CM_apa" local title = "APACHE2 acute severity score"
	if "`vbl'"=="CM_pfr" local title = "Respiratory severity (PaO{sub:2}/FiO{sub:2})"
	if "`vbl'"=="CM_ars" local title = "Advanced respiratory support"
	if "`vbl'"=="CM_pss" local title = "ICNARC Physiological severity score"
	if "`vbl'"=="Male" local title = "Sex"
	if "`vbl'"=="Outcome" local title = "Proportion dying within 30 days"
	if "`vbl'"=="Ethnicity" local title = "Ethnicity category"
	if "`vbl'"=="Deprivation" local title = "Deprivation quintile"
	if "`vbl'"=="BMIcat" local title = "Body mass index category"
	if "`vbl'"=="CM_dep3" local title = "Prior dependency"
	if "`vbl'"=="BMI_est" local title = "Body mass index measurement"
	if "`vbl'"=="Comorbidities" local title = "Comorbidities"
	* Define ytitles for each variable:
	if "`vbl'"=="Age" local ytitle = "Age (years)"
	if "`vbl'"=="BMI" local ytitle = "Body mass index (kg/m{sup:2})"
	if "`vbl'"=="CM_apa" local ytitle = "APACHE2 score"
	if "`vbl'"=="CM_pfr" local ytitle = "PaO{sub:2}/FiO{sub:2}"
	if "`vbl'"=="CM_ars" local ytitle = "Days of support"
	if "`vbl'"=="CM_pss" local ytitle = "ICNARC Physiological severity score"
	if "`vbl'"=="Male" local ytitle = "Proportion male (%)"
	if "`vbl'"=="Outcome" local ytitle = "30 day mortality (%)"
	if regexm(" `categorical_vbls' "," `vbl' ")==1 local ytitle = "Proportion (%)"
	if "`vbl'"=="Comorbidities" local ytitle = "Prevalence (%)"
	* Continuous variables:
	* Some plotting options for plots at this level:
	local opts_leg = `"legend(label(1 "Mean") label(2 "Median") position(3) cols(1) symxsize(*0.6))"'
	local opts_vbl = `"title(`title', position(11)) ytitle(`ytitle') ylabel(, angle(horizontal))"'
	if regexm(" `continuous_vbls' "," `vbl' ")==1{
		* Plots by month:
		local p1 = `"(scatter Mean Month, msymbol(S) mcolor(blue) msize(vsmall) connect(l) lcolor(blue) lpattern(solid))"'
		local p2 = `"(scatter Median Month, msymbol(D) mcolor(red) msize(vsmall) connect(l) lcolor(red) lpattern(shortdash))"'
		preserve
			collapse (mean) Mean=`vbl' (median) Median=`vbl' (count) N=`vbl' if Useme1==1, by(Month Condition)	
			* Exclude Feb 2020 and Aug 2021 for Covid (low N)
			quietly drop if Condition=="cov" & (Month==25 | Month==43)
			* Check that all remaining stats come from at least 5 people:
			quietly summarize N
			assert r(min)>=5
			* Plots for Covid & Non-covid patients:
			graph twoway `p1' `p2' if Condition=="cov", `opts_all_m' `opts_cov_m' `opts_leg' `opts_vbl'
			graph twoway `p1' `p2' if Condition=="flu", `opts_all_m' `opts_flu_m' `opts_leg' `opts_vbl'
			* Display the plotted values:
			display "Plotted data by month for variable `vbl'"
			list
		restore
		* Plots by region:
		local p1 = `"(bar Mean X1, barwidth(0.4) color(blue))"'
		local p2 = `"(bar Median X2, barwidth(0.4) color(red))"'
		preserve
			collapse (mean) Mean=`vbl' (median) Median=`vbl' (count) N=`vbl' if Useme1==1, by(Region6 Condition)	
			* Check that all remaining stats come from at least 5 people:
			quietly summarize N
			assert r(min)>=5
			* Plots for Covid & Non-covid patients:
			generate X1 = Region6-0.2
			generate X2 = Region6+0.2
			graph twoway `p1' `p2' if Condition=="cov", `opts_all_r' `opts_cov_r' `opts_leg' `opts_vbl' 
			graph twoway `p1' `p2' if Condition=="flu", `opts_all_r' `opts_flu_r' `opts_leg' `opts_vbl' 
			* Display the plotted values:
			display "Plotted data by region for variable `vbl'"
			list
		restore
	}
	* Binary variables:
	else if regexm(" `binary_vbls' "," `vbl' ")==1{
		* Some plotting options for plots at this level:
		local opts_vbl = `"title(`title', position(11)) ytitle(`ytitle') yscale(range(0 1)) ylabel(0 "0" 0.2 "20" 0.4 "40" 0.6 "60" 0.8 "80" 1 "100", angle(horizontal))"'
		local opts_leg = `"legend(off)"'
		* Plots by month:
		local p1 = `"(scatter Mean Month, msymbol(S) mcolor(black) msize(vsmall) connect(l) lcolor(black))"'
		preserve
			collapse (mean) Mean=`vbl' (count) N=`vbl' if Useme1==1, by(Month Condition)	
			* Exclude Feb 2020 and Aug 2021 for Covid (low N)
			drop if Condition=="cov" & (Month==25 | Month==43)
			* Check that all remaining stats come from at least 5 people:
			quietly summarize N
			assert r(min)>=5
			* Plots for Covid & Non-covid patients:
			graph twoway `p1' if Condition=="cov", `opts_all_m' `opts_cov_m' `opts_leg' `opts_vbl' graphregion(margin(r=30))
			graph twoway `p1' if Condition=="flu", `opts_all_m' `opts_flu_m' `opts_leg' `opts_vbl' graphregion(margin(r=30))
			* Display the plotted values:
			display "Plotted data by month for variable `vbl'"
			list
		restore
		* Plots by region:
		local p1 = `"(bar Mean Region6, barwidth(0.8) color(black))"'
		preserve
			collapse (mean) Mean=`vbl' (count) N=`vbl' if Useme1==1, by(Region6 Condition)	
			* Check that all remaining stats come from at least 5 people:
			quietly summarize N
			assert r(min)>=5
			* Plots for Covid & Non-covid patients:
			graph twoway `p1' if Condition=="cov", `opts_all_r' `opts_cov_r' `opts_leg' `opts_vbl' graphregion(margin(r=30)) 
			graph twoway `p1' if Condition=="flu", `opts_all_r' `opts_flu_r' `opts_leg' `opts_vbl' graphregion(margin(r=30))
			* Display the plotted values:
			display "Plotted data by region for variable `vbl'"
			list
		restore
	}
	* Categorical variables:
	else if regexm(" `categorical_vbls' "," `vbl' ")==1{
		quietly levelsof `vbl', local(levels)
		local Nlevels = wordcount("`levels'")
		if "`vbl'"=="Agecat" local opts_leg = `"legend(label(1 "16 to <35") label(2 "35 to <50") label(3 "50 to <60") label(4 "60 to <70") label(5 "70 to <80") label(6 "≥80") position(3) cols(1) symxsize(*0.6) size(small) rowgap(*0.4))"'
		if "`vbl'"=="Ethnicity" local opts_leg = `"legend(label(1 "Asian") label(2 "Black") label(3 "White") label(4 "Other") position(3) cols(1) symxsize(*0.6))"'
		if "`vbl'"=="Deprivation" local opts_leg = `"legend(label(1 "Least") label(2 "Second-least") label(3 "Middle") label(4 "second-most") label(5 "Most") position(3) cols(1) symxsize(*0.6))"'
		if "`vbl'"=="BMIcat" local opts_leg = `"legend(label(1 "Underweight") label(2 "Recommended") label(3 "Overweight") label(4 "Obesity 1") label(5 "Obesity 2") label(6 "Obesity 3") position(3) cols(1) symxsize(*0.6) size(small) rowgap(*0.4))"'
		if "`vbl'"=="CM_dep3" local opts_leg = `"legend(label(1 "Independent") label(2 "Some dependency") label(3 "Total dependency") position(3) cols(1) symxsize(*0.6))"'
		if "`vbl'"=="BMI_est" local opts_leg = `"legend(label(1 "Both measured") label(2 "Height only") label(3 "Weight only") label(4 "Neither measured") position(3) cols(1) symxsize(*0.6))"'
		* Plots by month:
		local plots = ""
		forvalues i = 1/`Nlevels'{
			local level = word("`levels'",`i')
			local cat_plot_colour = word("`cat_plot_colours'",`i')
			local cat_plot_symbol = word("`cat_plot_symbols'",`i')
			local cat_plot_lpattern = word("`cat_plot_lpatterns'", `i')
			local plots = "`plots' (scatter Percentage Month if `vbl'==`level', msymbol(`cat_plot_symbol') mcolor(`cat_plot_colour') msize(vsmall) connect(l) lcolor(`cat_plot_colour') lpattern(`cat_plot_lpattern'))"
		}
		preserve
			contract Month Condition `vbl' if Useme1==1, zero freq(N) nomiss
			bysort Condition Month: egen Percentage = pc(N)
			bysort Condition Month: egen Ntot = total(N)
			* Restrict to the study period for each condition (exclude Feb 2020 and Aug 2021 for Covid due to low N):
			keep if (Condition=="cov" & Month>=26 & Month<=42) | (Condition=="flu" & Month>=1 & Month<=19)
			* Check that all remaining stats come from at least 5 people:
			quietly summarize Ntot
			assert r(min)>=5
			* Some plotting options get updated specific to the vbl:
			local opts_vbl = `"title(`title', position(11)) ytitle(`ytitle') yscale(range(0 1)) ylabel(0 "0" 20 "20" 40 "40" 60 "60" 80 "80" 100 "100", angle(horizontal))"'
			* Plots for Covid & Non-covid patients:
			graph twoway `plots' if Condition=="cov", `opts_all_m' `opts_cov_m' `opts_leg' `opts_vbl'
			graph twoway `plots' if Condition=="flu", `opts_all_m' `opts_flu_m' `opts_leg' `opts_vbl'
			* Display the plotted values:
			display "Plotted data by month for variable `vbl'"
			list
		restore
		* Plots by region:
		local plots = ""
		local cat_barwidth = 0.8/`Nlevels'
		forvalues i = 1/`Nlevels'{
			local level = word("`levels'",`i')
			local cat_plot_colour = word("`cat_plot_colours'",`i')
			local plots = "`plots' (bar Percentage X`i' if `vbl'==`level', barwidth(`cat_barwidth') color(`cat_plot_colour'))"
		}
		preserve
			contract Region6 Condition `vbl' if Useme1==1, zero freq(N) nomiss
			bysort Condition Region6: egen Percentage = pc(N)
			bysort Condition Region6: egen Ntot = total(N)
			* Check that all remaining stats come from at least 5 people:
			quietly summarize Ntot
			assert r(min)>=5
			* Plots for Covid & Non-covid patients:
			forvalues i = 1/`Nlevels'{
				generate X`i' = Region6-0.4+(`i'-0.5)*`cat_barwidth'
			}
			graph twoway `plots' if Condition=="cov", `opts_all_r' `opts_cov_r' `opts_leg' `opts_vbl' 
			graph twoway `plots' if Condition=="flu", `opts_all_r' `opts_flu_r' `opts_leg' `opts_vbl' 
			* Display the plotted values:
			display "Plotted data by region for variable `vbl'"
			list
		restore
	}
	* Comorbidity variables (on one plot):
	else if "`vbl'"=="Comorbidities"{
		local Nplots = wordcount("`comorbidity_vbls'")
		local opts_leg = `"legend(label(1 "Previous severe illness") label(2 "Cardiovascular disease") label(3 "Haematological disease") label(4 "Immunocompromised") label(5 "Liver disease") label(6 "Metastatic disease") label(7 "Renal disease") label(8 "Respiratory disease") position(3) cols(1) symxsize(*0.6) size(vsmall) rowgap(0))"'
		local opts_vbl = `"title(`title', position(11)) ytitle(`ytitle') yscale(range(0 0.3)) ylabel(0 "0" 0.1 "10" 0.2 "20" 0.3 "30", angle(horizontal))"'
		* Plots by month:
		local plots = ""
		local collapse_code = ""
		forvalues i = 1/`Nplots'{
			local subvbl = word("`comorbidity_vbls'",`i')
			local cat_plot_colour = word("`cat_plot_colours'",`i')
			local cat_plot_symbol = word("`cat_plot_symbols'",`i')
			local cat_plot_lpattern = word("`cat_plot_lpatterns'", `i')
			local plots = "`plots' (scatter Mean_`subvbl' Month, msymbol(`cat_plot_symbol') mcolor(`cat_plot_colour') msize(vsmall) connect(l) lcolor(`cat_plot_colour') lpattern(`cat_plot_lpattern'))"
			local collapse_code = "`collapse_code' (mean) Mean_`subvbl'=`subvbl' (count) N_`subvbl'=`subvbl'"
		}
		preserve
			collapse `collapse_code' if Useme1==1, by(Month Condition)
			* Restrict to the study period for each condition (exclude Feb 2020 and Aug 2021 for Covid due to low N):
			keep if (Condition=="cov" & Month>=26 & Month<=42) | (Condition=="flu" & Month>=1 & Month<=19)
			* Check that all remaining stats come from at least 5 people:
			foreach subvbl of local comorbidity_vbls{
				quietly summarize N_`subvbl'
				assert r(min)>=5
			}
			* Plots for Covid & Non-covid patients:
			graph twoway `plots' if Condition=="cov", `opts_all_m' `opts_cov_m' `opts_leg' `opts_vbl'
			graph twoway `plots' if Condition=="flu", `opts_all_m' `opts_flu_m' `opts_leg' `opts_vbl'
			* Display the plotted values:
			display "Plotted data by month for variable `vbl'"
			list, abbreviate(11)
		restore
		* Plots by region:
		local plots = ""
		local collapse_code = ""
		local cat_barwidth = 0.8/`Nplots'
		forvalues i = 1/`Nplots'{
			local subvbl = word("`comorbidity_vbls'",`i')
			local cat_plot_colour = word("`cat_plot_colours'",`i')
			local plots = "`plots' (bar Mean_`subvbl' X`i', barwidth(`cat_barwidth') color(`cat_plot_colour'))"
			local collapse_code = "`collapse_code' (mean) Mean_`subvbl'=`subvbl' (count) N_`subvbl'=`subvbl'"
		}
		preserve
			collapse `collapse_code' if Useme1==1, by(Region6 Condition)
			* Check that all remaining stats come from at least 5 people:
			foreach subvbl of local comorbidity_vbls{
				quietly summarize N_`subvbl'
				assert r(min)>=5
			}
			* Plots for Covid & Non-covid patients:
			forvalues i = 1/`Nplots'{
				generate X`i' = Region6-0.4+(`i'-0.5)*`cat_barwidth'
			}
			graph twoway `plots' if Condition=="cov", `opts_all_r' `opts_cov_r' `opts_leg' `opts_vbl'
			graph twoway `plots' if Condition=="flu", `opts_all_r' `opts_flu_r' `opts_leg' `opts_vbl'
			* Display the plotted values:
			display "Plotted data by region for variable `vbl'"
			list, abbreviate(11)
		restore
	}
	* Combine the Covid and Flu graphs:
	graph combine p_cov_m p_flu_m, scheme(s2mono) graphregion(color(white)) rows(1) xsize(10) ysize(1.7) name(p_`vbl'_m, replace) ycommon	
	graph drop p_cov_m p_flu_m	
	graph combine p_cov_r p_flu_r, scheme(s2mono) graphregion(color(white)) rows(1) xsize(7) ysize(1.7) name(p_`vbl'_r, replace) ycommon	
	graph drop p_cov_r p_flu_r
}
* Combine the graphs over all variables:	
local opts_title = `"title("      {bf:COVID-19 patients}", size(large) position(11) ring(7)) subtitle("{bf:Non-COVID-19 patients                     }", size(large) position(1) ring(7))"'
graph combine p_Age_m p_Agecat_m p_Male_m, scheme(s2mono) graphregion(color(white)) cols(1) imargin(zero) xsize(10) ysize(5.4) `opts_title' name(FS1, replace)
graph combine p_Ethnicity_m p_Deprivation_m p_BMI_m, scheme(s2mono) graphregion(color(white)) cols(1) imargin(zero) xsize(10) ysize(5.4) `opts_title' name(FS2, replace)
graph combine p_BMIcat_m p_CM_dep3_m p_Comorbidities_m, scheme(s2mono) graphregion(color(white)) cols(1) imargin(zero) xsize(10) ysize(5.4) `opts_title' name(FS3, replace)
graph combine p_CM_apa_m p_CM_pss_m p_CM_pfr_m, scheme(s2mono) graphregion(color(white)) cols(1) imargin(zero) xsize(10) ysize(5.4) `opts_title' name(FS4, replace)
graph combine p_CM_ars_m p_BMI_est_m p_Outcome_m, scheme(s2mono) graphregion(color(white)) cols(1) imargin(zero) xsize(10) ysize(5.4) `opts_title' name(FS5, replace) 
graph combine p_Age_r p_Agecat_r, scheme(s2mono) graphregion(color(white)) cols(1) imargin(zero) xsize(7) ysize(3.7) `opts_title' name(FS6, replace)
graph combine p_Male_r p_Ethnicity_r, scheme(s2mono) graphregion(color(white)) cols(1) imargin(zero) xsize(7) ysize(3.7) `opts_title' name(FS7, replace)
graph combine p_Deprivation_r p_BMI_r, scheme(s2mono) graphregion(color(white)) cols(1) imargin(zero) xsize(7) ysize(3.7) `opts_title' name(FS8, replace)
graph combine p_BMIcat_r p_CM_dep3_r, scheme(s2mono) graphregion(color(white)) cols(1) imargin(zero) xsize(7) ysize(3.7) `opts_title' name(FS9, replace)
graph combine p_Comorbidities_r p_CM_apa_r, scheme(s2mono) graphregion(color(white)) cols(1) imargin(zero) xsize(7) ysize(3.7) `opts_title' name(FS10, replace)
graph combine p_CM_pss_r p_CM_pfr_r, scheme(s2mono) graphregion(color(white)) cols(1) imargin(zero) xsize(7) ysize(3.7) `opts_title' name(FS11, replace)
graph combine p_CM_ars_r p_BMI_est_r, scheme(s2mono) graphregion(color(white)) cols(1) imargin(zero) xsize(7) ysize(3.7) `opts_title' name(FS12, replace)
graph combine p_Outcome_r , scheme(s2mono) graphregion(color(white)) cols(1) imargin(zero) xsize(7) ysize(2.0) `opts_title' name(FS13a, replace)
* Alternative version of Fig S13 which needs cropping, but has standard font sizes:
graph combine p_Outcome_r p_Outcome_r, scheme(s2mono) graphregion(color(white)) cols(1) imargin(zero) xsize(7) ysize(3.7) `opts_title' name(FS13b, replace)
graph drop p_*

*------------------------------------------------
* Figure 3, Figures S14-S15 (Mortality~BMI_cspl):
*------------------------------------------------
* [Note: Consistent with the use of condition-specific zBMI, I've plotted cov and flu on separate X axes; HR in each are relative to mean BMI for that condition]
* Define the plotted ranges of BMI and HR:
* I'm going to truncate each plot at the overall 1st and 99th percentiles of (condition-specific) zBMI (or the observed range of BMI for the stratum and condition if that's narrower):
summarize zBMI if Useme1==1, detail
local plotXmin_z = r(p1)
local plotXmax_z = r(p99)
* The X-axis range will be the same, but converted back to BMI for each condition:
foreach condition in "cov" "flu"{
	local Xmin_`condition' = `plotXmin_z'*`SD_BMI_`condition''+`Mean_BMI_`condition''
	local Xmax_`condition' = `plotXmax_z'*`SD_BMI_`condition''+`Mean_BMI_`condition''
}	
local x1labels = "20(10)50"
local x2labels = "20(10)50"
* The Y range is just defined manually:
local Ymin = 0.4
local Ymax = 3.5
local ylabels = "0.5 0.7 1 1.4 2 3.2"
local plotpoints = 400
* Make splines of zBMI, storing the knots:
mkspline "zBMI_cspl" = zBMI if Useme1==1, cubic nknots(5) displayknots
matrix K = r(knots)
local knots  "`=K[1,1]'  `=K[1,2]' `=K[1,3]'  `=K[1,4]'  `=K[1,5]'"
* stset the data:
stset FUT, failure(Outcome) id(recordid)
* Define the adjustment set (except for period & region):
local full_adjustment = "c.Male c.Age_cspl1 c.Age_cspl2 c.Age_cspl3 c.Age_cspl4 i.Ethnicity i.Deprivation i.Period i.Region6"
* Loop through the different stratification options:
local strata = "None Period==1 Period==2 Period==3 Period==4 Period==5 Period==6 Region6==0 Region6==1 Region6==2 Region6==3 Region6==4 Region6==5"
local Nplots = wordcount("`strata'")
forvalues plotno = 1/`Nplots'{
	local stratum = word("`strata'", `plotno')
	local stratification = cond("`stratum'"=="None",""," & `stratum'")
	* Remove the stratifier from the adjustment:
	if "`stratum'"=="None"{
		local adjustment = "`full_adjustment'"
		local xsize = 6
		local ysize = 5
	}
	if "`stratum'"!="None"{
		local xsize = 3.3
		local ysize = 3
	}
	if substr("`stratum'",1,6)=="Period" local adjustment = strtrim(regexr(" `full_adjustment' "," i.Period "," "))
	if substr("`stratum'",1,7)=="Region6" local adjustment = strtrim(regexr(" `full_adjustment' "," i.Region6 "," "))
	* Determine the plot's title:	
	*if "`stratum'"=="None" local title_opts = `"title("All periods and regions", color(black) size(large) position(11))"'
	if "`stratum'"=="None" local title_opts = `""'
	if "`stratum'"=="Period==1" local title_opts = `"title("Feb - Apr 2020 (COVID-19)" "Feb - Apr 2018 (non-COVID-19)", color(black) size(medlarge) position(11))"'
	if "`stratum'"=="Period==2" local title_opts = `"title("May - Jul 2020 (COVID-19)" "May - Jul 2018 (non-COVID-19)", color(black) size(medlarge) position(11))"'
	if "`stratum'"=="Period==3" local title_opts = `"title("Aug - Oct 2020 (COVID-19)" "Aug - Oct 2018 (non-COVID-19)", color(black) size(medlarge) position(11))"'
	if "`stratum'"=="Period==4" local title_opts = `"title("Nov 2020 - Jan 2021 (COVID-19)" "Nov 2018 - Jan 2019 (non-COVID-19)", color(black) size(medlarge) position(11))"'
	if "`stratum'"=="Period==5" local title_opts = `"title("Feb - Apr 2021 (COVID-19)" "Feb - Apr 2019 (non-COVID-19)", color(black) size(medlarge) position(11))"'
	if "`stratum'"=="Period==6" local title_opts = `"title("May - Aug 2021 (COVID-19)" "May - Aug 2019 (non-COVID-19)", color(black) size(medlarge) position(11))"'
	if "`stratum'"=="Region6==0" local title_opts = `"title("London, England", color(black) size(large) position(11))"'
	if "`stratum'"=="Region6==1" local title_opts = `"title("E England & Midlands", color(black) size(large) position(11))"'
	if "`stratum'"=="Region6==2" local title_opts = `"title("NE & NW England, Yorkshire", color(black) size(large) position(11))"'
	if "`stratum'"=="Region6==3" local title_opts = `"title("SE & SW England", color(black) size(large) position(11))"'
	if "`stratum'"=="Region6==4" local title_opts = `"title("Wales", color(black) size(large) position(11))"'
	if "`stratum'"=="Region6==5" local title_opts = `"title("Northern Ireland", color(black) size(large) position(11))"'
	* Loop though the conditions, running models and calculating fitted values wrt BMI:
	foreach condition in "cov" "flu"{
		generate In_plot_range = 1
		* Note which zBMI are within the plotting range:
		replace In_plot_range = 0 if zBMI<`plotXmin_z' | zBMI>`plotXmax_z'
		* Make new rows containing fitted data to plot:
		local row_start = _N+1
		local row_finish = `row_start'+(`plotpoints'-1)
		set obs `row_finish'
		generate Plotme = 1 in `row_start'/`row_finish'
		* In the new data, set zBMI at equal intervals within the plotted (and observed) range:
		summarize zBMI if Useme1==1 & Condition=="`condition'" & In_plot_range==1 `stratification'
		local obsXmin = r(min)
		local obsXmax = r(max)
		local step = (`obsXmax'-`obsXmin')/(`plotpoints'-1)
		replace zBMI = `obsXmin'+(_n-`row_start')*`step' if Plotme==1
		mkspline "new_zBMI_cspl" = zBMI if Plotme==1, cubic knots(`knots') displayknots
		forvalues k = 1/4{
			replace zBMI_cspl`k' = new_zBMI_cspl`k' if Plotme==1
			drop new_zBMI_cspl`k'
		}
		drop In_plot_range
		* In the new data, fill in all other covariates with the reference value (it doesn't matter what value they take so long as it's the same for all):
		foreach vbl in "Male" "Age_cspl1" "Age_cspl2" "Age_cspl3" "Age_cspl4" "Period" "Region6"{
			replace `vbl' = 0 if Plotme==1
		}
		foreach vbl in "Ethnicity" "Deprivation"{
			replace `vbl' = 1 if Plotme==1
		}
		* Run the model on the real data (including the BMI outside the plotting range):
		streg zBMI_cspl* `adjustment' if Useme1==1 & Condition=="`condition'" `stratification', distribution(`psa_distribution')
		* In the new data, make fitted values to plot:
		generate XR = round(zBMI,0.001)
		levelsof XR if Plotme==1, local(levels)
		xblc zBMI_cspl*, covname(XR) at(`levels') eform reference(0) generate(plotX`condition' plotY`condition' plotLCL`condition' plotUCL`condition')
		drop XR	
		* Convert plotted zBMI back to kg per m^2:
		replace plotX`condition' = plotX`condition'*`SD_BMI_`condition''+`Mean_BMI_`condition''
		* Calculate outer percentiles in kg per m^2 (used only if plotting outer percentiles as vertical lines):
		summarize zBMI if Useme1==1 & Condition=="`condition'" `stratification', detail
		local X_LC_`condition' = r(p1)*`SD_BMI_`condition''+`Mean_BMI_`condition''
		local X_UC_`condition' = r(p99)*`SD_BMI_`condition''+`Mean_BMI_`condition''
		drop if Plotme==1
		drop Plotme
		* Make an indicator variable to plot the extreme of X within the range, if more extreme values have been omitted from the plot:
		generate plotXT`condition' = .
		count if Useme1==1 & Condition=="`condition'" & zBMI<`plotXmin_z' `stratification' 
		if r(N)>0{
			summarize plotX`condition' if !missing(plotX`condition')
			replace plotXT`condition' = 1 if plotX`condition'==r(min)
		}
		count if Useme1==1 & Condition=="`condition'" & zBMI>`plotXmax_z' `stratification' 
		if r(N)>0{
			summarize plotX`condition' if !missing(plotX`condition')
			replace plotXT`condition' = 1 if plotX`condition'==r(max)
		}
		* Make an indicator variable to plot the extreme of Y within the range, if more extreme values have been omitted from the plot:
		* (If some fitted values are omitted due to extreme X _and_ extreme Y, code would need adapting. In this case, no fitted values get omitted due to extreme Y)
		generate plotYT`condition' = .
		count if !missing(plotY`condition') & plotY`condition'<`Ymin' 
		if r(N)>0{
			summarize plotY`condition' if !missing(plotY`condition') & plotY`condition'>`Ymin'
			replace plotYT`condition' = 1 if plotY`condition'==r(min)
		}
		count if !missing(plotY`condition') & plotY`condition'>`Ymax' 
		if r(N)>0{
			summarize plotY`condition' if !missing(plotY`condition') & plotY`condition'<`Ymax'
			replace plotYT`condition' = 1 if plotY`condition'==r(max)
		}
		* Truncate CI at Ymin and Ymax and make indicator variables:
		generate plotLCLT`condition'=.
		replace plotLCLT`condition'=1 if !missing(plotLCL`condition') & plotLCL`condition'<`Ymin'
		replace plotLCL`condition' = `Ymin' if !missing(plotLCL`condition') & plotLCL`condition'<`Ymin'
		generate plotUCLT`condition'=.
		replace plotUCLT`condition'=1 if !missing(plotUCL`condition') & plotUCL`condition'>`Ymax'
		replace plotUCL`condition' = `Ymax' if !missing(plotUCL`condition') & plotUCL`condition'>`Ymax'
		format plotY`condition' %2.1f
		format plotLCL`condition' %2.1f
		format plotUCL`condition' %2.1f
	}
	* Make colour and greyscale versions:
	foreach cg in "c" "g"{
		if "`cg'"=="c"{
			local cov_colour = "red"
			local flu_colour = "blue"
			local flu_lpattern = "dash"
			local flu_xlinepattern = "shortdash"
			local legend_opts = `"legend(off)"'
		}
		if "`cg'"=="g"{
			local cov_colour = "black"
			local flu_colour = "black"
			local flu_lpattern = "dash"
			local flu_xlinepattern = "shortdash_dot"
			local legend_opts = `"legend(order(2 10) label(2 "COVID-19") label(10 "Non-COVID-19") position(6) rows(1) symxsize(*0.6) size(medium) rowgap(0))"'
		}
		local X1_opts = `"xtitle("BMI (kg/m{sup:2}) in COVID-19 patients", color(`cov_colour')) xscale(range(`Xmin_cov' `Xmax_cov')) xlabel(`x1labels', labgap(*0.4) tlength(*0.5) tlcolor(black) labcolor(`cov_colour'))"'
		local X2_opts = `"xtitle("BMI (kg/m{sup:2}) in non-COVID-19 patients", color(`flu_colour') axis(2)) xscale(range(`Xmin_flu' `Xmax_flu') axis(2)) xlabel(`x2labels', labgap(*0.4) tlength(*0.5) tlcolor(black) labcolor(`flu_colour') axis(2))"'
		local Y_opts = `"ytitle("HR (95% CI) relative to mean BMI") yscale(log range(`Ymin' `Ymax')) ylabel(`ylabels', labgap(*0.5) angle(horizontal))"'
		*local xline_opts = `"xline(`X_LC_cov' `X_UC_cov', lcolor(`cov_colour') lwidth(*0.5) lpattern(shortdash) axis(1)) xline(`X_LC_flu' `X_UC_flu', lcolor(`flu_colour') lwidth(*0.5) lpattern(`flu_xlinepattern') axis(2))"'
		local other_opts = `"name(plot`plotno'`cg',replace) scheme(s2mono) graphregion(color(white)) xsize(`xsize') ysize(`ysize')"'
		local pCIcov = `"(rarea plotLCLcov plotUCLcov plotXcov,bcolor(`cov_colour'%30) lwidth(none) xaxis(1))"'
		local pHRcov = `"(line plotYcov plotXcov if plotYcov>=`Ymin' & plotYcov<=`Ymax', lpattern(solid) lcolor(`cov_colour') xaxis(1))"'
		local pCIflu = `"(rarea plotLCLflu plotUCLflu plotXflu,bcolor(`flu_colour'%30) lwidth(none) xaxis(2))"'
		local pHRflu = `"(line plotYflu plotXflu if plotYflu>=`Ymin' & plotYflu<=`Ymax', lpattern(`flu_lpattern') lcolor(`flu_colour') xaxis(2))"'
		local pXT = `"(scatter plotYcov plotXcov if plotXTcov==1, mcolor(`cov_colour') msymbol(x)) (scatter plotYflu plotXflu if plotXTflu==1, mcolor(`flu_colour') msymbol(x) xaxis(2))"'
		local pYT = `"(scatter plotYcov plotXcov if plotYTcov==1, mcolor(`cov_colour') msymbol(+)) (scatter plotYflu plotXflu if plotYTflu==1, mcolor(`flu_colour') msymbol(+) xaxis(2))"'
		local pLCLTlo = `"(line plotLCLcov plotXcov if plotLCLTcov==1 & plotXcov<`Mean_BMI_cov', lcolor(`cov_colour') lpattern(shortdash) lwidth(thin)) (line plotLCLflu plotXflu if plotLCLTflu==1 & plotXflu<`Mean_BMI_flu', lcolor(`flu_colour') lpattern(shortdash) lwidth(thin) xaxis(2))"'
		local pLCLThi = `"(line plotLCLcov plotXcov if plotLCLTcov==1 & plotXcov>`Mean_BMI_cov', lcolor(`cov_colour') lpattern(shortdash) lwidth(thin)) (line plotLCLflu plotXflu if plotLCLTflu==1 & plotXflu>`Mean_BMI_flu', lcolor(`flu_colour') lpattern(shortdash) lwidth(thin) xaxis(2))"'
		local pUCLTlo = `"(line plotUCLcov plotXcov if plotUCLTcov==1 & plotXcov<`Mean_BMI_cov', lcolor(`cov_colour') lpattern(shortdash) lwidth(thin)) (line plotUCLflu plotXflu if plotUCLTflu==1 & plotXcov<`Mean_BMI_flu', lcolor(`flu_colour') lpattern(shortdash) lwidth(thin) xaxis(2))"'
		local pUCLThi = `"(line plotUCLcov plotXcov if plotUCLTcov==1 & plotXcov>`Mean_BMI_cov', lcolor(`cov_colour') lpattern(shortdash) lwidth(thin)) (line plotUCLflu plotXflu if plotUCLTflu==1 & plotXcov>`Mean_BMI_flu', lcolor(`flu_colour') lpattern(shortdash) lwidth(thin) xaxis(2))"'
		graph twoway  `pCIcov' `pHRcov' `pCIflu' `pHRflu' `pXT' `pYT' `pLCLTlo' `pLCLThi' `pUCLTlo' `pUCLThi', `X1_opts' `X2_opts' `Y_opts' `legend_opts' `xline_opts' `title_opts' `other_opts'
	}
	drop plotXcov plotYcov plotLCLcov plotUCLcov plotXTcov plotYTcov plotLCLTcov plotUCLTcov plotXflu plotYflu plotLCLflu plotUCLflu plotXTflu plotYTflu plotLCLTflu plotUCLTflu
}			
* Combine the stratified graphs for the manuscript:	
graph rename plot1g F3, replace
graph combine plot2c plot3c plot4c plot5c plot6c plot7c, scheme(s2mono) graphregion(color(white)) imargin(small) cols(3) xsize(10) ysize(6) name(FS14, replace)
graph combine plot8c plot9c plot10c plot11c plot12c plot13c, scheme(s2mono) graphregion(color(white)) imargin(small) cols(3) xsize(9.9) ysize(6) name(FS15, replace)
graph drop plot1* plot2* plot3* plot4* plot5* plot6* plot7* plot8* plot9* plot10* plot11* plot12* plot13*

*------------
* Wrap it up:
*------------
* Display and export the graphs:
set graphics on
foreach graph in "F1" "FS1" "FS2" "FS3" "FS4" "FS5" "FS6" "FS7" "FS8" "FS9" "FS10" "FS11" "FS12" "FS13a" "FS13b" "F3" "FS14" "FS15"{
	graph display `graph'
	graph export `graph'.pdf, replace
}
display c(current_date)+", "+c(current_time)
log close


