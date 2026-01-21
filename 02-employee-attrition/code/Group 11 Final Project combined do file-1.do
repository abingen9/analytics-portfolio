**MHR 614 Final Project Group 11 - Annotated STATA Do-File

*Q.1: Does gender differ in the extent to which job satisfaction is related to the likelihood of attrition?

*Load and merge general and employee survey data
import delimited "/Users/laisi/Desktop/MHR 614 project/general_data.csv", clear case(preserve)
sort EmployeeID
save temp1, replace

import delimited "/Users/laisi/Desktop/MHR 614 project/employee_survey_data.csv", clear case(preserve)
sort EmployeeID
save temp2, replace

use temp1, clear
merge 1:1 EmployeeID using temp2

*Encode categorical variables
encode Gender, gen(gend)
encode JobSatisfaction, gen(jobsatis)
encode Attrition, gen(attri)

*Create binary attrition variable
gen turnover = 1 if attri == 2
replace turnover = 0 if attri == 1

*Frequency tables
tabulate gend
tabulate turnover
tabulate jobsatis

*Logistic regression: Main effects only
logit turnover jobsatis i.gend, vce(robust)

*Logistic regression: Interaction model
logit turnover c.jobsatis##i.gend, vce(robust)

*Marginal effects of job satisfaction by gender
margins, dydx(jobsatis) at(gend = (1 2)) predict(xb)
margins, dydx(jobsatis) at(gend = (1 2)) predict(pr)


*Q.2: How do engagement and job satisfaction impact the attrition rate?

*Import and prep general data
import delimited "general_data.csv", varnames(1) clear
rename attrition attrition_str
generate byte attrition = (attrition_str == "Yes")
destring employeeid, replace
sort employeeid
save gen.dta, replace

*Import and prep employee survey data
import delimited "employee_survey_data.csv", varnames(1) clear
destring employeeid, replace
destring jobsatisfaction, generate(jobsat)
sort employeeid
save emp.dta, replace

*Import and prep manager survey data
import delimited "manager_survey_data.csv", varnames(1) clear
destring employeeid, replace
sort employeeid
save mgr.dta, replace

*Merge all datasets
use gen.dta, clear
merge 1:1 employeeid using emp.dta, keep(match) nogen
merge 1:1 employeeid using mgr.dta, keep(match) nogen

*Explore dataset
describe
summarize
tabulate age
tabulate attrition
tabulate gender
tabulate maritalstatus

*1 Leavers vs. Stayers on Engagement & JobSat*
rename jobsat jobsat
rename jobinvolvement  engagement 

tabulate attrition  
ttest jobsat,  by(attrition)
ttest engagement,  by(attrition)

*2 Create high/low groups using median splits
egen med_Eng = median(engage)
generate byte HiEng   = (engage  > med_Eng)

egen med_Sat = median(jobsat)
generate byte HiSat   = (jobsat  > med_Sat)

ttest attrition, by(HiEng)
ttest attrition, by(HiSat)

*2 Reshape for fixed-effects panel model
rename engage  score0
rename jobsat  score1
reshape long score, i(employeeid) j(metric) 

xtset employeeid metric
label define MET 0 "engagement" 1 "jobsatisfaction"
label values metric MET

*3A Within each metric, does a one point change in score predict attrition
bysort metric: xtreg attrition c.score, fe

*3B 
xtreg attrition c.score##i.metric, vce(robust)
margins, dydx(score) at (metric=(0 1))

*3C
encode gender, gen(gend)
xtreg attrition c.score##i.metric i.gend c.yearsatcompany c.totalworkingyears, vce(robust)
margins, dydx(score) at (metric=(0 1))


*Q.3: Are certain departments experiencing higher employee attrition than others, and what factors might explain this?

*Set working directory
cd "/Users/Patron/Downloads/614 Final Project Files"

*Load and merge raw data
import delimited "general_data.csv", clear
sort employeeid
save general_data, replace

import delimited "employee_survey_data.csv", clear
sort employeeid
save employee_survey_data, replace

import delimited "manager_survey_data.csv", clear
sort employeeid
save manager_survey_data, replace

use general_data, clear
merge 1:1 employeeid using employee_survey_data, nogen
merge 1:1 employeeid using manager_survey_data, nogen
save merged_data, replace

*Prep variables
use merged_data, clear
gen attrition_flag = (attrition == "Yes")
label define attrlbl 0 "No" 1 "Yes"
label values attrition_flag attrlbl

encode department, gen(dept_encoded)
destring jobsatisfaction, gen(job_sat_num) force
label define job_sat_lbl 1 "Low" 2 "Medium-Low" 3 "Medium-High" 4 "High"
label values job_sat_num job_sat_lbl

*Descriptive stats
tab attrition
tab department attrition, row chi2

*Graphs
graph bar (percent), over(attrition_flag) over(department, label(angle(45))) ///
    blabel(bar, format(%9.1g)) title("Attrition Rate by Department") ///
    ytitle("Percentage") legend(pos(6)) name(attrition_chart, replace)

graph bar (mean) attrition_flag if inrange(job_sat_num, 1, 4), ///
    over(job_sat_num) title("Attrition Rate by Job Satisfaction Level") ///
    ytitle("Mean Attrition Rate") blabel(bar, format(%4.3f)) bar(1, color(navy))

graph box monthlyincome, over(attrition_flag) ///
    title("Monthly Income Distribution by Attrition Status") ///
    ytitle("Monthly Income") box(1, color(navy)) box(2, color(navy))

*Logistic regression: base model
logit attrition_flag age monthlyincome joblevel totalworkingyears jobsatisfaction jobinvolvement
logit attrition_flag age monthlyincome joblevel totalworkingyears jobsatisfaction jobinvolvement, or

*Multicollinearity check
estat vif

*Margins and prediction plots
margins, at(joblevel=(1 2 3 4 5))
marginsplot, title("Predicted Attrition by Job Level") ytitle("Predicted Probability")

margins department
marginsplot, title("Predicted Attrition by Department") ytitle("Predicted Probability") ///
    ylabel(, format(%4.2f))

*Histograms of tenure
histogram totalworkingyears if attrition_flag == 1, color(gs14) width(2) ///
    title("Total Working Years - Employees Who Left") xtitle("Years")

histogram totalworkingyears if attrition_flag == 0, color(navy) width(2) ///
    title("Total Working Years - Employees Who Stayed") xtitle("Years")

*Export regression output (esttab must be installed)
esttab base_model using "regression_output.rtf", replace ///
    title("Logistic Regression - Attrition Model") ///
    star(* 0.10 ** 0.05 *** 0.01) b(%9.3f) se(%9.3f) ar2

*Alternate model: include department
encode department, gen(dept_encoded)
logit attrition_flag age monthlyincome joblevel job_sat_num jobinvolvement i.dept_encoded

*Optional: summary stats export
outreg2 using "summary_stats.doc", replace ctitle("Summary Stats") sum(logit) dec(2)

*Save final cleaned dataset
save merged_final, replace
