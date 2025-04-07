
clear


version 18.0

/*

For comparison, IRR estimates for 12-59 mo children

*/


cd ..
cd "public"

*use "LAKANA data with code, 12-59 mo.dta"

import delimited "LAKANA Primary outcome data_older.csv"

cd ..
cd "results"



putpdf begin

putpdf paragraph




* 
 * * TABLE 2
*



preserve

gen totalDeath = died_1259
rename totalpyr totalPYR

collapse (sum) totalDeath (sum) totalPYR, by(intv)

* Deaths / 1000 person-years-at-risk
gen mortality = (totalDeath/totalPYR)*1000

** Deaths
sum totalDeath if intv==1
local deaths_1 = r(mean) //There should be only one value so mean should be ok

sum totalDeath if intv==2
local deaths_2 = r(mean)

sum totalDeath if intv==3
local deaths_3 = r(mean)


** PYR
sum totalPYR if intv==1
local PYR_1 : disp %5.2f r(mean) //Formated to two decimals

sum totalPYR if intv==2
local PYR_2 : disp %5.2f r(mean)

sum totalPYR if intv==3
local PYR_3 : disp %5.2f r(mean)

** deaths / 1000 PYR
sum mortality if intv==1
local mortality_1 : disp %5.2f r(mean)

sum mortality if intv==2
local mortality_2 : disp %5.2f r(mean)

sum mortality if intv==3
local mortality_3 : disp %5.2f r(mean)


* Start second table
putpdf paragraph
putpdf text ("Supplementary Table 3. All-cause mortality among participants who were 12 – 59 months old at the time of mass-drug administration"), bold /*font(Consolas,12)*/

putpdf table Table2 = (7,5), border(all, nil) /*border(start) border(insideV, nil) border(end) border(insideH, nil) */

putpdf table Table2(1,1) = (" ")
putpdf table Table2(1,2) = ("Control"), bold halign(center)
putpdf table Table2(1,3) = ("Biannual-AZI"), bold halign(center)
putpdf table Table2(1,4) = ("Quarterly-AZI"), bold halign(center)
putpdf table Table2(1,.),  border(bottom)
putpdf table Table2(5,5) = ("Quarterly-AZI vs Biannual-AZI"), bold halign(center)
putpdf table Table2(5,5),  border(top)
putpdf table Table2(5,5),  border(bottom)

putpdf table Table2(2,1) = ("Number of deaths")
putpdf table Table2(2,2) = (  `deaths_1' ), halign(center)
putpdf table Table2(2,3) = (  `deaths_2' ), halign(center)
putpdf table Table2(2,4) = ( `deaths_3'), halign(center)
putpdf table Table2(2,5) = (" "), halign(center)
                                                
putpdf table Table2(3,1) = ("Number of PYR")
putpdf table Table2(3,2) = ( `PYR_1' ), halign(center)
putpdf table Table2(3,3) = ( `PYR_2' ), halign(center)
putpdf table Table2(3,4) = ( `PYR_3' ), halign(center)
putpdf table Table2(3,5) = (" "), halign(center)

putpdf table Table2(4,1) = ("Number of deaths/1000 PYR")
putpdf table Table2(4,2) = ( `mortality_1' ), nformat(%5.2f) halign(center)
putpdf table Table2(4,3) = ( `mortality_2' ), halign(center)
putpdf table Table2(4,4) = ( `mortality_3' ), halign(center)
putpdf table Table2(4,5) = (" "), halign(center)
                                                
restore









*** *** *** *** *** *** *** *** ***
    *** MAIN OUTCOME MODEL  ***
*** *** *** *** *** *** *** *** ***

preserve

set seed 123
xtset villageid

xtpoisson died_1259 i.intv i.villageclass, exposure(totalpyr) dif iter(10000) 

matrix k = r(table)
matlist k

svmat k

test _b[2.intv]=_b[3.intv]=0
local pglobal=r(p)

gen pglobalMain = `pglobal'


* Get the estimates for the table
local est_1 = exp(_b[2.intv])
local est_1  : display %5.2f `est_1'

local est_2 : display %5.2f exp( _b[3.intv] )

* One sided p-values
local p1 =  normprob(_b[2.intv]/_se[2.intv])
local p1 : display %5.3f `p1'
local p2 : display %5.3f normprob(_b[3.intv]/_se[3.intv])
                                                
* Lower bound of 95% CI confidence interval
local lb_1 =  exp( _b[2.intv] - 1.96* _se[2.intv] )
local lb_1 : display %5.2f `lb_1'
local lb_2 : display %5.2f  exp( _b[3.intv] - 1.96* _se[3.intv] )

* Upper bound of 95% CI confidence interval
local ub_1 = exp( _b[2.intv] + 1.96* _se[2.intv] )
local ub_1 : display %5.2f `ub_1'
local ub_2 : disp %5.2f exp( _b[3.intv] + 1.96* _se[3.intv] )
                                                
                                                
* Group 2 vs group 3
qui lincom 3.intv-2.intv
local est_3 : disp %5.2f exp( r(estimate) )
local p3 : disp %5.3f normprob(r(estimate)/r(se))
local lb_3 : disp %5.2f exp( r(estimate) - 1.96* r(se) )
local ub_3 : disp %5.2f exp( r(estimate) + 1.96* r(se) )

gen est3v2 = r(estimate)
gen se3v2 = r(se)


* Table 2, add IRR information
putpdf table Table2(6,1) = ("Incidence rate ratio (95% CI, one-sided p-value)\")
putpdf table Table2(6,2) = ("REF"), halign(center)
putpdf table Table2(6,3) = ("`est_1' (`lb_1' to `ub_1', `p1')"), halign(center)
putpdf table Table2(6,4) = ("`est_2' (`lb_2' to `ub_2', `p2')"), halign(center)
putpdf table Table2(6,5) = ("`est_3' (`lb_3' to `ub_3', `p3')"), halign(center)
                                                

**** Setting up a result excel to make a graph ****
putexcel set lakanaresults.xlsx, modify

putexcel A5=`est_1' 
putexcel B5=`lb_1' 
putexcel C5=`ub_1' 
putexcel D5=`p1'
putexcel E5=1

putexcel A6=`est_2' 
putexcel B6=`lb_2' 
putexcel C6=`ub_2' 
putexcel D6=`p2'
putexcel E6=2


putexcel A7=`est_3' 
putexcel B7=`lb_3' 
putexcel C7=`ub_3' 
putexcel D7=`p3'
putexcel E7=3

putexcel G5:G7=`pglobal'
putexcel F5:F7="12to59"

putexcel save
*******                                                
												
* * IRD
* Predict the \"average\" incidence rate and calculate IRD
gen tempPYR = totalpyr/1000
xtpoisson died_1259 i.intv i.villageclass, exposure(tempPYR) dif iter(10000) 
margins intv, predict(iru0) pwcompare(effects) 
matrix b = r(table_vs)
                                                
* Get the estimates and CIs for the table
local est_1 : display %5.3f b[1,1]
local lb_1 : display %5.3f b[5,1]
local ub_1 : display %5.3f b[6,1]
local p1 : display %5.3f b[4,1]
                                                
local est_2 : display %5.3f b[1,2]
local lb_2 : display %5.3f b[5,2]
local ub_2 : display %5.3f b[6,2]
local p2 : display %5.3f b[4,2]
                                                
local est_3 : display %5.3f b[1,3]
local lb_3 : display %5.3f b[5,3]
local ub_3 : display %5.3f b[6,3]
local p3 : display %5.3f b[4,3]
                                                
* Table 2, add IRD information
putpdf table Table2(7,1) = ("Incidence rate difference (95% CI, two-sided p-value)")
putpdf table Table2(7,2) = ("REF"), halign(center)
putpdf table Table2(7,3) = ("`est_1' (`lb_1' to `ub_1', `p1')"), halign(center)
putpdf table Table2(7,4) = ("`est_2' (`lb_2' to `ub_2', `p2')"), halign(center)
putpdf table Table2(7,5) = ("`est_3' (`lb_3' to `ub_3', `p3')"), halign(center)
                                                
putpdf table Table2(7,.), border(bottom)



keep k* est3v2 se3v2 pglobalMain

keep if k2!=.


save "modelEstimatesIRR_12_59.dta", replace

restore           




putpdf save "mortality_results_12_59.pdf", replace
putpdf clear
            