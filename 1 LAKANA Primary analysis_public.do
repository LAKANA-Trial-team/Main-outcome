
clear

version 18.0

/*


Analytical code for LAKANA Main outcome analysis.


*/

* Go into the main directory
cd ..

* Go to the "public" folder
cd "public"

import delimited "LAKANA Primary outcome data_de-ident_HIPAA.csv"

* Go back to the main directory
cd ..

* Set the directory to "results" folder
cd "results"

putpdf begin

putpdf paragraph



* 
 * * TABLE 2
*


preserve

gen totalDeath = eligibledeath
rename totalpyr totalPYR

collapse (sum) totalDeath (sum) totalPYR, by(intv)

* Deaths / 1000 person-years-at-risk
gen mortality = (totalDeath/totalPYR)*1000

** Deaths
sum totalDeath if intv==1
local deaths_1 = r(mean) 

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


* Start table
putpdf paragraph
putpdf text ("Table 2. All-cause mortality among participants who were 1.00 – 11.99-months old at the time of mass-drug administration"), bold /*font(Consolas,12)*/

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

xtpoisson eligibledeath i.intv i.villageclass, exposure(totalpyr) dif iter(10000) 

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
local p1 =  normprob((_b[2.intv]/_se[2.intv]))
local p1 : display %5.3f `p1'
local p2 : display %5.3f normprob((_b[3.intv]/_se[3.intv]))
                                                
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
local p3 : disp %5.3f normprob((r(estimate)/r(se)))
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
putexcel set lakanaresults.xlsx, replace
putexcel A1="irr"
putexcel B1="lb"
putexcel C1="ub"
putexcel D1="pvalue"
putexcel E1="group"
putexcel F1="age"
putexcel G1="globalp"

putexcel A2=`est_1' 
putexcel B2=`lb_1' 
putexcel C2=`ub_1' 
putexcel D2=`p1'
putexcel E2=1

putexcel A3=`est_2' 
putexcel B3=`lb_2' 
putexcel C3=`ub_2' 
putexcel D3=`p2'
putexcel E3=2


putexcel A4=`est_3' 
putexcel B4=`lb_3' 
putexcel C4=`ub_3' 
putexcel D4=`p3'
putexcel E4=3

putexcel G2:G4=`pglobal'
putexcel F2:F4="1to11"

putexcel save
*****

* * IRD
* Predict the \"average\" incidence rate and calculate IRD
gen tempPYR = totalpyr/1000
xtpoisson eligibledeath i.intv i.villageclass, exposure(tempPYR) dif iter(10000) 
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


save "modelEstimatesIRR.dta", replace

restore                                                
                                                


* Next page
putpdf pagebreak

putpdf paragraph

putpdf text ("Supplementary Table 2. Number of recorded infant deaths, by round of mass drug administration (MDA) and study group  "), bold /*font(Consolas,12)*/



matrix w  = (20, 20, 20, 20, 20)
putpdf table Table1 = (11,5), border(all, nil)  width(w) /*border(start) border(insideV, nil) border(end) border(insideH, nil)*/



putpdf table Table1(1,2) = ("Number of Deaths/PYR (Deaths per 1000 PYR)"), colspan(3) halign(center) 

putpdf table Table1(2,1) = ("")
putpdf table Table1(2,2) = ("All"), bold halign(center)
putpdf table Table1(2,3) = ("Control"), bold halign(center)
putpdf table Table1(2,4) = ("Biannual-AZI"), bold halign(center)
putpdf table Table1(2,5) = ("Quarterly-AZI"), bold halign(center)


putpdf table Table1(2,.), border(top) border(bottom)

putpdf table Table1(3,1) = ("MDA 2")
putpdf table Table1(4,1) = ("MDA 3")
putpdf table Table1(5,1) = ("MDA 4")
putpdf table Table1(6,1) = ("MDA 5")
putpdf table Table1(7,1) = ("MDA 6")
putpdf table Table1(8,1) = ("MDA 7")
putpdf table Table1(9,1) = ("MDA 8")
putpdf table Table1(10,1) = ("Visit 9")
putpdf table Table1(11,1) = ("Total")


* This loop sets values to their places in the table

foreach visit in  2 3 4 5 6 7 8 9{
	foreach group in 1 2 3 {
		
	* Count deaths for each MDA and intervention group
	count if death_`visit' == 1 & intv == `group' & eligibledeath == 1
	local death_`visit'_`group' = r(N)
	
	sum pyr_`visit' if intv == `group'
	local pyr_`visit'_`group' : disp %5.0f r(sum)
	disp `pyr_`visit'_`group''
	
	local mortality_`visit'_`group' = `death_`visit'_`group''/ `pyr_`visit'_`group'' * 1000

	local tableLocation1 = `visit' + 1
	local tableLocation2 = `group'+ 2
	
	** Format to reasonable decimals
	local death_`visit'_`group' : disp %5.0f `death_`visit'_`group''
	
	local mortality_`visit'_`group' : disp %5.0f `mortality_`visit'_`group''
	
	putpdf table Table1(`tableLocation1',`tableLocation2') = ("`death_`visit'_`group'' / `pyr_`visit'_`group'' (`mortality_`visit'_`group'') "), ///
	halign(left)
	
	disp `death_`visit'_`group'' 
	disp `mortality_`visit'_`group''
	
	
	local totalDeaths_`group' = `totalDeaths_`group'' + `death_`visit'_`group''
	local totalPYR_`group' = `totalPYR_`group'' + `pyr_`visit'_`group''
	
	
	local totalPYR_`group' : disp %5.0f `totalPYR_`group''
	
	
	
	}
	

	local tableLocationAll1 = `visit' + 1
	local tableLocationAll2 = 2
	
	local totalDeaths_all = `death_`visit'_1' + `death_`visit'_2' + `death_`visit'_3' 
	local totalPYR_all = `pyr_`visit'_1' + `pyr_`visit'_2' + `pyr_`visit'_3'
	local totalMortality_all = `totalDeaths_all' / `totalPYR_all' * 1000
	local totalMortality_all : disp %5.0f `totalMortality_all'
	
	putpdf table Table1(`tableLocationAll1',`tableLocationAll2') = ///
	("`totalDeaths_all' / `totalPYR_all' (`totalMortality_all') "), ///
	halign(left)
	
}

local totalDeaths_all = `totalDeaths_1' + `totalDeaths_2' + `totalDeaths_3'
local totalPYR_all = `totalPYR_1' + `totalPYR_2' + `totalPYR_3'

local totalMortality_all = `totalDeaths_all' / `totalPYR_all' * 1000
local totalMortality_all : disp %5.0f `totalMortality_all'


local totalMortality_1 = `totalDeaths_1' / `totalPYR_1' * 1000
local totalMortality_1 : disp %5.2f `totalMortality_1'

local totalMortality_2 = `totalDeaths_2' / `totalPYR_2' * 1000
local totalMortality_2 : disp %5.2f `totalMortality_2'

local totalMortality_3 = `totalDeaths_3' / `totalPYR_3' * 1000
local totalMortality_3 : disp %5.2f `totalMortality_3'

putpdf table Table1(11,2) = ("`totalDeaths_all' / `totalPYR_all' (`totalMortality_all') "), halign(left)
putpdf table Table1(11,3) = ("`totalDeaths_1' / `totalPYR_1' (`totalMortality_1') "), halign(left)
putpdf table Table1(11,4) = ("`totalDeaths_2' / `totalPYR_2' (`totalMortality_2') "), halign(left)
putpdf table Table1(11,5) = ("`totalDeaths_3' / `totalPYR_3' (`totalMortality_3') "), halign(left)

putpdf table Table1(11,.), border(bottom)


putpdf save "mortality_results.pdf", replace
putpdf clear


xtset villageid

********************************************************************
********************************************************************
********* TIME INVARIANT VARIABLES**********************************
********************************************************************
********************************************************************
**** Sex subgroup ****
xtpoisson eligibledeath i.intv i.hhcompchildsex i.villageclass , exposure(totalpyr) dif iter(100) 
estimates store M0

xtpoisson eligibledeath i.intv##i.hhcompchildsex i.villageclass , exposure(totalpyr) dif iter(100) 
estimates store M1
lrtest M0 M1

* Save p-value of the interaction test for the forest plot 
gen sexIntrP = r(p)

xtpoisson eligibledeath i.intv##i.hhcompchildsex i.villageclass , exposure(totalpyr) dif iter(100) 
contrast intv@hhcompchildsex, effects eform

* Save the estimates and the confidence intervals for the forest plot
matrix b = r(table)
matlist b
svmat b, names(sexCtrlBase)

* Obtain the biannual vs quarterly estimates using biannual AZI as baseline
xtpoisson eligibledeath ib2.intv##i.hhcompchildsex i.villageclass , exposure(totalpyr) dif iter(100) 
contrast intv@hhcompchildsex, effects eform

matrix b = r(table)
matlist b
svmat b, names(sexBianBase)


* Get the absoulte numbers
qui sum eligibledeath if intv==1 & hhcompchildsex==1
gen sexCtrlBoysD = r(sum)
qui sum eligibledeath if intv==1 & hhcompchildsex==2
gen sexCtrlGirlsD = r(sum)
qui sum eligibledeath if intv==2 & hhcompchildsex==1
gen sexBianBoysD = r(sum)
qui sum eligibledeath if intv==2 & hhcompchildsex==2
gen sexBianGirlsD = r(sum)
qui sum eligibledeath if intv==3 & hhcompchildsex==1
gen sexQuartBoysD = r(sum)
qui sum eligibledeath if intv==3 & hhcompchildsex==2
gen sexQuartGirlsD = r(sum)


qui sum totalpyr if intv==1 & hhcompchildsex==1
gen sexCtrlBoysPYR = r(sum)
qui sum totalpyr if intv==1 & hhcompchildsex==2
gen sexCtrlGirlsPYR = r(sum)
qui sum totalpyr if intv==2 & hhcompchildsex==1
gen sexBianBoysPYR = r(sum)
qui sum totalpyr if intv==2 & hhcompchildsex==2
gen sexBianGirlsPYR = r(sum)
qui sum totalpyr if intv==3 & hhcompchildsex==1
gen sexQuartBoysPYR = r(sum)
qui sum totalpyr if intv==3 & hhcompchildsex==2
gen sexQuartGirlsPYR = r(sum)





**** Strategie Avance subgroup ****

xtpoisson eligibledeath i.intv i.strategieavance i.villageclass, exposure(totalpyr) dif iter(100) 
estimates store M0

xtpoisson eligibledeath i.intv##i.strategieavance i.villageclass, exposure(totalpyr) dif iter(100) 
estimates store M1
lrtest M0 M1

gen strategieIntrP = r(p)

xtpoisson eligibledeath i.intv##i.strategieavance i.villageclass , exposure(totalpyr) dif iter(100) 
contrast intv@strategieavance, effects eform

matrix b = r(table)
matlist b
svmat b, names(strategieCtrlBase)


xtpoisson eligibledeath ib2.intv##i.strategieavance i.villageclass , exposure(totalpyr) dif iter(100) 
contrast intv@strategieavance, effects eform

matrix b = r(table)
matlist b
svmat b, names(strategieBianBase)

qui sum eligibledeath if intv==1 & strategieavance==1
gen strategieCtrlFarD = r(sum)
qui sum eligibledeath if intv==1 & strategieavance==0
gen strategieCtrlCloseD = r(sum)
qui sum eligibledeath if intv==2 & strategieavance==1
gen strategieBianFarD = r(sum)
qui sum eligibledeath if intv==2 & strategieavance==0
gen strategieBianCloseD = r(sum)
qui sum eligibledeath if intv==3 & strategieavance==1
gen strategieQuartFarD = r(sum)
qui sum eligibledeath if intv==3 & strategieavance==0
gen strategieQuartCloseD = r(sum)

qui sum totalpyr if intv==1 & strategieavance==1
gen strategieCtrlFarPYR = r(sum)
qui sum totalpyr if intv==1 & strategieavance==0
gen strategieCtrlClosePYR = r(sum)
qui sum totalpyr if intv==2 & strategieavance==1
gen strategieBianFarPYR = r(sum)
qui sum totalpyr if intv==2 & strategieavance==0
gen strategieBianClosePYR = r(sum)
qui sum totalpyr if intv==3 & strategieavance==1
gen strategieQuartFarPYR = r(sum)
qui sum totalpyr if intv==3 & strategieavance==0
gen strategieQuartClosePYR = r(sum)



**** Distance subgroup ****


xtpoisson eligibledeath i.intv i.dist5km i.villageclass, exposure(totalpyr) dif iter(100) 
estimates store M0

xtpoisson eligibledeath i.intv##i.dist5km i.villageclass, exposure(totalpyr) dif iter(100) 
estimates store M1
lrtest M0 M1

gen distanceIntrP = r(p)

xtpoisson eligibledeath i.intv##i.dist5km i.villageclass , exposure(totalpyr) dif iter(100) 
contrast intv@dist5km, effects eform

matrix b = r(table)
matlist b
svmat b, names(distanceCtrlBase)


xtpoisson eligibledeath ib2.intv##i.dist5km i.villageclass , exposure(totalpyr) dif iter(100) 
contrast intv@dist5km, effects eform

matrix b = r(table)
matlist b
svmat b, names(distanceBianBase)

qui sum eligibledeath if intv==1 & dist5km==0
gen distanceCtrlFarD = r(sum)
qui sum eligibledeath if intv==1 & dist5km==1
gen distanceCtrlCloseD = r(sum)
qui sum eligibledeath if intv==2 & dist5km==0
gen distanceBianFarD = r(sum)
qui sum eligibledeath if intv==2 & dist5km==1
gen distanceBianCloseD = r(sum)
qui sum eligibledeath if intv==3 & dist5km==0
gen distanceQuartFarD = r(sum)
qui sum eligibledeath if intv==3 & dist5km==1
gen distanceQuartCloseD = r(sum)

qui sum totalpyr if intv==1 & dist5km==0
gen distanceCtrlFarPYR = r(sum)
qui sum totalpyr if intv==1 & dist5km==1
gen distanceCtrlClosePYR = r(sum)
qui sum totalpyr if intv==2 & dist5km==0
gen distanceBianFarPYR = r(sum)
qui sum totalpyr if intv==2 & dist5km==1
gen distanceBianClosePYR = r(sum)
qui sum totalpyr if intv==3 & dist5km==0
gen distanceQuartFarPYR = r(sum)
qui sum totalpyr if intv==3 & dist5km==1
gen distanceQuartClosePYR = r(sum)



**** Assets subgroup ****

qui sum assetindex,detail
local med = r(p50)

gen sub_assetIndex = 0 if assetindex <= `med'
replace sub_assetIndex = 1 if assetindex > `med'

xtpoisson eligibledeath i.intv i.sub_assetIndex i.villageclass, exposure(totalpyr) dif iter(100) 
estimates store M0

xtpoisson eligibledeath i.intv##i.sub_assetIndex i.villageclass, exposure(totalpyr) dif iter(100) 
estimates store M1
lrtest M0 M1

gen assetIntrP = r(p)

xtpoisson eligibledeath i.intv##i.sub_assetIndex i.villageclass, exposure(totalpyr) dif iter(100) 
contrast intv@sub_assetIndex, effects eform

matrix b = r(table)
matlist b
svmat b, names(sub_assetsCtrlBase)

xtpoisson eligibledeath ib2.intv##i.sub_assetIndex i.villageclass, exposure(totalpyr) dif iter(100) 
contrast intv@sub_assetIndex, effects eform

matrix b = r(table)
matlist b
svmat b, names(sub_assetsBianBase)

qui sum eligibledeath if intv==1 & sub_assetIndex==1
gen assetCtrlAboveD = r(sum)
qui sum eligibledeath if intv==1 & sub_assetIndex==0
gen assetCtrlBelowD = r(sum)
qui sum eligibledeath if intv==2 & sub_assetIndex==1
gen assetBianAboveD = r(sum)
qui sum eligibledeath if intv==2 & sub_assetIndex==0
gen assetBianBelowD = r(sum)
qui sum eligibledeath if intv==3 & sub_assetIndex==1
gen assetQuartAboveD = r(sum)
qui sum eligibledeath if intv==3 & sub_assetIndex==0
gen assetQuartBelowD = r(sum)


qui sum totalpyr if intv==1 & sub_assetIndex==1
gen assetCtrlAbovePYR = r(sum)
qui sum totalpyr if intv==1 & sub_assetIndex==0
gen assetCtrlBelowPYR = r(sum)
qui sum totalpyr if intv==2 & sub_assetIndex==1
gen assetBianAbovePYR = r(sum)
qui sum totalpyr if intv==2 & sub_assetIndex==0
gen assetBianBelowPYR = r(sum)
qui sum totalpyr if intv==3 & sub_assetIndex==1
gen assetQuartAbovePYR = r(sum)
qui sum totalpyr if intv==3 & sub_assetIndex==0
gen assetQuartBelowPYR = r(sum)



**** wash subgroup ****
qui sum wash,detail
local med = r(p50)

gen sub_washindex = 0 if wash <= `med'
replace sub_washindex = 1 if wash > `med'

xtpoisson eligibledeath i.intv i.sub_washindex i.villageclass, exposure(totalpyr) dif iter(100) 
estimates store M0

xtpoisson eligibledeath i.intv##i.sub_washindex i.villageclass, exposure(totalpyr) dif iter(100) 
estimates store M1
lrtest M0 M1

gen washIntrP = r(p)

xtpoisson eligibledeath i.intv##i.sub_washindex i.villageclass, exposure(totalpyr) dif iter(100) 
contrast intv@sub_washindex, effects eform

matrix b = r(table)
matlist b
svmat b, names(sub_washsCtrlBase)

xtpoisson eligibledeath ib2.intv##i.sub_washindex i.villageclass, exposure(totalpyr) dif iter(100) 
contrast intv@sub_washindex, effects eform

matrix b = r(table)
matlist b
svmat b, names(sub_washsBianBase)


qui sum eligibledeath if intv==1 & sub_washindex==1
gen washCtrlAboveD = r(sum)
qui sum eligibledeath if intv==1 & sub_washindex==0
gen washCtrlBelowD = r(sum)
qui sum eligibledeath if intv==2 & sub_washindex==1
gen washBianAboveD = r(sum)
qui sum eligibledeath if intv==2 & sub_washindex==0
gen washBianBelowD = r(sum)
qui sum eligibledeath if intv==3 & sub_washindex==1
gen washQuartAboveD = r(sum)
qui sum eligibledeath if intv==3 & sub_washindex==0
gen washQuartBelowD = r(sum)


qui sum totalpyr if intv==1 & sub_washindex==1
gen washCtrlAbovePYR = r(sum)
qui sum totalpyr if intv==1 & sub_washindex==0
gen washCtrlBelowPYR = r(sum)
qui sum totalpyr if intv==2 & sub_washindex==1
gen washBianAbovePYR = r(sum)
qui sum totalpyr if intv==2 & sub_washindex==0
gen washBianBelowPYR = r(sum)
qui sum totalpyr if intv==3 & sub_washindex==1
gen washQuartAbovePYR = r(sum)
qui sum totalpyr if intv==3 & sub_washindex==0
gen washQuartBelowPYR = r(sum)


**** distr subgroup ****

xtpoisson eligibledeath i.intv i.distr i.villageclass, exposure(totalpyr) dif iter(1000) 
estimates store M0

xtpoisson eligibledeath i.intv##i.distr i.villageclass, exposure(totalpyr) dif iter(100) 
estimates store M1
lrtest M0 M1

gen distrIntrP = r(p)


xtpoisson eligibledeath i.intv##i.distr i.villageclass, exposure(totalpyr) dif iter(100) 
contrast intv@distr, effects eform

matrix b = r(table)
matlist b
svmat b, names(distrCtrlBase)

xtpoisson eligibledeath ib2.intv##i.distr i.villageclass, exposure(totalpyr) dif iter(100) 
contrast intv@distr, effects eform

matrix b = r(table)
matlist b
svmat b, names(distrBianBase)



**Ctrl
qui sum eligibledeath if intv==1 & distr==1
gen distrCtrlFanaD = r(sum)
qui sum eligibledeath if intv==1 & distr==2
gen distrCtrlKalabancoroD = r(sum)
qui sum eligibledeath if intv==1 & distr==3
gen distrCtrlKangabaD = r(sum)
qui sum eligibledeath if intv==1 & distr==4
gen distrCtrlKatiD = r(sum)
qui sum eligibledeath if intv==1 & distr==5
gen distrCtrlKitaD = r(sum)
qui sum eligibledeath if intv==1 & distr==6
gen distrCtrlKoulikoroD = r(sum)
qui sum eligibledeath if intv==1 & distr==7
gen distrCtrlKeniebaD = r(sum)
qui sum eligibledeath if intv==1 & distr==8
gen distrCtrlOuelessebougouD = r(sum)
qui sum eligibledeath if intv==1 & distr==9
gen distrCtrlSagabariD = r(sum)
qui sum eligibledeath if intv==1 & distr==10
gen distrCtrlSefetoD = r(sum)
qui sum eligibledeath if intv==1 & distr==11
gen distrCtrlSelingueD = r(sum)
**Ctrl

**Bian
qui sum eligibledeath if intv==2 & distr==1
gen distrBianFanaD = r(sum)
qui sum eligibledeath if intv==2 & distr==2
gen distrBianKalabancoroD = r(sum)
qui sum eligibledeath if intv==2 & distr==3
gen distrBianKangabaD = r(sum)
qui sum eligibledeath if intv==2 & distr==4
gen distrBianKatiD = r(sum)
qui sum eligibledeath if intv==2 & distr==5
gen distrBianKitaD = r(sum)
qui sum eligibledeath if intv==2 & distr==6
gen distrBianKoulikoroD = r(sum)
qui sum eligibledeath if intv==2 & distr==7
gen distrBianKeniebaD = r(sum)
qui sum eligibledeath if intv==2 & distr==8
gen distrBianOuelessebougouD = r(sum)
qui sum eligibledeath if intv==2 & distr==9
gen distrBianSagabariD = r(sum)
qui sum eligibledeath if intv==2 & distr==10
gen distrBianSefetoD = r(sum)
qui sum eligibledeath if intv==2 & distr==11
gen distrBianSelingueD = r(sum)

**Bian

**Quart
qui sum eligibledeath if intv==3 & distr==1
gen distrQuartFanaD = r(sum)
qui sum eligibledeath if intv==3 & distr==2
gen distrQuartKalabancoroD = r(sum)
qui sum eligibledeath if intv==3 & distr==3
gen distrQuartKangabaD = r(sum)
qui sum eligibledeath if intv==3 & distr==4
gen distrQuartKatiD = r(sum)
qui sum eligibledeath if intv==3 & distr==5
gen distrQuartKitaD = r(sum)
qui sum eligibledeath if intv==3 & distr==6
gen distrQuartKoulikoroD = r(sum)
qui sum eligibledeath if intv==3 & distr==7
gen distrQuartKeniebaD = r(sum)
qui sum eligibledeath if intv==3 & distr==8
gen distrQuartOuelessebougouD = r(sum)
qui sum eligibledeath if intv==3 & distr==9
gen distrQuartSagabariD = r(sum)
qui sum eligibledeath if intv==3 & distr==10
gen distrQuartSefetoD = r(sum)
qui sum eligibledeath if intv==3 & distr==11
gen distrQuartSelingueD = r(sum)
**Quart



***** totalPYR 

**Ctrl
qui sum totalpyr if intv==1 & distr==1
gen distrCtrlFanaPYR = r(sum)
qui sum totalpyr if intv==1 & distr==2
gen distrCtrlKalabancoroPYR = r(sum)
qui sum totalpyr if intv==1 & distr==3
gen distrCtrlKangabaPYR = r(sum)
qui sum totalpyr if intv==1 & distr==4
gen distrCtrlKatiPYR = r(sum)
qui sum totalpyr if intv==1 & distr==5
gen distrCtrlKitaPYR = r(sum)
qui sum totalpyr if intv==1 & distr==6
gen distrCtrlKoulikoroPYR = r(sum)
qui sum totalpyr if intv==1 & distr==7
gen distrCtrlKeniebaPYR = r(sum)
qui sum totalpyr if intv==1 & distr==8
gen distrCtrlOuelessebougouPYR = r(sum)
qui sum totalpyr if intv==1 & distr==9
gen distrCtrlSagabariPYR = r(sum)
qui sum totalpyr if intv==1 & distr==10
gen distrCtrlSefetoPYR = r(sum)
qui sum totalpyr if intv==1 & distr==11
gen distrCtrlSelinguePYR = r(sum)
**Ctrl

**Bian
qui sum totalpyr if intv==2 & distr==1
gen distrBianFanaPYR = r(sum)
qui sum totalpyr if intv==2 & distr==2
gen distrBianKalabancoroPYR = r(sum)
qui sum totalpyr if intv==2 & distr==3
gen distrBianKangabaPYR = r(sum)
qui sum totalpyr if intv==2 & distr==4
gen distrBianKatiPYR = r(sum)
qui sum totalpyr if intv==2 & distr==5
gen distrBianKitaPYR = r(sum)
qui sum totalpyr if intv==2 & distr==6
gen distrBianKoulikoroPYR = r(sum)
qui sum totalpyr if intv==2 & distr==7
gen distrBianKeniebaPYR = r(sum)
qui sum totalpyr if intv==2 & distr==8
gen distrBianOuelessebougouPYR = r(sum)
qui sum totalpyr if intv==2 & distr==9
gen distrBianSagabariPYR = r(sum)
qui sum totalpyr if intv==2 & distr==10
gen distrBianSefetoPYR = r(sum)
qui sum totalpyr if intv==2 & distr==11
gen distrBianSelinguePYR = r(sum)

**Bian

**Quart
qui sum totalpyr if intv==3 & distr==1
gen distrQuartFanaPYR = r(sum)
qui sum totalpyr if intv==3 & distr==2
gen distrQuartKalabancoroPYR = r(sum)
qui sum totalpyr if intv==3 & distr==3
gen distrQuartKangabaPYR = r(sum)
qui sum totalpyr if intv==3 & distr==4
gen distrQuartKatiPYR = r(sum)
qui sum totalpyr if intv==3 & distr==5
gen distrQuartKitaPYR = r(sum)
qui sum totalpyr if intv==3 & distr==6
gen distrQuartKoulikoroPYR = r(sum)
qui sum totalpyr if intv==3 & distr==7
gen distrQuartKeniebaPYR = r(sum)
qui sum totalpyr if intv==3 & distr==8
gen distrQuartOuelessebougouPYR = r(sum)
qui sum totalpyr if intv==3 & distr==9
gen distrQuartSagabariPYR = r(sum)
qui sum totalpyr if intv==3 & distr==10
gen distrQuartSefetoPYR = r(sum)
qui sum totalpyr if intv==3 & distr==11
gen distrQuartSelinguePYR = r(sum)
**Quart







* Save results for the forestplot
preserve 


keep distr* distance* sex* sub_washs* sub_assets* asset* wash* strategie* // To add: Strategie Avance
drop if sexCtrlBase1==.

save "Results.dta", replace

restore

drop distr* distance* sex* sub_washs* sub_assets*




/*

Data into longitudinal format

*/


reshape long eligible_ pyr_ age_ death_ ///
 smc_ season_ calendaryear_ sub_age_ underweight_, i(anon_id) j(visit)





** Make 'lag' variables: ** 
*** Values need to be moved one row "downward" so that 
*** the effect modifier variable corresponds to "value at last MDA" as opposed to
*** "value at this MDA"
sort anon_id visit
by anon_id: gen lagAge = sub_age_[_n-1]
replace sub_age_ = lagAge



sort anon_id visit
by anon_id: gen lagCalendar = calendaryear_[_n-1]
replace calendaryear_ = lagCalendar

gen underweight = underweight_

sort anon_id visit
by anon_id: gen lagWAZ = underweight[_n-1]
replace underweight = lagWAZ

*bysort anon_id: carryforward lagWAZ if age_<15, gen(sensitivityWAZ)
*replace underweight = sensitivityWAZ


sort anon_id visit
by anon_id: gen lagSMC = smc_[_n-1]
replace smc = lagSMC

sort anon_id visit
by anon_id: gen lagSeason = season_[_n-1]
replace season_ = lagSeason




sort anon_id visit
by anon_id: gen lagVisit = visit[_n-1]
gen orderOfVisit = lagVisit


replace death_=. if death_==1 & eligibledeath!=1

******************************************************************

******************************************************************

******************************************************************

**** TIME VARIANT ANALYSIS STARTS ********************************

******************************************************************

******************************************************************

**** Age subgroup



xtpoisson death_ i.intv i.sub_age i.villageclass,  exposure(pyr_) dif iter(100) 
estimates store M0

xtpoisson death_ i.intv##i.sub_age i.villageclass,  exposure(pyr_) dif iter(100) 
estimates store M1
lrtest M0 M1


gen ageIntrP = r(p)

xtpoisson death_ i.intv##i.sub_age  i.villageclass ,  exposure(pyr_) dif iter(100) 
contrast intv@sub_age, effects eform
 matrix k = r(table)
 svmat k, names(ageCtrlBase)

xtpoisson death_ ib2.intv##i.sub_age  i.villageclass ,  exposure(pyr_) dif iter(100) 
contrast intv@sub_age, effects eform
 matrix k = r(table)
 svmat k, names(ageBianBase) 
 
 
 
qui sum death_ if intv==1 & sub_age==1 
gen ageCtrlyoungD = r(sum)
qui sum death_ if intv==1 & sub_age==2 
gen ageCtrloldD = r(sum)

qui sum death_ if intv==2 & sub_age==1 
gen ageBianyoungD = r(sum)
qui sum death_ if intv==2 & sub_age==2 
gen ageBianoldD = r(sum)

qui sum death_ if intv==3 & sub_age==1 
gen ageQuartyoungD = r(sum)
qui sum death_ if intv==3 & sub_age==2 
gen ageQuartoldD = r(sum)


**PYR 
qui sum pyr_ if intv==1 & sub_age==1 
gen ageCtrlyoungPYR = r(sum)
qui sum pyr_ if intv==1 & sub_age==2 
gen ageCtrloldPYR = r(sum)

qui sum pyr_ if intv==2 & sub_age==1 
gen ageBianyoungPYR = r(sum)
qui sum pyr_ if intv==2 & sub_age==2 
gen ageBianoldPYR = r(sum)

qui sum pyr_ if intv==3 & sub_age==1 
gen ageQuartyoungPYR = r(sum)
qui sum pyr_ if intv==3 & sub_age==2 
gen ageQuartoldPYR = r(sum)
 
*********************************







**** Underweight subgroup ****

xtpoisson death_ i.intv i.underweight i.villageclass ,  exposure(pyr_) dif iter(100) 
estimates store M0

xtpoisson death_ i.intv##i.underweight i.villageclass ,  exposure(pyr_) dif iter(100) 
estimates store M1
lrtest M0 M1

gen wazIntrP = r(p)

xtpoisson death_ i.intv##i.underweight i.villageclass ,  exposure(pyr_) dif iter(100) 
contrast intv@underweight, effects eform

matrix b = r(table)
matlist b
svmat b, names(wazCtrlBase)


xtpoisson death_ ib2.intv##i.underweight i.villageclass , exposure(pyr_) dif iter(100) 
contrast intv@underweight, effects eform

matrix b = r(table)
matlist b
svmat b, names(wazBianBase)


qui sum death_ if intv==1 & underweight==0
gen wazCtrlNonUnD = r(sum)
qui sum death_ if intv==1 & underweight==1
gen wazCtrlUnD = r(sum)

qui sum death_ if intv==2 & underweight==0
gen wazBianNonUnD = r(sum)
qui sum death_ if intv==2 & underweight==1
gen wazBianUnD = r(sum)

qui sum death_ if intv==3 & underweight==0
gen wazQuartNonUnD = r(sum)
qui sum death_ if intv==3 & underweight==1
gen wazQuartUnD = r(sum)

** PYR

qui sum pyr_ if intv==1 & underweight==0
gen wazCtrlNonUnPYR = r(sum)
qui sum pyr_ if intv==1 & underweight==1
gen wazCtrlUnPYR = r(sum)

qui sum pyr_ if intv==2 & underweight==0
gen wazBianNonUnPYR = r(sum)
qui sum pyr_ if intv==2 & underweight==1
gen wazBianUnPYR = r(sum)

qui sum pyr_ if intv==3 & underweight==0
gen wazQuartNonUnPYR = r(sum)
qui sum pyr_ if intv==3 & underweight==1
gen wazQuartUnPYR = r(sum)


**** SMC subgroup ****


xtpoisson death_ i.intv i.smc i.villageclass,  exposure(pyr_) dif iter(100) 
estimates store M0

xtpoisson death_ i.intv##i.smc i.villageclass,  exposure(pyr_) dif iter(100) 
estimates store M1

lrtest M0 M1


gen smcIntrP = r(p)

xtpoisson death_ i.intv##i.smc_ i.villageclass, exposure(pyr_) dif iter(100) 
contrast intv@smc_, effects eform
matrix b = r(table)
matlist b
svmat b, names(smcCtrlBase)

xtpoisson death_ ib2.intv##i.smc_ i.villageclass, exposure(pyr_) dif iter(100) 
contrast intv@smc_, effects eform
matrix b = r(table)
matlist b
svmat b, names(smcBianBase)


qui sum death_ if intv==1 & smc_==0
gen smcCtrlNonSmcD = r(sum)
qui sum death_ if intv==1 & smc_==1
gen smcCtrlSMCD = r(sum)

qui sum death_ if intv==2 & smc_==0
gen smcBianNonSmcD = r(sum)
qui sum death_ if intv==2 & smc_==1
gen smcBianSMCD = r(sum)

qui sum death_ if intv==3 & smc_==0
gen smcQuartNonSmcD = r(sum)
qui sum death_ if intv==3 & smc_==1
gen smcQuartSMCD = r(sum)


** PYR
qui sum pyr_ if intv==1 & smc_==0
gen smcCtrlNonSmcPYR = r(sum)
qui sum pyr_ if intv==1 & smc_==1
gen smcCtrlSMCPYR = r(sum)

qui sum pyr_ if intv==2 & smc_==0
gen smcBianNonSmcPYR = r(sum)
qui sum pyr_ if intv==2 & smc_==1
gen smcBianSMCPYR = r(sum)

qui sum pyr_ if intv==3 & smc_==0
gen smcQuartNonSmcPYR = r(sum)
qui sum pyr_ if intv==3 & smc_==1
gen smcQuartSMCPYR = r(sum)


drop smc_






**** Order of MDA visit **** ADD CALENDAR YEAR
xtpoisson death_ i.intv i.orderOfVisit sub_age season_ i.villageclass calendaryear_, exposure(pyr_) dif iter(100) 
estimates store M0

xtpoisson death_ i.intv##i.orderOfVisit sub_age season_  i.villageclass calendaryear_, exposure(pyr_) dif iter(100) 
estimates store M1
lrtest M0 M1
gen orderIntP = r(p)

xtpoisson death_ i.intv##i.orderOfVisit sub_age season_  i.villageclass calendaryear_ , exposure(pyr_) dif iter(100) 
contrast intv@orderOfVisit, effects eform
matrix b = r(table)
matlist b
svmat b, names(orderCtrlBase)

xtpoisson death_ ib2.intv##i.orderOfVisit sub_age season_  i.villageclass calendaryear_ , exposure(pyr_) dif iter(100) 
contrast intv@orderOfVisit, effects eform
matrix b = r(table)
matlist b
svmat b, names(orderBianBase)


qui sum death_ if intv==1 & orderOfVisit==1
gen orderCtrlOneD = r(sum)
qui sum death_ if intv==1 & orderOfVisit==2
gen orderCtrlTwoD = r(sum)
qui sum death_ if intv==1 & orderOfVisit==3
gen orderCtrlThreeD = r(sum)
qui sum death_ if intv==1 & orderOfVisit==4
gen orderCtrlFourD = r(sum)
qui sum death_ if intv==1 & orderOfVisit==5
gen orderCtrlFiveD = r(sum)
qui sum death_ if intv==1 & orderOfVisit==6
gen orderCtrlSixD = r(sum)
qui sum death_ if intv==1 & orderOfVisit==7
gen orderCtrlSevenD = r(sum)
qui sum death_ if intv==1 & orderOfVisit==8
gen orderCtrlEightD = r(sum)

qui sum death_ if intv==2 & orderOfVisit==1
gen orderBianOneD = r(sum)
qui sum death_ if intv==2 & orderOfVisit==2
gen orderBianTwoD = r(sum)
qui sum death_ if intv==2 & orderOfVisit==3
gen orderBianThreeD = r(sum)
qui sum death_ if intv==2 & orderOfVisit==4
gen orderBianFourD = r(sum)
qui sum death_ if intv==2 & orderOfVisit==5
gen orderBianFiveD = r(sum)
qui sum death_ if intv==2 & orderOfVisit==6
gen orderBianSixD = r(sum)
qui sum death_ if intv==2 & orderOfVisit==7
gen orderBianSevenD = r(sum)
qui sum death_ if intv==2 & orderOfVisit==8
gen orderBianEightD = r(sum)

qui sum death_ if intv==3 & orderOfVisit==1
gen orderQuartOneD = r(sum)
qui sum death_ if intv==3 & orderOfVisit==2
gen orderQuartTwoD = r(sum)
qui sum death_ if intv==3 & orderOfVisit==3
gen orderQuartThreeD = r(sum)
qui sum death_ if intv==3 & orderOfVisit==4
gen orderQuartFourD = r(sum)
qui sum death_ if intv==3 & orderOfVisit==5
gen orderQuartFiveD = r(sum)
qui sum death_ if intv==3 & orderOfVisit==6
gen orderQuartSixD = r(sum)
qui sum death_ if intv==3 & orderOfVisit==7
gen orderQuartSevenD = r(sum)
qui sum death_ if intv==3 & orderOfVisit==8
gen orderQuartEightD = r(sum)



*

qui sum pyr_ if intv==1 & orderOfVisit==1
gen orderCtrlOnePYR = r(sum)
qui sum pyr_ if intv==1 & orderOfVisit==2
gen orderCtrlTwoPYR = r(sum)
qui sum pyr_ if intv==1 & orderOfVisit==3
gen orderCtrlThreePYR = r(sum)
qui sum pyr_ if intv==1 & orderOfVisit==4
gen orderCtrlFourPYR = r(sum)
qui sum pyr_ if intv==1 & orderOfVisit==5
gen orderCtrlFivePYR = r(sum)
qui sum pyr_ if intv==1 & orderOfVisit==6
gen orderCtrlSixPYR = r(sum)
qui sum pyr_ if intv==1 & orderOfVisit==7
gen orderCtrlSevenPYR = r(sum)
qui sum pyr_ if intv==1 & orderOfVisit==8
gen orderCtrlEightPYR = r(sum)

qui sum pyr_ if intv==2 & orderOfVisit==1
gen orderBianOnePYR = r(sum)
qui sum pyr_ if intv==2 & orderOfVisit==2
gen orderBianTwoPYR = r(sum)
qui sum pyr_ if intv==2 & orderOfVisit==3
gen orderBianThreePYR = r(sum)
qui sum pyr_ if intv==2 & orderOfVisit==4
gen orderBianFourPYR = r(sum)
qui sum pyr_ if intv==2 & orderOfVisit==5
gen orderBianFivePYR = r(sum)
qui sum pyr_ if intv==2 & orderOfVisit==6
gen orderBianSixPYR = r(sum)
qui sum pyr_ if intv==2 & orderOfVisit==7
gen orderBianSevenPYR = r(sum)
qui sum pyr_ if intv==2 & orderOfVisit==8
gen orderBianEightPYR = r(sum)

qui sum pyr_ if intv==3 & orderOfVisit==1
gen orderQuartOnePYR = r(sum)
qui sum pyr_ if intv==3 & orderOfVisit==2
gen orderQuartTwoPYR = r(sum)
qui sum pyr_ if intv==3 & orderOfVisit==3
gen orderQuartThreePYR = r(sum)
qui sum pyr_ if intv==3 & orderOfVisit==4
gen orderQuartFourPYR = r(sum)
qui sum pyr_ if intv==3 & orderOfVisit==5
gen orderQuartFivePYR = r(sum)
qui sum pyr_ if intv==3 & orderOfVisit==6
gen orderQuartSixPYR = r(sum)
qui sum pyr_ if intv==3 & orderOfVisit==7
gen orderQuartSevenPYR = r(sum)
qui sum pyr_ if intv==3 & orderOfVisit==8
gen orderQuartEightPYR = r(sum)



drop orderOfVisit



**** Seasonality subgroup ****

xtpoisson death_ i.intv i.season_ i.villageclass, exposure(pyr_) dif iter(100) 
estimates store M0

xtpoisson death_ i.intv##i.season_ i.villageclass, exposure(pyr_) dif iter(100) 
estimates store M1
lrtest M0 M1

gen seasonIntP = r(p)

xtpoisson death_ i.intv##i.season_ i.villageclass , exposure(pyr_) dif iter(100) 
contrast intv@season_, effects eform
matrix b = r(table)
matlist b
svmat b, names(seasonCtrlBase)

xtpoisson death_ ib2.intv##i.season_ i.villageclass , exposure(pyr_) dif iter(100) 
contrast intv@season_, effects eform
matrix b = r(table)
matlist b
svmat b, names(seasonBianBase)


qui sum death_ if intv==1 & season_==1
gen seasonCtrlDryD = r(sum)
qui sum death_ if intv==1 & season_==2
gen seasonCtrlRainyD = r(sum)
qui sum death_ if intv==2 & season_==1
gen seasonBianDryD = r(sum)
qui sum death_ if intv==2 & season_==2
gen seasonBianRainyD = r(sum)
qui sum death_ if intv==3 & season_==1
gen seasonQuartDryD = r(sum)
qui sum death_ if intv==3 & season_==2
gen seasonQuartRainyD = r(sum)

qui sum pyr_ if intv==1 & season_==1
gen seasonCtrlDryPYR = r(sum)
qui sum pyr_ if intv==1 & season_==2
gen seasonCtrlRainyPYR = r(sum)
qui sum pyr_ if intv==2 & season_==1
gen seasonBianDryPYR = r(sum)
qui sum pyr_ if intv==2 & season_==2
gen seasonBianRainyPYR = r(sum)
qui sum pyr_ if intv==3 & season_==1
gen seasonQuartDryPYR = r(sum)
qui sum pyr_ if intv==3 & season_==2
gen seasonQuartRainyPYR = r(sum)


drop season_


preserve 
keep age* smc* season*  waz* order* 


save "ResultsLong.dta", replace

restore

clear