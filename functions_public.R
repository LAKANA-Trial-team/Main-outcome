### LAKANA
### Public functions for data processing
### 




customMerge <- function(df,integrateDF,keyX,keyY,vectorOfVariables) {
  
  
  df$id <- df[,keyX]
  df <- df[order(df$id),]
  setDT(df)
  setkey(df,id)
  
  integrateDF$id <- integrateDF[,keyY]
  
  integrateDF <- integrateDF[order(integrateDF$id),]
  
  setDT(integrateDF)
  setkey(integrateDF,id)  
  
  for(var in vectorOfVariables){
    print(var)
    
    integrateDF[,variable := get(var)]
    
    df[integrateDF, paste0(var) :=i.variable]
    
    
  }
  
  df <- as.data.frame(df)
  
  
  return(df[,-which(names(df)=="id")])   
}



## If NULL, give NA value
populateNull <- function(someValue) {
  
  returnValue <- ifelse(!is.null(someValue),someValue,NA)
  
  return(returnValue)
}






makeTableFromStataOutput <- function() {
  
  library(stringi)
  library(forestplot)
  library(tidyverse)
  # Time-invariant
  results1 <- haven::read_dta(paste0(here::here("results"),"/Results.dta")) %>% 
    as.data.frame()
  
  sub_effect_list <- list()
  sub_pvalues_list <- list()
  sub_numbers_list <- list()
  ## P-values
  sub_pvalues_list[["sex_pvalue"]] = results1$sexIntrP[1]  %>% round(2)
  sub_pvalues_list[["dist_pvalue"]] = results1$distanceIntrP[1]  %>% round(2)
  sub_pvalues_list[["strategie_pvalue"]] = results1$strategieIntrP[1]  %>% round(2)
  
  sub_pvalues_list[["assetIndex_pvalue"]] = results1$assetIntrP[1]  %>% round(2)
  sub_pvalues_list[["washindex_pvalue"]] = results1$washIntrP[1]  %>% round(2)
  
  
  sub_pvalues_list[["distr_pvalue"]] = results1$distrIntrP[1]  %>% round(2)
  
  
  
  
  
  ## Estimates and CIs
  
  
  
  
  # sex
  #Ctrl vs bian
  sub_effect_list[["irr_ctrl_bian_sex_Male"]] = results1$sexCtrlBase1[1]
  sub_effect_list[["irr_ctrl_bian_sex_Male_lb"]] = results1$sexCtrlBase1[5]
  sub_effect_list[["irr_ctrl_bian_sex_Male_ub"]] = results1$sexCtrlBase1[6]
  
  sub_effect_list[["irr_ctrl_bian_sex_Female"]] = results1$sexCtrlBase2[1]
  sub_effect_list[["irr_ctrl_bian_sex_Female_lb"]] = results1$sexCtrlBase2[5]
  sub_effect_list[["irr_ctrl_bian_sex_Female_ub"]] = results1$sexCtrlBase2[6]
  
  sub_effect_list[["irr_ctrl_quart_sex_Male"]] = results1$sexCtrlBase3[1]
  sub_effect_list[["irr_ctrl_quart_sex_Male_lb"]] = results1$sexCtrlBase3[5]
  sub_effect_list[["irr_ctrl_quart_sex_Male_ub"]] = results1$sexCtrlBase3[6]
  
  sub_effect_list[["irr_ctrl_quart_sex_Female"]] = results1$sexCtrlBase4[1]
  sub_effect_list[["irr_ctrl_quart_sex_Female_lb"]] = results1$sexCtrlBase4[5]
  sub_effect_list[["irr_ctrl_quart_sex_Female_ub"]] = results1$sexCtrlBase4[6]
  
  
  sub_effect_list[["irr_bian_quart_sex_Male"]] = results1$sexBianBase3[1]
  sub_effect_list[["irr_bian_quart_sex_Male_lb"]] = results1$sexBianBase3[5]
  sub_effect_list[["irr_bian_quart_sex_Male_ub"]] = results1$sexBianBase3[6]
  
  sub_effect_list[["irr_bian_quart_sex_Female"]] = results1$sexBianBase4[1]
  sub_effect_list[["irr_bian_quart_sex_Female_lb"]] = results1$sexBianBase4[5]
  sub_effect_list[["irr_bian_quart_sex_Female_ub"]] = results1$sexBianBase4[6]
  
  
  
  
  
  ## Distance 
  sub_effect_list[["irr_ctrl_bian_distance_less5"]] = results1$distanceCtrlBase1[1]
  sub_effect_list[["irr_ctrl_bian_distance_over5"]] = results1$distanceCtrlBase2[1]
  sub_effect_list[["irr_ctrl_quart_distance_less5"]] = results1$distanceCtrlBase3[1]
  sub_effect_list[["irr_ctrl_quart_distance_over5"]] = results1$distanceCtrlBase4[1]
  
  sub_effect_list[["irr_ctrl_bian_distance_less5_lb"]] = results1$distanceCtrlBase1[5]
  sub_effect_list[["irr_ctrl_bian_distance_over5_lb"]] = results1$distanceCtrlBase2[5]
  sub_effect_list[["irr_ctrl_quart_distance_less5_lb"]] = results1$distanceCtrlBase3[5]
  sub_effect_list[["irr_ctrl_quart_distance_over5_lb"]] = results1$distanceCtrlBase4[5]
  
  sub_effect_list[["irr_ctrl_bian_distance_less5_ub"]] = results1$distanceCtrlBase1[6]
  sub_effect_list[["irr_ctrl_bian_distance_over5_ub"]] = results1$distanceCtrlBase2[6]
  sub_effect_list[["irr_ctrl_quart_distance_less5_ub"]] = results1$distanceCtrlBase3[6]
  sub_effect_list[["irr_ctrl_quart_distance_over5_ub"]] = results1$distanceCtrlBase4[6]
  
  #
  sub_effect_list[["irr_bian_quart_distance_less5"]] = results1$distanceBianBase3[1]
  sub_effect_list[["irr_bian_quart_distance_over5"]] = results1$distanceBianBase4[1]
  
  sub_effect_list[["irr_bian_quart_distance_less5_lb"]] = results1$distanceBianBase3[5]
  sub_effect_list[["irr_bian_quart_distance_over5_lb"]] = results1$distanceBianBase4[5]
  
  sub_effect_list[["irr_bian_quart_distance_less5_ub"]] = results1$distanceBianBase3[6]
  sub_effect_list[["irr_bian_quart_distance_over5_ub"]] = results1$distanceBianBase4[6]
  
  
  
  
  ## Strategie Avance 
  sub_effect_list[["irr_ctrl_bian_strategie_advanced"]] = results1$strategieCtrlBase1[1]
  sub_effect_list[["irr_ctrl_bian_strategie_standard"]] = results1$strategieCtrlBase2[1]
  sub_effect_list[["irr_ctrl_quart_strategie_advanced"]] = results1$strategieCtrlBase3[1]
  sub_effect_list[["irr_ctrl_quart_strategie_standard"]] = results1$strategieCtrlBase4[1]
  
  sub_effect_list[["irr_ctrl_bian_strategie_advanced_lb"]] = results1$strategieCtrlBase1[5]
  sub_effect_list[["irr_ctrl_bian_strategie_standard_lb"]] = results1$strategieCtrlBase2[5]
  sub_effect_list[["irr_ctrl_quart_strategie_advanced_lb"]] = results1$strategieCtrlBase3[5]
  sub_effect_list[["irr_ctrl_quart_strategie_standard_lb"]] = results1$strategieCtrlBase4[5]
  
  sub_effect_list[["irr_ctrl_bian_strategie_advanced_ub"]] = results1$strategieCtrlBase1[6]
  sub_effect_list[["irr_ctrl_bian_strategie_standard_ub"]] = results1$strategieCtrlBase2[6]
  sub_effect_list[["irr_ctrl_quart_strategie_advanced_ub"]] = results1$strategieCtrlBase3[6]
  sub_effect_list[["irr_ctrl_quart_strategie_standard_ub"]] = results1$strategieCtrlBase4[6]
  
  #
  sub_effect_list[["irr_bian_quart_strategie_advanced"]] = results1$strategieBianBase3[1]
  sub_effect_list[["irr_bian_quart_strategie_standard"]] = results1$strategieBianBase4[1]
  
  sub_effect_list[["irr_bian_quart_strategie_advanced_lb"]] = results1$strategieBianBase3[5]
  sub_effect_list[["irr_bian_quart_strategie_standard_lb"]] = results1$strategieBianBase4[5]
  
  sub_effect_list[["irr_bian_quart_strategie_advanced_ub"]] = results1$strategieBianBase3[6]
  sub_effect_list[["irr_bian_quart_strategie_standard_ub"]] = results1$strategieBianBase4[6]
  
  
  
  
  
  
  
  
  
  # Asset index
  #Ctrl vs bian
  sub_effect_list[["irr_ctrl_bian_assetIndex_below"]] = results1$sub_assetsCtrlBase1[1]
  sub_effect_list[["irr_ctrl_bian_assetIndex_below_lb"]] = results1$sub_assetsCtrlBase1[5]
  sub_effect_list[["irr_ctrl_bian_assetIndex_below_ub"]] = results1$sub_assetsCtrlBase1[6]
  
  sub_effect_list[["irr_ctrl_bian_assetIndex_above"]] = results1$sub_assetsCtrlBase2[1]
  sub_effect_list[["irr_ctrl_bian_assetIndex_above_lb"]] = results1$sub_assetsCtrlBase2[5]
  sub_effect_list[["irr_ctrl_bian_assetIndex_above_ub"]] = results1$sub_assetsCtrlBase2[6]
  
  #Ctrl vs quart
  sub_effect_list[["irr_ctrl_quart_assetIndex_below"]] = results1$sub_assetsCtrlBase3[1]
  sub_effect_list[["irr_ctrl_quart_assetIndex_below_lb"]] = results1$sub_assetsCtrlBase3[5]
  sub_effect_list[["irr_ctrl_quart_assetIndex_below_ub"]] = results1$sub_assetsCtrlBase3[6]
  
  sub_effect_list[["irr_ctrl_quart_assetIndex_above"]] = results1$sub_assetsCtrlBase4[1]
  sub_effect_list[["irr_ctrl_quart_assetIndex_above_lb"]] = results1$sub_assetsCtrlBase4[5]
  sub_effect_list[["irr_ctrl_quart_assetIndex_above_ub"]] = results1$sub_assetsCtrlBase4[6]
  
  
  #Biannual vs quarterly
  sub_effect_list[["irr_bian_quart_assetIndex_below"]] = results1$sub_assetsBianBase3[1]
  sub_effect_list[["irr_bian_quart_assetIndex_below_lb"]] = results1$sub_assetsBianBase3[5]
  sub_effect_list[["irr_bian_quart_assetIndex_below_ub"]] = results1$sub_assetsBianBase3[6]
  
  sub_effect_list[["irr_bian_quart_assetIndex_above"]] = results1$sub_assetsBianBase4[1]
  sub_effect_list[["irr_bian_quart_assetIndex_above_lb"]] = results1$sub_assetsBianBase4[5]
  sub_effect_list[["irr_bian_quart_assetIndex_above_ub"]] = results1$sub_assetsBianBase4[6]
  
  
  # wash index
  #Ctrl vs bian
  sub_effect_list[["irr_ctrl_bian_washindex_below"]] = results1$sub_washsCtrlBase1[1]
  sub_effect_list[["irr_ctrl_bian_washindex_below_lb"]] = results1$sub_washsCtrlBase1[5]
  sub_effect_list[["irr_ctrl_bian_washindex_below_ub"]] = results1$sub_washsCtrlBase1[6]
  
  sub_effect_list[["irr_ctrl_bian_washindex_above"]] = results1$sub_washsCtrlBase2[1]
  sub_effect_list[["irr_ctrl_bian_washindex_above_lb"]] = results1$sub_washsCtrlBase2[5]
  sub_effect_list[["irr_ctrl_bian_washindex_above_ub"]] = results1$sub_washsCtrlBase2[6]
  
  #Ctrl vs quart
  sub_effect_list[["irr_ctrl_quart_washindex_below"]] = results1$sub_washsCtrlBase3[1]
  sub_effect_list[["irr_ctrl_quart_washindex_below_lb"]] = results1$sub_washsCtrlBase3[5]
  sub_effect_list[["irr_ctrl_quart_washindex_below_ub"]] = results1$sub_washsCtrlBase3[6]
  
  sub_effect_list[["irr_ctrl_quart_washindex_above"]] = results1$sub_washsCtrlBase4[1]
  sub_effect_list[["irr_ctrl_quart_washindex_above_lb"]] = results1$sub_washsCtrlBase4[5]
  sub_effect_list[["irr_ctrl_quart_washindex_above_ub"]] = results1$sub_washsCtrlBase4[6]
  
  
  #Biannual vs quarterly
  sub_effect_list[["irr_bian_quart_washindex_below"]] = results1$sub_washsBianBase3[1]
  sub_effect_list[["irr_bian_quart_washindex_below_lb"]] = results1$sub_washsBianBase3[5]
  sub_effect_list[["irr_bian_quart_washindex_below_ub"]] = results1$sub_washsBianBase3[6]
  
  sub_effect_list[["irr_bian_quart_washindex_above"]] = results1$sub_washsBianBase4[1]
  sub_effect_list[["irr_bian_quart_washindex_above_lb"]] = results1$sub_washsBianBase4[5]
  sub_effect_list[["irr_bian_quart_washindex_above_ub"]] = results1$sub_washsBianBase4[6]
  
  
  
  
  # Distance
  sub_effect_list[["irr_ctrl_bian_dist_less5"]] = results1$distanceCtrlBase1[1]
  sub_effect_list[["irr_ctrl_bian_dist_over5"]] = results1$distanceCtrlBase2[1]
  sub_effect_list[["irr_ctrl_quart_dist_less5"]] = results1$distanceCtrlBase3[1]
  sub_effect_list[["irr_ctrl_quart_dist_over5"]] = results1$distanceCtrlBase4[1]
  
  sub_effect_list[["irr_bian_quart_dist_less5"]] = results1$distanceBianBase3[1]
  sub_effect_list[["irr_bian_quart_dist_over5"]] = results1$distanceBianBase4[1]
  
  ##
  sub_effect_list[["irr_ctrl_bian_dist_less5_lb"]] = results1$distanceCtrlBase1[5]
  sub_effect_list[["irr_ctrl_bian_dist_over5_lb"]] = results1$distanceCtrlBase2[5]
  sub_effect_list[["irr_ctrl_quart_dist_less5_lb"]] = results1$distanceCtrlBase3[5]
  sub_effect_list[["irr_ctrl_quart_dist_over5_lb"]] = results1$distanceCtrlBase4[5]
  
  sub_effect_list[["irr_bian_quart_dist_less5_lb"]] = results1$distanceBianBase3[5]
  sub_effect_list[["irr_bian_quart_dist_over5_lb"]] = results1$distanceBianBase4[5]
  
  
  sub_effect_list[["irr_ctrl_bian_dist_less5_ub"]] = results1$distanceCtrlBase1[6]
  sub_effect_list[["irr_ctrl_bian_dist_over5_ub"]] = results1$distanceCtrlBase2[6]
  sub_effect_list[["irr_ctrl_quart_dist_less5_ub"]] = results1$distanceCtrlBase3[6]
  sub_effect_list[["irr_ctrl_quart_dist_over5_ub"]] = results1$distanceCtrlBase4[6]
  
  sub_effect_list[["irr_bian_quart_dist_less5_ub"]] = results1$distanceBianBase3[6]
  sub_effect_list[["irr_bian_quart_dist_over5_ub"]] = results1$distanceBianBase4[6]
  
  
  # District
  
  i=1
  for(distr in c("Fana","Kalabancoro","Kangaba","Kati","Kita",
                 "Koulikoro/Kati","Kéniéba","Ouélessébougou","Sagabari","Sefeto","Selingue")){
    
    sub_effect_list[[paste0("irr_ctrl_bian_distr_",distr)]] = results1[1,paste0("distrCtrlBase",i)]
    sub_effect_list[[paste0("irr_ctrl_bian_distr_",distr,"_lb")]] = results1[5,paste0("distrCtrlBase",i)]
    sub_effect_list[[paste0("irr_ctrl_bian_distr_",distr,"_ub")]] = results1[6,paste0("distrCtrlBase",i)]
    
    sub_effect_list[[paste0("irr_ctrl_quart_distr_",distr)]] = results1[1,paste0("distrCtrlBase",i+11)]
    sub_effect_list[[paste0("irr_ctrl_quart_distr_",distr,"_lb")]] = results1[5,paste0("distrCtrlBase",i+11)]
    sub_effect_list[[paste0("irr_ctrl_quart_distr_",distr,"_ub")]] = results1[6,paste0("distrCtrlBase",i+11)]
    
    sub_effect_list[[paste0("irr_bian_quart_distr_",distr)]] = results1[1,paste0("distrBianBase",i+11)]
    sub_effect_list[[paste0("irr_bian_quart_distr_",distr,"_lb")]] = results1[5,paste0("distrBianBase",i+11)]
    sub_effect_list[[paste0("irr_bian_quart_distr_",distr,"_ub")]] = results1[6,paste0("distrBianBase",i+11)]
    
    i = i+1
    
    distrNonAcc = stringi::stri_trans_general(str = distr, id = "Latin-ASCII")
    
    sub_numbers_list[[paste0("1. Control_distr_deaths_",distr)]] = 
      results1[1,paste0("distrCtrl",distrNonAcc,"D")]
    
    sub_numbers_list[[paste0("1. Control_distr_PYR_",distr)]] = 
      results1[1,paste0("distrCtrl",distrNonAcc,"PYR")]
    
    sub_numbers_list[[paste0("2. Biannual-AZI_distr_deaths_",distr)]] = 
      results1[1,paste0("distrBian",distrNonAcc,"D")]
    
    sub_numbers_list[[paste0("2. Biannual-AZI_distr_PYR_",distr)]] = 
      results1[1,paste0("distrBian",distrNonAcc,"PYR")]
    
    sub_numbers_list[[paste0("3. Quarterly-AZI_distr_deaths_",distr)]] = 
      results1[1,paste0("distrQuart",distrNonAcc,"D")]
    
    sub_numbers_list[[paste0("3. Quarterly-AZI_distr_PYR_",distr)]] = 
      results1[1,paste0("distrQuart",distrNonAcc,"PYR")]
    
    if(distr=="Koulikoro/Kati") {
      sub_numbers_list[[paste0("1. Control_distr_deaths_",distr)]] = 
        results1[1,paste0("distrCtrl","Koulikoro","D")]
      
      sub_numbers_list[[paste0("1. Control_distr_PYR_",distr)]] = 
        results1[1,paste0("distrCtrl","Koulikoro","PYR")]
      
      sub_numbers_list[[paste0("2. Biannual-AZI_distr_deaths_",distr)]] = 
        results1[1,paste0("distrBian","Koulikoro","D")]
      
      sub_numbers_list[[paste0("2. Biannual-AZI_distr_PYR_",distr)]] = 
        results1[1,paste0("distrBian","Koulikoro","PYR")]
      
      sub_numbers_list[[paste0("3. Quarterly-AZI_distr_deaths_",distr)]] = 
        results1[1,paste0("distrQuart","Koulikoro","D")]
      
      sub_numbers_list[[paste0("3. Quarterly-AZI_distr_PYR_",distr)]] = 
        results1[1,paste0("distrQuart","Koulikoro","PYR")]
      
    }
    
  }
  
  
  
  
  
  
  
  
  
  
  
  # Time-variant
  resultsLong <- haven::read_dta(paste0(here::here("results"),"/ResultsLong.dta"))
  
  
  
  
  ## Age
  sub_pvalues_list[["age_pvalue"]] = round(resultsLong$ageIntrP[1],2)
  sub_effect_list[["irr_ctrl_bian_age_1-5 mo"]] = resultsLong$ageCtrlBase1[1]
  sub_effect_list[["irr_ctrl_bian_age_6-11 mo"]] = resultsLong$ageCtrlBase2[1]
  sub_effect_list[["irr_ctrl_quart_age_1-5 mo"]] = resultsLong$ageCtrlBase3[1]
  sub_effect_list[["irr_ctrl_quart_age_6-11 mo"]] = resultsLong$ageCtrlBase4[1]
  
  sub_effect_list[["irr_bian_quart_age_1-5 mo"]] = resultsLong$ageBianBase3[1]
  sub_effect_list[["irr_bian_quart_age_6-11 mo"]] = resultsLong$ageBianBase4[1]
  
  ## LB/ UB
  sub_effect_list[["irr_ctrl_bian_age_1-5 mo_lb"]] = resultsLong$ageCtrlBase1[5]
  sub_effect_list[["irr_ctrl_bian_age_6-11 mo_lb"]] = resultsLong$ageCtrlBase2[5]
  sub_effect_list[["irr_ctrl_quart_age_1-5 mo_lb"]] = resultsLong$ageCtrlBase3[5]
  sub_effect_list[["irr_ctrl_quart_age_6-11 mo_lb"]] = resultsLong$ageCtrlBase4[5]
  
  sub_effect_list[["irr_bian_quart_age_1-5 mo_lb"]] = resultsLong$ageBianBase3[5]
  sub_effect_list[["irr_bian_quart_age_6-11 mo_lb"]] = resultsLong$ageBianBase4[5]
  
  sub_effect_list[["irr_ctrl_bian_age_1-5 mo_ub"]] = resultsLong$ageCtrlBase1[6]
  sub_effect_list[["irr_ctrl_bian_age_6-11 mo_ub"]] = resultsLong$ageCtrlBase2[6]
  sub_effect_list[["irr_ctrl_quart_age_1-5 mo_ub"]] = resultsLong$ageCtrlBase3[6]
  sub_effect_list[["irr_ctrl_quart_age_6-11 mo_ub"]] = resultsLong$ageCtrlBase4[6]
  
  sub_effect_list[["irr_bian_quart_age_1-5 mo_ub"]] = resultsLong$ageBianBase3[6]
  sub_effect_list[["irr_bian_quart_age_6-11 mo_ub"]] = resultsLong$ageBianBase4[6]
  
  
  
  ## Seasonality
  sub_pvalues_list[["season_pvalue"]] = resultsLong$seasonIntP[1] %>% round(2)
  sub_effect_list[["irr_ctrl_bian_season_DRY"]] = resultsLong$seasonCtrlBase1[1]
  sub_effect_list[["irr_ctrl_bian_season_RAIN"]] = resultsLong$seasonCtrlBase2[1]
  sub_effect_list[["irr_ctrl_quart_season_DRY"]] = resultsLong$seasonCtrlBase3[1]
  sub_effect_list[["irr_ctrl_quart_season_RAIN"]] = resultsLong$seasonCtrlBase4[1]
  sub_effect_list[["irr_bian_quart_season_DRY"]] = resultsLong$seasonBianBase3[1]
  sub_effect_list[["irr_bian_quart_season_RAIN"]] = resultsLong$seasonBianBase4[1]
  
  
  sub_effect_list[["irr_ctrl_bian_season_DRY_lb"]] = resultsLong$seasonCtrlBase1[5]
  sub_effect_list[["irr_ctrl_bian_season_RAIN_lb"]] = resultsLong$seasonCtrlBase2[5]
  sub_effect_list[["irr_ctrl_quart_season_DRY_lb"]] = resultsLong$seasonCtrlBase3[5]
  sub_effect_list[["irr_ctrl_quart_season_RAIN_lb"]] = resultsLong$seasonCtrlBase4[5]
  sub_effect_list[["irr_bian_quart_season_DRY_lb"]] = resultsLong$seasonBianBase3[5]
  sub_effect_list[["irr_bian_quart_season_RAIN_lb"]] = resultsLong$seasonBianBase4[5]
  
  
  sub_effect_list[["irr_ctrl_bian_season_DRY_ub"]] = resultsLong$seasonCtrlBase1[6]
  sub_effect_list[["irr_ctrl_bian_season_RAIN_ub"]] = resultsLong$seasonCtrlBase2[6]
  sub_effect_list[["irr_ctrl_quart_season_DRY_ub"]] = resultsLong$seasonCtrlBase3[6]
  sub_effect_list[["irr_ctrl_quart_season_RAIN_ub"]] = resultsLong$seasonCtrlBase4[6]
  sub_effect_list[["irr_bian_quart_season_DRY_ub"]] = resultsLong$seasonBianBase3[6]
  sub_effect_list[["irr_bian_quart_season_RAIN_ub"]] = resultsLong$seasonBianBase4[6]
  
  
  
  ## SMC
  sub_pvalues_list[["smc_pvalue"]] = resultsLong$smcIntrP[1] %>% round(2)
  sub_effect_list[["irr_ctrl_bian_smc_notgiven"]] = resultsLong$smcCtrlBase1[1]
  sub_effect_list[["irr_ctrl_bian_smc_given"]] = resultsLong$smcCtrlBase2[1]
  sub_effect_list[["irr_ctrl_quart_smc_notgiven"]] = resultsLong$smcCtrlBase3[1]
  sub_effect_list[["irr_ctrl_quart_smc_given"]] = resultsLong$smcCtrlBase4[1]
  sub_effect_list[["irr_bian_quart_smc_notgiven"]] = resultsLong$smcBianBase3[1]
  sub_effect_list[["irr_bian_quart_smc_given"]] = resultsLong$smcBianBase4[1]
  
  
  sub_effect_list[["irr_ctrl_bian_smc_notgiven_lb"]] = resultsLong$smcCtrlBase1[5]
  sub_effect_list[["irr_ctrl_bian_smc_given_lb"]] = resultsLong$smcCtrlBase2[5]
  sub_effect_list[["irr_ctrl_quart_smc_notgiven_lb"]] = resultsLong$smcCtrlBase3[5]
  sub_effect_list[["irr_ctrl_quart_smc_given_lb"]] = resultsLong$smcCtrlBase4[5]
  sub_effect_list[["irr_bian_quart_smc_notgiven_lb"]] = resultsLong$smcBianBase3[5]
  sub_effect_list[["irr_bian_quart_smc_given_lb"]] = resultsLong$smcBianBase4[5]
  
  
  sub_effect_list[["irr_ctrl_bian_smc_notgiven_ub"]] = resultsLong$smcCtrlBase1[6]
  sub_effect_list[["irr_ctrl_bian_smc_given_ub"]] = resultsLong$smcCtrlBase2[6]
  sub_effect_list[["irr_ctrl_quart_smc_notgiven_ub"]] = resultsLong$smcCtrlBase3[6]
  sub_effect_list[["irr_ctrl_quart_smc_given_ub"]] = resultsLong$smcCtrlBase4[6]
  sub_effect_list[["irr_bian_quart_smc_notgiven_ub"]] = resultsLong$smcBianBase3[6]
  sub_effect_list[["irr_bian_quart_smc_given_ub"]] = resultsLong$smcBianBase4[6]
  
  
  
  
  ## WAZ
  sub_pvalues_list[["underweight_pvalue"]] = resultsLong$wazIntrP[1] %>% round(2)
  sub_effect_list[["irr_ctrl_bian_underweight_Not underweight"]] = resultsLong$wazCtrlBase1[1]
  sub_effect_list[["irr_ctrl_bian_underweight_Underweight"]] = resultsLong$wazCtrlBase2[1]
  sub_effect_list[["irr_ctrl_quart_underweight_Not underweight"]] = resultsLong$wazCtrlBase3[1]
  sub_effect_list[["irr_ctrl_quart_underweight_Underweight"]] = resultsLong$wazCtrlBase4[1]
  
  sub_effect_list[["irr_ctrl_bian_underweight_Not underweight_lb"]] = resultsLong$wazCtrlBase1[5]
  sub_effect_list[["irr_ctrl_bian_underweight_Underweight_lb"]] = resultsLong$wazCtrlBase2[5]
  sub_effect_list[["irr_ctrl_quart_underweight_Not underweight_lb"]] = resultsLong$wazCtrlBase3[5]
  sub_effect_list[["irr_ctrl_quart_underweight_Underweight_lb"]] = resultsLong$wazCtrlBase4[5]
  
  sub_effect_list[["irr_ctrl_bian_underweight_Not underweight_ub"]] = resultsLong$wazCtrlBase1[6]
  sub_effect_list[["irr_ctrl_bian_underweight_Underweight_ub"]] = resultsLong$wazCtrlBase2[6]
  sub_effect_list[["irr_ctrl_quart_underweight_Not underweight_ub"]] = resultsLong$wazCtrlBase3[6]
  sub_effect_list[["irr_ctrl_quart_underweight_Underweight_ub"]] = resultsLong$wazCtrlBase4[6]
  
  sub_effect_list[["irr_bian_quart_underweight_Not underweight"]] = resultsLong$wazBianBase3[1]
  sub_effect_list[["irr_bian_quart_underweight_Underweight"]] = resultsLong$wazBianBase4[1]
  
  sub_effect_list[["irr_bian_quart_underweight_Not underweight_lb"]] = resultsLong$wazBianBase3[5]
  sub_effect_list[["irr_bian_quart_underweight_Underweight_lb"]] = resultsLong$wazBianBase4[5]
  
  sub_effect_list[["irr_bian_quart_underweight_Not underweight_ub"]] = resultsLong$wazBianBase3[6]
  sub_effect_list[["irr_bian_quart_underweight_Underweight_ub"]] = resultsLong$wazBianBase4[6]
  
  
  
  
  
  
  
  ## Order of MDA in a village
  sub_pvalues_list[["order_pvalue"]] = resultsLong$orderIntP[1] %>% round(2)
  sub_effect_list[["irr_ctrl_bian_order_1"]] = resultsLong$orderCtrlBase1 [1]
  sub_effect_list[["irr_ctrl_bian_order_2"]] = resultsLong$orderCtrlBase2 [1]
  sub_effect_list[["irr_ctrl_bian_order_3"]] = resultsLong$orderCtrlBase3 [1]
  sub_effect_list[["irr_ctrl_bian_order_4"]] = resultsLong$orderCtrlBase4 [1]
  sub_effect_list[["irr_ctrl_bian_order_5"]] = resultsLong$orderCtrlBase5 [1]
  sub_effect_list[["irr_ctrl_bian_order_6"]] = resultsLong$orderCtrlBase6 [1]
  sub_effect_list[["irr_ctrl_bian_order_7"]] = resultsLong$orderCtrlBase7 [1]
  sub_effect_list[["irr_ctrl_bian_order_8"]] = resultsLong$orderCtrlBase8 [1]
  
  sub_effect_list[["irr_ctrl_bian_order_1_lb"]] = resultsLong$orderCtrlBase1 [5]
  sub_effect_list[["irr_ctrl_bian_order_2_lb"]] = resultsLong$orderCtrlBase2 [5]
  sub_effect_list[["irr_ctrl_bian_order_3_lb"]] = resultsLong$orderCtrlBase3 [5]
  sub_effect_list[["irr_ctrl_bian_order_4_lb"]] = resultsLong$orderCtrlBase4 [5]
  sub_effect_list[["irr_ctrl_bian_order_5_lb"]] = resultsLong$orderCtrlBase5 [5]
  sub_effect_list[["irr_ctrl_bian_order_6_lb"]] = resultsLong$orderCtrlBase6 [5]
  sub_effect_list[["irr_ctrl_bian_order_7_lb"]] = resultsLong$orderCtrlBase7 [5]
  sub_effect_list[["irr_ctrl_bian_order_8_lb"]] = resultsLong$orderCtrlBase8 [5]
  
  sub_effect_list[["irr_ctrl_bian_order_1_ub"]] = resultsLong$orderCtrlBase1 [6]
  sub_effect_list[["irr_ctrl_bian_order_2_ub"]] = resultsLong$orderCtrlBase2 [6]
  sub_effect_list[["irr_ctrl_bian_order_3_ub"]] = resultsLong$orderCtrlBase3 [6]
  sub_effect_list[["irr_ctrl_bian_order_4_ub"]] = resultsLong$orderCtrlBase4 [6]
  sub_effect_list[["irr_ctrl_bian_order_5_ub"]] = resultsLong$orderCtrlBase5 [6]
  sub_effect_list[["irr_ctrl_bian_order_6_ub"]] = resultsLong$orderCtrlBase6 [6]
  sub_effect_list[["irr_ctrl_bian_order_7_ub"]] = resultsLong$orderCtrlBase7 [6]
  sub_effect_list[["irr_ctrl_bian_order_8_ub"]] = resultsLong$orderCtrlBase8 [6]
  
  
  sub_effect_list[["irr_ctrl_quart_order_1"]] = resultsLong$orderCtrlBase9 [1]
  sub_effect_list[["irr_ctrl_quart_order_2"]] = resultsLong$orderCtrlBase10 [1]
  sub_effect_list[["irr_ctrl_quart_order_3"]] = resultsLong$orderCtrlBase11 [1]
  sub_effect_list[["irr_ctrl_quart_order_4"]] = resultsLong$orderCtrlBase12 [1]
  sub_effect_list[["irr_ctrl_quart_order_5"]] = resultsLong$orderCtrlBase13 [1]
  sub_effect_list[["irr_ctrl_quart_order_6"]] = resultsLong$orderCtrlBase14 [1]
  sub_effect_list[["irr_ctrl_quart_order_7"]] = resultsLong$orderCtrlBase15 [1]
  sub_effect_list[["irr_ctrl_quart_order_8"]] = resultsLong$orderCtrlBase16 [1]
  
  sub_effect_list[["irr_ctrl_quart_order_1_lb"]] = resultsLong$orderCtrlBase9 [5]
  sub_effect_list[["irr_ctrl_quart_order_2_lb"]] = resultsLong$orderCtrlBase10 [5]
  sub_effect_list[["irr_ctrl_quart_order_3_lb"]] = resultsLong$orderCtrlBase11 [5]
  sub_effect_list[["irr_ctrl_quart_order_4_lb"]] = resultsLong$orderCtrlBase12 [5]
  sub_effect_list[["irr_ctrl_quart_order_5_lb"]] = resultsLong$orderCtrlBase13 [5]
  sub_effect_list[["irr_ctrl_quart_order_6_lb"]] = resultsLong$orderCtrlBase14 [5]
  sub_effect_list[["irr_ctrl_quart_order_7_lb"]] = resultsLong$orderCtrlBase15 [5]
  sub_effect_list[["irr_ctrl_quart_order_8_lb"]] = resultsLong$orderCtrlBase16 [5]
  
  sub_effect_list[["irr_ctrl_quart_order_1_ub"]] = resultsLong$orderCtrlBase9 [6]
  sub_effect_list[["irr_ctrl_quart_order_2_ub"]] = resultsLong$orderCtrlBase10 [6]
  sub_effect_list[["irr_ctrl_quart_order_3_ub"]] = resultsLong$orderCtrlBase11 [6]
  sub_effect_list[["irr_ctrl_quart_order_4_ub"]] = resultsLong$orderCtrlBase12 [6]
  sub_effect_list[["irr_ctrl_quart_order_5_ub"]] = resultsLong$orderCtrlBase13 [6]
  sub_effect_list[["irr_ctrl_quart_order_6_ub"]] = resultsLong$orderCtrlBase14 [6]
  sub_effect_list[["irr_ctrl_quart_order_7_ub"]] = resultsLong$orderCtrlBase15 [6]
  sub_effect_list[["irr_ctrl_quart_order_8_ub"]] = resultsLong$orderCtrlBase16 [6]
  
  
  
  sub_effect_list[["irr_bian_quart_order_1"]] = resultsLong$orderBianBase9 [1]
  sub_effect_list[["irr_bian_quart_order_2"]] = resultsLong$orderBianBase10 [1]
  sub_effect_list[["irr_bian_quart_order_3"]] = resultsLong$orderBianBase11 [1]
  sub_effect_list[["irr_bian_quart_order_4"]] = resultsLong$orderBianBase12 [1]
  sub_effect_list[["irr_bian_quart_order_5"]] = resultsLong$orderBianBase13 [1]
  sub_effect_list[["irr_bian_quart_order_6"]] = resultsLong$orderBianBase14 [1]
  sub_effect_list[["irr_bian_quart_order_7"]] = resultsLong$orderBianBase15 [1]
  sub_effect_list[["irr_bian_quart_order_8"]] = resultsLong$orderBianBase16 [1]
  
  sub_effect_list[["irr_bian_quart_order_1_lb"]] = resultsLong$orderBianBase9 [5]
  sub_effect_list[["irr_bian_quart_order_2_lb"]] = resultsLong$orderBianBase10 [5]
  sub_effect_list[["irr_bian_quart_order_3_lb"]] = resultsLong$orderBianBase11 [5]
  sub_effect_list[["irr_bian_quart_order_4_lb"]] = resultsLong$orderBianBase12 [5]
  sub_effect_list[["irr_bian_quart_order_5_lb"]] = resultsLong$orderBianBase13 [5]
  sub_effect_list[["irr_bian_quart_order_6_lb"]] = resultsLong$orderBianBase14 [5]
  sub_effect_list[["irr_bian_quart_order_7_lb"]] = resultsLong$orderBianBase15 [5]
  sub_effect_list[["irr_bian_quart_order_8_lb"]] = resultsLong$orderBianBase16 [5]
  
  sub_effect_list[["irr_bian_quart_order_1_ub"]] = resultsLong$orderBianBase9 [6]
  sub_effect_list[["irr_bian_quart_order_2_ub"]] = resultsLong$orderBianBase10 [6]
  sub_effect_list[["irr_bian_quart_order_3_ub"]] = resultsLong$orderBianBase11 [6]
  sub_effect_list[["irr_bian_quart_order_4_ub"]] = resultsLong$orderBianBase12 [6]
  sub_effect_list[["irr_bian_quart_order_5_ub"]] = resultsLong$orderBianBase13 [6]
  sub_effect_list[["irr_bian_quart_order_6_ub"]] = resultsLong$orderBianBase14 [6]
  sub_effect_list[["irr_bian_quart_order_7_ub"]] = resultsLong$orderBianBase15 [6]
  sub_effect_list[["irr_bian_quart_order_8_ub"]] = resultsLong$orderBianBase16 [6]
  
  
  
  
  
  
  ## Numbers
  
  
  ## Season
  
  sub_numbers_list[["1. Control_season_deaths_DRY"]] = resultsLong$seasonCtrlDryD[1]
  sub_numbers_list[["1. Control_season_deaths_RAIN"]] = resultsLong$seasonCtrlRainyD[1]
  sub_numbers_list[["1. Control_season_PYR_DRY"]] = resultsLong$seasonCtrlDryPYR[1]
  sub_numbers_list[["1. Control_season_PYR_RAIN"]] = resultsLong$seasonCtrlRainyPYR[1]
  
  sub_numbers_list[["2. Biannual-AZI_season_deaths_DRY"]] = resultsLong$seasonBianDryD[1]
  sub_numbers_list[["2. Biannual-AZI_season_deaths_RAIN"]] = resultsLong$seasonBianRainyD[1]
  sub_numbers_list[["2. Biannual-AZI_season_PYR_DRY"]] = resultsLong$seasonBianDryPYR[1]
  sub_numbers_list[["2. Biannual-AZI_season_PYR_RAIN"]] = resultsLong$seasonBianRainyPYR[1]
  
  sub_numbers_list[["3. Quarterly-AZI_season_deaths_DRY"]] = resultsLong$seasonQuartDryD[1]
  sub_numbers_list[["3. Quarterly-AZI_season_deaths_RAIN"]] = resultsLong$seasonQuartRainyD[1]
  sub_numbers_list[["3. Quarterly-AZI_season_PYR_DRY"]] = resultsLong$seasonQuartDryPYR[1]
  sub_numbers_list[["3. Quarterly-AZI_season_PYR_RAIN"]] = resultsLong$seasonQuartRainyPYR[1]
  
  
  ## Order of MDA in a village
  sub_numbers_list[["1. Control_order_deaths_1"]] = resultsLong$orderCtrlOneD [1]
  sub_numbers_list[["1. Control_order_deaths_2"]] = resultsLong$orderCtrlTwoD [1]
  sub_numbers_list[["1. Control_order_deaths_3"]] = resultsLong$orderCtrlThreeD [1]
  sub_numbers_list[["1. Control_order_deaths_4"]] = resultsLong$orderCtrlFourD [1]
  sub_numbers_list[["1. Control_order_deaths_5"]] = resultsLong$orderCtrlFiveD [1]
  sub_numbers_list[["1. Control_order_deaths_6"]] = resultsLong$orderCtrlSixD [1]
  sub_numbers_list[["1. Control_order_deaths_7"]] = resultsLong$orderCtrlSevenD [1]
  sub_numbers_list[["1. Control_order_deaths_8"]] = resultsLong$orderCtrlEightD [1]
  
  sub_numbers_list[["2. Biannual-AZI_order_deaths_1"]] = resultsLong$orderBianOneD [1]
  sub_numbers_list[["2. Biannual-AZI_order_deaths_2"]] = resultsLong$orderBianTwoD [1]
  sub_numbers_list[["2. Biannual-AZI_order_deaths_3"]] = resultsLong$orderBianThreeD [1]
  sub_numbers_list[["2. Biannual-AZI_order_deaths_4"]] = resultsLong$orderBianFourD [1]
  sub_numbers_list[["2. Biannual-AZI_order_deaths_5"]] = resultsLong$orderBianFiveD [1]
  sub_numbers_list[["2. Biannual-AZI_order_deaths_6"]] = resultsLong$orderBianSixD [1]
  sub_numbers_list[["2. Biannual-AZI_order_deaths_7"]] = resultsLong$orderBianSevenD [1]
  sub_numbers_list[["2. Biannual-AZI_order_deaths_8"]] = resultsLong$orderBianEightD [1]
  
  sub_numbers_list[["3. Quarterly-AZI_order_deaths_1"]] = resultsLong$orderQuartOneD [1]
  sub_numbers_list[["3. Quarterly-AZI_order_deaths_2"]] = resultsLong$orderQuartTwoD [1]
  sub_numbers_list[["3. Quarterly-AZI_order_deaths_3"]] = resultsLong$orderQuartThreeD [1]
  sub_numbers_list[["3. Quarterly-AZI_order_deaths_4"]] = resultsLong$orderQuartFourD [1]
  sub_numbers_list[["3. Quarterly-AZI_order_deaths_5"]] = resultsLong$orderQuartFiveD [1]
  sub_numbers_list[["3. Quarterly-AZI_order_deaths_6"]] = resultsLong$orderQuartSixD [1]
  sub_numbers_list[["3. Quarterly-AZI_order_deaths_7"]] = resultsLong$orderQuartSevenD [1]
  sub_numbers_list[["3. Quarterly-AZI_order_deaths_8"]] = resultsLong$orderQuartEightD [1]
  
  
  sub_numbers_list[["1. Control_order_PYR_1"]] = resultsLong$orderCtrlOnePYR [1]
  sub_numbers_list[["1. Control_order_PYR_2"]] = resultsLong$orderCtrlTwoPYR [1]
  sub_numbers_list[["1. Control_order_PYR_3"]] = resultsLong$orderCtrlThreePYR [1]
  sub_numbers_list[["1. Control_order_PYR_4"]] = resultsLong$orderCtrlFourPYR [1]
  sub_numbers_list[["1. Control_order_PYR_5"]] = resultsLong$orderCtrlFivePYR [1]
  sub_numbers_list[["1. Control_order_PYR_6"]] = resultsLong$orderCtrlSixPYR [1]
  sub_numbers_list[["1. Control_order_PYR_7"]] = resultsLong$orderCtrlSevenPYR [1]
  sub_numbers_list[["1. Control_order_PYR_8"]] = resultsLong$orderCtrlEightPYR [1]
  
  sub_numbers_list[["2. Biannual-AZI_order_PYR_1"]] = resultsLong$orderBianOnePYR [1]
  sub_numbers_list[["2. Biannual-AZI_order_PYR_2"]] = resultsLong$orderBianTwoPYR [1]
  sub_numbers_list[["2. Biannual-AZI_order_PYR_3"]] = resultsLong$orderBianThreePYR [1]
  sub_numbers_list[["2. Biannual-AZI_order_PYR_4"]] = resultsLong$orderBianFourPYR [1]
  sub_numbers_list[["2. Biannual-AZI_order_PYR_5"]] = resultsLong$orderBianFivePYR [1]
  sub_numbers_list[["2. Biannual-AZI_order_PYR_6"]] = resultsLong$orderBianSixPYR [1]
  sub_numbers_list[["2. Biannual-AZI_order_PYR_7"]] = resultsLong$orderBianSevenPYR [1]
  sub_numbers_list[["2. Biannual-AZI_order_PYR_8"]] = resultsLong$orderBianEightPYR [1]
  
  sub_numbers_list[["3. Quarterly-AZI_order_PYR_1"]] = resultsLong$orderQuartOnePYR [1]
  sub_numbers_list[["3. Quarterly-AZI_order_PYR_2"]] = resultsLong$orderQuartTwoPYR [1]
  sub_numbers_list[["3. Quarterly-AZI_order_PYR_3"]] = resultsLong$orderQuartThreePYR [1]
  sub_numbers_list[["3. Quarterly-AZI_order_PYR_4"]] = resultsLong$orderQuartFourPYR [1]
  sub_numbers_list[["3. Quarterly-AZI_order_PYR_5"]] = resultsLong$orderQuartFivePYR [1]
  sub_numbers_list[["3. Quarterly-AZI_order_PYR_6"]] = resultsLong$orderQuartSixPYR [1]
  sub_numbers_list[["3. Quarterly-AZI_order_PYR_7"]] = resultsLong$orderQuartSevenPYR [1]
  sub_numbers_list[["3. Quarterly-AZI_order_PYR_8"]] = resultsLong$orderQuartEightPYR [1]
  
  
  
  
  
  
  
  
  ### SMC
  sub_numbers_list[["1. Control_smc_deaths_notgiven"]] = resultsLong$smcCtrlNonSmcD[1]
  sub_numbers_list[["1. Control_smc_deaths_given"]] = resultsLong$smcCtrlSMCD[1]
  sub_numbers_list[["1. Control_smc_PYR_notgiven"]] = resultsLong$smcCtrlNonSmcPYR[1]
  sub_numbers_list[["1. Control_smc_PYR_given"]] = resultsLong$smcCtrlSMCPYR[1]
  
  sub_numbers_list[["2. Biannual-AZI_smc_deaths_notgiven"]] = resultsLong$smcBianNonSmcD[1]
  sub_numbers_list[["2. Biannual-AZI_smc_deaths_given"]] = resultsLong$smcBianSMCD[1]
  sub_numbers_list[["2. Biannual-AZI_smc_PYR_notgiven"]] = resultsLong$smcBianNonSmcPYR[1]
  sub_numbers_list[["2. Biannual-AZI_smc_PYR_given"]] = resultsLong$smcBianSMCPYR[1]
  
  sub_numbers_list[["3. Quarterly-AZI_smc_deaths_notgiven"]] = resultsLong$smcQuartNonSmcD[1]
  sub_numbers_list[["3. Quarterly-AZI_smc_deaths_given"]] = resultsLong$smcQuartSMCD[1]
  sub_numbers_list[["3. Quarterly-AZI_smc_PYR_notgiven"]] = resultsLong$smcQuartNonSmcPYR[1]
  sub_numbers_list[["3. Quarterly-AZI_smc_PYR_given"]] = resultsLong$smcQuartSMCPYR[1]
  
  ### WAZ
  sub_numbers_list[["1. Control_underweight_deaths_Not underweight"]] = resultsLong$wazCtrlNonUnD[1]
  sub_numbers_list[["1. Control_underweight_deaths_Underweight"]] = resultsLong$wazCtrlUnD[1]
  sub_numbers_list[["1. Control_underweight_PYR_Not underweight"]] = resultsLong$wazCtrlNonUnPYR[1]
  sub_numbers_list[["1. Control_underweight_PYR_Underweight"]] = resultsLong$wazCtrlUnPYR[1]
  
  sub_numbers_list[["2. Biannual-AZI_underweight_deaths_Not underweight"]] = resultsLong$wazBianNonUnD[1]
  sub_numbers_list[["2. Biannual-AZI_underweight_deaths_Underweight"]] = resultsLong$wazBianUnD[1]
  sub_numbers_list[["2. Biannual-AZI_underweight_PYR_Not underweight"]] = resultsLong$wazBianNonUnPYR[1]
  sub_numbers_list[["2. Biannual-AZI_underweight_PYR_Underweight"]] = resultsLong$wazBianUnPYR[1]
  
  sub_numbers_list[["3. Quarterly-AZI_underweight_deaths_Not underweight"]] = resultsLong$wazQuartNonUnD[1]
  sub_numbers_list[["3. Quarterly-AZI_underweight_deaths_Underweight"]] = resultsLong$wazQuartUnD[1]
  sub_numbers_list[["3. Quarterly-AZI_underweight_PYR_Not underweight"]] = resultsLong$wazQuartNonUnPYR[1]
  sub_numbers_list[["3. Quarterly-AZI_underweight_PYR_Underweight"]] = resultsLong$wazQuartUnPYR[1]
  
  
  
  
  ### AGE
  sub_numbers_list[["1. Control_age_deaths_1-5 mo"]] = resultsLong$ageCtrlyoungD[1]
  sub_numbers_list[["1. Control_age_deaths_6-11 mo"]] = resultsLong$ageCtrloldD[1]
  sub_numbers_list[["1. Control_age_PYR_1-5 mo"]] = resultsLong$ageCtrlyoungPYR[1]
  sub_numbers_list[["1. Control_age_PYR_6-11 mo"]] = resultsLong$ageCtrloldPYR[1]
  
  sub_numbers_list[["2. Biannual-AZI_age_deaths_1-5 mo"]] = resultsLong$ageBianyoungD[1]
  sub_numbers_list[["2. Biannual-AZI_age_deaths_6-11 mo"]] = resultsLong$ageBianoldD[1]
  sub_numbers_list[["2. Biannual-AZI_age_PYR_1-5 mo"]] = resultsLong$ageBianyoungPYR[1]
  sub_numbers_list[["2. Biannual-AZI_age_PYR_6-11 mo"]] = resultsLong$ageBianoldPYR[1]
  
  sub_numbers_list[["3. Quarterly-AZI_age_deaths_1-5 mo"]] = resultsLong$ageQuartyoungD[1]
  sub_numbers_list[["3. Quarterly-AZI_age_deaths_6-11 mo"]] = resultsLong$ageQuartoldD[1]
  sub_numbers_list[["3. Quarterly-AZI_age_PYR_1-5 mo"]] = resultsLong$ageQuartyoungPYR[1]
  sub_numbers_list[["3. Quarterly-AZI_age_PYR_6-11 mo"]] = resultsLong$ageQuartoldPYR[1]
  
  
  
  
  ### SEX
  sub_numbers_list[["1. Control_sex_deaths_Female"]] = results1$sexCtrlGirlsD[1]
  sub_numbers_list[["1. Control_sex_deaths_Male"]] = results1$sexCtrlBoysD[1]
  sub_numbers_list[["1. Control_sex_PYR_Female"]] = results1$sexCtrlGirlsPYR[1]
  sub_numbers_list[["1. Control_sex_PYR_Male"]] = results1$sexCtrlBoysPYR[1]
  
  sub_numbers_list[["2. Biannual-AZI_sex_deaths_Female"]] = results1$sexBianGirlsD[1]
  sub_numbers_list[["2. Biannual-AZI_sex_deaths_Male"]] = results1$sexBianBoysD[1]
  sub_numbers_list[["2. Biannual-AZI_sex_PYR_Female"]] = results1$sexBianGirlsPYR[1]
  sub_numbers_list[["2. Biannual-AZI_sex_PYR_Male"]] = results1$sexBianBoysPYR[1]
  
  sub_numbers_list[["3. Quarterly-AZI_sex_deaths_Female"]] = results1$sexQuartGirlsD[1]
  sub_numbers_list[["3. Quarterly-AZI_sex_deaths_Male"]] = results1$sexQuartBoysD[1]
  sub_numbers_list[["3. Quarterly-AZI_sex_PYR_Female"]] = results1$sexQuartGirlsPYR[1]
  sub_numbers_list[["3. Quarterly-AZI_sex_PYR_Male"]] = results1$sexQuartBoysPYR[1]
  
  
  ### Asset
  sub_numbers_list[["1. Control_assetIndex_deaths_below"]] = results1$assetCtrlBelowD[1]
  sub_numbers_list[["1. Control_assetIndex_deaths_above"]] = results1$assetCtrlAboveD[1]
  sub_numbers_list[["1. Control_assetIndex_PYR_below"]] = results1$assetCtrlBelowPYR[1]
  sub_numbers_list[["1. Control_assetIndex_PYR_above"]] = results1$assetCtrlAbovePYR[1]
  
  sub_numbers_list[["2. Biannual-AZI_assetIndex_deaths_below"]] = results1$assetBianBelowD[1]
  sub_numbers_list[["2. Biannual-AZI_assetIndex_deaths_above"]] = results1$assetBianAboveD[1]
  sub_numbers_list[["2. Biannual-AZI_assetIndex_PYR_below"]] = results1$assetBianBelowPYR[1]
  sub_numbers_list[["2. Biannual-AZI_assetIndex_PYR_above"]] = results1$assetBianAbovePYR[1]
  
  sub_numbers_list[["3. Quarterly-AZI_assetIndex_deaths_below"]] = results1$assetQuartBelowD[1]
  sub_numbers_list[["3. Quarterly-AZI_assetIndex_deaths_above"]] = results1$assetQuartAboveD[1]
  sub_numbers_list[["3. Quarterly-AZI_assetIndex_PYR_below"]] = results1$assetQuartBelowPYR[1]
  sub_numbers_list[["3. Quarterly-AZI_assetIndex_PYR_above"]] = results1$assetQuartAbovePYR[1]
  
  
  
  ### wash
  sub_numbers_list[["1. Control_washindex_deaths_below"]] = results1$washCtrlBelowD[1]
  sub_numbers_list[["1. Control_washindex_deaths_above"]] = results1$washCtrlAboveD[1]
  sub_numbers_list[["1. Control_washindex_PYR_below"]] = results1$washCtrlBelowPYR[1]
  sub_numbers_list[["1. Control_washindex_PYR_above"]] = results1$washCtrlAbovePYR[1]
  
  sub_numbers_list[["2. Biannual-AZI_washindex_deaths_below"]] = results1$washBianBelowD[1]
  sub_numbers_list[["2. Biannual-AZI_washindex_deaths_above"]] = results1$washBianAboveD[1]
  sub_numbers_list[["2. Biannual-AZI_washindex_PYR_below"]] = results1$washBianBelowPYR[1]
  sub_numbers_list[["2. Biannual-AZI_washindex_PYR_above"]] = results1$washBianAbovePYR[1]
  
  sub_numbers_list[["3. Quarterly-AZI_washindex_deaths_below"]] = results1$washQuartBelowD[1]
  sub_numbers_list[["3. Quarterly-AZI_washindex_deaths_above"]] = results1$washQuartAboveD[1]
  sub_numbers_list[["3. Quarterly-AZI_washindex_PYR_below"]] = results1$washQuartBelowPYR[1]
  sub_numbers_list[["3. Quarterly-AZI_washindex_PYR_above"]] = results1$washQuartAbovePYR[1]
  
  
  ### Distance
  sub_numbers_list[["1. Control_dist_deaths_less5"]] = results1$distanceCtrlCloseD[1]
  sub_numbers_list[["1. Control_dist_PYR_less5"]] = results1$distanceCtrlClosePYR[1]
  sub_numbers_list[["1. Control_dist_deaths_over5"]] = results1$distanceCtrlFarD[1]
  sub_numbers_list[["1. Control_dist_PYR_over5"]] = results1$distanceCtrlFarPYR[1]
  
  sub_numbers_list[["2. Biannual-AZI_dist_deaths_less5"]] = results1$distanceBianCloseD[1]
  sub_numbers_list[["2. Biannual-AZI_dist_PYR_less5"]] = results1$distanceBianClosePYR[1]
  sub_numbers_list[["2. Biannual-AZI_dist_deaths_over5"]] = results1$distanceBianFarD[1]
  sub_numbers_list[["2. Biannual-AZI_dist_PYR_over5"]] = results1$distanceBianFarPYR[1]
  
  sub_numbers_list[["3. Quarterly-AZI_dist_deaths_less5"]] = results1$distanceQuartCloseD[1]
  sub_numbers_list[["3. Quarterly-AZI_dist_PYR_less5"]] = results1$distanceQuartClosePYR[1]
  sub_numbers_list[["3. Quarterly-AZI_dist_deaths_over5"]] = results1$distanceQuartFarD[1]
  sub_numbers_list[["3. Quarterly-AZI_dist_PYR_over5"]] = results1$distanceQuartFarPYR[1]
  
  
  ### Strategie
  sub_numbers_list[["1. Control_strategie_deaths_standard"]] = results1$strategieCtrlCloseD[1]
  sub_numbers_list[["1. Control_strategie_PYR_standard"]] = results1$strategieCtrlClosePYR[1]
  sub_numbers_list[["1. Control_strategie_deaths_advanced"]] = results1$strategieCtrlFarD[1]
  sub_numbers_list[["1. Control_strategie_PYR_advanced"]] = results1$strategieCtrlFarPYR[1]
  
  sub_numbers_list[["2. Biannual-AZI_strategie_deaths_standard"]] = results1$strategieBianCloseD[1]
  sub_numbers_list[["2. Biannual-AZI_strategie_PYR_standard"]] = results1$strategieBianClosePYR[1]
  sub_numbers_list[["2. Biannual-AZI_strategie_deaths_advanced"]] = results1$strategieBianFarD[1]
  sub_numbers_list[["2. Biannual-AZI_strategie_PYR_advanced"]] = results1$strategieBianFarPYR[1]
  
  sub_numbers_list[["3. Quarterly-AZI_strategie_deaths_standard"]] = results1$strategieQuartCloseD[1]
  sub_numbers_list[["3. Quarterly-AZI_strategie_PYR_standard"]] = results1$strategieQuartClosePYR[1]
  sub_numbers_list[["3. Quarterly-AZI_strategie_deaths_advanced"]] = results1$strategieQuartFarD[1]
  sub_numbers_list[["3. Quarterly-AZI_strategie_PYR_advanced"]] = results1$strategieQuartFarPYR[1]
  
  

  

  
  
  
  
  
  df <- data.frame(
    modifier = c(paste0('Age at the time of MDA, mo (p = ',sub_pvalues_list[["age_pvalue"]],")"),
                 "    1-5",
                 "    6-11",
                 '',
                 paste0('Sex (p = ',sub_pvalues_list[["sex_pvalue"]],")"),
                 '    Male',
                 '    Female',
                 '',
                 paste0('WAZ (p = ',sub_pvalues_list[["underweight_pvalue"]],")"),
                 '    >= -2',
                 '    < -2',
                 '',
                 paste0('Seasonality (p = ',sub_pvalues_list[["season_pvalue"]],")"),
                 '    Rainy Season',
                 '    Non-rainy Season',
                 '',
                 paste0('SMC (p = ',sub_pvalues_list[["smc_pvalue"]],")"),
                 '    Given 3 months before',
                 '    Not given 3 months before',
                 '',
                 paste0('District of residence (p = ',sub_pvalues_list[["distr_pvalue"]],")"),
                 "    Kita",
                 "    Sagabari",
                 "    Sefeto",
                 "    Koulikoro",
                 "    Kalabancoro",
                 "    Kati",
                 "    Ouelessebougou",
                 "    Kenieba",
                 "    Fana",
                 "    Kangaba",
                 "    Selingue",
                 '',
                 paste0('Order of MDA in a village (p = ',sub_pvalues_list[["order_pvalue"]],")"),
                 "    1st",
                 "    2nd",
                 "    3rd",
                 "    4th",
                 "    5th",
                 "    6th",
                 "    7th",
                 "    8th",
                 '',
                 paste0("Distance to Nearest Health Facility (p = ",sub_pvalues_list[["dist_pvalue"]],")"),
                 '    < 5km ',
                 '    >= 5km ',
                 '',
                 paste0("National outreach strategy (p = ",sub_pvalues_list[["strategie_pvalue"]],")"),
                 '    Standard ',
                 '    Advanced ',
                 '',
                 paste0("Household asset index (p = ",sub_pvalues_list[["assetIndex_pvalue"]],")"),
                 '    Above median',
                 '    Below median',
                 '',
                 paste0("wash index (p = ",sub_pvalues_list[["washindex_pvalue"]],")"),
                 "    Above median",
                 "    Below median"),
    
    effect_bian = c(NA,  # 'Age mo'
                    populateNull(  sub_effect_list[["irr_ctrl_bian_age_1-5 mo"]] ), # "1-5"
                    populateNull( sub_effect_list[["irr_ctrl_bian_age_6-11 mo"]] ), # "6-11",
                    NA,
                    NA,  # 'Sex - - -',
                    populateNull( sub_effect_list[["irr_ctrl_bian_sex_Male"]] ),   # 'Male',
                    populateNull( sub_effect_list[["irr_ctrl_bian_sex_Female"]] ), # 'Female'
                    NA,
                    NA,  # 'WAZ - - -'
                    populateNull( sub_effect_list[["irr_ctrl_bian_underweight_Underweight"]]),   # '>= -2'
                    populateNull( sub_effect_list[["irr_ctrl_bian_underweight_Not underweight"]]), # '< -2'
                    NA,
                    NA,  # 'Seasonality - - -'
                    populateNull( sub_effect_list[["irr_ctrl_bian_season_RAIN"]] ),   # 'Rainy Season'
                    populateNull( sub_effect_list[["irr_ctrl_bian_season_DRY"]] ), # 'Non-rainy Season'
                    NA,
                    NA,  # 'SMC - - -'
                    populateNull( sub_effect_list[["irr_ctrl_bian_smc_given"]] ), # 'given 3 months before',
                    populateNull( sub_effect_list[["irr_ctrl_bian_smc_notgiven"]] ), # 'not given 3 months before'
                    NA,
                    NA,  # 'District of residence - - -',
                    populateNull( sub_effect_list[["irr_ctrl_bian_distr_Kita"]] ), # "Kita",
                    populateNull( sub_effect_list[["irr_ctrl_bian_distr_Sagabari"]] ), # "Sagabari",
                    populateNull( sub_effect_list[["irr_ctrl_bian_distr_Sefeto"]] ), # "Sefeto",
                    populateNull( sub_effect_list[["irr_ctrl_bian_distr_Koulikoro/Kati"]] ), # "Koulikoro",
                    populateNull( sub_effect_list[["irr_ctrl_bian_distr_Kalabancoro"]] ), # "Koulikoro",
                    populateNull( sub_effect_list[["irr_ctrl_bian_distr_Kati"]] ), # "Kati",
                    populateNull( sub_effect_list[["irr_ctrl_bian_distr_Ouélessébougou"]] ), # "Ouelessebougou",
                    populateNull( sub_effect_list[["irr_ctrl_bian_distr_Kéniéba"]] ), # "Kenieba",
                    populateNull( sub_effect_list[["irr_ctrl_bian_distr_Fana"]] ), # "Siby",
                    populateNull( sub_effect_list[["irr_ctrl_bian_distr_Kangaba"]] ), # "Siby",
                    populateNull( sub_effect_list[["irr_ctrl_bian_distr_Selingue"]] ), # "Selingue",
                    NA,
                    NA,  # 'Order of MDA - - -',
                    populateNull( sub_effect_list[["irr_ctrl_bian_order_1"]] ), # 
                    populateNull( sub_effect_list[["irr_ctrl_bian_order_2"]] ), # 
                    populateNull( sub_effect_list[["irr_ctrl_bian_order_3"]] ), # 
                    populateNull( sub_effect_list[["irr_ctrl_bian_order_4"]] ), # 
                    populateNull( sub_effect_list[["irr_ctrl_bian_order_5"]] ), # 
                    populateNull( sub_effect_list[["irr_ctrl_bian_order_6"]] ), # 
                    populateNull( sub_effect_list[["irr_ctrl_bian_order_7"]] ), # 
                    populateNull( sub_effect_list[["irr_ctrl_bian_order_8"]] ), # 
                    NA,
                    NA,  #               "Distance to Nearest Health Facility - - -",
                    populateNull( sub_effect_list[["irr_ctrl_bian_dist_over5"]] ), # ' < 5km ',
                    populateNull( sub_effect_list[["irr_ctrl_bian_dist_less5"]] ), #' >= 5km ',
                    NA,
                    NA,  #               "National outreach strategy - - -",
                    populateNull( sub_effect_list[["irr_ctrl_bian_strategie_advanced"]] ), # ' < 5km ',
                    populateNull( sub_effect_list[["irr_ctrl_bian_strategie_standard"]] ), #' >= 5km ',
                    NA,
                    NA,  # "Household asset index - - -"
                    populateNull( sub_effect_list[["irr_ctrl_bian_assetIndex_above"]] ), # 'Above median'
                    populateNull( sub_effect_list[["irr_ctrl_bian_assetIndex_below"]] ), # 'Below median'
                    NA,
                    NA, # "wash index - - -"
                    populateNull( sub_effect_list[["irr_ctrl_bian_washindex_above"]] ), # "Above median"
                    populateNull( sub_effect_list[["irr_ctrl_bian_washindex_below"]] ) # "Below median"
    ),  # Risk ratios
    
    lower_ci_bian = c(NA,  # 'Age mo'
                      populateNull(  sub_effect_list[["irr_ctrl_bian_age_1-5 mo_lb"]] ), # "1-5"
                      populateNull( sub_effect_list[["irr_ctrl_bian_age_6-11 mo_lb"]] ), # "6-11",
                      NA,
                      NA,  # 'Sex - - -',
                      populateNull( sub_effect_list[["irr_ctrl_bian_sex_Male_lb"]] ),   # 'Male',
                      populateNull( sub_effect_list[["irr_ctrl_bian_sex_Female_lb"]] ), # 'Female'
                      NA,
                      NA,  # 'WAZ - - -'
                      populateNull( sub_effect_list[["irr_ctrl_bian_underweight_Underweight_lb"]]),   # '>= -2'
                      populateNull( sub_effect_list[["irr_ctrl_bian_underweight_Not underweight_lb"]]), # '< -2'
                      NA,
                      NA,  # 'Seasonality - - -'
                      populateNull( sub_effect_list[["irr_ctrl_bian_season_RAIN_lb"]] ),   # 'Rainy Season'
                      populateNull( sub_effect_list[["irr_ctrl_bian_season_DRY_lb"]] ), # 'Non-rainy Season'
                      NA,
                      NA,  # 'SMC - - -'
                      populateNull( sub_effect_list[["irr_ctrl_bian_smc_given_lb"]] ), # 'given 3 months before',
                      populateNull( sub_effect_list[["irr_ctrl_bian_smc_notgiven_lb"]] ), # 'not given 3 months before'
                      NA,
                      NA,  # 'District of residence - - -',
                      populateNull( sub_effect_list[["irr_ctrl_bian_distr_Kita_lb"]] ), # "Kita",
                      populateNull( sub_effect_list[["irr_ctrl_bian_distr_Sagabari_lb"]] ), # "Sagabari",
                      populateNull( sub_effect_list[["irr_ctrl_bian_distr_Sefeto_lb"]] ), # "Sefeto",
                      populateNull( sub_effect_list[["irr_ctrl_bian_distr_Koulikoro/Kati_lb"]] ), # "Koulikoro",
                      populateNull( sub_effect_list[["irr_ctrl_bian_distr_Kalabancoro_lb"]] ), # "Koulikoro",
                      populateNull( sub_effect_list[["irr_ctrl_bian_distr_Kati_lb"]] ), # "Kati",
                      populateNull( sub_effect_list[["irr_ctrl_bian_distr_Ouélessébougou_lb"]] ), # "Ouelessebougou",
                      populateNull( sub_effect_list[["irr_ctrl_bian_distr_Kéniéba_lb"]] ), # "Kenieba",
                      populateNull( sub_effect_list[["irr_ctrl_bian_distr_Fana_lb"]] ), # "Siby",
                      populateNull( sub_effect_list[["irr_ctrl_bian_distr_Kangaba_lb"]] ), # "Siby",
                      populateNull( sub_effect_list[["irr_ctrl_bian_distr_Selingue_lb"]] ), # "Selingue",
                      NA,
                      NA,  # 'Order of MDA - - -',
                      populateNull( sub_effect_list[["irr_ctrl_bian_order_1_lb"]] ), # 
                      populateNull( sub_effect_list[["irr_ctrl_bian_order_2_lb"]] ), # 
                      populateNull( sub_effect_list[["irr_ctrl_bian_order_3_lb"]] ), # 
                      populateNull( sub_effect_list[["irr_ctrl_bian_order_4_lb"]] ), # 
                      populateNull( sub_effect_list[["irr_ctrl_bian_order_5_lb"]] ), # 
                      populateNull( sub_effect_list[["irr_ctrl_bian_order_6_lb"]] ), # 
                      populateNull( sub_effect_list[["irr_ctrl_bian_order_7_lb"]] ), # 
                      populateNull( sub_effect_list[["irr_ctrl_bian_order_8_lb"]] ), # 
                      NA,
                      NA,  #               "Distance to Nearest Health Facility - - -",
                      populateNull( sub_effect_list[["irr_ctrl_bian_dist_over5_lb"]] ), # ' < 5km ',
                      populateNull( sub_effect_list[["irr_ctrl_bian_dist_less5_lb"]] ), #' >= 5km ',
                      NA,
                      NA,  #               "National outreach strategy - - -",
                      populateNull( sub_effect_list[["irr_ctrl_bian_strategie_advanced_lb"]] ), # ' < 5km ',
                      populateNull( sub_effect_list[["irr_ctrl_bian_strategie_standard_lb"]] ), #' >= 5km ',
                      NA,
                      NA,  # "Household asset index - - -"
                      populateNull( sub_effect_list[["irr_ctrl_bian_assetIndex_above_lb"]] ), # 'Above median'
                      populateNull( sub_effect_list[["irr_ctrl_bian_assetIndex_below_lb"]] ), # 'Below median'
                      NA,
                      NA, # "wash index - - -"
                      populateNull( sub_effect_list[["irr_ctrl_bian_washindex_above_lb"]] ), # "Above median"
                      populateNull( sub_effect_list[["irr_ctrl_bian_washindex_below_lb"]] ) # "Below median"
    ),  # Risk ratios
    
    
    upper_ci_bian = c(NA,  # 'Age mo'
                      populateNull(  sub_effect_list[["irr_ctrl_bian_age_1-5 mo_ub"]] ), # "1-5"
                      populateNull( sub_effect_list[["irr_ctrl_bian_age_6-11 mo_ub"]] ), # "6-11",
                      NA,
                      NA,  # 'Sex - - -',
                      populateNull( sub_effect_list[["irr_ctrl_bian_sex_Male_ub"]] ),   # 'Male',
                      populateNull( sub_effect_list[["irr_ctrl_bian_sex_Female_ub"]] ), # 'Female'
                      NA,
                      NA,  # 'WAZ - - -'
                      populateNull( sub_effect_list[["irr_ctrl_bian_underweight_Underweight_ub"]]),   # '>= -2'
                      populateNull( sub_effect_list[["irr_ctrl_bian_underweight_Not underweight_ub"]]), # '< -2'
                      NA,
                      NA,  # 'Seasonality - - -'
                      populateNull( sub_effect_list[["irr_ctrl_bian_season_RAIN_ub"]] ),   # 'Rainy Season'
                      populateNull( sub_effect_list[["irr_ctrl_bian_season_DRY_ub"]] ), # 'Non-rainy Season'
                      NA,
                      NA,  # 'SMC - - -'
                      populateNull( sub_effect_list[["irr_ctrl_bian_smc_given_ub"]] ), # 'given 3 months before',
                      populateNull( sub_effect_list[["irr_ctrl_bian_smc_notgiven_ub"]] ), # 'not given 3 months before'
                      NA,
                      NA,  # 'District of residence - - -',
                      populateNull( sub_effect_list[["irr_ctrl_bian_distr_Kita_ub"]] ), # "Kita",
                      populateNull( sub_effect_list[["irr_ctrl_bian_distr_Sagabari_ub"]] ), # "Sagabari",
                      populateNull( sub_effect_list[["irr_ctrl_bian_distr_Sefeto_ub"]] ), # "Sefeto",
                      populateNull( sub_effect_list[["irr_ctrl_bian_distr_Koulikoro/Kati_ub"]] ), # "Koulikoro",
                      populateNull( sub_effect_list[["irr_ctrl_bian_distr_Kalabancoro_ub"]] ), # "Koulikoro",
                      populateNull( sub_effect_list[["irr_ctrl_bian_distr_Kati_ub"]] ), # "Kati",
                      populateNull( sub_effect_list[["irr_ctrl_bian_distr_Ouélessébougou_ub"]] ), # "Ouelessebougou",
                      populateNull( sub_effect_list[["irr_ctrl_bian_distr_Kéniéba_ub"]] ), # "Kenieba",
                      populateNull( sub_effect_list[["irr_ctrl_bian_distr_Fana_ub"]] ), # "Siby",
                      populateNull( sub_effect_list[["irr_ctrl_bian_distr_Kangaba_ub"]] ), # "Siby",
                      populateNull( sub_effect_list[["irr_ctrl_bian_distr_Selingue_ub"]] ), # "Selingue",
                      NA,
                      NA,  # 'order of MDA - - -',
                      populateNull( sub_effect_list[["irr_ctrl_bian_order_1_ub"]] ), # "Kita",
                      populateNull( sub_effect_list[["irr_ctrl_bian_order_2_ub"]] ), # "Sagabari",
                      populateNull( sub_effect_list[["irr_ctrl_bian_order_3_ub"]] ), # "Sefeto",
                      populateNull( sub_effect_list[["irr_ctrl_bian_order_4_ub"]] ), # "Koulikoro",
                      populateNull( sub_effect_list[["irr_ctrl_bian_order_5_ub"]] ), # "Koulikoro",
                      populateNull( sub_effect_list[["irr_ctrl_bian_order_6_ub"]] ), # "Kati",
                      populateNull( sub_effect_list[["irr_ctrl_bian_order_7_ub"]] ), # "Ouelessebougou",
                      populateNull( sub_effect_list[["irr_ctrl_bian_order_8_ub"]] ), # "Kenieba",
                      NA,
                      NA,  #               "Distance to Nearest Health Facility - - -",
                      populateNull( sub_effect_list[["irr_ctrl_bian_dist_over5_ub"]] ), # ' < 5km ',
                      populateNull( sub_effect_list[["irr_ctrl_bian_dist_less5_ub"]] ), #' >= 5km ',
                      NA,
                      NA,  #               "National outreach strategy - - -",
                      populateNull( sub_effect_list[["irr_ctrl_bian_strategie_advanced_ub"]] ), # ' < 5km ',
                      populateNull( sub_effect_list[["irr_ctrl_bian_strategie_standard_ub"]] ), #' >= 5km ',
                      NA,
                      NA,  # "Household asset index - - -"
                      populateNull( sub_effect_list[["irr_ctrl_bian_assetIndex_above_ub"]] ), # 'Above median'
                      populateNull( sub_effect_list[["irr_ctrl_bian_assetIndex_below_ub"]] ), # 'Below median'
                      NA,
                      NA, # "wash index - - -"
                      populateNull( sub_effect_list[["irr_ctrl_bian_washindex_above_ub"]] ), # "Above median"
                      populateNull( sub_effect_list[["irr_ctrl_bian_washindex_below_ub"]] ) # "Below median"
    ),  # Risk ratios
    
    effect_quart = c(NA,  # 'Age mo'
                     populateNull(  sub_effect_list[["irr_ctrl_quart_age_1-5 mo"]] ), # "1-5"
                     populateNull( sub_effect_list[["irr_ctrl_quart_age_6-11 mo"]] ), # "6-11",
                     NA,
                     NA,  # 'Sex - - -',
                     populateNull( sub_effect_list[["irr_ctrl_quart_sex_Male"]] ),   # 'Male',
                     populateNull( sub_effect_list[["irr_ctrl_quart_sex_Female"]] ), # 'Female'
                     NA,
                     NA,  # 'WAZ - - -'
                     populateNull( sub_effect_list[["irr_ctrl_quart_underweight_Underweight"]]),   # '>= -2'
                     populateNull( sub_effect_list[["irr_ctrl_quart_underweight_Not underweight"]]), # '< -2'
                     NA,
                     NA,  # 'Seasonality - - -'
                     populateNull( sub_effect_list[["irr_ctrl_quart_season_RAIN"]] ),   # 'Rainy Season'
                     populateNull( sub_effect_list[["irr_ctrl_quart_season_DRY"]] ), # 'Non-rainy Season'
                     NA,
                     NA,  # 'SMC - - -'
                     populateNull( sub_effect_list[["irr_ctrl_quart_smc_given"]] ), # 'given 3 months before',
                     populateNull( sub_effect_list[["irr_ctrl_quart_smc_notgiven"]] ), # 'not given 3 months before'
                     NA,
                     NA,  # 'District of residence - - -',
                     populateNull( sub_effect_list[["irr_ctrl_quart_distr_Kita"]] ), # "Kita",
                     populateNull( sub_effect_list[["irr_ctrl_quart_distr_Sagabari"]] ), # "Sagabari",
                     populateNull( sub_effect_list[["irr_ctrl_quart_distr_Sefeto"]] ), # "Sefeto",
                     populateNull( sub_effect_list[["irr_ctrl_quart_distr_Koulikoro/Kati"]] ), # "Koulikoro",
                     populateNull( sub_effect_list[["irr_ctrl_quart_distr_Kalabancoro"]] ), # "Koulikoro",
                     populateNull( sub_effect_list[["irr_ctrl_quart_distr_Kati"]] ), # "Kati",
                     populateNull( sub_effect_list[["irr_ctrl_quart_distr_Ouélessébougou"]] ), # "Ouelessebougou",
                     populateNull( sub_effect_list[["irr_ctrl_quart_distr_Kéniéba"]] ), # "Kenieba",
                     populateNull( sub_effect_list[["irr_ctrl_quart_distr_Fana"]] ), # "Siby",
                     populateNull( sub_effect_list[["irr_ctrl_quart_distr_Kangaba"]] ), # "Siby",
                     populateNull( sub_effect_list[["irr_ctrl_quart_distr_Selingue"]] ), # "Selingue",
                     NA,
                     NA,  # 'order of residence - - -',
                     populateNull( sub_effect_list[["irr_ctrl_quart_order_1"]] ), # "Kita",
                     populateNull( sub_effect_list[["irr_ctrl_quart_order_2"]] ), # "Sagabari",
                     populateNull( sub_effect_list[["irr_ctrl_quart_order_3"]] ), # "Sefeto",
                     populateNull( sub_effect_list[["irr_ctrl_quart_order_4"]] ), # "Koulikoro",
                     populateNull( sub_effect_list[["irr_ctrl_quart_order_5"]] ), # "Koulikoro",
                     populateNull( sub_effect_list[["irr_ctrl_quart_order_6"]] ), # "Kati",
                     populateNull( sub_effect_list[["irr_ctrl_quart_order_7"]] ), # "Ouelessebougou",
                     populateNull( sub_effect_list[["irr_ctrl_quart_order_8"]] ), # "Kenieba",
                     NA,
                     NA,  #               "Distance to Nearest Health Facility - - -",
                     populateNull( sub_effect_list[["irr_ctrl_quart_dist_over5"]] ), # ' < 5km ',
                     populateNull( sub_effect_list[["irr_ctrl_quart_dist_less5"]] ), #' >= 5km ',
                     NA,
                     NA,  #               "National outreach strategy - - -",
                     populateNull( sub_effect_list[["irr_ctrl_quart_strategie_advanced"]] ), # '  ',
                     populateNull( sub_effect_list[["irr_ctrl_quart_strategie_standard"]] ), #'  ',
                     NA,
                     NA,  # "Household asset index - - -"
                     populateNull( sub_effect_list[["irr_ctrl_quart_assetIndex_above"]] ), # 'Above median'
                     populateNull( sub_effect_list[["irr_ctrl_quart_assetIndex_below"]] ), # 'Below median'
                     NA,
                     NA, # "wash index - - -"
                     populateNull( sub_effect_list[["irr_ctrl_quart_washindex_above"]] ), # "Above median"
                     populateNull( sub_effect_list[["irr_ctrl_quart_washindex_below"]] ) # "Below median"
    ),  # Risk ratios
    
    lower_ci_quart = c(NA,  # 'Age mo'
                       populateNull(  sub_effect_list[["irr_ctrl_quart_age_1-5 mo_lb"]] ), # "1-5"
                       populateNull( sub_effect_list[["irr_ctrl_quart_age_6-11 mo_lb"]] ), # "6-11",
                       NA,
                       NA,  # 'Sex - - -',
                       populateNull( sub_effect_list[["irr_ctrl_quart_sex_Male_lb"]] ),   # 'Male',
                       populateNull( sub_effect_list[["irr_ctrl_quart_sex_Female_lb"]] ), # 'Female'
                       NA,
                       NA,  # 'WAZ - - -'
                       populateNull( sub_effect_list[["irr_ctrl_quart_underweight_Underweight_lb"]]),   # '>= -2'
                       populateNull( sub_effect_list[["irr_ctrl_quart_underweight_Not underweight_lb"]]), # '< -2'
                       NA,
                       NA,  # 'Seasonality - - -'
                       populateNull( sub_effect_list[["irr_ctrl_quart_season_RAIN_lb"]] ),   # 'Rainy Season'
                       populateNull( sub_effect_list[["irr_ctrl_quart_season_DRY_lb"]] ), # 'Non-rainy Season'
                       NA,
                       NA,  # 'SMC - - -'
                       populateNull( sub_effect_list[["irr_ctrl_quart_smc_given_lb"]] ), # 'given 3 months before',
                       populateNull( sub_effect_list[["irr_ctrl_quart_smc_notgiven_lb"]] ), # 'not given 3 months before'
                       NA,
                       NA,  # 'District of residence - - -',
                       populateNull( sub_effect_list[["irr_ctrl_quart_distr_Kita_lb"]] ), # "Kita",
                       populateNull( sub_effect_list[["irr_ctrl_quart_distr_Sagabari_lb"]] ), # "Sagabari",
                       populateNull( sub_effect_list[["irr_ctrl_quart_distr_Sefeto_lb"]] ), # "Sefeto",
                       populateNull( sub_effect_list[["irr_ctrl_quart_distr_Koulikoro/Kati_lb"]] ), # "Koulikoro",
                       populateNull( sub_effect_list[["irr_ctrl_quart_distr_Kalabancoro_lb"]] ), # "Koulikoro",
                       populateNull( sub_effect_list[["irr_ctrl_quart_distr_Kati_lb"]] ), # "Kati",
                       populateNull( sub_effect_list[["irr_ctrl_quart_distr_Ouélessébougou_lb"]] ), # "Ouelessebougou",
                       populateNull( sub_effect_list[["irr_ctrl_quart_distr_Kéniéba_lb"]] ), # "Kenieba",
                       populateNull( sub_effect_list[["irr_ctrl_quart_distr_Fana_lb"]] ), # "Siby",
                       populateNull( sub_effect_list[["irr_ctrl_quart_distr_Kangaba_lb"]] ), # "Siby",
                       populateNull( sub_effect_list[["irr_ctrl_quart_distr_Selingue_lb"]] ), # "Selingue",
                       NA,
                       NA,  # 'order of MDA - - -',
                       populateNull( sub_effect_list[["irr_ctrl_quart_order_1_lb"]] ), # "Kita",
                       populateNull( sub_effect_list[["irr_ctrl_quart_order_2_lb"]] ), # "Sagabari",
                       populateNull( sub_effect_list[["irr_ctrl_quart_order_3_lb"]] ), # "Sefeto",
                       populateNull( sub_effect_list[["irr_ctrl_quart_order_4_lb"]] ), # "Koulikoro",
                       populateNull( sub_effect_list[["irr_ctrl_quart_order_5_lb"]] ), # "Koulikoro",
                       populateNull( sub_effect_list[["irr_ctrl_quart_order_6_lb"]] ), # "Kati",
                       populateNull( sub_effect_list[["irr_ctrl_quart_order_7_lb"]] ), # "Ouelessebougou",
                       populateNull( sub_effect_list[["irr_ctrl_quart_order_8_lb"]] ), # "Kenieba",
                       NA,
                       NA,  #               "Distance to Nearest Health Facility - - -",
                       populateNull( sub_effect_list[["irr_ctrl_quart_dist_over5_lb"]] ), # ' < 5km ',
                       populateNull( sub_effect_list[["irr_ctrl_quart_dist_less5_lb"]] ), #' >= 5km ',
                       NA,
                       NA,  #               "National outreach strategy - - -",
                       populateNull( sub_effect_list[["irr_ctrl_quart_strategie_advanced_lb"]] ), # ' < 5km ',
                       populateNull( sub_effect_list[["irr_ctrl_quart_strategie_standard_lb"]] ), #' >= 5km ',
                       NA,
                       NA,  # "Household asset index - - -"
                       populateNull( sub_effect_list[["irr_ctrl_quart_assetIndex_above_lb"]] ), # 'Above median'
                       populateNull( sub_effect_list[["irr_ctrl_quart_assetIndex_below_lb"]] ), # 'Below median'
                       NA,
                       NA, # "wash index - - -"
                       populateNull( sub_effect_list[["irr_ctrl_quart_washindex_above_lb"]] ), # "Above median"
                       populateNull( sub_effect_list[["irr_ctrl_quart_washindex_below_lb"]] ) # "Below median"
    ),  # Risk ratios
    
    
    upper_ci_quart = c(NA,  # 'Age mo'
                       populateNull(  sub_effect_list[["irr_ctrl_quart_age_1-5 mo_ub"]] ), # "1-5"
                       populateNull( sub_effect_list[["irr_ctrl_quart_age_6-11 mo_ub"]] ), # "6-11",
                       NA,
                       NA,  # 'Sex - - -',
                       populateNull( sub_effect_list[["irr_ctrl_quart_sex_Male_ub"]] ),   # 'Male',
                       populateNull( sub_effect_list[["irr_ctrl_quart_sex_Female_ub"]] ), # 'Female'
                       NA,
                       NA,  # 'WAZ - - -'
                       populateNull( sub_effect_list[["irr_ctrl_quart_underweight_Underweight_ub"]]),   # '>= -2'
                       populateNull( sub_effect_list[["irr_ctrl_quart_underweight_Not underweight_ub"]]), # '< -2'
                       NA,
                       NA,  # 'Seasonality - - -'
                       populateNull( sub_effect_list[["irr_ctrl_quart_season_RAIN_ub"]] ),   # 'Rainy Season'
                       populateNull( sub_effect_list[["irr_ctrl_quart_season_DRY_ub"]] ), # 'Non-rainy Season'
                       NA,
                       NA,  # 'SMC - - -'
                       populateNull( sub_effect_list[["irr_ctrl_quart_smc_given_ub"]] ), # 'given 3 months before',
                       populateNull( sub_effect_list[["irr_ctrl_quart_smc_notgiven_ub"]] ), # 'not given 3 months before'
                       NA,
                       NA,  # 'District of residence - - -',
                       populateNull( sub_effect_list[["irr_ctrl_quart_distr_Kita_ub"]] ), # "Kita",
                       populateNull( sub_effect_list[["irr_ctrl_quart_distr_Sagabari_ub"]] ), # "Sagabari",
                       populateNull( sub_effect_list[["irr_ctrl_quart_distr_Sefeto_ub"]] ), # "Sefeto",
                       populateNull( sub_effect_list[["irr_ctrl_quart_distr_Koulikoro/Kati_ub"]] ), # "Koulikoro",
                       populateNull( sub_effect_list[["irr_ctrl_quart_distr_Kalabancoro_ub"]] ), # "Koulikoro",
                       populateNull( sub_effect_list[["irr_ctrl_quart_distr_Kati_ub"]] ), # "Kati",
                       populateNull( sub_effect_list[["irr_ctrl_quart_distr_Ouélessébougou_ub"]] ), # "Ouelessebougou",
                       populateNull( sub_effect_list[["irr_ctrl_quart_distr_Kéniéba_ub"]] ), # "Kenieba",
                       populateNull( sub_effect_list[["irr_ctrl_quart_distr_Fana_ub"]] ), # "Siby",
                       populateNull( sub_effect_list[["irr_ctrl_quart_distr_Kangaba_ub"]] ), # "Siby",
                       populateNull( sub_effect_list[["irr_ctrl_quart_distr_Selingue_ub"]] ), # "Selingue",
                       NA,
                       NA,  # 'order of MDA - - -',
                       populateNull( sub_effect_list[["irr_ctrl_quart_order_1_ub"]] ), # "Kita",
                       populateNull( sub_effect_list[["irr_ctrl_quart_order_2_ub"]] ), # "Sagabari",
                       populateNull( sub_effect_list[["irr_ctrl_quart_order_3_ub"]] ), # "Sefeto",
                       populateNull( sub_effect_list[["irr_ctrl_quart_order_4_ub"]] ), # "Koulikoro",
                       populateNull( sub_effect_list[["irr_ctrl_quart_order_5_ub"]] ), # "Koulikoro",
                       populateNull( sub_effect_list[["irr_ctrl_quart_order_6_ub"]] ), # "Kati",
                       populateNull( sub_effect_list[["irr_ctrl_quart_order_7_ub"]] ), # "Ouelessebougou",
                       populateNull( sub_effect_list[["irr_ctrl_quart_order_8_ub"]] ), # "Kenieba",
                       NA,
                       NA,  #               "Distance to Nearest Health Facility - - -",
                       populateNull( sub_effect_list[["irr_ctrl_quart_dist_over5_ub"]] ), # ' < 5km ',
                       populateNull( sub_effect_list[["irr_ctrl_quart_dist_less5_ub"]] ), #' >= 5km ',
                       NA,
                       NA,  #               "National outreach strategy - - -",
                       populateNull( sub_effect_list[["irr_ctrl_quart_strategie_advanced_ub"]] ), # ' < 5km ',
                       populateNull( sub_effect_list[["irr_ctrl_quart_strategie_standard_ub"]] ), #' >= 5km ',
                       NA,
                       NA,  # "Household asset index - - -"
                       populateNull( sub_effect_list[["irr_ctrl_quart_assetIndex_above_ub"]] ), # 'Above median'
                       populateNull( sub_effect_list[["irr_ctrl_quart_assetIndex_below_ub"]] ), # 'Below median'
                       NA,
                       NA, # "wash index - - -"
                       populateNull( sub_effect_list[["irr_ctrl_quart_washindex_above_ub"]] ), # "Above median"
                       populateNull( sub_effect_list[["irr_ctrl_quart_washindex_below_ub"]] ) # "Below median"
    ),
    
    
    
    effect_bian_quart = c(NA,  # 'Age mo'
                          populateNull(  sub_effect_list[["irr_bian_quart_age_1-5 mo"]] ), # "1-5"
                          populateNull( sub_effect_list[["irr_bian_quart_age_6-11 mo"]] ), # "6-11",
                          NA,
                          NA,  # 'Sex - - -',
                          populateNull( sub_effect_list[["irr_bian_quart_sex_Male"]] ),   # 'Male',
                          populateNull( sub_effect_list[["irr_bian_quart_sex_Female"]] ), # 'Female'
                          NA,
                          NA,  # 'WAZ - - -'
                          populateNull( sub_effect_list[["irr_bian_quart_underweight_Underweight"]]),   # '>= -2'
                          populateNull( sub_effect_list[["irr_bian_quart_underweight_Not underweight"]]), # '< -2'
                          NA,
                          NA,  # 'Seasonality - - -'
                          populateNull( sub_effect_list[["irr_bian_quart_season_RAIN"]] ),   # 'Rainy Season'
                          populateNull( sub_effect_list[["irr_bian_quart_season_DRY"]] ), # 'Non-rainy Season'
                          NA,
                          NA,  # 'SMC - - -'
                          populateNull( sub_effect_list[["irr_bian_quart_smc_given"]] ), # 'given 3 months before',
                          populateNull( sub_effect_list[["irr_bian_quart_smc_notgiven"]] ), # 'not given 3 months before'
                          NA,
                          NA,  # 'District of residence - - -',
                          populateNull( sub_effect_list[["irr_bian_quart_distr_Kita"]] ), # "Kita",
                          populateNull( sub_effect_list[["irr_bian_quart_distr_Sagabari"]] ), # "Sagabari",
                          populateNull( sub_effect_list[["irr_bian_quart_distr_Sefeto"]] ), # "Sefeto",
                          populateNull( sub_effect_list[["irr_bian_quart_distr_Koulikoro/Kati"]] ), # "Koulikoro",
                          populateNull( sub_effect_list[["irr_bian_quart_distr_Kalabancoro"]] ), # "Koulikoro",
                          populateNull( sub_effect_list[["irr_bian_quart_distr_Kati"]] ), # "Kati",
                          populateNull( sub_effect_list[["irr_bian_quart_distr_Ouélessébougou"]] ), # "Ouelessebougou",
                          populateNull( sub_effect_list[["irr_bian_quart_distr_Kéniéba"]] ), # "Kenieba",
                          populateNull( sub_effect_list[["irr_bian_quart_distr_Fana"]] ), # "Siby",
                          populateNull( sub_effect_list[["irr_bian_quart_distr_Kangaba"]] ), # "Siby",
                          populateNull( sub_effect_list[["irr_bian_quart_distr_Selingue"]] ), # "Selingue",
                          NA,
                          NA,  # 'order of MDA - - -',
                          populateNull( sub_effect_list[["irr_bian_quart_order_1"]] ), # "Kita",
                          populateNull( sub_effect_list[["irr_bian_quart_order_2"]] ), # "Sagabari",
                          populateNull( sub_effect_list[["irr_bian_quart_order_3"]] ), # "Sefeto",
                          populateNull( sub_effect_list[["irr_bian_quart_order_4"]] ), # "Koulikoro",
                          populateNull( sub_effect_list[["irr_bian_quart_order_5"]] ), # "Koulikoro",
                          populateNull( sub_effect_list[["irr_bian_quart_order_6"]] ), # "Kati",
                          populateNull( sub_effect_list[["irr_bian_quart_order_7"]] ), # "Ouelessebougou",
                          populateNull( sub_effect_list[["irr_bian_quart_order_8"]] ), # "Kenieba",
                          NA,
                          NA,  #               "Distance to Nearest Health Facility - - -",
                          populateNull( sub_effect_list[["irr_bian_quart_dist_over5"]] ), # ' < 5km ',
                          populateNull( sub_effect_list[["irr_bian_quart_dist_less5"]] ), #' >= 5km ',
                          NA,
                          NA,  #               "National outreach strategy - - -",
                          populateNull( sub_effect_list[["irr_bian_quart_strategie_advanced"]] ), # ' < 5km ',
                          populateNull( sub_effect_list[["irr_bian_quart_strategie_standard"]] ), #' >= 5km ',
                          NA,
                          NA,  # "Household asset index - - -"
                          populateNull( sub_effect_list[["irr_bian_quart_assetIndex_above"]] ), # 'Above median'
                          populateNull( sub_effect_list[["irr_bian_quart_assetIndex_below"]] ), # 'Below median'
                          NA,
                          NA, # "wash index - - -"
                          populateNull( sub_effect_list[["irr_bian_quart_washindex_above"]] ), # "Above median"
                          populateNull( sub_effect_list[["irr_bian_quart_washindex_below"]] ) # "Below median"
    ),  # Risk ratios
    
    lower_ci_bian_quart = c(NA,  # 'Age mo'
                            populateNull(  sub_effect_list[["irr_bian_quart_age_1-5 mo_lb"]] ), # "1-5"
                            populateNull( sub_effect_list[["irr_bian_quart_age_6-11 mo_lb"]] ), # "6-11",
                            NA,
                            NA,  # 'Sex - - -',
                            populateNull( sub_effect_list[["irr_bian_quart_sex_Male_lb"]] ),   # 'Male',
                            populateNull( sub_effect_list[["irr_bian_quart_sex_Female_lb"]] ), # 'Female'
                            NA,
                            NA,  # 'WAZ - - -'
                            populateNull( sub_effect_list[["irr_bian_quart_underweight_Underweight_lb"]]),   # '>= -2'
                            populateNull( sub_effect_list[["irr_bian_quart_underweight_Not underweight_lb"]]), # '< -2'
                            NA,
                            NA,  # 'Seasonality - - -'
                            populateNull( sub_effect_list[["irr_bian_quart_season_RAIN_lb"]] ),   # 'Rainy Season'
                            populateNull( sub_effect_list[["irr_bian_quart_season_DRY_lb"]] ), # 'Non-rainy Season'
                            NA,
                            NA,  # 'SMC - - -'
                            populateNull( sub_effect_list[["irr_bian_quart_smc_given_lb"]] ), # 'given 3 months before',
                            populateNull( sub_effect_list[["irr_bian_quart_smc_notgiven_lb"]] ), # 'not given 3 months before'
                            NA,
                            NA,  # 'District of residence - - -',
                            populateNull( sub_effect_list[["irr_bian_quart_distr_Kita_lb"]] ), # "Kita",
                            populateNull( sub_effect_list[["irr_bian_quart_distr_Sagabari_lb"]] ), # "Sagabari",
                            populateNull( sub_effect_list[["irr_bian_quart_distr_Sefeto_lb"]] ), # "Sefeto",
                            populateNull( sub_effect_list[["irr_bian_quart_distr_Koulikoro/Kati_lb"]] ), # "Koulikoro",
                            populateNull( sub_effect_list[["irr_bian_quart_distr_Kalabancoro_lb"]] ), # "Koulikoro",
                            populateNull( sub_effect_list[["irr_bian_quart_distr_Kati_lb"]] ), # "Kati",
                            populateNull( sub_effect_list[["irr_bian_quart_distr_Ouélessébougou_lb"]] ), # "Ouelessebougou",
                            populateNull( sub_effect_list[["irr_bian_quart_distr_Kéniéba_lb"]] ), # "Kenieba",
                            populateNull( sub_effect_list[["irr_bian_quart_distr_Fana_lb"]] ), # "Siby",
                            populateNull( sub_effect_list[["irr_bian_quart_distr_Kangaba_lb"]] ), # "Siby",
                            populateNull( sub_effect_list[["irr_bian_quart_distr_Selingue_lb"]] ), # "Selingue",
                            NA,
                            NA,  # 'order of MDA - - -',
                            populateNull( sub_effect_list[["irr_bian_quart_order_1_lb"]] ), # "Kita",
                            populateNull( sub_effect_list[["irr_bian_quart_order_2_lb"]] ), # "Sagabari",
                            populateNull( sub_effect_list[["irr_bian_quart_order_3_lb"]] ), # "Sefeto",
                            populateNull( sub_effect_list[["irr_bian_quart_order_4_lb"]] ), # "Koulikoro",
                            populateNull( sub_effect_list[["irr_bian_quart_order_5_lb"]] ), # "Koulikoro",
                            populateNull( sub_effect_list[["irr_bian_quart_order_6_lb"]] ), # "Kati",
                            populateNull( sub_effect_list[["irr_bian_quart_order_7_lb"]] ), # "Ouelessebougou",
                            populateNull( sub_effect_list[["irr_bian_quart_order_8_lb"]] ), # "Kenieba",
                            NA,
                            NA,  #               "Distance to Nearest Health Facility - - -",
                            populateNull( sub_effect_list[["irr_bian_quart_dist_over5_lb"]] ), # ' < 5km ',
                            populateNull( sub_effect_list[["irr_bian_quart_dist_less5_lb"]] ), #' >= 5km ',
                            NA,
                            NA,  #               "National outreach strategy - - -",
                            populateNull( sub_effect_list[["irr_bian_quart_strategie_advanced_lb"]] ), # ' < 5km ',
                            populateNull( sub_effect_list[["irr_bian_quart_strategie_standard_lb"]] ), #' >= 5km ',
                            NA,
                            NA,  # "Household asset index - - -"
                            populateNull( sub_effect_list[["irr_bian_quart_assetIndex_above_lb"]] ), # 'Above median'
                            populateNull( sub_effect_list[["irr_bian_quart_assetIndex_below_lb"]] ), # 'Below median'
                            NA,
                            NA, # "wash index - - -"
                            populateNull( sub_effect_list[["irr_bian_quart_washindex_above_lb"]] ), # "Above median"
                            populateNull( sub_effect_list[["irr_bian_quart_washindex_below_lb"]] ) # "Below median"
    ),  # Risk ratios
    
    
    upper_ci_bian_quart = c(NA,  # 'Age mo'
                            populateNull(  sub_effect_list[["irr_bian_quart_age_1-5 mo_ub"]] ), # "1-5"
                            populateNull( sub_effect_list[["irr_bian_quart_age_6-11 mo_ub"]] ), # "6-11",
                            NA,
                            NA,  # 'Sex - - -',
                            populateNull( sub_effect_list[["irr_bian_quart_sex_Male_ub"]] ),   # 'Male',
                            populateNull( sub_effect_list[["irr_bian_quart_sex_Female_ub"]] ), # 'Female'
                            NA,
                            NA,  # 'WAZ - - -'
                            populateNull( sub_effect_list[["irr_bian_quart_underweight_Underweight_ub"]]),   # '>= -2'
                            populateNull( sub_effect_list[["irr_bian_quart_underweight_Not underweight_ub"]]), # '< -2'
                            NA,
                            NA,  # 'Seasonality - - -'
                            populateNull( sub_effect_list[["irr_bian_quart_season_RAIN_ub"]] ),   # 'Rainy Season'
                            populateNull( sub_effect_list[["irr_bian_quart_season_DRY_ub"]] ), # 'Non-rainy Season'
                            NA,
                            NA,  # 'SMC - - -'
                            populateNull( sub_effect_list[["irr_bian_quart_smc_given_ub"]] ), # 'given 3 months before',
                            populateNull( sub_effect_list[["irr_bian_quart_smc_notgiven_ub"]] ), # 'not given 3 months before'
                            NA,
                            NA,  # 'District of residence - - -',
                            populateNull( sub_effect_list[["irr_bian_quart_distr_Kita_ub"]] ), # "Kita",
                            populateNull( sub_effect_list[["irr_bian_quart_distr_Sagabari_ub"]] ), # "Sagabari",
                            populateNull( sub_effect_list[["irr_bian_quart_distr_Sefeto_ub"]] ), # "Sefeto",
                            populateNull( sub_effect_list[["irr_bian_quart_distr_Koulikoro/Kati_ub"]] ), # "Koulikoro",
                            populateNull( sub_effect_list[["irr_bian_quart_distr_Kalabancoro_ub"]] ), # "Koulikoro",
                            populateNull( sub_effect_list[["irr_bian_quart_distr_Kati_ub"]] ), # "Kati",
                            populateNull( sub_effect_list[["irr_bian_quart_distr_Ouélessébougou_ub"]] ), # "Ouelessebougou",
                            populateNull( sub_effect_list[["irr_bian_quart_distr_Kéniéba_ub"]] ), # "Kenieba",
                            populateNull( sub_effect_list[["irr_bian_quart_distr_Fana_ub"]] ), # "Siby",
                            populateNull( sub_effect_list[["irr_bian_quart_distr_Kangaba_ub"]] ), # "Siby",
                            populateNull( sub_effect_list[["irr_bian_quart_distr_Selingue_ub"]] ), # "Selingue",
                            NA,
                            NA,  # 'order of MDA - - -',
                            populateNull( sub_effect_list[["irr_bian_quart_order_1_ub"]] ), # "Kita",
                            populateNull( sub_effect_list[["irr_bian_quart_order_2_ub"]] ), # "Sagabari",
                            populateNull( sub_effect_list[["irr_bian_quart_order_3_ub"]] ), # "Sefeto",
                            populateNull( sub_effect_list[["irr_bian_quart_order_4_ub"]] ), # "Koulikoro",
                            populateNull( sub_effect_list[["irr_bian_quart_order_5_ub"]] ), # "Koulikoro",
                            populateNull( sub_effect_list[["irr_bian_quart_order_6_ub"]] ), # "Kati",
                            populateNull( sub_effect_list[["irr_bian_quart_order_7_ub"]] ), # "Ouelessebougou",
                            populateNull( sub_effect_list[["irr_bian_quart_order_8_ub"]] ), # "Kenieba",
                            NA,
                            NA,  #               "Distance to Nearest Health Facility - - -",
                            populateNull( sub_effect_list[["irr_bian_quart_dist_over5_ub"]] ), # ' < 5km ',
                            populateNull( sub_effect_list[["irr_bian_quart_dist_less5_ub"]] ), #' >= 5km ',
                            NA,
                            NA,  #               "National outreach strategy - - -",
                            populateNull( sub_effect_list[["irr_bian_quart_strategie_advanced_ub"]] ), # ' < 5km ',
                            populateNull( sub_effect_list[["irr_bian_quart_strategie_standard_ub"]] ), #' >= 5km ',
                            NA,
                            NA,  # "Household asset index - - -"
                            populateNull( sub_effect_list[["irr_bian_quart_assetIndex_above_ub"]] ), # 'Above median'
                            populateNull( sub_effect_list[["irr_bian_quart_assetIndex_below_ub"]] ), # 'Below median'
                            NA,
                            NA, # "wash index - - -"
                            populateNull( sub_effect_list[["irr_bian_quart_washindex_above_ub"]] ), # "Above median"
                            populateNull( sub_effect_list[["irr_bian_quart_washindex_below_ub"]] ) # "Below median"
    ),
    
    
    deaths_ctrl = c(NA,  # 'Age mo'
                    populateNull( sub_numbers_list[["1. Control_age_deaths_1-5 mo"]] ), # "1-5"
                    populateNull( sub_numbers_list[["1. Control_age_deaths_6-11 mo"]] ), # "6-11",
                    NA,
                    NA,  # 'Sex - - -',
                    populateNull( sub_numbers_list[["1. Control_sex_deaths_Male"]] ),   # 'Male',
                    populateNull( sub_numbers_list[["1. Control_sex_deaths_Female"]] ), # 'Female'
                    NA,
                    NA,  # 'WAZ - - -'
                    populateNull( sub_numbers_list[["1. Control_underweight_deaths_Not underweight"]] ),   # '>= -2'
                    populateNull( sub_numbers_list[["1. Control_underweight_deaths_Underweight"]] ), # '< -2'
                    NA,
                    NA,  # 'Seasonality - - -'
                    populateNull( sub_numbers_list[["1. Control_season_deaths_RAIN"]] ),   # 'Rainy Season'
                    populateNull( sub_numbers_list[["1. Control_season_deaths_DRY"]] ), # 'Non-rainy Season'
                    NA,
                    NA,  # 'SMC - - -'
                    populateNull( sub_numbers_list[["1. Control_smc_deaths_given"]] ), # 'given 3 months before',
                    populateNull( sub_numbers_list[["1. Control_smc_deaths_notgiven"]] ), # 'not given 3 months before'
                    NA,
                    NA,  # 'District of residence - - -',
                    populateNull( sub_numbers_list[["1. Control_distr_deaths_Kita"]] ), # "Kita",
                    populateNull( sub_numbers_list[["1. Control_distr_deaths_Sagabari"]] ), # "Sagabari",
                    populateNull( sub_numbers_list[["1. Control_distr_deaths_Sefeto"]] ), # "Sefeto",
                    populateNull( sub_numbers_list[["1. Control_distr_deaths_Koulikoro/Kati"]] ), # "Koulikoro",
                    populateNull( sub_numbers_list[["1. Control_distr_deaths_Kalabancoro"]] ), # "Koulikoro",
                    populateNull( sub_numbers_list[["1. Control_distr_deaths_Kati"]] ), # "Kati",
                    populateNull( sub_numbers_list[["1. Control_distr_deaths_Ouélessébougou"]] ), # "Ouélessébougou",
                    populateNull( sub_numbers_list[["1. Control_distr_deaths_Kéniéba"]] ), # "Kenieba",
                    populateNull( sub_numbers_list[["1. Control_distr_deaths_Fana"]] ), # "Fana",
                    populateNull( sub_numbers_list[["1. Control_distr_deaths_Kangaba"]] ), # "Kangaba",
                    populateNull( sub_numbers_list[["1. Control_distr_deaths_Selingue"]] ), # "Selingue",
                    NA,
                    NA,  # 'order of residence - - -',
                    populateNull( sub_numbers_list[["1. Control_order_deaths_1"]] ), # "Kita",
                    populateNull( sub_numbers_list[["1. Control_order_deaths_2"]] ), # "Sagabari",
                    populateNull( sub_numbers_list[["1. Control_order_deaths_3"]] ), # "Sefeto",
                    populateNull( sub_numbers_list[["1. Control_order_deaths_4"]] ), # "Koulikoro",
                    populateNull( sub_numbers_list[["1. Control_order_deaths_5"]] ), # "Koulikoro",
                    populateNull( sub_numbers_list[["1. Control_order_deaths_6"]] ), # "Kati",
                    populateNull( sub_numbers_list[["1. Control_order_deaths_7"]] ), # "Ouélessébougou",
                    populateNull( sub_numbers_list[["1. Control_order_deaths_8"]] ), # "Kenieba",
                    NA,
                    NA,  #               "Distance to Nearest Health Facility - - -",
                    populateNull( sub_numbers_list[["1. Control_dist_deaths_over5"]] ), # ' < 5km ',
                    populateNull( sub_numbers_list[["1. Control_dist_deaths_less5"]] ), #' >= 5km ',
                    NA,
                    NA,  #               "National outreach strategy - - -",
                    populateNull( sub_numbers_list[["1. Control_strategie_deaths_advanced"]] ), # ' < 5km ',
                    populateNull( sub_numbers_list[["1. Control_strategie_deaths_standard"]] ), #' >= 5km ',
                    NA,
                    NA,  # "Household asset index - - -"
                    populateNull( sub_numbers_list[["1. Control_assetIndex_deaths_above"]] ), # 'Above median'
                    populateNull( sub_numbers_list[["1. Control_assetIndex_deaths_below"]] ), # 'Below median'
                    NA,
                    NA, # "wash index - - -"
                    populateNull( sub_numbers_list[["1. Control_washindex_deaths_above"]] ), # "Above median"
                    populateNull( sub_numbers_list[["1. Control_washindex_deaths_below"]] ) # "Below median"
    ),
    
    PYR_ctrl = c(NA,  # 'Age mo'
                 populateNull( sub_numbers_list[["1. Control_age_PYR_1-5 mo"]] ), # "1-5"
                 populateNull( sub_numbers_list[["1. Control_age_PYR_6-11 mo"]] ), # "6-11",
                 NA,
                 NA,  # 'Sex - - -',
                 populateNull( sub_numbers_list[["1. Control_sex_PYR_Male"]] ),   # 'Male',
                 populateNull( sub_numbers_list[["1. Control_sex_PYR_Female"]] ), # 'Female'
                 NA,
                 NA,  # 'WAZ - - -'
                 populateNull( sub_numbers_list[["1. Control_underweight_PYR_Not underweight"]] ),   # '>= -2'
                 populateNull( sub_numbers_list[["1. Control_underweight_PYR_Underweight"]] ), # '< -2'
                 NA,
                 NA,  # 'Seasonality - - -'
                 populateNull( sub_numbers_list[["1. Control_season_PYR_RAIN"]] ),   # 'Rainy Season'
                 populateNull( sub_numbers_list[["1. Control_season_PYR_DRY"]] ), # 'Non-rainy Season'
                 NA,
                 NA,  # 'SMC - - -'
                 populateNull( sub_numbers_list[["1. Control_smc_PYR_given"]] ), # 'given 3 months before',
                 populateNull( sub_numbers_list[["1. Control_smc_PYR_notgiven"]] ), # 'not given 3 months before'
                 NA,
                 NA,  # 'District of residence - - -',
                 populateNull( sub_numbers_list[["1. Control_distr_PYR_Kita"]] ), # "Kita",
                 populateNull( sub_numbers_list[["1. Control_distr_PYR_Sagabari"]] ), # "Sagabari",
                 populateNull( sub_numbers_list[["1. Control_distr_PYR_Sefeto"]] ), # "Sefeto",
                 populateNull( sub_numbers_list[["1. Control_distr_PYR_Koulikoro/Kati"]] ), # "Koulikoro",
                 populateNull( sub_numbers_list[["1. Control_distr_PYR_Kalabancoro"]] ), # "Koulikoro",
                 populateNull( sub_numbers_list[["1. Control_distr_PYR_Kati"]] ), # "Kati",
                 populateNull( sub_numbers_list[["1. Control_distr_PYR_Ouélessébougou"]] ), # "Ouélessébougou",
                 populateNull( sub_numbers_list[["1. Control_distr_PYR_Kéniéba"]] ), # "Kenieba",
                 populateNull( sub_numbers_list[["1. Control_distr_PYR_Fana"]] ), # "Fana",
                 populateNull( sub_numbers_list[["1. Control_distr_PYR_Kangaba"]] ), # "Kangaba",
                 populateNull( sub_numbers_list[["1. Control_distr_PYR_Selingue"]] ), # "Selingue",
                 NA,
                 NA,  # 'order of residence - - -',
                 populateNull( sub_numbers_list[["1. Control_order_PYR_1"]] ), # "Kita",
                 populateNull( sub_numbers_list[["1. Control_order_PYR_2"]] ), # "Sagabari",
                 populateNull( sub_numbers_list[["1. Control_order_PYR_3"]] ), # "Sefeto",
                 populateNull( sub_numbers_list[["1. Control_order_PYR_4"]] ), # "Koulikoro",
                 populateNull( sub_numbers_list[["1. Control_order_PYR_5"]] ), # "Koulikoro",
                 populateNull( sub_numbers_list[["1. Control_order_PYR_6"]] ), # "Kati",
                 populateNull( sub_numbers_list[["1. Control_order_PYR_7"]] ), # "Ouélessébougou",
                 populateNull( sub_numbers_list[["1. Control_order_PYR_8"]] ), # "Kenieba",
                 NA,
                 NA,  #               "Distance to Nearest Health Facility - - -",
                 populateNull( sub_numbers_list[["1. Control_dist_PYR_over5"]] ), # ' < 5km ',
                 populateNull( sub_numbers_list[["1. Control_dist_PYR_less5"]] ), #' >= 5km ',
                 NA,
                 NA,  #               "National outreach strategy - - -",
                 populateNull( sub_numbers_list[["1. Control_strategie_PYR_advanced"]] ), # ' < 5km ',
                 populateNull( sub_numbers_list[["1. Control_strategie_PYR_standard"]] ), #' >= 5km ',
                 NA,
                 NA,  # "Household asset index - - -"
                 populateNull( sub_numbers_list[["1. Control_assetIndex_PYR_above"]] ), # 'Above median'
                 populateNull( sub_numbers_list[["1. Control_assetIndex_PYR_below"]] ), # 'Below median'
                 NA,
                 NA, # "wash index - - -"
                 populateNull( sub_numbers_list[["1. Control_washindex_PYR_above"]] ), # "Above median"
                 populateNull( sub_numbers_list[["1. Control_washindex_PYR_below"]] ) # "Below median"
    ),
    
    
    deaths_bian = c(NA,  # 'Age mo'
                    populateNull( sub_numbers_list[["2. Biannual-AZI_age_deaths_1-5 mo"]] ), # "1-5"
                    populateNull( sub_numbers_list[["2. Biannual-AZI_age_deaths_6-11 mo"]] ), # "6-11",
                    NA,
                    NA,  # 'Sex - - -',
                    populateNull( sub_numbers_list[["2. Biannual-AZI_sex_deaths_Male"]] ),   # 'Male',
                    populateNull( sub_numbers_list[["2. Biannual-AZI_sex_deaths_Female"]] ), # 'Female'
                    NA,
                    NA,  # 'WAZ - - -'
                    populateNull( sub_numbers_list[["2. Biannual-AZI_underweight_deaths_Not underweight"]] ),   # '>= -2'
                    populateNull( sub_numbers_list[["2. Biannual-AZI_underweight_deaths_Underweight"]] ), # '< -2'
                    NA,
                    NA,  # 'Seasonality - - -'
                    populateNull( sub_numbers_list[["2. Biannual-AZI_season_deaths_RAIN"]] ),   # 'Rainy Season'
                    populateNull( sub_numbers_list[["2. Biannual-AZI_season_deaths_DRY"]] ), # 'Non-rainy Season'
                    NA,
                    NA,  # 'SMC - - -'
                    populateNull( sub_numbers_list[["2. Biannual-AZI_smc_deaths_given"]] ), # 'given 3 months before',
                    populateNull( sub_numbers_list[["2. Biannual-AZI_smc_deaths_notgiven"]] ), # 'not given 3 months before'
                    NA,
                    NA,  # 'District of residence - - -',
                    populateNull( sub_numbers_list[["2. Biannual-AZI_distr_deaths_Kita"]] ), # "Kita",
                    populateNull( sub_numbers_list[["2. Biannual-AZI_distr_deaths_Sagabari"]] ), # "Sagabari",
                    populateNull( sub_numbers_list[["2. Biannual-AZI_distr_deaths_Sefeto"]] ), # "Sefeto",
                    populateNull( sub_numbers_list[["2. Biannual-AZI_distr_deaths_Koulikoro/Kati"]] ), # "Koulikoro",
                    populateNull( sub_numbers_list[["2. Biannual-AZI_distr_deaths_Kalabancoro"]] ), # "Koulikoro",
                    populateNull( sub_numbers_list[["2. Biannual-AZI_distr_deaths_Kati"]] ), # "Kati",
                    populateNull( sub_numbers_list[["2. Biannual-AZI_distr_deaths_Ouélessébougou"]] ), # "Ouélessébougou",
                    populateNull( sub_numbers_list[["2. Biannual-AZI_distr_deaths_Kéniéba"]] ), # "Kenieba",
                    populateNull( sub_numbers_list[["2. Biannual-AZI_distr_deaths_Fana"]] ), # "Fana",
                    populateNull( sub_numbers_list[["2. Biannual-AZI_distr_deaths_Kangaba"]] ), # "Kangaba",
                    populateNull( sub_numbers_list[["2. Biannual-AZI_distr_deaths_Selingue"]] ), # "Selingue",
                    NA,
                    NA,  # 'order of residence - - -',
                    populateNull( sub_numbers_list[["2. Biannual-AZI_order_deaths_1"]] ), # "Kita",
                    populateNull( sub_numbers_list[["2. Biannual-AZI_order_deaths_2"]] ), # "Sagabari",
                    populateNull( sub_numbers_list[["2. Biannual-AZI_order_deaths_3"]] ), # "Sefeto",
                    populateNull( sub_numbers_list[["2. Biannual-AZI_order_deaths_4"]] ), # "Koulikoro",
                    populateNull( sub_numbers_list[["2. Biannual-AZI_order_deaths_5"]] ), # "Koulikoro",
                    populateNull( sub_numbers_list[["2. Biannual-AZI_order_deaths_6"]] ), # "Kati",
                    populateNull( sub_numbers_list[["2. Biannual-AZI_order_deaths_7"]] ), # "Ouélessébougou",
                    populateNull( sub_numbers_list[["2. Biannual-AZI_order_deaths_8"]] ), # "Kenieba",
                    NA,
                    NA,  #               "Distance to Nearest Health Facility - - -",
                    populateNull( sub_numbers_list[["2. Biannual-AZI_dist_deaths_over5"]] ), # ' < 5km ',
                    populateNull( sub_numbers_list[["2. Biannual-AZI_dist_deaths_less5"]] ), #' >= 5km ',
                    NA,
                    NA,  #               "National outreach strategy - - -",
                    populateNull( sub_numbers_list[["2. Biannual-AZI_strategie_deaths_advanced"]] ), # ' < 5km ',
                    populateNull( sub_numbers_list[["2. Biannual-AZI_strategie_deaths_standard"]] ), #' >= 5km ',
                    NA,
                    NA,  # "Household asset index - - -"
                    populateNull( sub_numbers_list[["2. Biannual-AZI_assetIndex_deaths_above"]] ), # 'Above median'
                    populateNull( sub_numbers_list[["2. Biannual-AZI_assetIndex_deaths_below"]] ), # 'Below median'
                    NA,
                    NA, # "wash index - - -"
                    populateNull( sub_numbers_list[["2. Biannual-AZI_washindex_deaths_above"]] ), # "Above median"
                    populateNull( sub_numbers_list[["2. Biannual-AZI_washindex_deaths_below"]] ) # "Below median"
    ),
    
    PYR_bian = c(NA,  # 'Age mo'
                 populateNull( sub_numbers_list[["2. Biannual-AZI_age_PYR_1-5 mo"]] ), # "1-5"
                 populateNull( sub_numbers_list[["2. Biannual-AZI_age_PYR_6-11 mo"]] ), # "6-11",
                 NA,
                 NA,  # 'Sex - - -',
                 populateNull( sub_numbers_list[["2. Biannual-AZI_sex_PYR_Male"]] ),   # 'Male',
                 populateNull( sub_numbers_list[["2. Biannual-AZI_sex_PYR_Female"]] ), # 'Female'
                 NA,
                 NA,  # 'WAZ - - -'
                 populateNull( sub_numbers_list[["2. Biannual-AZI_underweight_PYR_Not underweight"]] ),   # '>= -2'
                 populateNull( sub_numbers_list[["2. Biannual-AZI_underweight_PYR_Underweight"]] ), # '< -2'
                 NA,
                 NA,  # 'Seasonality - - -'
                 populateNull( sub_numbers_list[["2. Biannual-AZI_season_PYR_RAIN"]] ),   # 'Rainy Season'
                 populateNull( sub_numbers_list[["2. Biannual-AZI_season_PYR_DRY"]] ), # 'Non-rainy Season'
                 NA,
                 NA,  # 'SMC - - -'
                 populateNull( sub_numbers_list[["2. Biannual-AZI_smc_PYR_given"]] ), # 'given 3 months before',
                 populateNull( sub_numbers_list[["2. Biannual-AZI_smc_PYR_notgiven"]] ), # 'not given 3 months before'
                 NA,
                 NA,  # 'District of residence - - -',
                 populateNull( sub_numbers_list[["2. Biannual-AZI_distr_PYR_Kita"]] ), # "Kita",
                 populateNull( sub_numbers_list[["2. Biannual-AZI_distr_PYR_Sagabari"]] ), # "Sagabari",
                 populateNull( sub_numbers_list[["2. Biannual-AZI_distr_PYR_Sefeto"]] ), # "Sefeto",
                 populateNull( sub_numbers_list[["2. Biannual-AZI_distr_PYR_Koulikoro/Kati"]] ), # "Koulikoro",
                 populateNull( sub_numbers_list[["2. Biannual-AZI_distr_PYR_Kalabancoro"]] ), # "Koulikoro",
                 populateNull( sub_numbers_list[["2. Biannual-AZI_distr_PYR_Kati"]] ), # "Kati",
                 populateNull( sub_numbers_list[["2. Biannual-AZI_distr_PYR_Ouélessébougou"]] ), # "Ouélessébougou",
                 populateNull( sub_numbers_list[["2. Biannual-AZI_distr_PYR_Kéniéba"]] ), # "Kenieba",
                 populateNull( sub_numbers_list[["2. Biannual-AZI_distr_PYR_Fana"]] ), # "Fana",
                 populateNull( sub_numbers_list[["2. Biannual-AZI_distr_PYR_Kangaba"]] ), # "Kangaba",
                 populateNull( sub_numbers_list[["2. Biannual-AZI_distr_PYR_Selingue"]] ), # "Selingue",
                 NA,
                 NA,  # 'order of residence - - -',
                 populateNull( sub_numbers_list[["2. Biannual-AZI_order_PYR_1"]] ), # "Kita",
                 populateNull( sub_numbers_list[["2. Biannual-AZI_order_PYR_2"]] ), # "Sagabari",
                 populateNull( sub_numbers_list[["2. Biannual-AZI_order_PYR_3"]] ), # "Sefeto",
                 populateNull( sub_numbers_list[["2. Biannual-AZI_order_PYR_4"]] ), # "Koulikoro",
                 populateNull( sub_numbers_list[["2. Biannual-AZI_order_PYR_5"]] ), # "Koulikoro",
                 populateNull( sub_numbers_list[["2. Biannual-AZI_order_PYR_6"]] ), # "Kati",
                 populateNull( sub_numbers_list[["2. Biannual-AZI_order_PYR_7"]] ), # "Ouélessébougou",
                 populateNull( sub_numbers_list[["2. Biannual-AZI_order_PYR_8"]] ), # "Kenieba",
                 NA,
                 NA,  #               "Distance to Nearest Health Facility - - -",
                 populateNull( sub_numbers_list[["2. Biannual-AZI_dist_PYR_over5"]] ), # ' < 5km ',
                 populateNull( sub_numbers_list[["2. Biannual-AZI_dist_PYR_less5"]] ), #' >= 5km ',
                 NA,
                 NA,  #               "National outreach strategy - - -",
                 populateNull( sub_numbers_list[["2. Biannual-AZI_strategie_PYR_advanced"]] ), # ' < 5km ',
                 populateNull( sub_numbers_list[["2. Biannual-AZI_strategie_PYR_standard"]] ), #' >= 5km ',
                 NA,
                 NA,  # "Household asset index - - -"
                 populateNull( sub_numbers_list[["2. Biannual-AZI_assetIndex_PYR_above"]] ), # 'Above median'
                 populateNull( sub_numbers_list[["2. Biannual-AZI_assetIndex_PYR_below"]] ), # 'Below median'
                 NA,
                 NA, # "wash index - - -"
                 populateNull( sub_numbers_list[["2. Biannual-AZI_washindex_PYR_above"]] ), # "Above median"
                 populateNull( sub_numbers_list[["2. Biannual-AZI_washindex_PYR_below"]] ) # "Below median"
    ),
    
    
    deaths_quart = c(NA,  # 'Age mo'
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_age_deaths_1-5 mo"]] ), # "1-5"
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_age_deaths_6-11 mo"]] ), # "6-11",
                     NA,
                     NA,  # 'Sex - - -',
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_sex_deaths_Male"]] ),   # 'Male',
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_sex_deaths_Female"]] ), # 'Female'
                     NA,
                     NA,  # 'WAZ - - -'
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_underweight_deaths_Not underweight"]] ),   # '>= -2'
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_underweight_deaths_Underweight"]] ), # '< -2'
                     NA,
                     NA,  # 'Seasonality - - -'
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_season_deaths_RAIN"]] ),   # 'Rainy Season'
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_season_deaths_DRY"]] ), # 'Non-rainy Season'
                     NA,
                     NA,  # 'SMC - - -'
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_smc_deaths_given"]] ), # 'given 3 months before',
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_smc_deaths_notgiven"]] ), # 'not given 3 months before'
                     NA,
                     NA,  # 'District of residence - - -',
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_distr_deaths_Kita"]] ), # "Kita",
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_distr_deaths_Sagabari"]] ), # "Sagabari",
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_distr_deaths_Sefeto"]] ), # "Sefeto",
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_distr_deaths_Koulikoro/Kati"]] ), # "Koulikoro",
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_distr_deaths_Kalabancoro"]] ), # "Koulikoro",
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_distr_deaths_Kati"]] ), # "Kati",
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_distr_deaths_Ouélessébougou"]] ), # "Ouélessébougou",
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_distr_deaths_Kéniéba"]] ), # "Kenieba",
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_distr_deaths_Fana"]] ), # "Fana",
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_distr_deaths_Kangaba"]] ), # "Kangaba",
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_distr_deaths_Selingue"]] ), # "Selingue",
                     NA,
                     NA,  # 'order of residence - - -',
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_order_deaths_1"]] ), # "Kita",
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_order_deaths_2"]] ), # "Sagabari",
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_order_deaths_3"]] ), # "Sefeto",
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_order_deaths_4"]] ), # "Koulikoro",
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_order_deaths_5"]] ), # "Koulikoro",
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_order_deaths_6"]] ), # "Kati",
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_order_deaths_7"]] ), # "Ouélessébougou",
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_order_deaths_8"]] ), # "Kenieba",
                     NA,
                     NA,  #               "Distance to Nearest Health Facility - - -",
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_dist_deaths_over5"]] ), # ' < 5km ',
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_dist_deaths_less5"]] ), #' >= 5km ',
                     NA,
                     NA,  #               "National outreach strategy - - -",
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_strategie_deaths_advanced"]] ), # ' < 5km ',
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_strategie_deaths_standard"]] ), #' >= 5km ',
                     NA,
                     NA,  # "Household asset index - - -"
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_assetIndex_deaths_above"]] ), # 'Above median'
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_assetIndex_deaths_below"]] ), # 'Below median'
                     NA,
                     NA, # "wash index - - -"
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_washindex_deaths_above"]] ), # "Above median"
                     populateNull( sub_numbers_list[["3. Quarterly-AZI_washindex_deaths_below"]] ) # "Below median"
    ),
    
    PYR_quart = c(NA,  # 'Age mo'
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_age_PYR_1-5 mo"]] ), # "1-5"
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_age_PYR_6-11 mo"]] ), # "6-11",
                  NA,
                  NA,  # 'Sex - - -',
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_sex_PYR_Male"]] ),   # 'Male',
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_sex_PYR_Female"]] ), # 'Female'
                  NA,
                  NA,  # 'WAZ - - -'
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_underweight_PYR_Not underweight"]] ),   # '>= -2'
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_underweight_PYR_Underweight"]] ), # '< -2'
                  NA,
                  NA,  # 'Seasonality - - -'
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_season_PYR_RAIN"]] ),   # 'Rainy Season'
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_season_PYR_DRY"]] ), # 'Non-rainy Season'
                  NA,
                  NA,  # 'SMC - - -'
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_smc_PYR_given"]] ), # 'given 3 months before',
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_smc_PYR_notgiven"]] ), # 'not given 3 months before'
                  NA,
                  NA,  # 'District of residence - - -',
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_distr_PYR_Kita"]] ), # "Kita",
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_distr_PYR_Sagabari"]] ), # "Sagabari",
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_distr_PYR_Sefeto"]] ), # "Sefeto",
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_distr_PYR_Koulikoro/Kati"]] ), # "Koulikoro",
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_distr_PYR_Kalabancoro"]] ), # "Koulikoro",
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_distr_PYR_Kati"]] ), # "Kati",
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_distr_PYR_Ouélessébougou"]] ), # "Ouélessébougou",
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_distr_PYR_Kéniéba"]] ), # "Kenieba",
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_distr_PYR_Fana"]] ), # "Fana",
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_distr_PYR_Kangaba"]] ), # "Kangaba",
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_distr_PYR_Selingue"]] ), # "Selingue",
                  NA,
                  NA,  # 'order of residence - - -',
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_order_PYR_1"]] ), # "Kita",
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_order_PYR_2"]] ), # "Sagabari",
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_order_PYR_3"]] ), # "Sefeto",
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_order_PYR_4"]] ), # "Koulikoro",
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_order_PYR_5"]] ), # "Koulikoro",
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_order_PYR_6"]] ), # "Kati",
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_order_PYR_7"]] ), # "Ouélessébougou",
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_order_PYR_8"]] ), # "Kenieba",
                  NA,
                  NA,  #               "Distance to Nearest Health Facility - - -",
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_dist_PYR_over5"]] ), # ' < 5km ',
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_dist_PYR_less5"]] ), #' >= 5km ',
                  NA,
                  NA,  #               "National outreach strategy - - -",
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_strategie_PYR_advanced"]] ), # ' < 5km ',
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_strategie_PYR_standard"]] ), #' >= 5km ',
                  NA,
                  NA,  # "Household asset index - - -"
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_assetIndex_PYR_above"]] ), # 'Above median'
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_assetIndex_PYR_below"]] ), # 'Below median'
                  NA,
                  NA, # "wash index - - -"
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_washindex_PYR_above"]] ), # "Above median"
                  populateNull( sub_numbers_list[["3. Quarterly-AZI_washindex_PYR_below"]] ) # "Below median"
    ),
    
    
    
    pvalues_int = c(NA,  # 'Age mo'
                    populateNull( sub_pvalues_list[["age_pvalue"]] ), # "1-5"
                    populateNull( sub_pvalues_list[["age_pvalue"]] ), # "6-11",
                    NA,
                    NA,  # 'Sex - - -',
                    populateNull( sub_pvalues_list[["sex_pvalue"]] ),   # 'Male',
                    populateNull( sub_pvalues_list[["sex_pvalue"]] ), # 'Female'
                    NA,
                    NA,  # 'WAZ - - -'
                    populateNull( sub_pvalues_list[["underweight_pvalue"]] ),   # '>= -2'
                    populateNull( sub_pvalues_list[["underweight_pvalue"]] ), # '< -2'
                    NA,
                    NA,  # 'Seasonality - - -'
                    populateNull( sub_pvalues_list[["season_pvalue"]] ),   # 'Rainy Season'
                    populateNull( sub_pvalues_list[["season_pvalue"]] ), # 'Non-rainy Season'
                    NA,
                    NA,  # 'SMC - - -'
                    populateNull( sub_pvalues_list[["smc_pvalue"]] ), # 'given 3 months before',
                    populateNull( sub_pvalues_list[["smc_pvalue"]] ), # 'not given 3 months before'
                    NA,
                    NA,  # 'District of residence - - -',
                    populateNull( sub_pvalues_list[["distr_pvalue"]] ), # "Kita",
                    populateNull( sub_pvalues_list[["distr_pvalue"]] ), # "Sagabari",
                    populateNull( sub_pvalues_list[["distr_pvalue"]] ), # "Sefeto",
                    populateNull( sub_pvalues_list[["distr_pvalue"]] ), # "Koulikoro",
                    populateNull( sub_pvalues_list[["distr_pvalue"]] ), # "Kalabancoro",
                    populateNull( sub_pvalues_list[["distr_pvalue"]] ), # "Kati",
                    populateNull( sub_pvalues_list[["distr_pvalue"]] ), # "Ouélessébougou",
                    populateNull( sub_pvalues_list[["distr_pvalue"]] ), # "Kenieba",
                    populateNull( sub_pvalues_list[["distr_pvalue"]] ), # "Fana",
                    populateNull( sub_pvalues_list[["distr_pvalue"]] ), # "Kangaba",
                    populateNull( sub_pvalues_list[["distr_pvalue"]] ), # "Selingue",
                    NA,
                    NA,  # 'District of residence - - -',
                    populateNull( sub_pvalues_list[["order_pvalue"]] ), # "Kita",
                    populateNull( sub_pvalues_list[["order_pvalue"]] ), # "Sagabari",
                    populateNull( sub_pvalues_list[["order_pvalue"]] ), # "Sefeto",
                    populateNull( sub_pvalues_list[["order_pvalue"]] ), # "Koulikoro",
                    populateNull( sub_pvalues_list[["order_pvalue"]] ), # "Kalabancoro",
                    populateNull( sub_pvalues_list[["order_pvalue"]] ), # "Kati",
                    populateNull( sub_pvalues_list[["order_pvalue"]] ), # "Ouélessébougou",
                    populateNull( sub_pvalues_list[["order_pvalue"]] ), # "Kenieba",
                    NA,
                    NA,  #               "Distance to Nearest Health Facility - - -",
                    populateNull( sub_pvalues_list[["dist_pvalue"]] ), # ' < 5km ',
                    populateNull( sub_pvalues_list[["dist_pvalue"]] ), #' >= 5km ',
                    NA,
                    NA,  #               "National outreach strategy - - -",
                    populateNull( sub_pvalues_list[["strategie_pvalue"]] ), # ' < 5km ',
                    populateNull( sub_pvalues_list[["strategie_pvalue"]] ), #' >= 5km ',
                    NA,
                    NA,  # "Household asset index - - -"
                    populateNull( sub_pvalues_list[["assetIndex_pvalue"]] ), # 'Above median'
                    populateNull( sub_pvalues_list[["assetIndex_pvalue"]] ), # 'Below median'
                    NA,
                    NA, # "wash index - - -"
                    populateNull( sub_pvalues_list[["washindex_pvalue"]] ), # "Above median"
                    populateNull( sub_pvalues_list[["washindex_pvalue"]] ) # "Below median"
    )
    
  )
  
  
  
  
  zeros_ctrl <- c()
  for(i in 1:length(df$deaths_ctrl)){
    zeros_ctrl[i] <- ifelse(df$deaths_ctrl[i]==0 , i,NA )
  }
  zeros_ctrl <- ifelse(all(is.na(zeros_ctrl)),NA,na.omit(zeros_ctrl))
  
  
  zeros_bian <- c()
  for(i in 1:length(df$deaths_bian)){
    zeros_bian[i] <- ifelse(df$deaths_bian[i]==0 , i,NA )
  }
  zeros_bian <- ifelse(all(is.na(zeros_bian)),NA,na.omit(zeros_bian))
  # 
  # 
  # 
  zeros_quart <- c()
  for(i in 1:length(df$deaths_quart)){
    zeros_quart[i] <- ifelse(df$deaths_quart[i]==0 , i,NA )
  }
  zeros_quart <- as.vector(na.omit(zeros_quart))
  # 
  # 
  if(any(!is.na(zeros_ctrl)))
    df[zeros_ctrl,c("effect_bian","upper_ci_bian","lower_ci_bian",
                    "effect_quart","upper_ci_quart","lower_ci_quart")] <- NA
  
  if(any(!is.na(zeros_bian)))
    df[zeros_bian,c("effect_bian","upper_ci_bian","lower_ci_bian",
                    "effect_bian_quart","upper_ci_bian_quart","lower_ci_bian_quart")] <- NA
  
  if(any(!is.na(zeros_quart))){
    df[zeros_quart,c("effect_bian_quart","upper_ci_bian_quart","lower_ci_bian_quart",
                     "effect_quart","upper_ci_quart","lower_ci_quart")] <- NA
  }
  
  
  

  
  
  df$reportIRR_bian <- ifelse(!is.na(df$effect_bian),paste0(round(df$effect_bian,2)," (",round(df$lower_ci_bian,2),", ", round(df$upper_ci_bian,2),")"),"")
  df$reportIRR_quart <- ifelse(!is.na(df$effect_quart),paste0(round(df$effect_quart,2)," (",round(df$lower_ci_quart,2),", ",round(df$upper_ci_quart,2),")"),"")
  df$reportIRR_bian_quart <- ifelse(!is.na(df$effect_bian_quart),paste0(round(df$effect_bian_quart,2)," (",round(df$lower_ci_bian_quart,2),", ",round(df$upper_ci_bian_quart,2),")"),"")
  
  df$deaths_PYR_ctrl <- ifelse(!is.na(df$PYR_ctrl),paste0(round(df$deaths_ctrl,2)," (",formatC(df$PYR_ctrl,format = "f", big.mark=",", digits=1),")"),"")
  
  df$deaths_PYR_bian <- ifelse(!is.na(df$PYR_bian),paste0(round(df$deaths_bian,2)," (",formatC(df$PYR_bian,format = "f", big.mark=",", digits=1),")"),"")
  
  df$deaths_PYR_quart <- ifelse(!is.na(df$PYR_quart),paste0(round(df$deaths_quart,2)," (",formatC(df$PYR_quart,format = "f", big.mark=",", digits=1),")"),"")
  
  df$pvalues_int = round(df$pvalues_int,2)
  
  labelDF <- matrix(nrow = 2, ncol = ncol(df)) %>% as.data.frame()
  names(labelDF) <- names(df)
  
  labelDF$modifier <- c("Variable", "    (p-for-interaction)")
  labelDF$deaths_PYR_ctrl <- c("CONTROL", "Deaths (PYR)")
  labelDF$deaths_PYR_bian <- c("BIANNUAL-AZI", "Deaths (PYR)")
  labelDF$deaths_PYR_quart <- c("QUARTERLY-AZI", "Deaths (PYR)")
  
  labelDF$reportIRR_bian <- c("Incidence rate ratio", "(95% CI)")
  labelDF$reportIRR_quart <- c("Incidence rate ratio", "(95% CI)")
  labelDF$reportIRR_bian_quart <- c("Incidence rate ratio", "(95% CI)")
  
  df <- rbind.data.frame(labelDF,df)
  
  
  
  ## Separate individual and village characteristics
  df_ind <- df[c(1:14,19:22,54:60),]
  df_vil <- df[c(1:2,15:18,23:35,36:45,46:49,50:53),]
  
  
  linetype_ind <- ifelse(df_ind$pvalues_int<.1 ,
                         1,2)
  
  linetype_vil <- ifelse(df_vil$pvalues_int<.1 ,
                         1,2)
  
  
  
  ### Control vs Biannual, Individual variables
  fp_ctrl_bian_ind = forestplot(df_ind[,c("modifier","deaths_PYR_bian","deaths_PYR_ctrl","reportIRR_bian")],
                                labeltext="",
                                title="",
                                col=fpColors(box="black", lines="black", zero = "gray50"),
                                
                                is.summary=c(TRUE,TRUE,rep(FALSE,nrow(df_ind)-2)),
                                graphwidth=unit(45,"mm"),
                                lineheight=unit(3.5,"mm"),
                                txt_gp=fpTxtGp(
                                  ticks=gpar(cex=1),
                                  xlab=gpar(cex=1)),
                                colgap=unit(3.5,"mm"),
                                xticks.digits = 2,
                                align="l",
                                
                                graph.pos=4,
                                mean=as.numeric(df_ind$effect_bian) %>% log(), 
                                lower=as.numeric(df_ind$lower_ci_bian) %>% log(), 
                                upper=as.numeric(df_ind$upper_ci_bian) %>% log(),
                                # 
                                boxsize=0.15,
                                # 
                                zero=0,
                                xlab=paste0("Incidence rate ratio (log scale)\n",
                                            "Favors ","AZI",
                                            "                     Favors ","Control"),
                                
                                lwd.ci=2, 
                                ci.vertices=TRUE, 
                                ci.vertices.height = 0.2,
                                lty.ci = linetype_ind
  )
  
  ### Control vs Biannual, Village variables
  fp_ctrl_bian_vil = forestplot(df_vil[,c("modifier","deaths_PYR_bian","deaths_PYR_ctrl","reportIRR_bian")],
                                labeltext="",
                                title="",
                                col=fpColors(box="black", lines="black", zero = "gray50"),
                                
                                is.summary=c(TRUE,TRUE,rep(FALSE,nrow(df_vil)-2)),
                                graphwidth=unit(45,"mm"),
                                lineheight=unit(3.5,"mm"),
                                txt_gp=fpTxtGp(
                                  ticks=gpar(cex=1),
                                  xlab=gpar(cex=1)),
                                colgap=unit(3.5,"mm"),
                                xticks.digits = 2,
                                align="l",
                                
                                graph.pos=4,
                                
                                mean=as.numeric(df_vil$effect_bian) %>% log(), 
                                lower=as.numeric(df_vil$lower_ci_bian) %>% log(), 
                                upper=as.numeric(df_vil$upper_ci_bian) %>% log(),
                                # 
                                boxsize=0.15,
                                # xlog=all(prettydat$measure!="MD"),
                                zero=0,
                                xlab=paste0("Incidence rate ratio (log scale)\n",
                                            "Favors ","AZI",
                                            "                     Favors ","Control"),
                                
                                lwd.ci=2, 
                                ci.vertices=TRUE, 
                                ci.vertices.height = 0.2,
                                lty.ci = linetype_vil
  )
  
  
  
  
  
  ### Control vs Quarterly, Individual variables
  fp_ctrl_quart_ind = forestplot(df_ind[,c("modifier","deaths_PYR_quart","deaths_PYR_ctrl","reportIRR_quart")],
                                 labeltext="",
                                 title="",
                                 col=fpColors(box="black", lines="black", zero = "gray50"),
                                 
                                 is.summary=c(TRUE,TRUE,rep(FALSE,nrow(df_ind)-2)),
                                 graphwidth=unit(45,"mm"),
                                 lineheight=unit(3.5,"mm"),
                                 txt_gp=fpTxtGp(
                                   ticks=gpar(cex=1),
                                   xlab=gpar(cex=1)),
                                 colgap=unit(3.5,"mm"),
                                 xticks.digits = 2,
                                 align="l",
                                 
                                 graph.pos=4,
                                 
                                 mean=as.numeric(df_ind$effect_quart) %>% log(), 
                                 lower=as.numeric(df_ind$lower_ci_quart) %>% log(), 
                                 upper=as.numeric(df_ind$upper_ci_quart) %>% log(),
                                 # 
                                 boxsize=0.15,
                                 # 
                                 zero=0,
                                 xlab=paste0("Incidence rate ratio (log scale)\n",
                                             "Favors ","AZI",
                                             "                     Favors ","Control"),
                                 
                                 lwd.ci=2, 
                                 ci.vertices=TRUE, 
                                 ci.vertices.height = 0.2,
                                 lty.ci = linetype_ind
  )
  
  
  
  ### Control vs Quarterly, Village variables
  fp_ctrl_quart_vil = forestplot(df_vil[,c("modifier","deaths_PYR_quart","deaths_PYR_ctrl","reportIRR_quart")],
                                 labeltext="",
                                 title="",
                                 col=fpColors(box="black", lines="black", zero = "gray50"),
                                 
                                 is.summary=c(TRUE,TRUE,rep(FALSE,nrow(df_vil)-2)),
                                 graphwidth=unit(45,"mm"),
                                 lineheight=unit(3.5,"mm"),
                                 txt_gp=fpTxtGp(
                                   ticks=gpar(cex=1),
                                   xlab=gpar(cex=1)),
                                 colgap=unit(3.5,"mm"),
                                 xticks.digits = 2,
                                 align="l",
                                 
                                 graph.pos=4,
                                 
                                 mean=as.numeric(df_vil$effect_quart) %>% log(), 
                                 lower=as.numeric(df_vil$lower_ci_quart) %>% log(), 
                                 upper=as.numeric(df_vil$upper_ci_quart) %>% log(),
                                 # 
                                 boxsize=0.15,
                                 # 
                                 zero=0,
                                 xlab=paste0("Incidence rate ratio (log scale)\n",
                                             "Favors ","AZI",
                                             "                     Favors ","Control"),
                                 
                                 lwd.ci=2, 
                                 ci.vertices=TRUE, 
                                 ci.vertices.height = 0.2,
                                 lty.ci = linetype_vil
  )
  
  
  
  
  ### Biannual vs Quarterly, Individual variables
  fp_bian_quart_ind = forestplot(df_ind[,c("modifier","deaths_PYR_quart","deaths_PYR_bian","reportIRR_bian_quart")],
                                 labeltext="",
                                 title="",
                                 col=fpColors(box="black", lines="black", zero = "gray50"),
                                 
                                 is.summary=c(TRUE,TRUE,rep(FALSE,nrow(df_ind)-2)),
                                 graphwidth=unit(45,"mm"),
                                 lineheight=unit(3.5,"mm"),
                                 txt_gp=fpTxtGp(
                                   ticks=gpar(cex=1),
                                   xlab=gpar(cex=1)),
                                 colgap=unit(3.5,"mm"),
                                 xticks.digits = 2,
                                 align="l",
                                 
                                 graph.pos=4,
                                 
                                 mean=as.numeric(df_ind$effect_bian_quart) %>% log(), 
                                 lower=as.numeric(df_ind$lower_ci_bian_quart) %>% log(), 
                                 upper=as.numeric(df_ind$upper_ci_bian_quart) %>% log(),
                                 # 
                                 boxsize=0.15,
                                 # 
                                 zero=0,
                                 xlab=paste0("Incidence rate ratio (log scale)\n",
                                             "Favors ","Quarterl-AZI",
                                             "                     Favors ","Biannual-AZI"),
                                 
                                 lwd.ci=2, 
                                 ci.vertices=TRUE, 
                                 ci.vertices.height = 0.2,
                                 lty.ci = linetype_ind
  )
  
  
  
  ### Biannual vs Quarterly, Village variables
  fp_bian_quart_vil = forestplot(df_vil[,c("modifier","deaths_PYR_quart","deaths_PYR_bian","reportIRR_bian_quart")],
                                 labeltext="",
                                 title="",
                                 col=fpColors(box="black", lines="black", zero = "gray50"),
                                 
                                 is.summary=c(TRUE,TRUE,rep(FALSE,nrow(df_vil)-2)),
                                 graphwidth=unit(45,"mm"),
                                 lineheight=unit(3.5,"mm"),
                                 txt_gp=fpTxtGp(
                                   ticks=gpar(cex=1),
                                   xlab=gpar(cex=1)),
                                 colgap=unit(3.5,"mm"),
                                 xticks.digits = 2,
                                 align="l",
                                 
                                 graph.pos=4,
                                 
                                 mean=as.numeric(df_vil$effect_bian_quart) %>% log(), 
                                 lower=as.numeric(df_vil$lower_ci_bian_quart) %>% log(), 
                                 upper=as.numeric(df_vil$upper_ci_bian_quart) %>% log(),
                                 # 
                                 boxsize=0.15,
                                 # 
                                 zero=0,
                                 xlab=paste0("Incidence rate ratio (log scale)\n",
                                             "Favors ","Quarterl-AZI",
                                             "                     Favors ","Biannual-AZI"),
                                 
                                 lwd.ci=2, 
                                 ci.vertices=TRUE, 
                                 ci.vertices.height = 0.2,
                                 lty.ci = linetype_vil
  )
  
  
  plotList <- list()
  
  plotList[[1]] <- grid.grabExpr(print(fp_ctrl_bian_ind))
  plotList[[2]] <- grid.grabExpr(print(fp_ctrl_bian_vil))
  plotList[[3]] <- grid.grabExpr(print(fp_ctrl_quart_ind))
  plotList[[4]] <- grid.grabExpr(print(fp_ctrl_quart_vil))
  plotList[[5]] <- grid.grabExpr(print(fp_bian_quart_ind))
  plotList[[6]] <- grid.grabExpr(print(fp_bian_quart_vil))
  
  labels=c("A)          Control vs Biannual-AZI, individual level",
           "A)          Control vs Biannual-AZI, village level",
           "B)          Control vs Quarterly-AZI, individual level",
           "B)          Control vs Quarterly-AZI, village level",
           "C)          Biannual-AZI vs Quarterly-AZI, individual level",
           "C)          Biannual-AZI vs Quarterly-AZI, village level")
  
  labelList <- as.list(labels)
  
  
  path_res <- here::here("results")
  
  
  for(i in 1:6){
    
    cairo_pdf(paste0(path_res,"/forestplots_",i,".pdf"),
              width=7*2,height=7*2)
    

    
    p<- ggpubr::ggarrange(plotList[[i]],
                          labels=labelList[[i]],
                          ncol=1,nrow=1)
    
    print(ggpubr::annotate_figure(p,top=ggpubr::text_grob(" ",
                                                          face="bold",size=8)))
    
    dev.off()   
  }
  
  
  
}

