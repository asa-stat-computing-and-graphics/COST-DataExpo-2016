rm(list=setdiff(ls(), c("yrs", "base_dir", "data_base_dir", "extracted_data_dir")))
library(data.table)
require(bit64)
library(devtools)
library(sas7bdat)
library(dplyr)
##Year 2010############
data_out <- file.path(extracted_data_dir, "data10.RData")

if (!file.exists(data_out)) {
  acc_10 <- read.sas7bdat(paste0(data_base_dir, "GES10/GES10PCSAS/accident.sas7bdat"))
  acc10_sel <- acc_10[,c('CASENUM','YEAR','MONTH','DAY_WEEK','HOUR', 'MINUTE','VEH_INVL','NON_INVL','MAN_COL','INT_HWY',
                         'RELJCT2','REL_ROAD','LGT_COND', 'WEATHER1','SCH_BUS','NUM_INJ',
                         'WEIGHT', 'STRATUM','PSU','PSUSTRAT')]
  
  acc10_sel$CASENUM <- as.character(acc10_sel$CASENUM)
  
  veh_10 <- read.sas7bdat(paste0(data_base_dir, "GES10/GES10PCSAS/vehicle.sas7bdat"))
  veh10_sel <- veh_10[,c('CASENUM','MODEL_YR','BODY_TYP','TRAV_SP','P_CRASH1',
                         'P_CRASH2','VEHNO','SPEEDREL', 'VNUM_LAN','VALIGN','VPROFILE','VSURCOND',
                         'VTRAFCON','OCC_INVL','VSPD_LIM')]
  veh10_sel$CASENUM <- as.character(veh10_sel$CASENUM)
  veh10_sel$VEHNO <- as.character(veh10_sel$VEHNO)
  
  per_10 <- read.sas7bdat(paste0(data_base_dir, "GES10/GES10PCSAS/person.sas7bdat"))
  per10_sel <- per_10[,c('CASENUM','PERNO','PER_TYP','VEHNO','SEX','AGE','INJ_SEV','PER_ALCH', 'PER_DRUG')]
  per10_sel <- per10_sel %>% filter(PER_TYP %in% c(1,3,4,5,6,7,8))
  
  per10_sel$CASENUM <- as.character(per10_sel$CASENUM)
  per10_sel$VEHNO <- as.character(per10_sel$VEHNO)
  
  imp_10 <- read.sas7bdat(paste0(data_base_dir, "GES10/GES10PCSAS/impair.sas7bdat"))
  imp10_sel <- imp_10[,c('CASENUM','VEHNO','MIMPAIR')]
  
  imp10_sel$CASENUM <- as.character(imp10_sel$CASENUM)
  imp10_sel$VEHNO <- as.character(imp10_sel$VEHNO)
  
  dis_10 <- read.sas7bdat(paste0(data_base_dir, "GES10/GES10PCSAS/distract.sas7bdat"))
  dis10_sel <- dis_10[,c('CASENUM','VEHNO','MDRDSTRD')]
  
  dis10_sel$CASENUM <- as.character(dis10_sel$CASENUM)
  dis10_sel$VEHNO <- as.character(dis10_sel$VEHNO)
  
  vis_10 <- read.sas7bdat(paste0(data_base_dir, "GES10/GES10PCSAS/vision.sas7bdat"))
  vis10_sel <- vis_10[,c('CASENUM','VEHNO','MVISOBSC')]
  
  vis10_sel$CASENUM <- as.character(vis10_sel$CASENUM)
  vis10_sel$VEHNO <- as.character(vis10_sel$VEHNO)
  
  dat1 <- inner_join(veh10_sel, dis10_sel, by = c('CASENUM', 'VEHNO'))
  dat1 <- inner_join(dat1, vis10_sel, by = c('CASENUM', 'VEHNO'))
  dat1 <- inner_join(dat1, imp10_sel, by = c('CASENUM', 'VEHNO'))
  dat1 <- left_join(per10_sel, dat1, by = c('CASENUM', 'VEHNO'))
  dat1 <- data.table(dat1)
  
  # grep('*.y',colnames(dat1) )
  # dat1 <- dat1 %>% select(-c(17, 18, 20, 25, 34, 35))
  # grep('*.x',colnames(dat1) )
  # dat1 <- dat1 %>% select(-c(20, 21))
  #Bus Accidents#
  dat_bus <- dat1 %>% filter(BODY_TYP %in% c(50, 51,52,58, 59))
  dat_nobus <- setdiff(dat1, dat_bus)
  dat_bus$bus_age <- 2010 - dat_bus$MODEL_YR
  dat_bus$bus_service_type <- as.numeric(dat_bus$BODY_TYP == 50)
  dat_bus$iad <-  as.numeric(dat_bus$MIMPAIR %in% c(1,2,3,4,5,6,7,8,9,10,96) | 
                               dat_bus$PER_ALCH == 1 | dat_bus$PER_DRUG == 1) 
  #Vehicle not a bus#
  
  #Weird case: 200511123729
  
  dat_nobus$vis_obs_partner <- as.numeric(!(dat_nobus$MVISOBSC %in% c(0,50,99,93,94,NA))) 
  dat_nobus$distr_partner <- as.numeric(!(dat_nobus$MDRDSTRD %in% c(0,99,93,94,NA)))
  dat_nobus$alc_partner <- as.numeric(!(dat_nobus$PER_ALCH %in% c(0,8,9)))
  dat_nobus$drug_partner <- as.numeric(!(dat_nobus$PER_DRUG %in% c(0,8,9)))
  dat_nobus$imp_partner <- as.numeric(!(dat_nobus$MIMPAIR %in% c(0,95,98,99)))
  
  
  
  
  dat_nobus$car_partner <- as.numeric(dat_nobus$BODY_TYP %in% c(1,2,4,6,17,8,9,12,13))
  dat_nobus$pick_partner <- as.numeric(dat_nobus$BODY_TYP %in% c(10,11,14,15,16,19))
  dat_nobus$suv_partner <- as.numeric(dat_nobus$BODY_TYP %in% c(3,5,7))
  dat_nobus$vans_partner <- as.numeric(dat_nobus$BODY_TYP %in% c(20,21,22,23,24,25,28,29))
  dat_nobus$ltruck_partner <- as.numeric(dat_nobus$BODY_TYP %in% c(30,31,32,33,39,40,41,42,45,48,49))
  dat_nobus$htruck_partner <- as.numeric(dat_nobus$BODY_TYP %in% c(60,61,62,63,64,65,66,67,68,71,72,78,79))
  dat_nobus$mc_partner <- as.numeric(dat_nobus$BODY_TYP %in% c(80,81,82,83,88,89))
  dat_nobus$oth_partner <- as.numeric(dat_nobus$BODY_TYP %in% c(90,91,92,93,94,95,97,98,99))
  
  #dat_nobus <- dat_nobus %>% select(-c(2,3,4,5,6,7,8,10,12,13,15,17,18,19,20))
  
  
  dat2 <- dat_nobus %>% group_by(CASENUM) %>% summarize(age_p = mean(AGE),
                                                        car_p = as.numeric(sum(car_partner)>0),
                                                        pick_p = as.numeric(sum(pick_partner)>0),
                                                        suv_p = as.numeric(sum(suv_partner)>0),
                                                        van_p = as.numeric(sum(vans_partner)>0),
                                                        ltruck_p = as.numeric(sum(ltruck_partner)>0),
                                                        htruck_p = as.numeric(sum(htruck_partner)>0),
                                                        mc_p = as.numeric(sum(mc_partner)>0),
                                                        oth_p = as.numeric(sum(oth_partner)>0),
                                                        speed_p = as.numeric(sum(SPEEDREL,na.rm=TRUE)>0),
                                                        visob_p = as.numeric(sum(vis_obs_partner)>0),
                                                        distr_p = as.numeric(sum(distr_partner)>0),
                                                        alc_p = as.numeric(sum(alc_partner)>0),
                                                        drug_p = as.numeric(sum(drug_partner)>0),
                                                        imp_p = as.numeric(sum(imp_partner)>0))
  
  
  dat3 <- inner_join(dat_bus, dat2, by = c('CASENUM'))
  acc10_sel <- data.table(acc10_sel)
  dat10 <- inner_join(dat3, acc10_sel, by = c('CASENUM'))
  save(dat10, file = data_out)
}
