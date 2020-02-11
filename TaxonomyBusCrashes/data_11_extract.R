rm(list=setdiff(ls(), c("yrs", "base_dir", "data_base_dir", "extracted_data_dir")))
library(data.table)
require(bit64)
library(devtools)
library(sas7bdat)
library(dplyr)
##Year 2011############
data_out <- file.path(extracted_data_dir, "data11.RData")

if (!file.exists(data_out)) {
  acc_11 <- read.sas7bdat(paste0(data_base_dir, "GES11/GES11PCSAS/accident.sas7bdat"))
  acc11_sel <- acc_11[,c('CASENUM','YEAR','MONTH','DAY_WEEK','HOUR', 'MINUTE', 'PERNOTMVIT','VE_TOTAL','MAN_COLL','INT_HWY',
                         'RELJCT2','REL_ROAD','LGT_COND', 'WEATHER1','SCH_BUS','NUM_INJ',
                         'WEIGHT', 'STRATUM','PSU','PSUSTRAT')]
  
  acc11_sel$CASENUM <- as.character(acc11_sel$CASENUM)
  
  veh_11 <- read.sas7bdat(paste0(data_base_dir, "GES11/GES11PCSAS/vehicle.sas7bdat"))
  veh11_sel <- veh_11[, c('CASENUM','MOD_YEAR','BODY_TYP','TRAV_SP','P_CRASH1','P_CRASH2','VEH_NO','SPEEDREL', 'VNUM_LAN','VALIGN','VPROFILE','VSURCOND',
                          'VTRAFCON','VSPD_LIM')]
  veh11_sel$CASENUM <- as.character(veh11_sel$CASENUM)
  veh11_sel$VEH_NO <- as.character(veh11_sel$VEH_NO)
  
  per_11 <- read.sas7bdat(paste0(data_base_dir, "GES11/GES11PCSAS/person.sas7bdat"))
  per11_sel <- per_11[,c('CASENUM','PER_NO','PER_TYP','VEH_NO','SEX','AGE','INJ_SEV','DRINKING', 'DRUGS')]
  per11_sel <- per11_sel %>% filter(PER_TYP %in% c(1,3,4,5,6,7,8))
  
  per11_sel$CASENUM <- as.character(per11_sel$CASENUM)
  per11_sel$VEH_NO <- as.character(per11_sel$VEH_NO)
  
  imp_11 <- read.sas7bdat(paste0(data_base_dir, "GES11/GES11PCSAS/nmimpair.sas7bdat"))
  nmimp11_sel <- imp_11[,c('CASENUM','VEH_NO','NMIMPAIR')]
  
  nmimp11_sel$CASENUM <- as.character(nmimp11_sel$CASENUM)
  nmimp11_sel$VEH_NO <- as.character(nmimp11_sel$VEH_NO)
  
  dimp_11 <- read.sas7bdat(paste0(data_base_dir, "GES11/GES11PCSAS/drimpair.sas7bdat"))
  drimp11_sel <- dimp_11[,c('CASENUM','VEH_NO','DRIMPAIR')]
  
  drimp11_sel$CASENUM <- as.character(drimp11_sel$CASENUM)
  drimp11_sel$VEH_NO <- as.character(drimp11_sel$VEH_NO)
  
  dis_11 <- read.sas7bdat(paste0(data_base_dir, "GES11/GES11PCSAS/distract.sas7bdat"))
  dis11_sel <- dis_11[,c('CASENUM','VEH_NO','MDRDSTRD')]
  
  dis11_sel$CASENUM <- as.character(dis11_sel$CASENUM)
  dis11_sel$VEH_NO <- as.character(dis11_sel$VEH_NO)
  
  vis_11 <- read.sas7bdat(paste0(data_base_dir, "GES11/GES11PCSAS/vision.sas7bdat"))
  vis11_sel <- vis_11[,c('CASENUM','VEH_NO','MVISOBSC')]
  
  vis11_sel$CASENUM <- as.character(vis11_sel$CASENUM)
  vis11_sel$VEH_NO <- as.character(vis11_sel$VEH_NO)
  
  dat1 <- inner_join(veh11_sel, dis11_sel, by = c('CASENUM', 'VEH_NO'))
  dat1 <- inner_join(dat1, vis11_sel, by = c('CASENUM', 'VEH_NO'))
  dat1 <- inner_join(dat1, drimp11_sel, by = c('CASENUM', 'VEH_NO'))
  dat1 <- left_join(per11_sel, dat1, by = c('CASENUM', 'VEH_NO'))
  dat1 <- left_join(dat1, nmimp11_sel, by = c('CASENUM', 'VEH_NO'))
  dat1 <- data.table(dat1)
  
  # grep('*.y',colnames(dat1) )
  # dat1 <- dat1 %>% select(-c(17, 18, 20, 25, 34, 35))
  # grep('*.x',colnames(dat1) )
  # dat1 <- dat1 %>% select(-c(20, 21))
  #Bus Accidents#
  dat_bus <- dat1 %>% filter(BODY_TYP %in% c(50, 51,52,58, 59))
  dat_nobus <- setdiff(dat1, dat_bus)
  dat_bus$bus_age <- 2011 - dat_bus$MOD_YEAR
  dat_bus$bus_service_type <- as.numeric(dat_bus$BODY_TYP == 50)
  dat_bus$iad <-  as.numeric(dat_bus$DRIMPAIR %in% c(1,2,3,4,5,6,7,8,9,10,96) |
                               dat_bus$DRINKING == 1 | dat_bus$DRUGS == 1) 
  #Vehicle not a bus#
  #dat_nobus <- dat1 %>% filter(!BODY_TYP %in% c(50, 58, 59))
  
  #Weird case: 200511123729
  
  dat_nobus$vis_obs_partner <- as.numeric(!(dat_nobus$MVISOBSC %in% c(0,50,99,93,94,NA))) 
  dat_nobus$distr_partner <- as.numeric(!(dat_nobus$MDRDSTRD %in% c(0,99,93,94,NA)))
  dat_nobus$alc_partner <- as.numeric(!(dat_nobus$DRINKING %in% c(0,8,9)))
  dat_nobus$drug_partner <- as.numeric(!(dat_nobus$DRUGS %in% c(0,8,9)))
  dat_nobus$imp_partner <- as.numeric(!(dat_nobus$DRIMPAIR %in% c(0,95,98,99,NA)) | !(dat_nobus$NMIMPAIR %in% c(0,95,98,99,NA)))
  
  
  
  
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
  acc11_sel <- data.table(acc11_sel)
  dat11 <- inner_join(dat3, acc11_sel, by = c('CASENUM'))
  save(dat11, file = data_out)
}
