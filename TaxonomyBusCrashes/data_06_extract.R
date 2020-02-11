rm(list=setdiff(ls(), c("yrs", "base_dir", "data_base_dir", "extracted_data_dir")))
library(data.table)
require(bit64)
library(devtools)
library(sas7bdat)
library(dplyr)
##Year 2006############
data_out <- file.path(extracted_data_dir, "data06.RData")

if(!file.exists(data_out)) {
  acc_06 <- read.sas7bdat(paste0(data_base_dir, "GES06/SAS/GES2006/accident.sas7bdat"))
  acc06_sel <- acc_06[,c('CASENUM','YEAR','MONTH','WEEKDAY','HOUR_I', 'MINUTE_I','VEH_INVL','NON_INVL','MANCOL_I','INT_HWY',
                         'REL_JCT','REL_RWY','NUM_LAN','ALIGN','PROFILE','SUR_COND','TRAF_CON','SPD_LIM',
                         'LGHT_CON','SCHL_BUS','WEIGHT', 'STRATUM','PSU','PSUSTRAT')]
  
  acc06_sel$CASENUM <- as.character(acc06_sel$CASENUM)
  
  veh_06 <- read.sas7bdat(paste0(data_base_dir, "GES06/SAS/GES2006/vehicle.sas7bdat"))
  veh06_sel <- veh_06[,c('CASENUM','MODEL_YR','VIN','REGION','MAKE','BODY_TYP','SPEED','P_CRASH1',
                         'P_CRASH2','VEHNO','DR_DSTRD','SPEEDREL')]
  
  veh06_sel$CASENUM <- as.character(veh06_sel$CASENUM)
  veh06_sel$VEHNO <- as.character(veh06_sel$VEHNO)
  
  per_06 <- read.sas7bdat(paste0(data_base_dir, "GES06/SAS/GES2006/person.sas7bdat"))
  per06_sel <- per_06[,c('CASENUM','PERNO','PER_TYPE','VEHNO','SEX','AGE','INJ_SEV','PER_ALCH', 'PER_DRUG','IMPAIRMT', 'ACTION')]
  per06_sel <- per06_sel %>% filter(PER_TYPE %in% c(1,3,4,5,6,7,8))
  
  per06_sel$CASENUM <- as.character(per06_sel$CASENUM)
  per06_sel$VEHNO <- as.character(per06_sel$VEHNO)
  
  dis_06 <- read.sas7bdat(paste0(data_base_dir, "GES06/SAS/GES2006/distract.sas7bdat"))
  dis06_sel <- dis_06[,c('CASENUM','VEHNO','MDRDSTRD')]
  
  dis06_sel$CASENUM <- as.character(dis06_sel$CASENUM)
  dis06_sel$VEHNO <- as.character(dis06_sel$VEHNO)
  
  vis_06 <- read.sas7bdat(paste0(data_base_dir, "GES06/SAS/GES2006/vision.sas7bdat"))
  vis06_sel <- vis_06[,c('CASENUM','VEHNO','MVISOBSC')]
  
  vis06_sel$CASENUM <- as.character(vis06_sel$CASENUM)
  vis06_sel$VEHNO <- as.character(vis06_sel$VEHNO)
  
  dat1 <- inner_join(veh06_sel, dis06_sel, by = c('CASENUM', 'VEHNO'))
  dat1 <- inner_join(dat1, vis06_sel, by = c('CASENUM', 'VEHNO'))
  dat1 <- left_join(per06_sel, dat1, by = c('CASENUM', 'VEHNO'))
  dat1 <- data.table(dat1)
  dat1[,VIN:=NULL]
  # grep('*.y',colnames(dat1) )
  # dat1 <- dat1 %>% select(-c(17, 18, 20, 25, 34, 35))
  # grep('*.x',colnames(dat1) )
  # dat1 <- dat1 %>% select(-c(20, 21))
  #Bus Accidents#
  dat_bus <- dat1 %>% filter(BODY_TYP %in% c(50, 58, 59))
  dat_bus$bus_age <- 2006 - dat_bus$MODEL_YR
  dat_bus$bus_service_type <- as.numeric(dat_bus$BODY_TYP == 50)
  dat_bus$iad <-  as.numeric((dat_bus$IMPAIRMT %in% c(1,2,3,4,5,6,7,97,98)) | dat_bus$PER_ALCH == 2 | dat_bus$PER_DRUG == 2) 
  #Vehicle not a bus#
  dat_nobus <- dat1 %>% filter(!BODY_TYP %in% c(50, 58, 59))
  
  #Weird case: 200511123729
  
  dat_nobus$vis_obs_partner <- as.numeric(!(dat_nobus$MVISOBSC %in% c(0,50,99,93,94,NA))) 
  dat_nobus$distr_partner <- as.numeric(!(dat_nobus$MDRDSTRD %in% c(0,99,93,94,NA)))
  dat_nobus$alc_partner <- as.numeric(!(dat_nobus$PER_ALCH %in% c(0,1,9,6,7)))
  dat_nobus$drug_partner <- as.numeric(!(dat_nobus$PER_DRUG %in% c(0,1,9,6,7)))
  dat_nobus$imp_partner <- as.numeric(!(dat_nobus$IMPAIRMT %in% c(0,99)))
  
  
  dat_nobus$car_partner <- as.numeric(dat_nobus$BODY_TYP %in% c(1,2,4,6,17,8,9,12,13))
  dat_nobus$pick_partner <- as.numeric(dat_nobus$BODY_TYP %in% c(10,11,14,15,16,19))
  dat_nobus$suv_partner <- as.numeric(dat_nobus$BODY_TYP %in% c(3,5,7))
  dat_nobus$vans_partner <- as.numeric(dat_nobus$BODY_TYP %in% c(20,21,22,23,24,25,28,29))
  dat_nobus$ltruck_partner <- as.numeric(dat_nobus$BODY_TYP %in% c(30,31,32,33,39,40,41,42,45,48,49))
  dat_nobus$htruck_partner <- as.numeric(dat_nobus$BODY_TYP %in% c(60,64,65,66,78,79))
  dat_nobus$mc_partner <- as.numeric(dat_nobus$BODY_TYP %in% c(80,81,82,88,89))
  dat_nobus$oth_partner <- as.numeric(dat_nobus$BODY_TYP %in% c(90,91,92,93,97,99))
  
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
  acc06_sel <- data.table(acc06_sel)
  dat06 <- inner_join(dat3, acc06_sel, by = c('CASENUM'))
  save(dat06, file = data_out)
}
