rm(list=setdiff(ls(), c("yrs", "base_dir", "data_base_dir", "extracted_data_dir")))
library(dplyr)
library(data.table)
library(dplyr)

outfile <- file.path(extracted_data_dir, "data_final2010_2015.RData")

load(file.path(extracted_data_dir, 'data10.RData'))
load(file.path(extracted_data_dir, 'data11.RData'))
load(file.path(extracted_data_dir, 'data12.RData'))
load(file.path(extracted_data_dir, 'data13.RData'))
load(file.path(extracted_data_dir, 'data14.RData'))
load(file.path(extracted_data_dir, 'data15.RData'))

dat10_f <- dat10 %>% rename(DRINKING = PER_ALCH,
                            VE_TOTAL = VEH_INVL,
                            MAN_COLL = MAN_COL,
                            PERNOTMVIT = NON_INVL,
                            VEH_NO = VEHNO,
                            PER_NO = PERNO,
                            DRUGS = PER_DRUG,
                            MOD_YEAR  = MODEL_YR)
dat10_f <- dat10_f %>% select(-OCC_INVL,-MIMPAIR)


dat_fin <- rbind(dat11,dat12,dat13,dat14,dat15)
dat_fin <- dat_fin %>% select(-DRIMPAIR,-NMIMPAIR)

dat_final <- rbind(dat10_f,dat_fin)
tmp <- dat_final %>% dplyr::select(-CASENUM:-VEH_NO, 
                                 -DRINKING:-DRUGS,
                                 -MOD_YEAR:-TRAV_SP,
                                 -STRATUM:-PSUSTRAT)
tmp$daytime <- as.numeric(tmp$HOUR <= 19 & tmp$HOUR >= 7)
tmp$weekday <- as.numeric(tmp$DAY_WEEK < 7 & tmp$DAY_WEEK > 1)
tmp$veh_invl <- cut(tmp$VE_TOTAL, breaks = c(0,1,2,16), labels = FALSE)
tmp$nonm_invl <- as.numeric(tmp$PERNOTMVIT != 0)
tmp$onroadway <- as.numeric(tmp$REL_ROAD == 1)
tmp$crit_event <- cut(tmp$P_CRASH2, breaks = c(1,9,19,59,78,85,92,99), labels = FALSE)
tmp$single_lane <- as.numeric(tmp$VNUM_LAN == 1)
tmp$intersection <- as.numeric(tmp$RELJCT2 == 2 | tmp$RELJCT2 == 3)
tmp$str_align <- as.numeric(tmp$VALIGN == 1 | tmp$VALIGN == 9)
tmp$profile_level <- as.numeric(tmp$VPROFILE == 1 | tmp$VPROFILE == 9)
tmp$sur_dry <- as.numeric(tmp$VSURCOND == 1 | tmp$VSURCOND == 9)
tmp$traf_control <- cut(tmp$VTRAFCON, breaks = c(-1,0,9,90,100), labels = FALSE)
tmp$light_cond <- cut(tmp$LGT_COND, breaks = c(0,1,2,3,4,9), labels = FALSE)
tmp$badweather <- as.numeric(!(tmp$WEATHER1 == 1 |tmp$WEATHER1 == 9))
tmp$dr_speeding <- as.numeric(!(tmp$SPEEDREL == 0 | tmp$SPEEDREL == 9))
tmp$dr_distracted <- as.numeric(!(tmp$MDRDSTRD == 0 | tmp$MDRDSTRD == 99))
tmp$sch_bus <- as.numeric(tmp$SCH_BUS == 1)

tmp <- tmp %>% dplyr::select(-YEAR:-PERNOTMVIT,-REL_ROAD,-P_CRASH2,
                             -VNUM_LAN,-RELJCT2,-VALIGN,-VPROFILE,-VSURCOND,-VTRAFCON,
                             -LGT_COND,-SPEEDREL,-MDRDSTRD, -WEATHER1, -SCH_BUS)
tmp <- data.table(tmp)
tmp$spd_lim <- tmp$VSPD_LIM
tmp[spd_lim==0,spd_lim:=15]
tmp[spd_lim==70,spd_lim:=65]
tmp[spd_lim==75,spd_lim:=65]
tmp[spd_lim==97,spd_lim:=35]
tmp[spd_lim==98,spd_lim:=35]
tmp[spd_lim==99,spd_lim:=35]

tmp[(P_CRASH1==8 | P_CRASH1==9),bus_mov:=1]
tmp[P_CRASH1==1 ,bus_mov:=2]
tmp[P_CRASH1==5 ,bus_mov:=3]
tmp[P_CRASH1==3 ,bus_mov:=4]
tmp[P_CRASH1==2 ,bus_mov:=5]
tmp[P_CRASH1==10 ,bus_mov:=6]
tmp[P_CRASH1==11 ,bus_mov:=7]
tmp[P_CRASH1==6 ,bus_mov:=8]
tmp[P_CRASH1==13 ,bus_mov:=9]
tmp[P_CRASH1==14 ,bus_mov:=10]
tmp[P_CRASH1 %in% c(0,4,7,12,15,16,17,97,98,99) ,bus_mov:=11]

tmp <- tmp %>% dplyr::select(-VSPD_LIM, -P_CRASH1, -MVISOBSC)
tmp$bus_age1 <- tmp$bus_age
tmp$age_p1 <- tmp$age_p
tmp$age <- tmp$AGE
tmp[bus_age <0 ,bus_age1:=1]
tmp[age_p >70 ,age_p1:=70]
tmp[AGE >90 ,age:=85]
tmp <- tmp %>% dplyr::select(-AGE, -age_p, -bus_age)

tmp$sex <- tmp$SEX
tmp[SEX ==9 ,sex:=1]
tmp$psv_p <-  as.numeric(tmp$pick_p == 1 | tmp$van_p == 1 | tmp$suv_p == 1)
tmp$lht_p <-  as.numeric(tmp$ltruck_p == 1 | tmp$htruck_p == 1)

tmp <- tmp %>% dplyr::select(-SEX, -pick_p, -van_p, -suv_p, -ltruck_p, -htruck_p)
save(tmp, file = outfile)
