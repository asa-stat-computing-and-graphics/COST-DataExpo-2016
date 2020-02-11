rm(list=setdiff(ls(), c("yrs", "base_dir", "data_base_dir", "extracted_data_dir")))
library(data.table)
library(dplyr)

# Change these as needed
outfile <- file.path(extracted_data_dir, "data_final05_09.RData")

load(file.path(extracted_data_dir, 'data05.RData'))
load(file.path(extracted_data_dir, 'data06.RData'))
load(file.path(extracted_data_dir, 'data07.RData'))
load(file.path(extracted_data_dir, 'data08.RData'))
load(file.path(extracted_data_dir, 'data09.RData'))

dat09_f <- dat09 %>% rename(SPEED = TRAV_SP,
                            PER_TYPE = PER_TYP,
                            WEEKDAY = DAY_WEEK,
                            REL_RWY = REL_ROAD,
                            NUM_LAN = NO_LANES,
                            ALIGN = ALIGNMNT,
                            SPD_LIM  = SP_LIMIT,
                            LGHT_CON = LGT_COND,
                            SCHL_BUS = SCH_BUS)
dat_fin <- rbind(dat05,dat06,dat07,dat08,dat09_f)
tmp <- dat_fin %>% dplyr::select(-CASENUM:-VEHNO, 
                          -PER_ALCH:-IMPAIRMT,
                          -MODEL_YR:-SPEED,
                          -DR_DSTRD,
                          -STRATUM:-PSUSTRAT)
tmp$daytime <- as.numeric(tmp$HOUR_I <= 19 & tmp$HOUR_I >= 7)
tmp$weekday <- as.numeric(tmp$WEEKDAY < 7 & tmp$WEEKDAY > 1)
tmp$veh_invl <- cut(tmp$VEH_INVL, breaks = c(0,1,2,16), labels = FALSE)
tmp$nonm_invl <- as.numeric(tmp$NON_INVL != 0)
tmp$onroadway <- as.numeric(tmp$REL_RWY == 1)
tmp$crit_event <- cut(tmp$P_CRASH2, breaks = c(1,9,19,59,78,85,92,99), labels = FALSE)
tmp$single_lane <- as.numeric(tmp$NUM_LAN == 1)
tmp$intersection <- as.numeric(tmp$REL_JCT %in% c(1,2,11,12))
tmp$str_align <- as.numeric(tmp$ALIGN == 1 | tmp$ALIGN == 9)
tmp$profile_level <- as.numeric(tmp$PROFILE == 1 | tmp$PROFILE == 9)
tmp$sur_dry <- as.numeric(tmp$SUR_COND == 1 | tmp$SUR_COND == 9)
tmp$traf_control <- cut(tmp$TRAF_CON, breaks = c(-1,0,9,90,100), labels = FALSE)
tmp$light_cond <- cut(tmp$LGHT_CON, breaks = c(0,1,2,3,4,9), labels = FALSE)
tmp$dr_speeding <- as.numeric(!(tmp$SPEEDREL == 0 | tmp$SPEEDREL == 9))
tmp$dr_distracted <- as.numeric(!(tmp$MDRDSTRD == 0 | tmp$MDRDSTRD == 99))

tmp <- tmp %>% dplyr::select(-YEAR:-MINUTE_I,-VEH_INVL,-NON_INVL,-REL_RWY,-P_CRASH2,
                          -NUM_LAN,-REL_JCT,-ALIGN,-PROFILE,-SUR_COND,-TRAF_CON,
                          -LGHT_CON,-SPEEDREL,-MDRDSTRD)
tmp <- data.table(tmp)
tmp$spd_lim <- tmp$SPD_LIM
tmp[spd_lim==0,spd_lim:=15]
tmp[spd_lim==70,spd_lim:=65]
tmp[spd_lim==75,spd_lim:=65]
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
tmp[P_CRASH1 %in% c(0,4,7,12,15,16,17,97,99) ,bus_mov:=11]

tmp <- tmp %>% dplyr::select(-SPD_LIM, -P_CRASH1, -MVISOBSC)
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
