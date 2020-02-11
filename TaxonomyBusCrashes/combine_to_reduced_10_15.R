rm(list=setdiff(ls(), c("yrs", "base_dir", "data_base_dir", "extracted_data_dir")))
#### data processing
library(data.table)
library(dplyr)

# Change these as needed
load(file.path(extracted_data_dir, 'data_final2010_2015.RData'))
outfile <- file.path(extracted_data_dir, 'data_final_2010_2015_reducedcols.RData')

## look for NAs
sapply(tmp, function(t) sum(is.na(t)))


## convert to factors
df <- data.table(tmp)
## fix NA's, hack
nrow(df)
df[is.na(speed_p),speed_p:=0]
sapply(df, function(t) sum(is.na(t)))
df <- df[complete.cases(df)]
nrow(df)

### set each column to the correct type for Gower
## SEX: factor. Put 9's in 1's
df$sex <- as.factor(df$sex)
## INJ_SEV: ordinal factor. Put 9's in 0's. Put 5's in 1's
df[INJ_SEV==9, INJ_SEV:=0]
df[INJ_SEV==5, INJ_SEV:=1]
df$INJ_SEV <- as.ordered(df$INJ_SEV)

df$bus_service_type <- as.factor(df$bus_service_type)
df$iad <- as.factor(df$iad)
df$car_p <- as.factor(df$car_p)
df$psv_p <- as.factor(df$psv_p)
df$lht_p <- as.factor(df$lht_p)
df$mc_p <- as.factor(df$mc_p)
df$oth_p <- as.factor(df$oth_p)
df$speed_p <- as.factor(df$speed_p)
df$visob_p <- as.factor(df$visob_p)
df$distr_p <- as.factor(df$distr_p)
df$alc_p <- as.factor(df$alc_p)
df$drug_p <- as.factor(df$drug_p)
df$imp_p <- as.factor(df$imp_p)
df$MAN_COLL <- as.factor(df$MAN_COLL)
df$INT_HWY <- as.factor(df$INT_HWY)
df$sch_bus <- as.factor(df$sch_bus)
df$daytime <- as.factor(df$daytime)
df$weekday <- as.factor(df$weekday)

##veh_invl: ordered
df$veh_invl <- as.ordered(df$veh_invl)
df$nonm_invl <- as.factor(df$nonm_invl)
df$onroadway <- as.factor(df$onroadway)
df$crit_event <- as.factor(df$crit_event)
df$single_lane <- as.factor(df$single_lane)
df$intersection <- as.factor(df$intersection)
df$str_align <- as.factor(df$str_align)
df$profile_level <- as.factor(df$profile_level)
df$sur_dry <- as.factor(df$sur_dry)
df$traf_control <- as.factor(df$traf_control)
df$light_cond <- as.factor(df$light_cond)
df$badweather <- as.factor(df$badweather)
df$dr_speeding <- as.factor(df$dr_speeding)
df$dr_distracted <- as.factor(df$dr_distracted)

##spd_lim: ordered
df$spd_lim <- as.ordered(df$spd_lim)
df$bus_mov <- as.factor(df$bus_mov)

## only going to deal with 21 variables as per paper
df_reduced <- df %>% dplyr::select(veh_invl, nonm_invl,sch_bus,intersection,
                            single_lane,spd_lim,sur_dry,profile_level,crit_event,
                            light_cond,traf_control,dr_distracted,bus_mov,str_align,
                            iad,sex,psv_p,lht_p,alc_p,distr_p,speed_p,imp_p,WEIGHT)
save(df_reduced, file = outfile)
