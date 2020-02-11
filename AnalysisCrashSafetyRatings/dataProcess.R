###########################################
## This file does some final data cleaning 
## before analysis of the crash safety ratings
##
## The file "gesCrashRatingData.rds" contains the
## records of all crashes from GES (2002-2015) with safety ratings.
## Each row of the data "injDat" is a vehicle record including variable information
## from the GES data along with the NHTSA and IIHS safety ratings
## if Available.
##
## Previous processing of this data was completed in SAS and R
## and is available upon request. API access to the IIHS crash safety ratings
## must be obtained from IIHS. NHTSA data is all publically available.

library(dplyr)

load("gesCrashRatingData.rds")

## Select variables of interest and convert variable types when necessary
full.data <- injDat %>% dplyr::select(MAKE_MOD, MAX_VSEV, NUM_INJV, MAKE, MODEL, MODEL_YR,
                                      P_CRASH2, DEFORMED, VEH_ALCH, NUMOCCS=NUMOCCS.x, YEAR,
                                      INT_HWY, Make.y, Model.y, NHTSA, IIHS, isSpeed,
                                      NUMSEVINJ)
full.data$NHTSA <- as.factor(full.data$NHTSA)
full.data$IIHS <- as.factor(full.data$IIHS)
levels(full.data$NHTSA) <- c("1-Star", "2-Star", "3-Star", "4-Star", "5-Star")
levels(full.data$IIHS) <- c("Poor", "Marginal", "Acceptable", "Good")


#######
## acc.data is the dat we will use for modeling purposes
acc.data <- full.data %>% dplyr::select(NUM_INJV, NUMOCCS, NHTSA, IIHS, VEH_ALCH, isSpeed, YEAR, INT_HWY)
acc.data <- acc.data %>% dplyr::mutate(prop=NUM_INJV/NUMOCCS)
acc.data <- acc.data %>% dplyr::mutate(logProp = log10(prop+1))

## Here is acc.data but for severe injuries and deaths only
acc.sev.data <- full.data %>% dplyr::select(NUM_INJV, NUMSEVINJ, NUMOCCS, NHTSA, IIHS, VEH_ALCH, isSpeed, YEAR, INT_HWY)
acc.sev.data <- acc.sev.data %>% dplyr::mutate(prop.inj=NUM_INJV/NUMOCCS,
                                       prop.sev=NUMSEVINJ/NUMOCCS)
acc.sev.data <- acc.sev.data %>% dplyr::mutate(logPropInj = log10(prop.inj+1),
                                       logPropSev = log10(prop.sev+1))


## Remove all the NA terms
acc.data.trim <- na.omit(acc.data)
acc.data.trim$NUMOCCS = ifelse(acc.data.trim$NUMOCCS==99|acc.data.trim$NUMOCCS==999,NA, acc.data.trim$NUMOCCS)
acc.data.trim$INT_HWY = ifelse(acc.data.trim$INT_HWY==9, NA, acc.data.trim$INT_HWY)
acc.data.trim <- na.omit(acc.data.trim)

## Remove all the NA terms
acc.sev.data.trim <- na.omit(acc.sev.data)
acc.sev.data.trim$NUMOCCS = ifelse(acc.sev.data.trim$NUMOCCS==99|acc.sev.data.trim$NUMOCCS==999,NA, acc.sev.data.trim$NUMOCCS)
acc.sev.data.trim$INT_HWY = ifelse(acc.sev.data.trim$INT_HWY==9, NA, acc.sev.data.trim$INT_HWY)
acc.sev.data.trim <- na.omit(acc.sev.data.trim)


###
## Just checking what is going on
# sum(is.na(acc.data.trim$IIHS))
# sum(is.na(acc.data.trim$NHTSA))
# dim(acc.data.trim)
# dim(acc.sev.data.trim)

## Some final variable cleaning
acc.data.trim$OCCCAT <- acc.data.trim$NUMOCCS
acc.data.trim <- acc.data.trim %>% mutate(OCCCAT=ifelse(NUMOCCS>2, "3+", NUMOCCS))
acc.data.trim$INJCAT <- acc.data.trim$NUM_INJV
acc.data.trim <- acc.data.trim %>% mutate(INJCAT=ifelse(NUM_INJV>2, "3+", NUM_INJV))
acc.data.trim$alchSpeed <- ifelse(acc.data.trim$VEH_ALCH & acc.data.trim$isSpeed, "Both",
                                  ifelse(acc.data.trim$VEH_ALCH, "Alcohol", 
                                         ifelse(acc.data.trim$isSpeed, "Speeding", "Neither")))
acc.data.trim$alchSpeed <- factor(acc.data.trim$alchSpeed, levels=c("Neither", "Speeding", "Alcohol", "Both"))
acc.data.trim$highway <- ifelse(acc.data.trim$INT_HWY==1, "Interstate", "Roadway")
acc.data.trim$highway <- factor(acc.data.trim$highway, levels=c("Roadway", "Interstate"))


acc.sev.data.trim$OCCCAT <- acc.sev.data.trim$NUMOCCS
acc.sev.data.trim <- acc.sev.data.trim %>% mutate(OCCCAT=ifelse(NUMOCCS>2, "3+", NUMOCCS))
acc.sev.data.trim$INJCAT <- acc.sev.data.trim$NUMSEVINJ
acc.sev.data.trim <- acc.sev.data.trim %>% mutate(INJCAT=ifelse(NUMSEVINJ>1, "2+", NUMSEVINJ))
acc.sev.data.trim$alchSpeed <- ifelse(acc.sev.data.trim$VEH_ALCH & acc.sev.data.trim$isSpeed, "Both",
                                  ifelse(acc.sev.data.trim$VEH_ALCH, "Alcohol", 
                                         ifelse(acc.sev.data.trim$isSpeed, "Speeding", "Neither")))
acc.sev.data.trim$alchSpeed <- factor(acc.sev.data.trim$alchSpeed, levels=c("Neither", "Speeding", "Alcohol", "Both"))
acc.sev.data.trim$highway <- ifelse(acc.sev.data.trim$INT_HWY==1, "Interstate", "Roadway")
acc.sev.data.trim$highway <- factor(acc.sev.data.trim$highway, levels=c("Roadway", "Interstate"))
acc.sev.data.trim <- acc.sev.data.trim %>%
  mutate(no.injured=NUMOCCS-NUM_INJV,
         mod.injured=NUM_INJV-NUMSEVINJ)

