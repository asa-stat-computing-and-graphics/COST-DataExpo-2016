*Final model;

* Please change the path to the folder that contains the datasets ;
* training.sas7bdat and validation.sas7bdat ;

libname JSMlib "C:\";    
ods pdf file = "C:\SASOutput.pdf";

proc surveylogistic data= JSMlib.Training;
class  IMP_ALCHL_IM(ref="2") IMP_SEX_IM(ref="1") IMP_VSURCOND(ref="1")
	   REGION(ref="1") IMP_LAND_USE(ref="1") overspeed(ref="0")/ param = ref;
weight WEIGHT;
cluster PSU;
strata PSUSTRAT;
model  SEV(ref="PdoCrash") =  IMP_TRAV_SP  IMP_VE_TOTAL  IMP_HOUR_IM IMP_VSPD_LIM 
			  IMP_SEX_IM  IMP_VSURCOND  REGION  IMP_SAFETY_RATING  IMP_LAND_USE  IMP_driver_age 
			  overspeed IMP_ALCHL_IM/link=glogit covb df=infinity;*drop age since we have driver age;
store model2p;
output out=model2 predprobs=CROSSVALIDATE;
run;
ods pdf close;


proc plm restore=model2p;
score data=jsmlib.validation out=scored_model2 predicted/ ilink;
run;

*******************************check fitting result***************************;
data jsmlib.sort_scored_2;
set scored_model2;
run;
proc sort data=jsmlib.sort_scored_2; by _level_;
run;

proc surveymeans data=jsmlib.sort_scored_2 mean median std;
weight WEIGHT;
cluster PSU;
strata PSUSTRAT;
by _level_;
var predicted;
run;

proc surveyfreq data=jsmlib.training;
title "training data";
weight WEIGHT;
cluster PSU;
strata PSUSTRAT;
table sev;
run;

proc surveyfreq data=jsmlib.validation;
title "validation data";
weight WEIGHT;
cluster PSU;
strata PSUSTRAT;
table SEV;
run;
ods pdf close;
