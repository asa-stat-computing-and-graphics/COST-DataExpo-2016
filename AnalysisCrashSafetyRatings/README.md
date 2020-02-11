# crash_safety
JSM 2016 GSS Data Challenge -- Analyzing Crash-Safety ratings as a predictor for injury

Supplemental source code to the special edition of Computational Statistics article: An analysis of crash-safety ratings and the true assessment of injuries by vehicle. This code allows replication of all modeling, tables and plots.

makeModelCodes.csv -- A CSV file mapping vehicle makes and models to their GES codes

gesCrashRatingData.rds -- The merged GES data with crash-safety ratings. 

dataProcess.R -- additional cleaning of the above data.

fitBetaBinomFreq.R -- perform the Beta Binominal modeling for injuries per car.

fitBetaBinomSevereInjuryFreq.R -- performs the Beta Binomial modeling for severe injuries & death.

modelPlots.R -- Generates plots in the paper.

