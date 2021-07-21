# Analysis of Crash Safety Ratings

This repository contains supplementary materials for the paper by Philips et al. (2021) to enable the reader to reproduce the numerical analysis and the graphical results presented in that paper.

The full reference is: Cody R. Philips, Robert C. Garrett, Alan J. Tatro & Thomas J. Fisher (2021) "An analysis of crash-safety ratings and the true assessment of injuries by vehicle", Computational Statistics 36(3): 1639–1660.

The final publication is available at the Springer web site via https://doi.org/10.1007/s00180-021-01072-9. © Springer-Verlag GmbH Germany, part of Springer Nature, 2021.

## Outline of repository

JSM 2016 GSS Data Challenge -- Analyzing Crash-Safety ratings as a predictor for injury

Supplemental source code to the special edition of Computational Statistics article: An analysis of crash-safety ratings and the true assessment of injuries by vehicle. This code allows replication of all modeling, tables and plots.

makeModelCodes.csv -- A CSV file mapping vehicle makes and models to their GES codes

gesCrashRatingData.rds -- The merged GES data with crash-safety ratings. 

dataProcess.R -- additional cleaning of the above data.

fitBetaBinomFreq.R -- perform the Beta Binominal modeling for injuries per car.

fitBetaBinomSevereInjuryFreq.R -- performs the Beta Binomial modeling for severe injuries & death.

modelPlots.R -- Generates plots in the paper.

