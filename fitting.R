#Fitting

library(deSolve)
source("PSmodel.R")
source("Parameter_combinations.R")

#Community mixing and NTP case treatment scenarios:
basepars <- parametercombinations(list(r = c(10, 40, 70),
                                       k = c(40, 60, 80, 100)))
#target incidence: 112.34 p 100000 pa