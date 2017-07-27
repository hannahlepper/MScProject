#Fitting

library(deSolve)
source("PSmodel.R")
source("Parameter_combinations.R")

#Community mixing and NTP case treatment scenarios:
basescenarios <- parametercombinations(list(r = c(10, 40, 70),
                                       k = c(40, 60, 80, 100)))
#target incidence: 112.34 p 100000 pa

#function for finding equilibrium point
#Suggest just find point when change in very small rather than actual equilibrium for now
#Need to compare last value to other values to find when they are (almost) the same
#Suggest 'same' = same to 10 decimal places
#Probably wouldn't work for other models!

#tells you time at which equilibrium was reached
equilibrium_inc <- function(data, last) {
  min(data[which(data[,2] == last),1])
}

sol_base_df <- ode(y=yinit,times=seq(0,600, by=0.02),func=PSmodel,parms=pars_base)
equilibrium_inc(sol_base_df[,c(1,9)], sol_base_df[dim(sol_base_df)[1],9])
sol_base_df[dim(sol_base_df)[1],9]

#600 y, ts = 0.02; Inc = 896.3172



