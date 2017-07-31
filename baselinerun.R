# Run baseline models

#====Needs====
library(deSolve)
library(rootSolve)
library(rlist)
library(plyr)
library(readr)
setwd("C://Users/hanna/Documents/GitHub/MSc project/")
source("PSmodel.R")
source("Parameter_combinations.R")
source("matchclosestMALDIquant.R")
source("fitting.R")

read.csv("fitting.csv", header = TRUE)

pars <- c(other_pars_lessblessMu, 
          Mu = as.numeric(lowrlowk["Mu"]), 
          b = as.numeric(lowrlowk["beta"]))

other_pars_lessblessMu <- c(p=0.01, a=0.11, vf=0.67, vs=0.0005, 
                            sg=0.45, x=0.65, nc=0.2, theta=0.015,  
                            Mui=0.3, Mun=0.21, CDR=0.7, 
                            CDR_survey=CDR_int(CDR = 0.7, cov = 0, sens = 0),
                            tau=0.91, k = .4, r=0.1,c=0.22, Ic = 0.002, 
                            survey_interval=5)

#Try to get as close to equilibrium as possible - may or may not use

fastrun <- function(y, pars) {
  initrun<-runsteady(y=y,times=c(0,Inf), func=PSmodel, parms=pars)
  yinit<-initrun$y
  sol_base<-ode(y=yinit,times=seq(0,500, by=0.02),func=PSmodel,parms=pars)
  sol_base_df <- as.data.frame(sol_base)
  print(equilibrium_test(sol_base_df))
  return(sol_base_df)
}

test <- fastrun(yinit, pars)