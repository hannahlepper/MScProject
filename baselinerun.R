# Run baseline models

#====Needs====
library(deSolve)
library(rootSolve)
library(plyr)
library(readr)
source("C://Users/hanna/Documents/GitHub/MSc project/PSmodel.R")
source("C://Users/hanna/Documents/GitHub/MSc project/Parameter_combinations.R")
source("C://Users/hanna/Documents/GitHub/MSc project/fitting.R")

#Functions====
#Try to get as close to equilibrium as possible - may or may not use
convertdftonumeric <- function(df){
  pars <- c("p", "a", "vf", "vs", "sg", "x", "nc", "theta", 
            "Mui", "Mun", "CDR", "CDR_survey", "tau", "k",
            "r", "c", "Ic", "survey_interval", "b", "Mu")
  num <- as.numeric(df)
  setNames(num, pars)
}
fastrun <- function(pars) {
  y <- c(U=1-0.2,Ls=0.99*0.2,Lf=0,I=0.01*0.2,N=0,C=0)
  initrun<-runsteady(y=y,times=c(0,Inf), func=PSmodel, parms=pars)
  yinit<-initrun$y
  sol_base<-ode(y=yinit,times=seq(0,500, by=0.02),func=PSmodel,parms=convertdftonumeric(pars))
  sol_base_df <- as.data.frame(sol_base)
  print(equilibrium_test(sol_base_df))
  return(sol_base_df)
}
#get fitting data
#function that I need -returns time taken to do function and returns function output:
outputplustime <- function(func, ...){
  start <- Sys.time()
  output <- func(...)
  print(Sys.time() - start)
  return(output)
}

#get fitted param data====
#get fitting data
fittedparams <- read.csv("C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/fittedparms.csv", header = TRUE)
fittedparams <- rename(fittedparams, c("beta" = "b"))
fittedparams <- fittedparams[,-22]

#generate data====

#baselinedata <- ouputplustime(adply, fittedparams, 1, fastrun)
#1.7 minutes
#write.csv(baselinedata, ""C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/baselinedata.csv")

