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
convertdftonumeric <- function(df){
  pars <- names(df)
  num <- as.numeric(df)
  setNames(num, pars)
}
fastrun <- function(pars) {
  y <- c(U=1-0.2,Ls=0.99*0.2,Lf=0,I=0.01*0.2,N=0,C=0)
  initrun<-runsteady(y=y,times=c(0,Inf), func=PSmodel, parms=convertdftonumeric(pars))
  yinit<-initrun$y
  sol_base<-ode(y=yinit,times=seq(0,500, by=0.02),func=PSmodel,parms=convertdftonumeric(pars))
  sol_base_df <- as.data.frame(sol_base)
  print(equilibrium_test(sol_base_df))
  return(sol_base_df)
}
tosteadyrun <- function(pars) {
 y <- c(U=1-0.2,Ls=0.99*0.2,Lf=0,I=0.01*0.2,N=0,C=0)
 initrun <- runsteady(y=y,times=c(0,Inf), func=PSmodel, parms=convertdftonumeric(pars))
 y <- initrun$y
 y <- setNames(c(y["U"] + .1*y["I"] + .1*y["N"],
                 y["Ls"], y["Lf"],
                 .9*y["I"],.9*y["N"],
                 y["C"]), 
               names(y))
 data.frame(time = attr(runsteady(y=y, times=c(0,Inf), func=PSmodel, parms=convertdftonumeric(pars)), "time"),
            r = pars["r"])
}

#function that I need -returns time taken to do function and returns function output:
outputplustime <- function(func, ...){
  start <- print(Sys.time())
  output <- func(...)
  print(Sys.time() - start)
  return(output)
}
baseparfixer <- function(fitpars) {
  pars <- data.frame(b=10,Mu=0.006,Mui=0.3,Mun=0.21,a=0.115,theta=0.015,p=0.01,
               c=0.22,x=0.65,vs=0.0005,vf=0.67,nc=0.2,sg=0.45,CDR=0.77,sens=0,
               cov = 0,k = 0,tau=0.91,r=.4,Ic = 0.001,survey_interval=1)
  newfitpars <- fitpars[which(names(fitpars)%in%names(pars))]
  newpars <- pars[which(!names(pars)%in%names(fitpars))]
  setNames(
    as.numeric(c(newfitpars, newpars)), 
    c(names(newfitpars), names(newpars)))
}


#get fitted param data====
#get fitting data
fittedparams <- read.csv("C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/fittedparams.csv", header = TRUE)
fittedparams <- adply(fittedparams, 1, baseparfixer, .expand = F, .id = NULL)

#generate data====
# baselinedata <- outputplustime(adply, fittedparams, 1, fastrun)
# #1.7 minutes
# write.csv(baselinedata, "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/baselinedata.csv")
# 
# #moar data?!How many time points to steady state====
# tosteadydata <- outputplustime(adply, fittedparams, 1, tosteadyrun)
# write.csv(tosteadydata,
#           "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/tosteadydata.csv")
