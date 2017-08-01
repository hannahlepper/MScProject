# Run baseline models

#====Needs====
library(deSolve)
library(rootSolve)
library(plyr)
library(readr)
setwd("C://Users/hanna/Documents/GitHub/MSc project/")
source("PSmodel.R")
source("Parameter_combinations.R")
source("matchclosestMALDIquant.R")
source("fitting.R")

read.csv("fitting.csv", header = TRUE)

fittedparams <- rename(fittedparams, c("beta" = "b"))
fittedparams <- fittedparams[,-22]

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

initrun<-runsteady(y=y,times=c(0,Inf), func=PSmodel, parms=convertdftonumeric(fittedparams[1,]))

start <- Sys.time()
test <- fastrun(fittedparams[1,])
Sys.time() - start
#about 8 seconds

start <- Sys.time()
baselinedata <- adply(fittedparams, 1, fastrun)
Sys.time() - start
#1.7 minutes
write.csv(baselinedata, "baselinedata.csv")

