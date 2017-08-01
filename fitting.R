#Fitting

#====Needs====
library(deSolve)
library(rootSolve)
library(plyr)
library(readr)
setwd("C://Users/hanna/Documents/GitHub/MSc project/")
source("PSmodel.R")
source("Parameter_combinations.R")
source("matchclosestMALDIquant.R")
setwd("C://Users/hanna/Documents/")

#====Community mixing and NTP case treatment scenarios:====
pars <- list(p=0.01, a=0.115, vf=0.67, 
               vs=0.0005, sg=0.45, x=0.65, nc=0.2, 
               theta=0.015,  Mui=0.3, Mun=0.21,
               CDR=0.77, CDR_survey=CDR_int(CDR = 0.77, cov = 0, sens = 0),
               tau=0.91, c=0.22, Ic = 0.002, 
               survey_interval=5,
              r = c(.10, .40, .70),
              k = c(.40, .60, .80, 1))

tofit <- parametercombinations(pars)
#target incidence: 112.34 p 100000 pa

#====equlibrium functions====
#tells you time at which equilibrium was reached
equilibrium_inc <- function(data) {
  min(data[which(data[,"Inc"] == as.numeric(tail(data,1)["Inc"])),"time"])
}

#tests if at equilibrium
equilibrium_test <- function(data) {
  eqlm <- min(data[which(data[,"Inc"] == as.numeric(tail(data,1)["Inc"])),"time"])
  if(eqlm == as.numeric(tail(data,1)["time"]) | eqlm == as.numeric(tail(data,1)["Inc"])+1) 
    {flag <- FALSE}
  else {flag <- TRUE}
  return(flag)
}

#====fitting functions====
#1. make series of output incidences with input parameters
# modelrun <- function(parameters, init, changeparam) {
#   sol <- ode(y=init,times=seq(0,1, by=0.02),func=PSmodel,parms=parameters)
#   sol_df <- as.data.frame(sol)
#   if (equilibrium_test(sol_df)){
#     as.data.frame(cbind(parameters[changeparam], Inc = tail(sol_df$Inc, 1)))
#   } else {print("Eqlm not reached")}
# }

#run model to steady state
modelrun <- function(parameters, init, changeparam) {
  sol <- runsteady(y=init,times=c(0,Inf),func=PSmodel,parms=parameters)
  sol_df <- as.data.frame(sol)
  as.data.frame(cbind(parameters[changeparam], Inc = tail(sol_df$Inc, 1)))
}

#run model for beta and mu
bmodelrun <- function(parameters) {
  Init_inf <- 0.2 # Fraction of the pop initially infected
  init <- c(U=1-Init_inf,Ls=0.99*Init_inf,Lf=0,I=0.01*Init_inf,N=0,C=0)
  modelrun(parameters, init, "b")
}
mumodelrun <- function(parameters) {
  Init_inf <- 0.2 # Fraction of the pop initially infected
  init <- c(U=1-Init_inf,Ls=0.99*Init_inf,Lf=0,I=0.01*Init_inf,N=0,C=0)
  modelrun(parameters, init, "Mu")
}

#make parameter set for beta and mu
paramsetgenbeta <- function(fittingpars, otherpars) {
  parametercombinations(c(otherpars, b = list(c(fittingpars))))
}
paramsetgenmu <- function(fittingpars, otherpars) {
  parametercombinations(c(otherpars, Mu = list(c(fittingpars))))
}

sets <- function(paramset, type){
  if (type == "b") {ddply(paramset, as.factor("b"), bmodelrun)}
  else if (type == "Mu") {ddply(paramset, as.factor("Mu"), mumodelrun)}
}

#select beta giving rise to the closest Inc to target
selectbeta <- function(initfittingpars, other_pars){
  #1
  set <- sets(paramsetgenbeta(initfittingpars, other_pars), "b")
  rownum <- match.closest(112.34, set[,"Inc"])
  b <- set[rownum, "b"]
  
  #2
  set <- sets(paramsetgenbeta(seq(b-0.5, b+0.5, 0.1), other_pars), "b")
  rownum <- match.closest(112.34, set[,"Inc"])
  b <- set[rownum, "b"]
  
  return(c(beta = b, Inc = set[rownum, "Inc"]))
}

#select Mu giving rise to the closest Inc to target
selectmu <- function(initfittingpars, other_pars){
  #1
  set <- sets(paramsetgenmu(initfittingpars, other_pars), "Mu")
  set <- set[order(set[,"Inc"], decreasing = F),]
  rownames(set) <- NULL
  row <- set[match.closest(112.34, set[, "Inc"]),]
  Mu <- row[, "Mu"]
  
  #2
  set <- sets(paramsetgenmu(seq(Mu-0.005, Mu+0.005, 0.001), other_pars), "Mu")
  set <- set[order(set[,"Inc"], decreasing = F),]
  rownames(set) <- NULL
  row <- set[match.closest(112.34, set[, "Inc"]),]
  Mu <- row[, "Mu"]
  
  return(c(Mu = Mu, Inc = row[, "Inc"]))
}

#combine functions
selectpars <- function(pars) {
  b <- selectbeta(3:16, c(pars, Mu = 0.06))
  Mu <- selectmu(seq(0.03,0.08,length.out = 10), c(pars, b = unname(b[1])))
  df<- as.data.frame(cbind(beta = b[1], Mu = Mu[1], Inc = Mu[2]))
  rownames(df) <-NULL
  return(df)
}

# start <- Sys.time()
# lowrlowk <- selectpars(other_pars_lessblessMu)
# Sys.time() - start
#25 seconds

# start <- Sys.time()
# fittedparams <- adply(tofit, 1, selectpars) #runs across all 12
# Sys.time() - start
# #4.3 minutes
# #2 mins on Mike's
# write.csv(fittedparams, "fittedparms.csv")



