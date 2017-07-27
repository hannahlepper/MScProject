#Fitting

#====Needs====
library(deSolve)
library(rlist)
library(plyr)
source("PSmodel.R")
source("Parameter_combinations.R")
source("matchclosestMALDIquant.R")

#====Community mixing and NTP case treatment scenarios:====
basescenarios <- parametercombinations(list(r = c(10, 40, 70),
                                       k = c(40, 60, 80, 100)))
pars_base <- c(b=22,  p=0.01, a=0.11, vf=0.67, 
               vs=0.0005, sg=0.45, x=0.65, nc=0.2, 
               theta=0.015,  Mu=0.06, Mui=0.3, Mun=0.21,
               CDR=0.7, CDR_survey=CDR_int(CDR = 0.7, cov = 0, sens = 0),
               tau=0.91, k = 0.79, r=0.1, c=0.22, Ic = 0.002, 
               survey_interval=5)
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

#Try to get as close to equilibrium as possible
final <- function (data, cat) {
  as.numeric(tail(data[cat], 1))
}
newyinit <- function(data) {
  c(U=final(data, "U"), 
    Ls=final(data, "Ls") + 0.005, 
    Lf=final(data, "Lf") - 0.005, 
    I=final(data, "I"), 
    N=final(data, "N"), 
    C=final(data, "C"))
}
yinit <-newyinit(sol_base_df)

#====fitting functions====
#1. make series of output incidences with input parameters
modelrun <- function(parameters, init, changeparam) {
  sol <- ode(y=init,times=seq(0,1, by=0.02),func=PSmodel,parms=parameters)
  sol_df <- as.data.frame(sol)
#  if (equilibrium_test(sol_df)){
    as.data.frame(cbind(parameters[changeparam], tail(sol_df$Inc, 1)))
#  } else {print("Eqlm not reached")}
}

modelrun(pars_base, yinit, "b")

betamodelrun <- function(parameters) {
  Init_inf <- 0.2 # Fraction of the pop initially infected
  init <- c(U=1-Init_inf,Ls=0.99*Init_inf,Lf=0,I=0.01*Init_inf,N=0,C=0)
  modelrun(parameters, init, "b")
}
mumodelrun <- function(parameters) {
  Init_inf <- 0.2 # Fraction of the pop initially infected
  init <- c(U=1-Init_inf,Ls=0.99*Init_inf,Lf=0,I=0.01*Init_inf,N=0,C=0)
  modelrun(parameters, init, "mu")
}

other_pars <- list(p=0.01, a=0.11, vf=0.67, vs=0.0005, 
               sg=0.45, x=0.65, nc=0.2, theta=0.015,  
               Mu=0.06, Mui=0.3, Mun=0.21, CDR=0.7, 
               CDR_survey=CDR_int(CDR = 0.7, cov = 0, sens = 0),
               tau=0.91, k = .4, r=0.1,c=0.22, Ic = 0.002, 
               survey_interval=5)

paramsetgenbeta <- function(fittingpars, otherpars) {
  cbind(parametercombinations(list.append(otherpars, b = fittingpars)), 
        run = as.factor(1:length(fittingpars)))
}
paramsetgenmu <- function(fittingpars, otherpars) {
  cbind(parametercombinations(list.append(otherpars, mu = fittingpars)),
        run = as.factor(1:length(fittingpars)))
}

paramset <- paramsetgenbeta(seq(10,20,1), pars_fit_test)

sets <- function(paramset, type){
  if(type == "b") {ddply(paramset, "run", each(betamodelrun))}
  else if (type == "Mu") {ddply(paramset, "run", each(mumodelrun))}
}

set <- sets(parameterset, "b")

#select beta giving rise to the closest Inc to target
selectbeta <- function(initfittingpars, other_pars){
  par_set <- paramsetgenbeta(initfittingpars, other_pars)
  set <- sets(par_set, "b")
  rownum <- match.closest(112, set[,3])
  suggestedb <- set[rownum, "b"]
  if(set[rownum,3] > 115 | set[rownum,3] < 110){
    newpars <- paramsetgenbeta(seq(suggestedb-0.5, suggestedb+0.5, 0.1), other_pars)
    set <- sets(newpars,"b")
    rownum <- match.closest(112, set[,3])
    suggestedb <- set[rownum, "b"]
  }
  return(c(suggestedb, set))
}

selectbeta(c(10:15), other_pars)
