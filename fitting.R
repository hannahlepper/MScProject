#Fitting

#====Needs====
library(deSolve)
library(rootSolve)
library(plyr)
library(readr)
source("C://Users/hanna/Documents/GitHub/MSc project/PSmodel.R")
source("C://Users/hanna/Documents/GitHub/MSc project/Parameter_combinations.R")
source("C://Users/hanna/Documents/GitHub/MSc project/matchclosestMALDIquant.R")

#====Community mixing par scenarios====
#pars <- list(b=10,
             # Mu=0.006,
             # Mui=0.3,
             # Mun=0.21,
             # a=0.115,
             # theta=0.015,
             # p=0.01,
             # c=0.22,
             # x=0.65,
             # vs=0.0005,
             # vf=0.67,
             # nc=0.2,
             # sg=0.45,
             # CDR=0.77,
             # cov = 0, 
             # k = 0,
             # tau=0.91,
             # r=c(0.1, .4, .7),
             # Ic = 0.001,
             # survey_interval=1)

#tofit <- parametercombinations(pars)
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

#run model to steady state and output fitted parameters
modelrun <- function(parameters) {
  Init_inf <- 0.2 # Fraction of the pop initially infected
  init <- c(U=1-Init_inf,Ls=0.99*Init_inf,Lf=0,I=0.01*Init_inf,N=0,C=0)
  sol <- runsteady(y=init,times=c(0,Inf),func=PSmodel,parms=parameters)
  sol_df <- as.data.frame(sol)
  as.data.frame(cbind(parameters[c("b", "Mu", "Mui", "Mun", "nc", "CDR")], tail(sol_df[,c("Inc", "Prev", "Mort", "treatment_cov")], 1)))
}
  
#make parameter set
paramsetgen <- function(fittingparsetlist, pars) {
  parametercombinations(c(fittingparsetlist, 
                          pars[-which(names(pars)==names(fittingparsetlist))]))
}
sets <- function(paramset){
  adply(paramset, 1, modelrun)
}

#select a param giving rise to the closest output to target
findclosest <- function(set, f.output, f.outputname){
  set <- set[order(set[,f.outputname], decreasing = F),] #match.closest requires ascending
  rownames(set) <- NULL
  set[match.closest(f.output, set[, f.outputname]),]
}
  
selectpar <- function(pars, fittingparlist, f.output, f.outputname){
  #1
  set <- sets(paramsetgen(fittingparlist, pars))
  closest <- findclosest(set, f.output, f.outputname)
  
  #2
  fitpar <- closest[,which(names(closest) == names(fittingparlist))]
  newfittingparlist <- list(seq(
    fitpar-(fitpar*0.1), fitpar + (fitpar*0.1), 
    length.out = 10))
  names(newfittingparlist) <- names(fittingparlist)
  set <- sets(paramsetgen(newfittingparlist, pars))
  closest <- findclosest(set, f.output, f.outputname)
  as.numeric(closest[which(names(closest)==names(fittingparlist))])
}

#Manual fitting====
#1. r = 0.1
# pars <- tofit[1,]
#  
# pars["CDR"] <- selectpar(pars, list(CDR = seq(0.62,0.9,length.out = 10)),
#                        0.79, "treatment_cov")
# pars["b"] <- selectpar(pars, list(b = 8:22),
#                        112.34, "Inc")
# pars["Mu"] <- selectpar(pars, list(Mu = seq(0.003,0.008,length.out = 10)),
#                         112.34, "Inc")
# pars["nc"] <- selectpar(pars, list(nc = seq(0.1,0.3,length.out = 10)),
#                         162.36, "Prev")
# pars["Mui"] <- selectpar(pars, list(Mui = seq(0.11,0.2,length.out = 10)),
#                          14.84, "Mort")
# pars["Mun"] <- selectpar(pars, list(Mun = seq(0.12,0.2,length.out = 10)),
#                          14.84, "Mort")
# pars["CDR"] <- selectpar(pars, list(CDR = seq(0.62,0.9,length.out = 10)),
#                          0.79, "treatment_cov")
# pars["b"] <- selectpar(pars, list(b = 8:22),
#                        112.34, "Inc")
# pars["Mu"] <- selectpar(pars, list(Mu = seq(0.003,0.008,length.out = 10)),
#                         112.34, "Inc")
# pars["nc"] <- selectpar(pars, list(nc = seq(0.1,0.3,length.out = 10)),
#                         162.36, "Prev")
# pars["Mui"] <- selectpar(pars, list(Mui = seq(0.11,0.2,length.out = 10)),
#                          14.84, "Mort")
# pars["Mun"] <- selectpar(pars, list(Mun = seq(0.12,0.2,length.out = 10)),
#                          14.84, "Mort")
# pars["CDR"] <- selectpar(pars, list(CDR = seq(0.62,0.9,length.out = 10)),
#                          0.79, "treatment_cov")
# pars["b"] <- selectpar(pars, list(b = 8:22),
#                        112.34, "Inc")
# pars["Mu"] <- selectpar(pars, list(Mu = seq(0.003,0.008,length.out = 10)),
#                         112.34, "Inc")
# pars["nc"] <- selectpar(pars, list(nc = seq(0.1,0.3,length.out = 10)),
#                         162.36, "Prev")
# pars["Mui"] <- selectpar(pars, list(Mui = seq(0.11,0.2,length.out = 10)),
#                          14.84, "Mort")
# pars["Mun"] <- selectpar(pars, list(Mun = seq(0.12,0.2,length.out = 10)),
#                          14.84, "Mort")
# 
# r0.1 <- modelrun(pars) #check...
# 
# # 
# # #2. r = 0.4
# pars <- tofit[2,]
# 
# for (i in names(r0.1)) {
#  pars[which(names(pars)==i)] <- r0.1[which(names(r0.1) == i)]
# }
# 
# pars["b"] <- selectpar(pars, list(b = 6.5:22),
#                       125.34, "Inc")
# r0.4  <- modelrun(pars)  
# #   
# # 
# #2. r = 0.4
# pars <- tofit[3,]
# for (i in names(r0.1)) {
#  pars[which(names(pars)==i)] <- r0.1[which(names(r0.1) == i)]
# }
# pars["b"] <- selectpar(pars, list(b = 6.5:22),
#                       125.34, "Inc")
# r0.7 <- modelrun(pars)
# 
# #Results====
# fittedparams <-cbind(rbind(r0.1,r0.4,r0.7), r = c(0.1, 0.4, 0.7))
# write.csv(fittedparams, 
#          "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/fittedparams.csv")
