#Fitting

#====Needs====
library(deSolve)
library(rootSolve)
library(plyr)
library(readr)
library(ggplot2)
library(tidyr)
library(gridExtra)
source("C://Users/hanna/Documents/GitHub/MSc project/PSmodel.R")
source("C://Users/hanna/Documents/GitHub/MSc project/Parameter_combinations.R")
source("C://Users/hanna/Documents/GitHub/MSc project/matchclosestMALDIquant.R")

#====Community mixing par scenarios====
pars <- list(b=10,
             Mu=0.006,
             Mui=0.3,
             Mun=0.21,
             a=0.115,
             theta=0.015,
             p=0.01,
             c=0.22,
             x=0.65,
             vs=0.0005,
             vf=0.67,
             nc=0.2,
             sg=0.45,
             CDR=0.77,
             cov = 0, 
             k = 0,
             tau=0.91,
             r=c(0.1, .4, .7),
             Ic = 0.001,
             survey_interval=1)

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

#run model to steady state and output fitted parameters
modelrun <- function(parameters) {
  Init_inf <- 0.2 # Fraction of the pop initially infected
  init <- c(U=1-Init_inf,Ls=0.99*Init_inf,Lf=0,I=0.01*Init_inf,N=0,C=0)
  sol <- runsteady(y=init,times=c(0,Inf),func=PSmodel,parms=parameters)
  sol_df <- as.data.frame(sol)
  as.data.frame(cbind(parameters[c("b", "Mu", "Mui", "Mun", "nc", "CDR", "tau")], tail(sol_df[,c("Inc", "Prev", "Mort", "treatment_cov")], 1)))
}

modelrun(tofit[1,])

# #run model for beta and mu
# bmodelrun <- function(parameters) {
#   Init_inf <- 0.2 # Fraction of the pop initially infected
#   init <- c(U=1-Init_inf,Ls=0.99*Init_inf,Lf=0,I=0.01*Init_inf,N=0,C=0)
#   modelrun(parameters, init, "b")
# }
# mumodelrun <- function(parameters) {
#   Init_inf <- 0.2 # Fraction of the pop initially infected
#   init <- c(U=1-Init_inf,Ls=0.99*Init_inf,Lf=0,I=0.01*Init_inf,N=0,C=0)
#   modelrun(parameters, init, "Mu")
# }

  
#make parameter set
paramsetgen <- function(fittingparsetlist, pars) {
  parametercombinations(c(fittingparsetlist, 
                          pars[-which(names(pars)==names(fittingparsetlist))]))
}

test <- paramsetgen(list(b = c(1,2)),tofit[1,])

sets <- function(paramset){
  adply(paramset, 1, modelrun)
}

testset <- sets(test)

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
    fitpar-(round(fitpar, digits = 3) - fitpar), 
    round(fitpar, digits = 3), 
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
#   pars["Mui"] <- selectpar(pars, list(Mui = seq(0.11,0.2,length.out = 10)), 
#                            14.84, "Mort")
#   pars["Mun"] <- selectpar(pars, list(Mun = seq(0.12,0.2,length.out = 10)), 
#                            14.84, "Mort")
#   pars["tau"] <- selectpar(pars, list(tau = seq(0.6,0.95,length.out = 10)), 
#                           162.36, "Prev")
#   pars["CDR"] <- selectpar(pars, list(CDR = seq(0.6,0.99,length.out = 10)), 
#                                      0.79, "treatment_cov")
#   pars["nc"] <- selectpar(pars, list(nc = seq(0.1,0.3,length.out = 10)), 
#                           162.36, "Prev")
#   pars["b"] <- selectpar(pars, list(b = 3:16), 
#                          112.34, "Inc")
#   pars["Mu"] <- selectpar(pars, list(Mu = seq(0.003,0.008,length.out = 10)), 
#                           112.34, "Inc")
#   pars["Mui"] <- selectpar(pars, list(Mui = seq(0.11,0.2,length.out = 10)), 
#                            14.84, "Mort")
#   pars["Mun"] <- selectpar(pars, list(Mun = seq(0.12,0.2,length.out = 10)), 
#                            14.84, "Mort")
#   pars["tau"] <- selectpar(pars, list(tau = seq(0.6,0.95,length.out = 10)), 
#                            162.36, "Prev")
#   pars["CDR"] <- selectpar(pars, list(CDR = seq(0.6,0.99,length.out = 10)), 
#                            0.79, "treatment_cov")
#   pars["nc"] <- selectpar(pars, list(nc = seq(0.1,0.3,length.out = 10)), 
#                           162.36, "Prev")
#   pars["b"] <- selectpar(pars, list(b = 3:16), 
#                          112.34, "Inc")
#   pars["Mu"] <- selectpar(pars, list(Mu = seq(0.003,0.008,length.out = 10)), 
#                           112.34, "Inc")
# 
# r0.1 <- modelrun(pars)
# 
# #2. r = 0.4
#   pars <- tofit[2,]
#   
#   pars["b"] <- selectpar(pars, list(b = 3:16), 
#                          112.34, "Inc")
#   pars["Mu"] <- selectpar(pars, list(Mu = seq(0.003,0.01,length.out = 10)), 
#                           112.34, "Inc")
#   pars["Mui"] <- selectpar(pars, list(Mui = seq(0.11,0.2,length.out = 10)), 
#                            14.84, "Mort")
#   pars["CDR"] <- selectpar(pars, list(CDR = seq(0.6,0.99,length.out = 10)), 
#                            0.79, "treatment_cov")
#   pars["Mun"] <- selectpar(pars, list(Mun = seq(0.11,0.2,length.out = 10)), 
#                            14.84, "Mort")
#   pars["nc"] <- selectpar(pars, list(nc = seq(0.1,0.3,length.out = 10)), 
#                           162.36, "Prev")
#   pars["b"] <- selectpar(pars, list(b = 3:16), 
#                          112.34, "Inc")
#   pars["Mu"] <- selectpar(pars, list(Mu = seq(0.003,0.01,length.out = 10)), 
#                           112.34, "Inc")
#   pars["Mui"] <- selectpar(pars, list(Mui = seq(0.11,0.2,length.out = 10)), 
#                            14.84, "Mort")
#   pars["CDR"] <- selectpar(pars, list(CDR = seq(0.6,0.99,length.out = 10)), 
#                            0.79, "treatment_cov")
#   pars["Mun"] <- selectpar(pars, list(Mun = seq(0.11,0.2,length.out = 10)), 
#                            14.84, "Mort")
#   pars["nc"] <- selectpar(pars, list(nc = seq(0.1,0.3,length.out = 10)), 
#                           162.36, "Prev")
#   pars["b"] <- selectpar(pars, list(b = 3:16), 
#                          112.34, "Inc")
#   pars["Mu"] <- selectpar(pars, list(Mu = seq(0.003,0.01,length.out = 10)), 
#                           112.34, "Inc")
#   pars["Mui"] <- selectpar(pars, list(Mui = seq(0.11,0.2,length.out = 10)), 
#                            14.84, "Mort")
#   pars["CDR"] <- selectpar(pars, list(CDR = seq(0.6,0.99,length.out = 10)), 
#                            0.79, "treatment_cov")
#   pars["Mun"] <- selectpar(pars, list(Mun = seq(0.11,0.2,length.out = 10)), 
#                            14.84, "Mort")
#   pars["nc"] <- selectpar(pars, list(nc = seq(0.1,0.3,length.out = 10)), 
#                           162.36, "Prev")
#   pars["b"] <- selectpar(pars, list(b = 3:16), 
#                          112.34, "Inc")
#   pars["Mu"] <- selectpar(pars, list(Mu = seq(0.003,0.01,length.out = 10)), 
#                           112.34, "Inc")
# 
# r0.4 <- modelrun(pars)
#   
# 
# #2. r = 0.4
# pars <- tofit[3,]
# 
# pars["b"] <- selectpar(pars, list(b = 3:16), 
#                        112.34, "Inc")
# pars["Mu"] <- selectpar(pars, list(Mu = seq(0.003,0.01,length.out = 10)), 
#                         112.34, "Inc")
# pars["Mui"] <- selectpar(pars, list(Mui = seq(0.11,0.2,length.out = 10)), 
#                          14.84, "Mort")
# pars["Mun"] <- selectpar(pars, list(Mun = seq(0.11,0.2,length.out = 10)), 
#                          14.84, "Mort")
# pars["CDR"] <- selectpar(pars, list(CDR = seq(0.6,0.99,length.out = 10)), 
#                          0.79, "treatment_cov")
# pars["nc"] <- selectpar(pars, list(nc = seq(0.1,0.3,length.out = 10)), 
#                         162.36, "Prev")
# pars["tau"] <- selectpar(pars, list(tau = seq(0.6,0.99,length.out = 10)), 
#                          162.36, "Prev")
# pars["b"] <- selectpar(pars, list(b = 3:16), 
#                        112.34, "Inc")
# pars["Mu"] <- selectpar(pars, list(Mu = seq(0.003,0.01,length.out = 10)), 
#                         112.34, "Inc")
# pars["Mui"] <- selectpar(pars, list(Mui = seq(0.11,0.2,length.out = 10)), 
#                          14.84, "Mort")
# pars["Mun"] <- selectpar(pars, list(Mun = seq(0.11,0.2,length.out = 10)), 
#                          14.84, "Mort")
# pars["CDR"] <- selectpar(pars, list(CDR = seq(0.6,0.99,length.out = 10)), 
#                          0.79, "treatment_cov")
# pars["nc"] <- selectpar(pars, list(nc = seq(0.1,0.3,length.out = 10)), 
#                         162.36, "Prev")
# pars["tau"] <- selectpar(pars, list(tau = seq(0.6,0.99,length.out = 10)), 
#                          162.36, "Prev")
# 
# r0.7 <- modelrun(pars)

#Results====
fittedparams <-cbind(rbind(r0.1,r0.4,r0.7), r = c(0.1, 0.4, 0.7))
#write.csv(fittedparams, "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/fittedparms.csv")

#Visualisation


barplot(outcomevalues$outcomes)

outcomevalues <- gather(fittedparams,outcome,outfitval, 
                                    Inc, Prev, Mort, treatment_cov)[,-c(1:7)]
outcomevalues <- cbind(rbind(data.frame(outcome=rep(c("Inc", "Prev", "Mort", "treatment_cov"), each = 3), 
            outfitval = rep(c(112.34, 162.36, 14.84, 0.79), each = 3), 
            r = rep(c(0.1, 0.4, 0.7), times = 4)), 
      outcomevalues),
      type = rep(c("Target value", "Fitted value"), each = 12))


fitInc <- ggplot(
  rbind(subset(outcomevalues, outcome == "Inc"), 
        subset(outcomevalues, outcome=="Inctarg")),
  aes(outcome, outfitval)) +
  geom_bar(stat="identity") +
  facet_grid(.~r) +
  theme_bw()
fitPrev <- ggplot(
  rbind(subset(outcomevalues, outcome == "Prev"), 
        subset(outcomevalues, outcome=="Prevtarg")),
  aes(outcome, outfitval)) +
  geom_bar(stat="identity") +
  facet_grid(.~r)
fitMort <- ggplot(
  rbind(subset(outcomevalues, outcome == "Mort"), 
        subset(outcomevalues, outcome=="Morttarg")),
  aes(outcome, outfitval)) +
  geom_bar(stat="identity") +
  facet_grid(.~r)

fittreat_cov <- ggplot(
  rbind(subset(outcomevalues, outcome == "treatment_cov"), 
        subset(outcomevalues, outcome=="treatment_covtarg")),
  aes(outcome, outfitval)) +
  geom_bar(stat="identity") +
  facet_grid(.~r, margins = F)
grid.arrange(fitInc, fitMort, fitPrev, fittreat_cov, ncol = 1)

fits <- ggplot(outcomevalues, aes(type, outfitval)) +
  geom_bar(stat="identity") +
  facet_grid(outcome~r, scales = "free_y", drop = TRUE) +
  theme_bw()
