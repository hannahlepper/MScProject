#Run experiments

##Needs re-runnning with new cov.!!!##


#Needs====
library(plyr)
library(deSolve)
library(rootSolve)
library(readr)
source("C://Users/hanna/Documents/GitHub/MSc project/baselinerun.R")


#prepareparset====
intandbasepars <- parametercombinations(list(r=c(.10,.40,.70),
                                             k=c(.40,.60,.80,1.00),
                                             cov=c(.09,.34, .60, .90),
                                             survey_interval=c(1,3,5)))
intandbasepars[,3] <- CDR_int(.77, intandbasepars[,3], .978)
fullparset <- data.frame(rep(NA, 144))

pars_nonexperiment <- c("p", "a", "vf", "vs", "sg", "x", "nc", "theta", 
          "Mui", "Mun", "CDR", "tau", "c", "Ic", "b", "Mu")
for (i in 1:length(pars_nonexperiment)) {
  fullparset[,i] <- rep(length.out=144, 
                        fittedparams[, which(colnames(fittedparams)==pars_nonexperiment[i])])
}

fullparset <- cbind(fullparset, rep(0.978, 144), intandbasepars)

fullparset <- setNames(fullparset, c(pars_nonexperiment, "sens", names(intandbasepars)))

#run====
#test 

fastrun_exp <- function(pars) {
  y <- c(U=1-0.2,Ls=0.99*0.2,Lf=0,I=0.01*0.2,N=0,C=0)
  pars <- convertdftonumeric_exp(pars)
  pars["cov"]<- CDR_int(pars["CDR"], pars["cov"], pars["sens"])
  pars <- rename(pars, c("cov" = "CDR_survey"))
  initrun<-runsteady(y=y,times=c(0,Inf), func=PSmodel, parms=pars)
  yinit<-initrun$y
  sol_base<-ode(y=yinit,times=seq(0,600, by=0.02),func=PSmodel,parms=pars)
  sol_base_df <- as.data.frame(sol_base)
  return(sol_base_df)
}

convertdftonumeric_exp <- function(df){
  pars <- names(fullparset)
  num <- as.numeric(df)
  setNames(num, pars)
}

#system.time(fastrun_exp(fullparset[1,])) # 19 seconds 
test <- fastrun_exp(fullparset[1,])
plot(test$time, test$I, type = "l")

start <- Sys.time()
experimentdata <- adply(fullparset, 1, fastrun_exp)
Sys.time() - start #46 minutes

write.csv(experimentdata, "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/expdata.csv")

