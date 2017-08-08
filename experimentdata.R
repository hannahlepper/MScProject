#Run experiments

##Needs re-runnning with new cov.!!!##


#Needs====
library(plyr)
library(deSolve)
library(rootSolve)
library(readr)
source("C://Users/hanna/Documents/GitHub/MSc project/baselinerun.R")


#prepareparset====
exp.parfixer <- function(changepars, fitpars) {
  pars <- data.frame(b=10,Mu=0.006,Mui=0.3,Mun=0.21,a=0.115,theta=0.015,p=0.01,
                     c=0.22,x=0.65,vs=0.0005,vf=0.67,nc=0.2,sg=0.45,CDR=0.77,sens=0.978,
                     cov = 0,k = 0,tau=0.91,r=.4,Ic = 0.001,survey_interval=0)
  newparset <- as.data.frame(c(changepars, 
                               fitpars[which(fittedparams$r==changepars$r),
                                       c("b","Mu","Mui","nc","CDR")]))
  addpars <- pars[which(!names(pars)%in%names(newparset))]
  setNames(
    as.numeric(c(newparset, addpars)), 
    c(names(newparset), names(addpars)))
}
intandbasepars <- parametercombinations(list(r=c(.10,.40,.70),
                                             k=c(.40,.60,.80,1.00),
                                             cov=c(.09,.3, .60, .90),
                                             survey_interval=c(1,3,5,10)))

fullparset <- adply(intandbasepars, 1, function(x) exp.parfixer(x, fittedparams),
                    .expand=F, .id=NULL)

#functions====
fastrun_exp <- function(pars) {
  y <- c(U=1-0.2,Ls=0.99*0.2,Lf=0,I=0.01*0.2,N=0,C=0)
  initrun<-runsteady(y=y,times=c(0,Inf), func=PSmodel, parms=pars)
  yinit<-initrun$y
  sol_base<-ode(y=yinit,times=seq(0,600, by=0.02),func=PSmodel,parms=pars)
  sol_base_df <- as.data.frame(sol_base)
  return(sol_base_df)
}
#run====
#system.time(fastrun_exp(fullparset[1,])) # 19 seconds 
test <- fastrun_exp(fullparset[1,])


experimentdata <- outputplustime(adply, fullparset, 1, fastrun_exp, .expand=F, .id=NULL)
#46 minutes

write.csv(experimentdata, "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/expdatav2.csv")

