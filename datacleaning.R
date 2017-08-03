#Data cleaning
#Needs====
library(plyr)
library(readr)
library(rlist)
source("C://Users/hanna/Documents/GitHub/MSc project/experimentdata.R")

#Functions====
#put exp data into list elements
getexpinputs <-function(i_b){
  setNames(as.numeric(i_b), c("r", "k", "cov", "survey_interval"))
}
#function - make subsets
getasubset <- function(params, nums){
  subsetting <- function(l){
    if (all(l[[1]][params]==nums)){return(l[[2]])}
  }
  out <- ldply(dslist, subsetting)
  return(out)
}

#Get data====
largeexperimentaldataset <- outputplustime(read.csv,"C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/expdata.csv")
#takes 5 mins

#clean data====
#get rid of inputs, add on experiment numbers
ds<-cbind(largeexperimentaldataset[,-c(1:22)], expnum = rep(c(1:144), each = 30001))

#get rid of large data set - fills up memory
rm(largeexperimentaldataset)

#Fix incidences
ds[,10:15] <- ds[,10:15] * 100000

#split into list of dataframes by experiment
dslist<- split(ds, f=ds[,23])

#insert input information
for (i in 1:length(dslist)){
  dslist[[i]] <- list(getexpinputs(intandbasepars[i,]), dslist[[i]]) 
}

#Check all working
# test <- list(data.frame(x=c(1,2), y=c(3,4)),
#              data.frame(x=c(1,6), y=c(7,8)))
# test[[1]] <- list(test[[1]], c(1,2,3))
