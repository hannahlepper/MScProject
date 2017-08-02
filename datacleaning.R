#Data cleaning
#Needs====
library(plyr)
library(readr)
source("C://Users/hanna/Documents/GitHub/MSc project/experimentdata.R")



#Get data
largeexperimentaldataset <- outputplustime(read.csv,"C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/expdata.csv")
#takes 5 mins

#get rid of inputs, add on experiment numbers
ds<-cbind(largeexperimentaldataset[,-c(1:22)], expnum = rep(c(1:144), each = 30001))

#get rid of large data set - fills up memory
rm(largeexperimentaldataset)

#Fix incidences
ds[,10:15] <- ds[,10:15] * 100000

#split into list of dataframes by experiment
dslist<- split(ds, f=ds[,23])

#name list elements
getexpname <-function(i_b){paste(paste(c("r", "k", "cov", "s_i"), 
                                       as.character(i_b), sep = "_"), collapse = "-")}
names(dslist) <- aaply(intandbasepars, 1, getexpname) 
