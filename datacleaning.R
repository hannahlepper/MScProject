#Data cleaning
#Needs====
library(plyr)
library(readr)
library(rlist)
library(tidyr)
source("C://Users/hanna/Documents/GitHub/MSc project/experimentdata.R")

#useful function====

repeatrows <- function(df, n){
  df[rep(seq_len(nrow(df)),n),]
}

#Get baseline data====
#  largebaselinedataset <- outputplustime(read.csv,
#                                         "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/baselinedata.csv")
# # #18 seconds
# bds <- cbind(largebaselinedataset, 
#              r = rep(c(.1, .4,.7),
#                      each = dim(largebaselinedataset)[1]/3)) %>%
#   split(., f=.$r) %>%
#   ldply(., function(x) tail(x, 1))

#write.csv(bdatashort, "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/shortbaselinedata.csv")

#Get experimental data====
# largeexperimentaldataset <- outputplustime(read.csv,
#     "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/expdatav4.csv")
# #takes 9 mins
#
#clean exp data====
# #get rid of inputs, add on experiment numbers
#
# ds<-cbind(largeexperimentaldataset[,-c(1, 7:22)],
#           expnum = rep(c(1:dim(intandbasepars)[1]),
#                        each = dim(largeexperimentaldataset)[1]/dim(intandbasepars)[1]))
# 
# rm(largeexperimentaldataset)
# #
# # #Get rid of not useful time points
# ds.short.time <- ddply(ds, .(expnum), function(x){
#   x[which(x$time==490):which(x$time==550),]
# })
# #
# rm(ds)
# write.csv(ds.short.time, 
#           "C:\\Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/shorttimeexpdata.csv")

#Keep only interesting experiments====
interestingexps <- data.frame(
  cbind(
    parametercombinations(list(r=c(0.1,0.4,0.7), k=c(0.4,0.8,1), survey_interval=c(1,3,5,10))),
    cov = rep(c(0.09,0.6,0.9),each = 3, length.out=36)
  ))
interestingexps <- interestingexps[,c(1,2,4,3)]

getexpnum <- function(pars){
  which(apply(intandbasepars, 1, function(x) all(x==pars)))
}

to.use <- apply(interestingexps,1,getexpnum)

# ds.short <- ds.short.time[which(ds.short.time[,"expnum"] %in% to.use),]
# 
# rm(ds.short.time)
# 
# write.csv(ds.short, 
#           "C:\\Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/shortexpdata.csv")

#get rid of large data set - fills up memory

#split into list of dataframes by experiment
#dslist<- split(ds, f=ds$expnum)

#Smaller datasets needed...
# shortdslist <- llply(dslist, function(x) {
#   data.frame(x[which(x[,"time"]==490):which(x[,"time"]==550),])
# })
# 
# #insert input information
# for (i in 1:length(dslist[1:5])){
#   shortdslist[[i]] <- list(getexpinputs(intandbasepars[i,]), shortdslist[[i]])
# }
# 


#Check all working
# test <- list(data.frame(x=c(1,2), y=c(3,4)),
#              data.frame(x=c(1,6), y=c(7,8)))
# test[[1]] <- list(test[[1]], c(1,2,3))

#Keep only values at surveys



# ds.survey.only <- ddply(ds.short.time, .(expnum), function(x){
#   x[c(which(x$time==survey_times(unique(x$survey_interval))[1]),
#       which(x$time==survey_times(unique(x$survey_interval))[4]),
#       which(x$time==survey_times(unique(x$survey_interval))[7])),]})
# 
# write.csv(ds.survey.only,
#           "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/expdatasurveytimesonly.csv")
# 
# ds.st.survey.only <- ddply(ds.short, .(expnum), function(x){
#   x[c(which(x$time==survey_times(unique(x$survey_interval))[1]),
#       which(x$time==survey_times(unique(x$survey_interval))[4]),
#       which(x$time==survey_times(unique(x$survey_interval))[7])),]})
