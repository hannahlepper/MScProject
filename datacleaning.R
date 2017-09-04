#Data cleaning
#Needs====
library(plyr)
library(readr)
library(rlist)
library(tidyr)
source("C://Users/hanna/Documents/GitHub/MSc project/experimentdata.R")

#useful functions====

repeatrows <- function(df, n){
  df[rep(seq_len(nrow(df)),n),]
}

getexpnum <- function(pars){
  which(apply(intandbasepars, 1, function(x) all(x==pars)))
}

#Get baseline data====
largebaselinedataset <- outputplustime(read.csv,
                                        "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/baselinedata.csv")
# # #18 seconds
bds <- cbind(largebaselinedataset,
             r = rep(c(.1, .4,.7),
                     each = dim(largebaselinedataset)[1]/3)) %>%
  split(., f=.$r) %>%
  ldply(., function(x) tail(x, 1))

write.csv(bds, "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/shortbaselinedata.csv")

#Get experimental data====
largeexperimentaldataset <- outputplustime(read.csv,
     "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/expdatav7.csv")
# #takes 9 mins


#clean exp data====
#get rid of inputs, add on experiment numbers

keep <- c("r", "k", "cov", "survey_interval",
          "time", 
          "U", "I", "N", "Ls", "Lf", "C",
          "foi_basic", "foi_reinf_comm", "foi_reinf", "foi_exo_react_comm", "foi_exo_react", "foi_comm",
          "Inc", "Inc_first", "Inc_react", "Inc_relap", "Inc_recent",
          "case_notifications", "treatment_cov", "cases_removed",
          "Prev", "Inf_prev", "Mort", "dur_active_TB", "dur_active_inf_TB")

ds<-cbind(largeexperimentaldataset[,which(names(largeexperimentaldataset)%in%keep)],
          expnum = rep(c(1:dim(intandbasepars)[1]),
                       each = dim(largeexperimentaldataset)[1]/dim(intandbasepars)[1]))


rm(largeexperimentaldataset)
#
# #Get rid of not useful time points
ds.short <- ddply(ds, .(expnum), function(x){
  x[which(x$time==490):which(x$time==550),]
})
#

write.csv(ds.short,
          "C:\\Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/shorttimeexpdata.csv")

#Keep only interesting experiments====
interestingexps <- data.frame(
  cbind(
    parametercombinations(list(r=c(0.1,0.4,0.7), k=c(0.4,0.8,1), survey_interval=c(1,3,5,10))),
    cov = rep(c(0.09,0.6,0.9),each = 3, length.out=36)
  ))
interestingexps <- interestingexps[,c(1,2,4,3)]

to.use <- apply(interestingexps,1,getexpnum)

ds.int <- ds.short[which(ds.short[,"expnum"] %in% to.use),]

write.csv(ds.int,
          "C:\\Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/interestingexpdata.csv")


ds.so <- ddply(ds.short, .(expnum), function(x){
  x[c(which(x$time==survey_times(unique(x$survey_interval))[1]),
      which(x$time==survey_times(unique(x$survey_interval))[4]),
      which(x$time==survey_times(unique(x$survey_interval))[7])),]})

write.csv(ds.so,
          "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/expdatasurveytimesonly.csv")

ds.int.so <- ddply(ds.int, .(expnum), function(x){
  x[c(which(x$time==survey_times(unique(x$survey_interval))[1]),
      which(x$time==survey_times(unique(x$survey_interval))[4]),
      which(x$time==survey_times(unique(x$survey_interval))[7])),]})

write.csv(ds.int.so,
          "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/interestingexpdatasurveytimesonly.csv")
