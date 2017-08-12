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
getaparamsubset <- function(params, ds){
    subsetting <- function(l){
      if (all(l[[1]]==keepnames(params))){return(l)}
    }
    out <- llply(ds, subsetting)
    return(out)
}
getaparamsubsetdf <- function(params, nums, ds){
  subsetting <- function(l){
    if (all(l[[1]][params]==nums)){return(l[[2]])}
  }
  out <- ldply(ds, subsetting)
}
getanouputsubset <- function(outputkeys, ds){
  newds <- NULL
  i <- 0
  repeat {
    i <- i+1
    newds <- rbind(newds, subset(ds, output == outputkeys[i]))
    if (i == length(outputkeys)) {break}
  }
  return(newds)
}

#Get baseline data====
largebaselinedataset <- outputplustime(read.csv,
                                       "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/baselinedata.csv")
#18 seconds
bds <- cbind(largebaselinedataset, r = rep(c(.1, .4,.7), 
                                               each = dim(largebaselinedataset)[1]/3))
bdatalist <- split(bds, f=bds$r)
rm(largebaselinedataset, bds)
bdatashort <- ldply(bdatalist, function(x) tail(x, 1))
#write.csv(bdatashort, "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/shortbaselinedata.csv")

#Get experimental data====
largeexperimentaldataset <- outputplustime(read.csv,
                                           "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/expdatav2.csv")
#takes 6.2 mins

#clean data====
#get rid of inputs, add on experiment numbers
ds<-cbind(largeexperimentaldataset, 
          expnum = rep(c(1:dim(intandbasepars)[1]), 
                       each = dim(largeexperimentaldataset)[1]/dim(intandbasepars)[1]),
          intandbasepars[rep(seq_len(nrow(intandbasepars)),each = 30001),]
          )

#get rid of large data set - fills up memory
rm(largeexperimentaldataset)

#split into list of dataframes by experiment
dslist<- split(ds, f=ds$expnum)

#Smaller datasets needed...
shortdslist <- llply(dslist, function(x) {
  data.frame(x[which(x[,"time"]==490):which(x[,"time"]==550),])
})

#insert input information
for (i in 1:length(dslist[1:5])){
  shortdslist[[i]] <- list(getexpinputs(intandbasepars[i,]), shortdslist[[i]])
}

interestingexps <- data.frame(
  rbind(
    c(r=0.1, k=0.4, cov=0.09,survey_interval=10),
    c(r=0.1, k=0.4, cov=0.09,survey_interval=5),
    c(r=0.1, k=0.4, cov=0.09,survey_interval=3),
    c(r=0.1, k=0.4, cov=0.09,survey_interval=1),
    c(r=0.1, k=1, cov=0.9,survey_interval=10),
    c(r=0.1, k=1, cov=0.9,survey_interval=5),
    c(r=0.1, k=1, cov=0.9,survey_interval=3),
    c(r=0.1, k=1, cov=0.9,survey_interval=1),
    c(r=0.1, k=0.8, cov=0.6,survey_interval=10),
    c(r=0.1, k=0.8, cov=0.6,survey_interval=5),
    c(r=0.1, k=0.8, cov=0.6,survey_interval=3),
    c(r=0.1, k=0.8, cov=0.6,survey_interval=1),
    c(r=0.7, k=0.4, cov=0.09,survey_interval=10),
    c(r=0.7, k=0.4, cov=0.09,survey_interval=5),
    c(r=0.7, k=0.4, cov=0.09,survey_interval=3),
    c(r=0.7, k=0.4, cov=0.09,survey_interval=1),
    c(r=0.7, k=1, cov=0.9,survey_interval=10),
    c(r=0.7, k=1, cov=0.9,survey_interval=5),
    c(r=0.7, k=1, cov=0.9,survey_interval=3),
    c(r=0.7, k=1, cov=0.9,survey_interval=1),
    c(r=0.7, k=0.8, cov=0.6,survey_interval=10),
    c(r=0.7, k=0.8, cov=0.6,survey_interval=5),
    c(r=0.7, k=0.8, cov=0.6,survey_interval=3),
    c(r=0.7, k=0.8, cov=0.6,survey_interval=1),
    c(r=0.4, k=0.8, cov=0.6,survey_interval=10),
    c(r=0.4, k=0.8, cov=0.6,survey_interval=5),
    c(r=0.4, k=0.8, cov=0.6,survey_interval=3),
    c(r=0.4, k=0.8, cov=0.6,survey_interval=1),
    c(r=0.4, k=1, cov=0.9,survey_interval=10),
    c(r=0.4, k=1, cov=0.9,survey_interval=5),
    c(r=0.4, k=1, cov=0.9,survey_interval=3),
    c(r=0.4, k=1, cov=0.9,survey_interval=1),
    c(r=0.4, k=0.4, cov=0.09,survey_interval=10),
    c(r=0.4, k=0.4, cov=0.09,survey_interval=5),
    c(r=0.4, k=0.4, cov=0.09,survey_interval=3),
    c(r=0.4, k=0.4, cov=0.09,survey_interval=1)
  )
)

getexpnum <- function(pars){
  which(apply(intandbasepars, 1, function(x) all(x==pars)))
}

touse <- apply(interestingexps,1,getexpnum)



#Check all working
# test <- list(data.frame(x=c(1,2), y=c(3,4)),
#              data.frame(x=c(1,6), y=c(7,8)))
# test[[1]] <- list(test[[1]], c(1,2,3))
