#Data cleaning

#3001 time points per run

rebind <- function(chars) {paste(chars, collapse = "")}
largeexperimentaldataset <- read.csv("C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/expdata.csv")

#Aim - get series of datasets
experimentdata[,44] <- rep(c(1:144), each = 30001)
experimentdata <- experimentdata[,-c(1:14, 17, 20)] #CHECK - get rid of all input parameters
#split into list of dataframes by experiment
experimentdatalist <- split(experimentdata, f=experimentdata[,23])
#name list elements
namesforlist <- NULL
for (i in 1:144){
  namesforlist <- c(namesforlist, paste(paste(c("r", "k", "cov", "s_i"), 
                                              as.character(intandbasepars[i,], sep = "_")), 
                                        collapse = "-"))
}
names(experimentdatalist) <- namesforlist
