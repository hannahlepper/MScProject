#Data cleaning

#3000 time points per run

rebind <- function(chars) {paste(chars, collapse = "")}
largeexperimentaldataset <- read.csv("C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/expdata.csv")

#Aim - get series of datasets of experimental inputs and Inc
experimentdata[,44] <- rep(c(1:144), each = 30001)
#split into list of dataframes by experiment
experimentdatalist <- split(experimentdata, f=experimentdata[,44])
#name list elements
namesforlist <- NULL
for (i in 1:length(experimentdatalist)){
  set <- experimentdatalist[[i]][1,]
  namesforlist <- c(namesforlist, rebind(c("r", as.character(set["r"]),
                                            "k", as.character(set["k"]),
                                            "cov", as.character(set["cov"]),
                                            "survey_interval", as.character(set["survey_interval"]))))
}

names(experimentdatalist) <- namesforlist
