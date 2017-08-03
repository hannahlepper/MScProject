#Graphs functions
#Simple graphs ====

#Add lines for locations of surveys onto graph
surveyablines <- function(int){
  abline(v=survey_times(int)[2])
  abline(v=survey_times(int)[5])
  abline(v=survey_times(int)[8])
  mtext("*", side=1, at = survey_times(int)[c(1,4,7)])
}
#Single line plots
simpleplot <- function(data, y, int, col, ...){
  plot(data$time, data$y, col = col, xlim = c(499, 499+(int*3 + 5)), type = "l", ...)
}
#Add another line
addlines <-function(x, y, col){
  lines(x, y, col = col, type = "l")
}

#not working
multilinegraph <- function(data, var){
  plot(0, type = "l", xlim = c(499, 510), 
       ylim = c(
         min(data[,var])-5, 
         max(data[,var])+5))
  for (i in 1:length(unique(data$expnum))){
    newdata <- subset(data, data$expnum == unique(data$expnum)[i])
    line(newdata$time, newdata[,var])
  }
}
