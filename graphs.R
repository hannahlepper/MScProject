#Graphs functions
#Simple graphs ====

#Add lines for locations of surveys onto graph
surveyablines <- function(int){
  abline(v=survey_times(int)[2])
  abline(v=survey_times(int)[5])
  abline(v=survey_times(int)[8])
  mtext("*", side=1, at = survey_times(int)[c(1,4,7)])
}

#Add another line
addlines <-function(x, y, col){
  lines(x, y, col = col, type = "l")
}

#not working
simplemultilinegraph <- function(data){
  plot(0, type = "l", 
       ylim = c(
         min(data$Inc)-5, 
         max(data$Inc)+5))
  
  
}
