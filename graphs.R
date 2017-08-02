#Graphs functions
#Simple graphs ====

#Add lines for locations of surveys onto graph
surveyablines <- function(int){
  abline(v=survey_times(int)[1])
  abline(v=survey_times(int)[4])
  abline(v=survey_times(int)[7])
  mtext("*", side=1, at = survey_times(int)[c(1,4,7)])
}
#Single line plots
simpleplot <- function(x, y, int, col){
  plot(x, y, col = col, xlim = c(499, 499+(int*3 + 5)), type = "l")
}
#Add another line
addlines <-function(x, y, col){
  lines(x, y, col = col, type = "l")
}
