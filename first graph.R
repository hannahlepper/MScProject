function(df){
  plot(df$time, df$Inc, type = "l")
  tit
}

efficientsurveylowcomm <- experimentdatalist[[
  which(names(experimentdatalist)=="r0.1k1cov0.9survey_interval1")[1]]]
efficientsurveyhighcomm <- experimentdatalist[[
  which(names(experimentdatalist)=="r0.7k1cov0.9survey_interval1")[1]]]

plot(efficientsurveylowcomm$time, efficientsurveylowcomm$Inc, type = "l", 
     xlim=c(499,510), ylim = c(110, 114), col = "red")
lines(efficientsurveyhighcomm$time, efficientsurveyhighcomm$Inc, col = "blue")
abline(v=survey_times(1)[1])
abline(v=survey_times(1)[4])
abline(v=survey_times(1)[7])

plot(efficientsurveylowcomm$time, efficientsurveylowcomm$cases_removed, type = "l", 
     xlim=c(499,510), col = "red")
lines(efficientsurveyhighcomm$time, efficientsurveyhighcomm$cases_removed, col = "blue")
abline(v=survey_times(1)[1])
abline(v=survey_times(1)[4])
abline(v=survey_times(1)[7])

plot(efficientsurveylowcomm$time, efficientsurveylowcomm$Prev, type = "l", 
     xlim=c(499,510), col = "red")
lines(efficientsurveyhighcomm$time, efficientsurveyhighcomm$Prev, col = "blue")
abline(v=survey_times(1)[1])
abline(v=survey_times(1)[4])
abline(v=survey_times(1)[7])

