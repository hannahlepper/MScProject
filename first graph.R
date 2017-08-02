#not working
efficientsurveylowcomm <- experimentdatalist[[
  which(names(experimentdatalist)=="r 0.1-k 1-cov 0.9-s_i 1")[1]]]
efficientsurveyhighcomm <- experimentdatalist[[
  which(names(experimentdatalist)=="r 0.7-k 1-cov 0.9-s_i 1")[1]]]

plot(efficientsurveylowcomm$time, efficientsurveylowcomm$Inc, type = "l", 
     xlim=c(499,510), col = "red")
lines(efficientsurveyhighcomm$time, efficientsurveyhighcomm$Inc, col = "blue")
abline(v=survey_times(1)[1])
abline(v=survey_times(1)[4])
abline(v=survey_times(1)[7])

plot(efficientsurveyhighcomm$time, efficientsurveyhighcomm$dur_active_inf_TB,
     type = "l",
     col = "green",
     xlim = c(499, 510))
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


plot(efficientsurveylowcomm$time, efficientsurveylowcomm$dur_active_inf_TB, xlim=c(499, 510))
