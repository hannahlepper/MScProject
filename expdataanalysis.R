#Experimental data analysis

library(tidyr)
library(ggplot2)
library(plyr)

source("C://Users/hanna/Documents/GitHub/MSc project/datacleaning.R")

#Smaller datasets needed...
shortdslist <- llply(dslist, function(x) {
  list(x[[1]], data.frame(x[[2]][which(x[[2]][,"time"]==490):which(x[[2]][,"time"]==550),]))
})


#tangent - look at fois before the surveys are introduced
foi <- rbind(shortdslist[[1]][[2]][1,], shortdslist[[2]][[2]][1,], shortdslist[[3]][[2]][1,])%>%
  .[,c(1,9, 14,28)] %>%
  gather(output, val, foi_basic,foi_comm) 

foiplot <- ggplot(foi, aes(expnum, val)) +
  geom_bar(stat="identity") +
  facet_grid(.~output)

#Question 1: do surveys have an effect on transmission?
incidencedata <- ldply(shortdslist, function(x){
  x[[2]][,c("time", "Inc", "Inc_recent", "expnum")]
}) %>% 
  gather(output, val, Inc, Inc_recent)

lookuppars <- cbind(intandbasepars, expnum = 1:192)

newcol <-NULL
for (i in incidencedata){
  newcol = rbind(newcol, intandbasepars[with(lookuppars,which(expnum==expnum)),])
}

r0.1indata <- incidencedata[with(incidencedata, which(expnum==with(intandbasepars,which(r==0.1)))),]
r0.4indata <- incidencedata[with(incidencedata, which(expnum==with(intandbasepars,which(r==0.4)))),]
r0.7indata <- incidencedata[with(incidencedata, which(expnum==with(intandbasepars,which(r==0.7)))),]


r0.1incidenceplot <- ggplot(subset(r0.1indata, output=="Inc"), aes(time, val))+
  geom_line() +       
  facet_grid(.~expnum)
