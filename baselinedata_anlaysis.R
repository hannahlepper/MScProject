#Baseline data analysis

library(ggplot2)
library(tidyr)
library(readr)


bdatatidy <- gather(bdatashort, output, val, -.id, -X, -time, -r)[,c("r", "output", "val")]

#add in % change
percentchange <- function (x, y){
  ((y-x)/x) * 100
}
percentchanges <- adply(unique(bdatatidy$output), 1, function(x) {
  new <- getanouputsubset(x, bdatatidy)
  cbind(p.change=c(0,
    percentchange(new[1,"val"], new[2, "val"]), 
    percentchange(new[1, "val"], new[3, "val"])),
    output = new[1:3,"output"])
})

bdatatidy <- cbind(bdatatidy, 
                   p.change = as.numeric(as.character(percentchanges[,"p.change"])))


#Look at different incidences                          
bdataincidence <- getanouputsubset(c("Inc", "Inc_recent", "Inc_first", "Inc_react",
                                     "Inc_relap"), bdatatidy)
incidenceplot <- ggplot(bdataincidence, aes(as.factor(r), val)) +
  geom_bar(stat="identity") +
  facet_grid(.~output, scale = "free") +
  xlab("Level of between community mixing, r") +
  ylab("Number per 100,000") +
  labs(title = "Effect of level of between community transmission on\n different types of incidence")

incpercentchange <- ggplot(bdataincidence, aes(output, p.change)) +
  geom_bar(stat="identity") +
  facet_grid(.~r, scale = "free")

#add in infectious prevalence
inf_case_prev <- function(I, N) {I + 0.22*N}
bd_inf_prev <- data.frame(r = c(0.1, 0.4, 0.7),
                     val = inf_case_prev(getanouputsubset("I", bdatatidy)[,"val"],
                                   getanouputsubset("N", bdatatidy)[,"val"]))
infectiouscaseprevalence <- ggplot(bd_inf_prev, aes(r, val)) +
  geom_point(stat = "identity") +
  ylim(c(0.0006,0.0008))

mean(bd_inf_prev$val)
