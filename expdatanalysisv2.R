library(tidyr)
library(ggplot2)
library(plyr)
library(readr)
library(Cairo)

ds.short <- read.csv(
  "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/shorttimeexpdata.csv")
ds.int <- read.csv(
  "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/interestingexpdata.csv")
ds.so <- read.csv(
  "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/expdatasurveytimesonly.csv"
)

source("C://Users/hanna/Documents/GitHub/MSc project/datacleaning.R")

#Samp sizes function ====
sampsize <- function(diff, sd){
  (0.84 + 1.96)^2 * ((2 * sd)^2)/(diff^2)
}

#Calculate differnces and percent changes between surveys====
#Differences
diff.df <- ds.so[,c("r","k","cov","survey_interval","Inc", "Prev", "expnum")] %>%
  .[which(!(1:dim(.)[1]%%3==0)),] %>%
  ddply(., .(expnum), function(x){
  c(incdiff = x[2,"Inc"]-x[1,"Inc"],
    prevdiff = x[2,"Prev"]-x[1,"Prev"],
    keepnames(x[1,c("r", "cov", "survey_interval", "k")]))
    }) %>%
  gather(., diff, diff.val, incdiff, prevdiff) %>%
  mutate(., diff = factor(.[,"diff"], labels = c("Incidence of active \ndisease, per 100,000\n",
                                                "Prevalence of active \ndisease, per 100,000\n")))

#Percent changes
percentchange <- function (x, y){
  ((y-x)/x) * 100
}

pc.df <- ds.so[,c("r","k","cov","survey_interval","Inc", "Prev", "expnum")] %>%
  .[which(!(1:dim(.)[1]%%3==0)),] %>%
  ddply(., .(expnum), function(x){
    c(incpchange = percentchange(x[1,"Inc"],x[2,"Inc"]),
      prevpchange = percentchange(x[1,"Prev"],x[2,"Prev"]),
      keepnames(x[1,c("r", "cov", "survey_interval", "k")]))
  }) %>%
  gather(., pc, pc.val, incpchange, prevpchange) %>%
  mutate(., pc = factor(.[,"pc"], labels = c("Incidence of active \ndisease, per 100,000\n",
                                                 "Prevalence of active \ndisease, per 100,000\n")))


#Average conditions ====

#Effect on incidence and prevalence
#Say modelled on South India surveys

avs <- list(0.1,0.4,0.7)
avnums <- sapply(avs, function(x) getexpnum(c(x,0.8,0.6,3)))

average.full <- read.csv("C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/fulltimeaveragesurveydata.csv")

average <- average.full[,c("Prev", "Inc", "time", "r")] %>%
  gather(., outcome, val, -time, -r) %>%
  mutate(., outcome = factor(.[,"outcome"], 
                             labels = c("Incidence of active disease, per 100,000",
                                        "Prevalence of active disease, per 100,000")))

average.so <- ds.so[which(ds.so$expnum %in% avnums),c("Prev", "Inc", "time", "r")] %>%
  gather(., outcome, val,-time, -r) %>%
  mutate(., outcome = factor(.[,"outcome"], 
                          labels = c("Incidence of active disease, per 100,000",
                                     "Prevalence of active disease, per 100,000")))

summaryplot <- ggplot(average, aes(time, val, col = as.factor(r))) + 
                        geom_line() +
facet_wrap(~outcome) +
  theme_bw() +
  labs(x="Time in years", y="", col = "Level of between \ncommunity transmission") +
  theme(strip.background = element_rect("white"),
        legend.direction = "horizontal",
        legend.position = "bottom") +
  scale_colour_grey(start=0,end=0.6) +
  scale_x_continuous(breaks = seq(0,600,100))


#Effect on transmission
t.averagefoi <- gather(average.full[which(average.full$time >=499 &
                                            average.full$time <=540),c("foi_basic", "foi_comm","time", "r")],
                       outcome, val, -time, -r) %>%
  mutate(., 
         outcome=factor(.[,"outcome"], 
                         labels = c("Incidence of primary infection, \nacquired within the community, \nper 100,00",
                                    "Incidence of primary infection \nacquired outside the community, \nper 100,00")))
  
summarytransplot <- ggplot(t.averagefoi, aes(time, val*100000, col = as.factor(r))) +
  geom_line() +
  scale_x_continuous(limits = c(499,512), breaks = seq(499,512,2)) +
  theme_bw() +
  labs(x= "Time in years", y = "", 
       col = "Level of between\ncommunity transmission") +
  facet_wrap(~outcome) +
  scale_colour_grey(start=0.6,end=0)+
  theme(strip.background = element_rect(fill = "white"),
        legend.direction = "horizontal",
        legend.position = "bottom") 
  
#Effect on persistence of effect

av.changes <- cbind(diff.df[which(diff.df$expnum %in% avnums),],
                    pc.df[which(pc.df$expnum %in% avnums),])

av.changes.plot <- ggplot(av.changes, aes(r, diff.val, linetype = diff)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  theme(strip.background =element_rect(fill = "white"),
        legend.direction = "horizontal",
        legend.position = "bottom") +
  labs(x = "Level of between community transmission",
       y = "Difference between survey one and two, \nper 100,00",
       linetype = "") +
  scale_x_continuous(breaks =c(0.1,0.4,0.7))

#Sensitivity analysis 1: survey coverage====
covs <- list(0.09,0.3,0.6,0.9)
coveragesensnums <- sapply(covs, function(x) getexpnum(c(0.4,0.8,x,3)))
coverage <- ds.short[which(ds.short$expnum %in% coveragesensnums),c("Prev", "Inc", "time", "cov")] %>%
  gather(., outcome, val, 
                    -time, -cov) %>%
  mutate(., 
         outcome = factor(.[,"outcome"], 
                          labels = c("Incidence of active disease, per 100,000",
                                     "Prevalence of active disease, per 100,000")))

coverage.so <- ds.so[which(ds.so$expnum %in% coveragesensnums),c("Prev", "Inc", "time", "cov")] %>%
  gather(., outcome, val, 
         -time, -cov) %>%
  mutate(., 
         outcome = factor(.[,"outcome"], 
                          labels = c("Incidence of active disease, per 100,000",
                                     "Prevalence of active disease, per 100,000")))

covincprecplot <-ggplot() +
  geom_line(data = coverage, aes(time, val, col = as.factor(cov))) +
  geom_point(data = coverage.so, aes(time, val, col = as.factor(cov)), shape = 8) +
  facet_wrap(~outcome) +
  theme_bw() +
  theme(strip.background=element_rect(fill = "white"),
        legend.direction = "horizontal",
        legend.position = "bottom") +
  labs(x = "Time in years", y = "", col = "Survey coverage") +
  scale_x_continuous(limits = c(499, 512), breaks = seq(500,512, 2)) +
  scale_colour_grey(start=0.6, end = 0)

cov.changes <- cbind(diff.df[which(diff.df$expnum %in% coveragesensnums),],
                    pc.df[which(pc.df$expnum %in% coveragesensnums),])

cov.changes.plot <- ggplot(cov.changes, aes(cov, diff.val, linetype = diff)) +
  geom_line() + 
  geom_point() +
  theme_bw() +
  theme(strip.background =element_rect(fill = "white"),
        legend.direction = "horizontal",
        legend.position = "bottom") +
  labs(x = "Survey coverage",
       y = "Difference between survey one and two, \nper 100,00",
       linetype = "")

#Sensitivity analysis 2: Linkage to treatment====

ks <- list(0.4,0.6,0.8,1)
ksensnums <- sapply(ks, function(x) getexpnum(c(0.4,x,0.6,3)))
ksens <- ds.short[which(ds.short$expnum %in% ksensnums),c("Prev", "Inc", "time", "k")] %>%
  gather(., outcome, val, 
         -time, -k) %>%
  mutate(., 
         outcome = factor(.[,"outcome"], 
                          labels = c("Incidence of active disease, per 100,000",
                                     "Prevalence of active disease, per 100,000")))

ksens.so <- ds.so[which(ds.so$expnum %in% ksensnums),c("Prev", "Inc", "time", "k")] %>%
  gather(., outcome, val, 
         -time, -k) %>%
  mutate(., 
         outcome = factor(.[,"outcome"], 
                          labels = c("Incidence of active disease, per 100,000",
                                     "Prevalence of active disease, per 100,000")))

kincprevplot <-ggplot() +
  geom_line(data = ksens, aes(time, val, col = as.factor(k))) +
  geom_point(data = ksens.so, aes(time, val, col = as.factor(k)), shape = 8) +
  facet_wrap(~outcome) +
  theme_bw() +
  theme(strip.background=element_rect(fill = "white"),
        legend.direction = "horizontal",
        legend.position = "bottom") +
  labs(x = "Time in years", y = "", col = "Linkage to treatment") +
  scale_x_continuous(limits = c(499, 512), breaks = seq(500,512, 2)) +
  scale_colour_grey(start=0.6, end = 0)

k.changes <- cbind(diff.df[which(diff.df$expnum %in% ksensnums),],
                    pc.df[which(pc.df$expnum %in% ksensnums),])

k.changes.plot <- ggplot(k.changes, aes(k, diff.val, linetype = diff)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  theme(strip.background =element_rect(fill = "white"),
        legend.direction = "horizontal",
        legend.position = "bottom") +
  labs(x = "Linkage to treatment",
       y = "Difference between survey one and two, \nper 100,00",
       linetype = "")

#Sensitivity analysis 3: Survey interval====

sis <- list(1,3,5,10)
sisnums <- sapply(sis, function(x) getexpnum(c(0.4,0.8,0.6,x)))
sisens <- ds.short[which(ds.short$expnum %in% sisnums),c("Prev", "Inc", "time", "survey_interval")] %>%
  gather(., outcome, val, 
         -time, -survey_interval) %>%
  mutate(., 
         outcome = factor(.[,"outcome"], 
                          labels = c("Incidence of active disease, per 100,000",
                                     "Prevalence of active disease, per 100,000")))

sisens.so <- ds.so[which(ds.so$expnum %in% sisnums),c("Prev", "Inc", "time", "survey_interval")] %>%
  gather(., outcome, val, 
         -time, -survey_interval) %>%
  mutate(., 
         outcome = factor(.[,"outcome"], 
                          labels = c("Incidence of active disease, per 100,000",
                                     "Prevalence of active disease, per 100,000")))

sisincprevplot <-ggplot() +
  geom_line(data = sisens, aes(time, val, col = as.factor(survey_interval))) +
  geom_point(data = sisens.so, aes(time, val, col = as.factor(survey_interval)), shape = 8) +
  facet_wrap(~outcome, nrow = 2, ncol =1) +
  theme_bw() +
  theme(strip.background=element_rect(fill = "white"),
        legend.direction = "horizontal",
        legend.position = "bottom") +
  labs(x = "Time in years", y = "", col = "Survey interval \nin years") +
  scale_x_continuous(limits = c(499, 530), breaks = seq(500,530, 5))

si.changes <- cbind(diff.df[which(diff.df$expnum %in% sisnums),],
                    pc.df[which(pc.df$expnum %in% sisnums),])

si.changes.plot <- ggplot(si.changes, aes(survey_interval, diff.val, linetype = diff)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  theme(strip.background =element_rect(fill = "white"),
        legend.direction = "horizontal",
        legend.position = "bottom") +
  labs(x = "Survey interval",
       y = "Difference between survey one and two, \nper 100,00",
       linetype = "")


#Extreme cases analysis====

extreme <- ds.short[which(ds.short$expnum %in% c(getexpnum(c(0.7,0.4,0.09,10)), 
                                                 getexpnum(c(0.1,1,0.9,1)))), 
                    c("Prev", "Inc", "time", "cov")] %>%
  gather(., outcome, val, Prev, Inc) %>%
  mutate(.,outcome = factor(.[,"outcome"], 
                            labels = c("Incidence of active disease, per 100,000",
                                       "Prevalence of active disease, per 100,000"))) %>%
  mutate(., cov = factor(.[,"cov"], labels = c("Minimum effect", "Maximum effect")))

extreme.so <- ds.so[which(ds.so$expnum %in% c(getexpnum(c(0.7,0.4,0.09,10)),
                                              getexpnum(c(0.1,1,0.9,1)))),
                    c("Prev", "Inc", "time", "cov")] %>%
  gather(., outcome, val, Prev, Inc) %>%
  mutate(.,outcome = factor(.[,"outcome"], 
                            labels = c("Incidence of active disease, per 100,000",
                                       "Prevalence of active disease, per 100,000"))) %>%
  mutate(., cov = factor(.[,"cov"], labels = c("Minimum effect", "Maximum effect")))

extremeincprevplot <-ggplot() +
  geom_line(data = extreme, aes(time, val, col = cov)) +
  geom_point(data = extreme.so, aes(time, val, col = cov), shape = 8) +
  facet_wrap(~outcome) +
  theme_bw() +
  theme(strip.background=element_rect(fill = "white"),
        legend.direction = "horizontal",
        legend.position = "bottom") +
  labs(x = "Time in years", y = "", col = "Survey effect \nscenario") +
  scale_x_continuous(limits = c(499, 525), breaks = seq(500,525, 5)) +
  scale_colour_grey(start=0.6, end = 0)

ex.changes <- cbind(diff.df[which(diff.df$expnum %in% c(getexpnum(c(0.7,0.4,0.09,10)),
                                                        getexpnum(c(0.1,1,0.9,1)))),],
                    pc.df[which(pc.df$expnum %in% c(getexpnum(c(0.7,0.4,0.09,10)),
                                                    getexpnum(c(0.1,1,0.9,1)))),]) %>%
  mutate(., cov = factor(.[,"cov"], labels = c("Minumum", 
                                               "Maximum")))


ex.changes.plot <- ggplot(ex.changes,aes(cov, diff.val)) +
  geom_bar(data = subset(ex.changes, diff == "Prevalence of active \ndisease, per 100,000\n"),
           stat = "identity", fill = "grey31") +
  geom_bar(data = subset(ex.changes, diff == "Incidence of active \ndisease, per 100,000\n"),
           stat = "identity", fill = "black") +
  theme_bw() +
  theme(strip.background =element_rect(fill = "white")) +
  labs(y = "Difference between survey one and two, \nper 100,00",
       x = "Extreme survey scenario") +
  scale_fill_grey(start = 0, end = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed")

ex.changes.plot.with.legend <- ggplot(ex.changes, aes(cov, diff.val, fill = diff)) +
  geom_bar(stat = "identity") +
  labs(fill = "") +
  scale_fill_manual(values = c("black", "grey31")) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

#Tables for reference====


testedscenarios <- unique(c(avnums, coveragesensnums, ksensnums, sisnums, 
                            getexpnum(c(0.7,0.4,0.09,10)), 
                            getexpnum(c(0.1,1,0.9,1))))
diff.data <- cbind(diff.df[which(diff.df$expnum %in% testedscenarios),],
                   pc.df[which(pc.df$expnum %in% testedscenarios),])

write.csv(diff.data, 
          "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/differencev2.csv")


#get numbers of initial declines in %
#For period between first and second survey, what is the minimum value?

testedscenarios <- unique(c(avnums, coveragesensnums, ksensnums, sisnums, 
       getexpnum(c(0.7,0.4,0.09,10)), 
         getexpnum(c(0.1,1,0.9,1))))
testeddata <- ds.short[which(ds.short$expnum %in% testedscenarios &
                               ds.short$time > 499 &
                               ds.short $time < 500.9),
                       c("Prev", "Inc", "r", "cov", "k", "survey_interval", "time", "expnum")]
minprevs <- ddply(testeddata, .(expnum), function(x) {
  rbind(x[1,],x[which(x$Prev == min(x$Prev)),])
})
write.csv(minprevs,
          "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/initialprevdecline.csv")

minincs <- ddply(testeddata, .(expnum), function(x) {
  rbind(x[1,], x[which(x$time==500.06),])
})
write.csv(minincs,
          "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/initialincdecline.csv")

#Distribution of differences; boxplot====

box.diff.dist <- ggplot(diff.df, aes(1, diff.val)) +
  geom_boxplot() +
  facet_wrap(~diff, drop = T) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white")) +
  labs(x ="", y = "Difference between survey 1 and 2, \nper 100,00") +
  scale_x_continuous(breaks = c(0,5)) +
  scale_y_continuous(breaks = seq(0,-40,-2))

#Save graphs====

#graphs
setwd("C://Users/hanna/Dropbox/Academic/LSHTM/Project/Graphs/")
svg("summaryplot.svg",height = 5)
summaryplot
dev.off()

svg("summarytransmission3.svg",height = 5)
summarytransplot
dev.off()

svg("summarydifferences.svg",height = 5)
av.changes.plot
dev.off()

svg("coverageincprev.svg",height = 5)
covincprecplot
dev.off()

svg("coveragedifferences.svg",height = 5)
cov.changes.plot
dev.off()

svg("linkageincprev.svg",height = 5)
kincprevplot
dev.off()

svg("linkagedifferences.svg",height = 5)
k.changes.plot
dev.off()

svg("surveyintervalincprev.svg",height = 5)
sisincprevplot
dev.off()

svg("surveyintervaldifferences.svg",height = 5)
si.changes.plot
dev.off()

svg("extremeincprev.svg",height = 5)
extremeincprevplot
dev.off()

svg("box.diff.dist.svg")
box.diff.dist
dev.off()
