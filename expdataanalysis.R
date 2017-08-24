#Experimental data analysis

library(tidyr)
library(ggplot2)
library(plyr)
library(readr)

source("C://Users/hanna/Documents/GitHub/MSc project/datacleaning.R")

ds.short.time <- read.csv(
  "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/shorttimeexpdata.csv")
ds.survey.only <- read.csv(
  "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/shortexpdata.csv")


#tangent - look at fois before the surveys are introduced====
foi <- ds.short[which(ds.short$time==490),c("time","expnum","foi_basic","foi_comm","r")] %>%
  gather(output, val, foi_basic,foi_comm) 
foi[,"output"] <- factor(foi[,"output"], 
                         labels = c("Force of primary\ninfection acquired\nwithin the community",
                                    "Force of primary\ninfection acquired\noutside the community"))

foiplot <- ggplot(foi, aes(r, val, linetype = output)) +
  geom_point(size = 2) +
  geom_line() +
  theme_bw() +
  labs(x = "Level of between community transmission",
       y = "Force of infection",
       linetype = "") +
  theme(panel.grid=element_blank(),
        legend.direction = "horizontal",
        legend.position = "top",
        legend.background = element_blank())

#Question 1: do surveys have an effect on transmission?====
#Incidence and prevalences====
inc.ds <- ds.short[,c("time", "Inc", "r","survey_interval", "k")]

inc.plot <- ggplot(inc.ds, aes(time, Inc, colour = as.factor(k))) +
  geom_line() +
  facet_grid(r~survey_interval, drop=T) +
  xlim(c(499,540)) +
  ylim(c(118,127.5)) +
  theme_bw()

prev.ds <- ds.short[,c("time", "Prev", "r", "survey_interval", "k")]
prev.plot <- ggplot(prev.ds, aes(time, Prev, colour = as.factor(k))) +
  geom_line() +
  facet_grid(r~survey_interval, drop=T, scale = "free_x") +
  xlim(c(499,530)) +
  theme_bw()

rm(inc.ds, prev.ds)

cat.plot <- rbind(subset(ds.short, expnum == getexpnum(c(0.1,0.8,0.6,10))),
                  subset(ds.short, expnum == getexpnum(c(0.4,0.8,0.6,10))),
                  subset(ds.short, expnum == getexpnum(c(0.7,0.8,0.6,10))))%>%
  .[,c("time", "Ls", "Lf", "I", "N","r")] %>%
  gather(.,output, val, -time, -r)

cat.plot.g <- ggplot(cat.plot, aes(time, val * 100000, 
                                   col = as.factor(output), 
                                   linetype = as.factor(r))) +
  geom_line(size = 2) +
  xlim(c(499, 510)) +
  facet_grid(output~., scales = "free_y") +
  scale_linetype_manual(values = c("solid", "dotted", "dashed")) +
  theme_bw()

ds.foi <- rbind(subset(ds.short, expnum == getexpnum(c(0.1,0.8,0.6,10))),
                  subset(ds.short, expnum == getexpnum(c(0.4,0.8,0.6,10))),
                  subset(ds.short, expnum == getexpnum(c(0.7,0.8,0.6,10))))%>%
  .[,c("time", "foi_basic","foi_comm","r")] %>%
  gather(., output, val, foi_basic, foi_comm)

foi.basic.plot <- ggplot(ds.foi,aes(time, val, linetype=output, col=as.factor(r))) +
  geom_line() +
  xlim(c(499, 510))

ds.inc.recvsold <- rbind(subset(ds.short, expnum == getexpnum(c(0.1,0.8,0.6,10))),
                subset(ds.short, expnum == getexpnum(c(0.4,0.8,0.6,10))),
                subset(ds.short, expnum == getexpnum(c(0.7,0.8,0.6,10))))%>%
  .[,c("time", "Inc_recent", "Inc_relap","r")] %>%
  gather(., output, val, -time, -r)

Inc.recentvrelap.plot <- ggplot(ds.inc.recvsold,aes(time, val, linetype=output)) +
  geom_line() +
  xlim(c(499, 510)) +
  facet_grid(r~., scale = "free_y")


#Duration of infection====

durdata <- subset(ds.short.time, survey_interval == 1)[,c(1:6,31,32)] %>%
  subset(., r==0.4) %>%
  with(.,rbind(subset(., cov == 0.3), subset(., cov==0.9)))%>%
  with(.,rbind(subset(.,k==0.4), subset(., k==0.8))) %>%
  gather(., 
  output, val, dur_active_TB, dur_active_inf_TB) 

durdata[,"output"] <- factor(durdata[,"output"], labels = c("Duration of \nactive, infectious \ndisease",
                                                            "Duration of \nactive disease"
                                                            ))
durdata[,"k"] <- factor(durdata[,"k"], labels = c("Low linkage \nto treatment",
                                                  "High linkage \nto treatment"))
durdata[,"cov"] <- factor(durdata[,"cov"], labels = c("Low survey coverage",
                                                  "High survey coverage"))

duractiveexpplot <- ggplot(durdata, aes(time, val, linetype = output, col = as.factor(k))) +
  geom_line()+
  facet_grid(cov~.) +
  scale_x_continuous(breaks = seq(500, 504), limits = c(500, 504)) +
  scale_y_continuous(breaks = seq(0.6,1.1,0.1), limits = c(0.6,1.1)) +
  theme_bw() +
  labs(linetype = "", col ="", y = "Duration in years", x = "Time in years") +
  theme(legend.key = element_blank(), legend.direction = "horizontal",
        legend.position = "top",
        panel.grid = element_blank())
  
  

#Question 2: do surveys have a detectable effect on transmission?====
#Measures of incidence and prevalence====

ds.st.survey.only[,"r"] <- factor(ds.st.survey.only[,"r"], labels = c("Low BCT,\n r=0.1",
                                                                      "Medium BCT,\n r=0.4",
                                                                      "High BCT,\n r=0.7"))
ds.st.survey.only[,"survey_interval"] <- factor(ds.st.survey.only[,"survey_interval"], 
                                                labels = c("1 year interval",
                                                "3 year interval",
                                                "5 year interval",
                                                "10 year interval"))

inc.so.plot <- ggplot(ds.st.survey.only, aes(time, Inc, colour = as.factor(k))) +
  geom_point(size = 2) +
  facet_grid(r~survey_interval, drop=T, scale = "free_x")+
  theme_bw() +
  geom_line() +
  labs(x = "Time in years", y = "Incidence of active\n disease per 100,000",
       colour = "Survey efficiency") +
  scale_colour_manual(labels = c("Low", "Medium", "High"), values = c("blue", "green", "red"))

prev.so.plot <- ggplot(ds.st.survey.only, aes(time, Prev, colour = as.factor(k))) +
  geom_point(size = 2) +
  facet_grid(r~survey_interval, drop=T, scale = "free_x")+
  theme_bw() +
  geom_line() +
  labs(x = "Time in years", y = "Prevalence of active\n disease per 100,000",
       colour = "Survey efficiency") +
  scale_colour_manual(labels = c("Low", "Medium", "High"), values = c("blue", "green", "red"))

#Get % changes between 1st and 2nd surveys====

percentchange <- function (x, y){
  ((y-x)/x) * 100
}

pc.expnums <- parametercombinations(list(k=c(0.6,0.8),
                                          cov=c(0.3,0.6),
                                          survey_interval=c(1,3,5,10))) %>%
  cbind(r = rep(0.4,dim(.)[1]),.) %>%
  apply(., 1, function(x) getexpnum(x))

ds.survey.only <- read.csv(
  "C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/expdatasurveytimesonly.csv"
)

ds.pc <- ds.survey.only[which(apply(ds.survey.only,1,
                                    function(x)x["expnum"] %in% pc.expnums)),]
ds.pc.tidy <- ds.pc[,c(1:5,7,21,25,29,30,34)] %>%
  gather(., output, val, Inc, Inc_recent, Prev, Inf_prev)
ds.pc.tidy <- cbind(ds.pc.tidy, 
        surveytype = apply(ds.pc.tidy[,c("k","cov")], 1, function(i){
          which(apply(unique(ds.pc.tidy[,c("k","cov")]),1,function(x){
            all(x==i)}))
        }))
ds.pc.tidy[,"time"] <- factor(rep(c(1,2,3), length.out=dim(ds.pc.tidy)[1]))
 
ds.pc.tidy.2 <- subset(ds.pc.tidy, time==2)

p.changes <- daply(ds.pc.tidy, 
                  .(output), function(i) daply(i, 
                                               .(expnum), function(x){
                                                 percentchange(x[1,"val"], x[2,"val"])}))
ds.pc.tidy.2 <- rbind(cbind(subset(ds.pc.tidy.2, output=="Inc"), p.change=p.changes["Inc",]),
                      cbind(subset(ds.pc.tidy.2, output=="Prev"), p.change=p.changes["Prev",])) 

p.change.plot.Inc <- ggplot(subset(ds.pc.tidy.2,output=="Inc"), 
                        aes(survey_interval, p.change,
                            linetype = as.factor(cov * 100),
                            shape = as.factor(k * 100))) + 
  geom_point() +
  geom_line() +
  scale_linetype_manual(values = c(1,3,4,5)) +
  scale_x_continuous(breaks = c(1,3,5,10)) +
  theme_bw() +
  labs(x="Survey interval", y="Percent change", 
       linetype = "Survey coverage",
       shape = "Linkage to treatment")

p.change.plot.Prev <- ggplot(subset(ds.pc.tidy.2,output=="Prev"), 
                            aes(survey_interval, p.change,
                                linetype = as.factor(cov * 100),
                                shape = as.factor(k * 100))) + 
  geom_point() +
  geom_line() +
  scale_linetype_manual(values = c(1,3,4,5)) +
  scale_x_continuous(breaks = c(1,3,5,10)) +
  theme_bw() +
  labs(x="Survey interval", y="Percent change", 
       linetype = "Survey coverage",
       shape = "Linkage to treatment")

#Would this change be detectable?====
#Sample sizes calculation
#sample size function: for 80% power and 5% significance. Assumed SD
sampsize <- function(diff, sd){
  (0.84 + 1.96)^2 * ((2 * sd)^2)/(diff^2)
}

~
diff.df <- ds.survey.only[,c("r","k","cov","survey_interval","Inc", "Prev", "expnum")] %>%
  .[which(!(1:dim(.)[1]%%3==0)),]

diffs <- ddply(diff.df, .(expnum), function(x){
  c(originc = x[1,"Inc"]-x[1,"Inc"], incdiff = x[1,"Inc"]-x[2,"Inc"],
    origprev = x[1,"Prev"]-x[1,"Prev"], prevdiff = x[1,"Prev"]-x[2,"Prev"])
})

diff.df <- ddply(diff.df, .(expnum), function(x) {
  data.frame(x[2,], diffs[which(diffs$expnum==x$expnum),c("incdiff", "prevdiff")])
  }) %>%
  cbind(., sampsizeinc = sampsize(.[,"incdiff"], 50),
        sampsizeprev = sampsize(.[,"prevdiff"], 50))

sampsizebydiff.inc <- ggplot(diff.df, aes(prevdiff, sampsizeprev, 
                    shape = as.factor(cov),
                    col = as.factor(k))) +
  geom_point(size = 3) +
  ylim(c(0,20000)) +
  facet_grid(r~survey_interval)

sampsizebydiff.prev <- ggplot(diff.df, aes(incdiff, sampsizeinc, 
                    size = as.factor(cov),
                    col = as.factor(k))) +
  geom_point() +
  ylim(c(0,20000)) +
  facet_grid(r~survey_interval)

dist.incdiff <- ggplot(diff.df, aes(incdiff)) + 
  geom_histogram() +
  facet_grid(r~survey_interval)

dist.incprev <- ggplot(diff.df, aes(prevdiff)) + 
  geom_histogram() +
  facet_grid(r~survey_interval)
