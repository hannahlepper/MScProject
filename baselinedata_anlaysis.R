#Baseline data analysis

library(ggplot2)
library(tidyr)
library(readr)
library(gridExtra)

#Get data
bdatashort <- read.csv("C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/shortbaselinedata.csv")

#Treatment cov * 100
bdatashort[,"treatment_cov"] <- bdatashort[,"treatment_cov"]*100

bdatatidy <- gather(bdatashort, output, val, -.id, -X, -time, -r)[,c("r", "output", "val")]

#Targets v model====
targets <- data.frame(type = rep("target", 12),
                      output = rep(c("Inc", "Prev", "Mort", "treatment_cov"), 3),
                      val = rep(c(112.3, 162.4, 15.1, 79.0), 3),
                      ymin = rep(c(84.46, 162.4, 9.88, 65), 3),
                      ymax = rep(c(127.1, 162.4, 21.78, 98),3),
                      m = rep("n", 12))

tidyincdata <- gather(bdatashort[,c(c("Inc", "Prev", "Mort", "treatment_cov"))],
                     output, val)
fit.bdata.tidy <- rbind(cbind(
  tidyincdata,
  m = rep("y", 12),
  type = rep(c("1low_bct", "2medium_bct", "3high_bct"), 4),
  ymin = tidyincdata$val,
  ymax = tidyincdata$val),
  targets)

dummy <- data.frame(output = rep(c("Prev", "Inc", "Mort", "treatment_cov"),each = 4, length.out = 32),
                    type = rep(c("target", "1low_bct", "2medium_bct", "3high_bct"), 4, length.out = 32),
                    val = c(rep(c(200, 170, 50, 100),each = 4), 
                            rep(c(100, 50, 5, 50),each = 4)))

tvmplotlabels <- c("Low (r=0.1)",
                   "Medium (r=0.4)",
                   "High (r=0.7)",
                   "Target value")

output_labels <- list("Inc" = "Incidence per 100,000",
                      "Mort" = "TB related mortality per 100,000",
                      "Prev" = "Prevalence of active pulmonary disease per 100,000",
                      "treatment_cov" = "Treatment coverage, %")

output_labeller <- function(variable,value){
  return(output_labels[value])
}

targetvmodel <- ggplot() +
  geom_blank(data = dummy, mapping = aes(type, val)) +
  facet_wrap(~output, scales = "free_y",
             labeller = as_labeller(output_labeller)) +
  geom_point(data=subset(fit.bdata.tidy, m == "y"), 
             mapping=aes(type, val),
             shape = 4,size = 2) +
  geom_point(data = subset(fit.bdata.tidy, m == "n"),
             mapping = aes(x = type, y = val), 
             size = 2, shape = 4) +
  geom_errorbar(data = subset(fit.bdata.tidy, m == "n"),
                  mapping = aes(x = type, y = val, ymin = ymin, ymax = ymax),
                width = 0.3) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        strip.background = element_rect(fill = "white")) +
  scale_x_discrete(labels = tvmplotlabels) +
  geom_vline(data = dummy, mapping = aes(x=type, y=val), xintercept = 3.5)


#Incidences across scenarios - not used====                         
# bdataincidence <- getanouputsubset(c("Inc", "Inc_recent", "Inc_first", "Inc_react",
#                                      "Inc_relap"), bdatatidy)
# incidenceplot <- ggplot(bdataincidence, aes(as.factor(r), val)) +
#   geom_bar(stat="identity") +
#   facet_grid(.~output, scale = "free") +
#   xlab("Level of between community mixing, r") +
#   ylab("Number per 100,000") +
#   labs(title = "Effect of level of between community transmission on\n different types of incidence")
# 
# incpercentchange <- ggplot(bdataincidence, aes(output, p.change)) +
#   geom_bar(stat="identity") +
#   facet_grid(.~r, scale = "free")
# 
# #add in infectious prevalence
# inf_case_prev <- function(I, N) {I + 0.22*N}
# bd_inf_prev <- data.frame(r = c(0.1, 0.4, 0.7),
#                      val = inf_case_prev(getanouputsubset("I", bdatatidy)[,"val"],
#                                    getanouputsubset("N", bdatatidy)[,"val"]))
# infectiouscaseprevalence <- ggplot(bd_inf_prev, aes(r, val)) +
#   geom_point(stat = "identity") +
#   ylim(c(0.0006,0.0008))
# 
# mean(bd_inf_prev$val)

#To steady data

# tosteadydata <- read.csv("C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/tosteadydata.csv")
# 
# tosteady <- ggplot(tosteadydata, mapping = aes(time, Prev, colour = as.factor(r))) +
#   geom_line() +
#   xlim(c(0, 5))
#This sort of works but not well enough to go into the diss: need to plot
#time to steady by r instead of prev by time
#Is there any difference in average durations?====
duractiveplot <- ggplot(bdatatidy[which(bdatatidy$output=="dur_active_TB" |
                         bdatatidy$output=="dur_active_inf_TB"),],
       aes(r, val, shape=output)) +
  geom_point(size = 2) +
  geom_line()+
  ylim(1,1.1)
  
#Time taken to steady

tosteadyrun <- function(pars) {
  y <- c(U=1-0.2,Ls=0.99*0.2,Lf=0,I=0.01*0.2,N=0,C=0)
  initrun<-runsteady(y=y,times=c(0,Inf), func=PSmodel, parms=convertdftonumeric(pars))
  y<-initrun$y
  newinit <- c(y["U"] + (y["I"]*0.1)+ (y["N"]*0.1),
               y["Ls"],y["Lf"],
               y["I"]-(y["I"]*0.1),
               y["N"] - (y["N"]*0.1),
               y["C"])
  runsteady(y=newinit,times=c(0,Inf), func=PSmodel, parms=convertdftonumeric(pars))
}

tosteady <- apply(fittedparams, 1, function(x) attr(tosteadyrun(x),"time"))
tosteadydata <- data.frame(cbind(time = tosteady,
                      r = c(0.1,0.4,0.7)))

tosteadyplot <- ggplot(tosteadydata, aes(r, time)) +
  geom_line() +
  geom_point() +
  labs(y = "Time to steady state", x = "Level of between community transmission") +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid = element_blank()) +
  scale_y_continuous(limits = c(0,1100),
                     breaks = seq(0,1100, 200))

#Comparison of categories====

#df: uninfected, latent infected, active infected, r


cat.df <- gather(bdatashort[,c("U","Lf","Ls","I","N", "C")],output, val) %>%
  cbind(., 
                type = c(rep("Uninfected", 3), rep("Latently infected", 6), 
                         rep("Actively infected", 6), rep("Recovered", 3)),
                r = rep(c(0.1,0.4,0.7), 6)) %>%
  ddply(., .(type, r), function(x) sum(x[,"val"])) %>%
  setNames(.,c("type", "r", "val"))


cat.plot <- ggplot(cat.df, aes(r, val * 100, linetype = as.factor(type))) +
  geom_point() +
  geom_line() +
  facet_grid(type~., scale = "free_y") 
  

#FOIs====
foi <- bdatashort[,c("foi_basic","foi_comm","r")] %>%
  gather(.,output, val, foi_basic,foi_comm) 
foi[,"output"] <- factor(foi[,"output"], 
                         labels = c("Force of primary\ninfection acquired\nwithin the community\n",
                                    "Force of primary\ninfection acquired\noutside the community\n"))

foiplot <- ggplot(foi, aes(r, val, linetype = output)) +
  geom_point(size = 2) +
  geom_line() +
  theme_bw() +
  labs(x = "Level of between community transmission",
       y = "Force of infection",
       linetype = "") +
  theme(panel.grid=element_blank(),
        legend.background = element_blank()) +
  scale_x_continuous(breaks = c(0.1,0.4,0.7))

#Time to steady

tosteadydata <- read.csv("C://Users/hanna/Dropbox/Academic/LSHTM/Project/Inputs and outputs/tosteadydata.csv")

tosteadyplot <- ggplot(tosteadydata, aes(r, time)) +
  geom_line() +
  geom_point() +
  labs(x = "Level of between community transmission",
       y = "Time in years to steady state") +
  theme_bw() +
  scale_x_continuous(breaks =c(0.1,0.4,0.7)) +
  scale_y_continuous(breaks = seq(500,1200,100))
