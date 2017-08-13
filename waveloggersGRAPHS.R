#waveloggers
#clear workspace 
rm(list=ls())
library(ggplot2)
library(dplyr)
#set working directory and read file 
setwd("C:/Users/Gabriela/Desktop/RUG/Project 1/DATA/waveloggersw/Results/")
d5 <- read.csv("5waves_OSSI.csv", header=T)
d8 <- read.csv("8waves_OSSI.csv", header=T)
d9 <- read.csv("9waves_OSSI.csv", header=T)
d10 <- read.csv("10waves_OSSI.csv", header=T)
d47 <- read.csv("47waves_OSSI.csv", header=T)
d51 <- read.csv("51waves_OSSI.csv", header=T)


#put all data together
d <- rbind(d5,d8,d9,d10,d47,d51)
d
names(d)


#make a summary of each variable (min, mean, max, std dev., std. error)
library(plyr)
library(reshape2)

#delete columns "bursts" and "date"
names(d)
d2 <- d[,c(-3,-4,-5,-6,-7)]
names(d2)


#method for getting the summary 
melted <- melt(d2, id.vars=c("meters","treatment"))
summary <- ddply(melted,c("meters","treatment","variable"),summarise,
      mean = mean(value), min= min(value), max=max(value), sd= sd(value),
      se =sd(value)/sqrt(length(value)))
#saving it as a file 
write.csv(summary, file="summary.csv")
names(summary)


xid <- data.frame(xintercept = c(0.23,3.23, 6.23,9.23), lty=factor(1))


hmax <- summary[summary$variable=="maxWaterHeight_m", c("meters","treatment","mean","min","max","sd","se")]
hmax
limitshmax <- aes(ymax = hmax$mean + hmax$se,
               ymin = hmax$mean - hmax$se)

ggplot(data = hmax, aes(x = meters, y = mean, color = treatment, shape=treatment)) +
  geom_point(size = 3) +
  geom_errorbar(limitshmax) +
  scale_color_manual(values=c("navy","seagreen3"))+
  labs(x = "distance (m.)", y = "maximum wave height (m.)") +
  ggtitle("Maximum Wave Height") +
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=0.6) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")

#summary of the storm 

sd5 <- read.csv("Stormy5waves_OSSI.csv", header=T)
sd9 <- read.csv("Stormy9waves_OSSI.csv", header=T)
sd47 <- read.csv("Stormy47waves_OSSI.csv", header=T)
sd51 <- read.csv("Stormy51waves_OSSI.csv", header=T)


#put all data together
dstorm <- rbind(sd5,sd9,sd47,sd51)
dstorm

#delete columns "bursts" and "date"
names(dstorm)
d2storm <- dstorm[,c(-3,-4,-5,-6,-7)]
names(d2storm)


#method for getting the summary 
melted <- melt(d2storm, id.vars=c("meters","treatment"))
stormsummary <- ddply(melted,c("meters","treatment","variable"),summarise,
                 mean = mean(value), min= min(value), max=max(value), sd= sd(value),
                 se =sd(value)/sqrt(length(value)))
#saving it as a file 
write.csv(stormsummary, file="summary.csv")
names(stormsummary)


stormhmax <- stormsummary[stormsummary$variable=="maxWaterHeight_m", c("meters","treatment","mean","min","max","sd","se")]
stormhmax

limitsstormhmax <- aes(ymax = stormhmax$mean + stormhmax$se,
                  ymin = stormhmax$mean - stormhmax$se)

ggplot(data = stormhmax, aes(x = meters, y = mean, color = treatment, shape=treatment)) +
  geom_point(size = 3) +
  geom_errorbar(limitsstormhmax) +
  scale_color_manual(values=c("navy","seagreen3"))+
  labs(x = "distance (m.)", y = "maximum wave height (m.)") +
  ggtitle("Maximum Wave Height") +
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=0.6) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")



#______________________________________________________________________________________
mean()#get names of columns
names(d5)
d5mean <- d5 %>% group_by(meters, treatment) %>% 
  summarize(ymin = min(h_mean_m),
            ymax = max(h_mean_m),
            ymean = mean(h_mean_m))

d8mean <- d8 %>% group_by(meters, treatment) %>% 
  summarize(ymin = min(h_mean_m),
            ymax = max(h_mean_m),
            ymean = mean(h_mean_m))

d9mean <- d9 %>% group_by(meters, treatment) %>% 
  summarize(ymin = min(h_mean_m),
            ymax = max(h_mean_m),
            ymean = mean(h_mean_m))

d10mean <- d10 %>% group_by(meters, treatment) %>% 
  summarize(ymin = min(h_mean_m),
            ymax = max(h_mean_m),
            ymean = mean(h_mean_m))

d47mean <- d47 %>% group_by(meters, treatment) %>% 
  summarize(ymin = min(h_mean_m),
            ymax = max(h_mean_m),
            ymean = mean(h_mean_m))

d51mean <- d51 %>% group_by(meters, treatment) %>% 
  summarize(ymin = min(h_mean_m),
            ymax = max(h_mean_m),
            ymean = mean(h_mean_m))


d5max <- d5 %>% group_by(meters, treatment) %>% 
  summarize(ymin = min(h_max_m),
            ymax = max(h_max_m),
            ymean = mean(h_max_m))

d8max <- d8 %>% group_by(meters, treatment) %>% 
  summarize(ymin = min(h_max_m),
            ymax = max(h_max_m),
            ymean = mean(h_max_m))

d9max <- d9 %>% group_by(meters, treatment) %>% 
  summarize(ymin = min(h_max_m),
            ymax = max(h_max_m),
            ymean = mean(h_max_m))

d10max <- d10 %>% group_by(meters, treatment) %>% 
  summarize(ymin = min(h_max_m),
            ymax = max(h_max_m),
            ymean = mean(h_max_m))

d47max <- d47 %>% group_by(meters, treatment) %>% 
  summarize(ymin = min(h_max_m),
            ymax = max(h_max_m),
            ymean = mean(h_max_m))

d51max <- d51 %>% group_by(meters, treatment) %>% 
  summarize(ymin = min(h_max_m),
            ymax = max(h_max_m),
            ymean = mean(h_max_m))

d5ov <- d5 %>% group_by(meters, treatment) %>% 
  summarize(ymin = min(orbital_velocity_m_sec),
            ymax = max(orbital_velocity_m_sec),
            ymean = mean(orbital_velocity_m_sec))

d8ov <- d8 %>% group_by(meters, treatment) %>% 
  summarize(ymin = min(orbital_velocity_m_sec),
            ymax = max(orbital_velocity_m_sec),
            ymean = mean(orbital_velocity_m_sec))

d9ov <- d9 %>% group_by(meters, treatment) %>% 
  summarize(ymin = min(orbital_velocity_m_sec),
            ymax = max(orbital_velocity_m_sec),
            ymean = mean(orbital_velocity_m_sec))

d10ov <- d10 %>% group_by(meters, treatment) %>% 
  summarize(ymin = min(orbital_velocity_m_sec),
            ymax = max(orbital_velocity_m_sec),
            ymean = mean(orbital_velocity_m_sec))

d47ov <- d47 %>% group_by(meters, treatment) %>% 
  summarize(ymin = min(orbital_velocity_m_sec),
            ymax = max(orbital_velocity_m_sec),
            ymean = mean(orbital_velocity_m_sec))

d51ov <- d51 %>% group_by(meters, treatment) %>% 
  summarize(ymin = min(orbital_velocity_m_sec),
            ymax = max(orbital_velocity_m_sec),
            ymean = mean(orbital_velocity_m_sec))

d5bss <- d5 %>% group_by(meters, treatment) %>% 
  summarize(ymin = min(BSS_Pa),
            ymax = max(BSS_Pa),
            ymean = mean(BSS_Pa))

d8bss <- d8 %>% group_by(meters, treatment) %>% 
  summarize(ymin = min(BSS_Pa),
            ymax = max(BSS_Pa),
            ymean = mean(BSS_Pa))

d9bss <- d9 %>% group_by(meters, treatment) %>% 
  summarize(ymin = min(BSS_Pa),
            ymax = max(BSS_Pa),
            ymean = mean(BSS_Pa))

d10bss <- d10 %>% group_by(meters, treatment) %>% 
  summarize(ymin = min(BSS_Pa),
            ymax = max(BSS_Pa),
            ymean = mean(BSS_Pa))

d47bss <- d47 %>% group_by(meters, treatment) %>% 
  summarize(ymin = min(BSS_Pa),
            ymax = max(BSS_Pa),
            ymean = mean(BSS_Pa))

d51bss <- d51 %>% group_by(meters, treatment) %>% 
  summarize(ymin = min(BSS_Pa),
            ymax = max(BSS_Pa),
            ymean = mean(BSS_Pa))


#d51 <- d51 %>% group_by(meters, treatment) %>% 
 # summarise_each(funs(mean))


#merging  data sets 
dmean <- rbind(d5mean,d8mean,d9mean,d10mean,d47mean,d51mean) 

dmax <- rbind(d5max,d8max,d9max,d10max,d47max,d51max)

dov <- rbind(d5ov,d8ov,d9ov,d10ov,d47ov,d51ov)

dbss <- rbind(d5bss,d8bss,d9bss,d10bss,d47bss,d51bss)


pdf("wavesimplebars.pdf", width=6 , height=4 , paper='special')


ggplot(dmean, aes(y = ymean, x=meters, color = treatment, shape = treatment)) +
  geom_point(size = 3) +
  scale_color_manual(values=c("navy","seagreen3"))+
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) +
  theme_light (base_size = 12, base_family = "") + 
  labs(title= "Mean wave height", x= "distance in meters", y="mean wave height (m.)", color="Treatment", shape="Treatment")



ggplot(dmax, aes(x = meters, y = ymean, color = treatment, shape = treatment)) +
  geom_point(size = 3) +
  scale_color_manual(values=c("navy","seagreen3"))+
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) +
  theme_light (base_size = 12, base_family = "") + 
  labs(title= "Maximum wave height", x= "distance in meters", y="maximum wave height (m.)", color="Treatment", shape="Treatment")

ggplot(dov, aes(x = meters, y = ymean, color = treatment, shape = treatment)) +
  geom_point(size = 3) +
  scale_color_manual(values=c("navy","seagreen3"))+
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) +
  theme_light (base_size = 12, base_family = "") + 
  labs(title= "Orbital Velocity", x= "distance in meters", y="orbital velocity (m/s)", color="Treatment", shape="Treatment")

ggplot(dbss, aes(x = meters, y = ymean, color = treatment, shape = treatment)) +
  geom_point(size = 3) +
  scale_color_manual(values=c("navy","seagreen3"))+
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) +
  theme_light (base_size = 12, base_family = "") + 
  labs(title= "Bed shear stress", x= "distance in meters", y="bed shear stress (Pa)", color="Treatment", shape="Treatment")


dev.off()
