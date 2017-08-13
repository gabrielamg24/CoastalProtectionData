#clear environment
rm(list=ls())
library(ggplot2)
library(dplyr)
library(lme4)
library(lmerTest)
#set working directory and call file 
setwd("C:/Users/Gabriela/Desktop/RUG/Project 1/DATA/gypsum sticks/")
d <- read.csv("gypsum.csv", header=T)
#get file names and summary
names(d)
str(d)

#create variable "percentage weight loss" by (lost_weight/beforeweight*100)
d$percentageLoss= (d$Lost_weight/d$Before_weigh)*100
d

#CREATE a copy of the data for the count analysis later which required also NA values 
dcounts <- d 

#cREATE a copy of the data for comparing the NMB & South Bese 
dcomp <- d 
dcomp

#remove NA (not avaialable points)
d <- d[-which(is.na(d$percentageLoss)),]
d
names(d)
#create a copy for barplots of data 
dbar <- d

#frame for bese structures lines on plot SOUTH
xid <- data.frame(xintercept = c(0.23,3.23, 6.23,9.23), lty=factor(1))

#frame for bese structures lines on plot NMB
xidN <- data.frame(xintercept = c(0), lty=factor(1))



#_____________________________________________________________________________________________
#plotting with standard deviation

library(dplyr)

d.summary <- d %>% group_by(Meters, Treatment) %>%
  summarize(ymin = min(percentageLoss),
            ymax = max(percentageLoss),
            ymean = mean(percentageLoss))
d.summary


#summary subset data from location "south"
summsouth <- d.summary[d.summary$Treatment %in% c('Mu','Control'),]
summsouth
names(summsouth)

#subset data from location "NMB" 
summnmb <- d.summary[d.summary$Treatment %in% c('NMB','ControlNM'),]
summnmb

#open pdf file to save all percentage loss plots
pdf("GypsumPLOTSstddev.pdf", width=6 , height=4 , paper='special')

#plots
ggplot(summsouth, aes(x = Meters, y = ymean, color = Treatment, shape = Treatment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) + 
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=0.6) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")+
  labs(title= "Percentage of Lost weight  of Plaster balls SOUTH", subtitle= "standard deviation", x= "distance in meters", y="percentage dissolved gypsum", color="Treatment", shape="Treatment")

ggplot(summsouth, aes(x = Meters, y = ymean, color = Treatment, shape = Treatment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) + 
  scale_x_continuous(limits = c(-5, 25)) + 
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=1) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")+
  labs(title= "Percentage of Lost weight  of  Plaster balls SOUTH", subtitle= "-5 -> 25 m., standard deviation", x= "distance in meters", y="percentage dissolved gypsum", color="Treatment", shape="Treatment")

#consistent graphs for SOUTH (same position for BESE and Control) 
keep <- c("-5","-0.1", "4.7", "7.7", "9.55", "20", "100")
d.consistent <- subset(summsouth, Meters %in% keep)
d.consistent

ggplot(d.consistent, aes(x = Meters, y = ymean, color = Treatment, shape = Treatment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) + 
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=0.6) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")+
  labs(title= "Percentage of Lost weight  of Plaster balls SOUTH", subtitle= "Consistent & standard deviation", x= "distance in meters", y="percentage dissolved gypsum", color="Treatment", shape="Treatment")

ggplot(d.consistent, aes(x = Meters, y = ymean, color = Treatment, shape = Treatment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) + 
  scale_x_continuous(limits = c(-5, 25)) + 
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=1) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")+
  labs(title= "Percentage of Lost weight  of  Plaster balls SOUTH", subtitle= "-5 -> 25 m. & consistent & standard deviation", x= "distance in meters", y="percentage dissolved gypsum", color="Treatment", shape="Treatment")

ggplot(summnmb, aes(x = Meters, y = ymean, color = Treatment, shape = Treatment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) + 
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xidN, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=0.6) +
  scale_linetype_manual(values = 1, name="",label="Natural Mussel Bed")+
  labs(title= "Percentage of Lost weight  of  Plaster balls NMB", subtitle= "standard deviation", x= "distance in meters", y="percentage dissolved gypsum", color="Treatment", shape="Treatment")

ggplot(summnmb, aes(x = Meters, y = ymean, color = Treatment, shape = Treatment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) + 
  scale_x_continuous(limits = c(-5, 25)) + 
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xidN, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=1) +
  scale_linetype_manual(values = 1, name="",label="Natural Mussel Bed")+
  labs(title= "Percentage of Lost weight  of Plaster balls NMB", subtitle= "-5 -> 25 m., standard deviation", x= "distance in meters", y="percentage dissolved gypsum", color="Treatment", shape="Treatment")

#close pdf file with percentage loss plots 
dev.off()


#______________________________________________________________________________________________
#plots with standard ERROR

#open pdf file to save all percentage loss plots
pdf("GypsumPLOTSstderror.pdf", width=6 , height=4 , paper='special')

#subset data 
#summary subset data from location "south"
ssouth <- d[d$Treatment %in% c('Mu','Control'),]
ssouth
names(ssouth)

#subset data from location "NMB" 
snmb <- d[d$Treatment %in% c('NMB','ControlNM'),]
snmb


myDataS <- aggregate(ssouth$percentageLoss,
                     by = list(meters = ssouth$Meters, treatment = ssouth$Treatment),
                     FUN = function(x) c(mean = mean(x), sd = sd(x),
                                         n = length(x)))
myDataS <- do.call(data.frame, myDataS)
myDataS$se <- myDataS$x.sd / sqrt(myDataS$x.n)

colnames(myDataS) <- c("meters", "treatment", "mean", "sd", "n", "se")

myDataS$names <- c(paste(myDataS$meters, "meters /",
                         myDataS$treatment, " treatment"))

limits <- aes(ymax = myDataS$mean + myDataS$se,
              ymin = myDataS$mean - myDataS$se)
names(myDataS)

ggplot(myDataS, aes(x = meters, y = mean, color = treatment, shape = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(limits) + 
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_classic() + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=0.4) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")+
  labs(title= "Percentage of Lost weight  of Plaster balls SOUTH", subtitle= "standard error", x= "distance in meters", y="Percentage of lost gypsum weight", color="Treatment", shape="Treatment")

ggplot(myDataS, aes(x = meters, y = mean, color = treatment, shape = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(limits) + 
  scale_x_continuous(limits = c(-5, 25)) +
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_classic() + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=0.8) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")+
  labs(title= "Percentage of Lost weight  of Plaster balls SOUTH", subtitle= "-5 -> 25 m., standard error", x= "distance in meters", y="Percentage of Lost gypsum weight", color="Treatment", shape="Treatment")                     

#consistent graphs for SOUTH (same position for BESE and Control) 
keep <- c("-5","-0.1", "4.7", "7.7", "9.55", "20", "100")
names(myDATASconsistent)
myDATASconsistent <- subset(myDataS, meters %in% keep)
myDATASconsistent

limitscons <- aes(ymax = myDATASconsistent$mean + myDATASconsistent$se,
              ymin = myDATASconsistent$mean - myDATASconsistent$se)


ggplot(myDATASconsistent, aes(x = meters, y = mean, color = treatment, shape = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(limitscons) + 
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_classic() + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=0.4) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")+
  labs(title= "Percentage of Lost weight  of Plaster balls SOUTH", subtitle= "Consistent & standard error", x= "distance in meters", y="Percentage of Lost gypsum weight", color="Treatment", shape="Treatment")

ggplot(myDATASconsistent, aes(x = meters, y = mean, color = treatment, shape = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(limitscons) + 
  scale_x_continuous(limits = c(-5, 25)) +                      
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_classic() + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=0.8) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")+
  labs(title= "Percentage of Lost weight  of Plaster balls SOUTH", subtitle= "-5 -> 25 m., Consistent & standard error", x= "distance in meters", y="Percentage of Lost gypsum weight", color="Treatment", shape="Treatment")

#NMB
myDataN <- aggregate(snmb$percentageLoss,
                     by = list(meters = snmb$Meters, treatment = snmb$Treatment),
                     FUN = function(x) c(mean = mean(x), sd = sd(x),
                                         n = length(x)))
myDataN <- do.call(data.frame, myDataN)
myDataN$se <- myDataN$x.sd / sqrt(myDataN$x.n)

colnames(myDataN) <- c("meters", "treatment", "mean", "sd", "n", "se")

myDataN$names <- c(paste(myDataN$meters, "meters /",
                         myDataN$treatment, " treatment"))

limitsN <- aes(ymax = myDataN$mean + myDataN$se,
               ymin = myDataN$mean - myDataN$se)
names(myDataN)

ggplot(myDataN, aes(x = meters, y = mean, color = treatment, shape = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(limitsN) + 
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_classic() + 
  geom_vline(data=xidN, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=0.6) +
  scale_linetype_manual(values = 1, name="",label="Natural Mussel Bed")+
  labs(title= "Percentage of Lost weight  of Plaster balls NMB", subtitle= "standard error", x= "distance in meters", y="Percentage of Lost gypsum weight", color="Treatment", shape="Treatment")

ggplot(myDataN, aes(x = meters, y = mean, color = treatment, shape = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(limitsN) + 
  scale_x_continuous(limits = c(-5, 25)) +
  scale_color_manual(values=c("navy","seagreen3"))+
  geom_vline(data=xidN, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=1) +
  scale_linetype_manual(values = 1, name="",label="Natural Mussel Bed")+
  theme_classic() + 
  labs(title= "Percentage of Lost weight  of Plaster balls NMB", subtitle= "-5 -> 25 m., standard error", x= "distance in meters", y="Percentage of Lost gypsum weight", color="Treatment", shape="Treatment")                     



#close pdf file with percentage loss plots 
dev.off()

#______________________________________________________________________________________________
#plots with standard ERROR WITH GRID 

#open pdf file to save all percentage loss plots
pdf("GypsumPLOTSstderrorGRID.pdf", width=6 , height=4 , paper='special')

#subset data 
#summary subset data from location "south"
ssouth <- d[d$Treatment %in% c('Mu','Control'),]
ssouth
names(ssouth)

#subset data from location "NMB" 
snmb <- d[d$Treatment %in% c('NMB','ControlNM'),]
snmb


myDataS <- aggregate(ssouth$percentageLoss,
                     by = list(meters = ssouth$Meters, treatment = ssouth$Treatment),
                     FUN = function(x) c(mean = mean(x), sd = sd(x),
                                         n = length(x)))
myDataS <- do.call(data.frame, myDataS)
myDataS$se <- myDataS$x.sd / sqrt(myDataS$x.n)

colnames(myDataS) <- c("meters", "treatment", "mean", "sd", "n", "se")

myDataS$names <- c(paste(myDataS$meters, "meters /",
                         myDataS$treatment, " treatment"))

limits <- aes(ymax = myDataS$mean + myDataS$se,
              ymin = myDataS$mean - myDataS$se)
names(myDataS)

ggplot(myDataS, aes(x = meters, y = mean, color = treatment, shape = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(limits) + 
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=0.4) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")+
  labs(title= "Percentage of Lost weight  of Plaster balls SOUTH", subtitle= "standard error", x= "distance in meters", y="Percentage of lost gypsum weight", color="Treatment", shape="Treatment")

ggplot(myDataS, aes(x = meters, y = mean, color = treatment, shape = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(limits) + 
  scale_x_continuous(limits = c(-5, 25)) +
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=0.8) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")+
  labs(title= "Percentage of Lost weight  of Plaster balls SOUTH", subtitle= "-5 -> 25 m., standard error", x= "distance in meters", y="Percentage of Lost gypsum weight", color="Treatment", shape="Treatment")                     

#consistent graphs for SOUTH (same position for BESE and Control) 
keep <- c("-5","-0.1", "4.7", "7.7", "9.55", "20", "100")
names(myDATASconsistent)
myDATASconsistent <- subset(myDataS, meters %in% keep)
myDATASconsistent

limitscons <- aes(ymax = myDATASconsistent$mean + myDATASconsistent$se,
                  ymin = myDATASconsistent$mean - myDATASconsistent$se)


ggplot(myDATASconsistent, aes(x = meters, y = mean, color = treatment, shape = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(limitscons) + 
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=0.4) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")+
  labs(title= "Percentage of Lost weight  of Plaster balls SOUTH", subtitle= "Consistent & standard error", x= "distance in meters", y="Percentage of Lost gypsum weight", color="Treatment", shape="Treatment")

ggplot(myDATASconsistent, aes(x = meters, y = mean, color = treatment, shape = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(limitscons) + 
  scale_x_continuous(limits = c(-5, 25)) +                      
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=0.8) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")+
  labs(title= "Percentage of Lost weight  of Plaster balls SOUTH", subtitle= "-5 -> 25 m., Consistent & standard error", x= "distance in meters", y="Percentage of Lost gypsum weight", color="Treatment", shape="Treatment")

#NMB
myDataN <- aggregate(snmb$percentageLoss,
                     by = list(meters = snmb$Meters, treatment = snmb$Treatment),
                     FUN = function(x) c(mean = mean(x), sd = sd(x),
                                         n = length(x)))
myDataN <- do.call(data.frame, myDataN)
myDataN$se <- myDataN$x.sd / sqrt(myDataN$x.n)

colnames(myDataN) <- c("meters", "treatment", "mean", "sd", "n", "se")

myDataN$names <- c(paste(myDataN$meters, "meters /",
                         myDataN$treatment, " treatment"))

limitsN <- aes(ymax = myDataN$mean + myDataN$se,
               ymin = myDataN$mean - myDataN$se)
names(myDataN)

ggplot(myDataN, aes(x = meters, y = mean, color = treatment, shape = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(limitsN) + 
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xidN, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=0.6) +
  scale_linetype_manual(values = 1, name="",label="Natural Mussel Bed")+
  labs(title= "Percentage of Lost weight  of Plaster balls NMB", subtitle= "standard error", x= "distance in meters", y="Percentage of Lost gypsum weight", color="Treatment", shape="Treatment")

ggplot(myDataN, aes(x = meters, y = mean, color = treatment, shape = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(limitsN) + 
  scale_x_continuous(limits = c(-5, 25)) +
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xidN, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=1) +
  scale_linetype_manual(values = 1, name="",label="Natural Mussel Bed")+
  labs(title= "Percentage of Lost weight  of Plaster balls NMB", subtitle= "-5 -> 25 m., standard error", x= "distance in meters", y="Percentage of Lost gypsum weight", color="Treatment", shape="Treatment")                     


#close pdf file with percentage loss plots 
dev.off()


#_______________________________________________________________________________________________
#BAR plots

#open pdf file to save all percentage loss plots
pdf("GypsumPLOTSbar.pdf", width=6 , height=4 , paper='special')


#BAR PLOTS with error bars
names(dbar)
dbar <- aggregate(dbar$percentageLoss,
                  by = list(meters = dbar$Meters, treatment = dbar$Treatment),
                  FUN = function(x) c(mean = mean(x), sd = sd(x),
                                      n = length(x)))
dbar <- do.call(data.frame, dbar)
dbar$se <- dbar$x.sd / sqrt(dbar$x.n)

colnames(dbar) <- c("meters", "treatment", "mean", "sd", "n", "se")

dbar$names <- c(paste(dbar$meters, "meters /",
                      dbar$treatment, " treatment"))
dbar

barsouth <- dbar[dbar$treatment %in% c('Mu','Control'),]
barnmb <- dbar[dbar$treatment %in% c('NMB','ControlNM'),]

#SOUTH bar plots 
limitssouth <- aes(ymax = barsouth$mean + barsouth$se,
                   ymin = barsouth$mean - barsouth$se)

psouth <- ggplot(data = barsouth, aes(x = factor(meters), y = mean,
                                      fill = factor(treatment)))

psouth + geom_bar(stat = "identity",
                  position = position_dodge(0.9)) +
  geom_errorbar(limitssouth, position = position_dodge(0.9),
                width = 0.25) +
  labs(x = "distance (m.)", y = "Percentage of Lost gypsum weight") +
  ggtitle("Plaster balls south") +
  theme_light (base_size = 12, base_family = "") + 
  scale_fill_manual(values=c("darkseagreen2","lightblue3"))

barconsistent <- subset(barsouth, meters %in% keep)

limitsconssouth <- aes(ymax = barconsistent$mean + barconsistent$se,
                       ymin = barconsistent$mean - barconsistent$se)

pconsistent <- ggplot(data = barconsistent, aes(x = factor(meters), y = mean,
                                                fill = factor(treatment)))

pconsistent  + geom_bar(stat = "identity",
                        position = position_dodge(0.9)) +
  geom_errorbar(limitsconssouth, position = position_dodge(0.9),
                width = 0.25) +
  labs(x = "distance (m.)", y = "Percentage of Lost gypsum weight") +
  ggtitle("Plaster balls consistent south") +
  scale_fill_manual(values=c("darkseagreen2","lightblue3"))+
  theme_light (base_size = 12, base_family = "") 



#NMB bar plots 
limitsnmb <- aes(ymax = barnmb$mean + barnmb$se,
                 ymin = barnmb$mean - barnmb$se)

pnmb <- ggplot(data = barnmb, aes(x = factor(meters), y = mean,
                                  fill = factor(treatment)))

pnmb + geom_bar(stat = "identity",
                position = position_dodge(0.9)) +
  geom_errorbar(limitsnmb, position = position_dodge(0.9),
                width = 0.1) +
  labs(x = "distance (m.)", y = "Percentage of Lost gypsum weight") +
  ggtitle("Plaster balls NMB") +
  scale_fill_manual(values=c("darkseagreen2","lightblue3")) +
  theme_light (base_size = 12, base_family = "") 


#close pdf file with percentage loss plots 
dev.off()

#______________________________________________________________________________________________-----

#COUNT PLOT get plot for present gypsum sticks

#count the frequency of retrieved plaster balls per treatment and per distance 
detach("package:dplyr", unload=TRUE)
library(plyr)
names(dcounts)
dcounts

#the "count" function does not work when the package "dplyr" is active 
count = count(dcounts, c('Treatment','Meters','Retrieved'))
count

#subset for SOUTHERN SITE
countsouth <- count[count$Treatment %in% c('Mu','Control'),]
names(countsouth)
countsouth

#subset for NMB
countnmb <- count[count$Treatment %in% c('NMB','ControlNM'),]
countnmb

#activate dplyr to turn the variable meters from numerical to categorical 
library(dplyr)

countsouth <- countsouth %>%
  mutate(Meters = as.factor(Meters))

countnmb <- countnmb%>%
  mutate(Meters = as.factor(Meters))

#open pdf file to save all count plots
pdf("GypsumPLOTScounts.pdf", width=6 , height=4 , paper='special')


#get a grouped stacked barplot of retrieval frequency  in SOUTH
countsouthplot <- ggplot() +
  geom_bar(data=countsouth, aes(y = freq, x = Meters, fill = Retrieved), stat="identity",
           position='stack') +
  scale_fill_manual(values=c("darkseagreen2","lightblue3")) +
  theme_light (base_size = 12, base_family = "") + 
  facet_grid( ~ Treatment) +
  labs(title= "Retrieved plaster balls SOUTH", x= "Meters", y="number of retrieved plaster balls")

countsouthplot

#consistent count graphs for SOUTH (same position for BESE and Control) 
keep <- c("-5","-0.1", "4.7", "7.7", "9.55", "20", "100")
countconsistent <- subset(countsouth, Meters %in% keep)
countconsistent

countsouthconsistent <- ggplot() +
  geom_bar(data=countconsistent, aes(y = freq, x = Meters, fill = Retrieved), stat="identity",
           position='stack') +
  scale_fill_manual(values=c("darkseagreen2","lightblue3")) +
  theme_light (base_size = 12, base_family = "") +
  facet_grid( ~ Treatment) +
  labs(title= "Retrieved plaster balls SOUTH", subtitle= "consistent", x= "Meters", y="number of retrieved plaster balls")

countsouthconsistent 

#get a grouped stacked barplot of retrieval frequency  in NMB 
countnmb <- ggplot() +
  geom_bar(data=countnmb, aes(y = freq, x = Meters, fill = Retrieved), stat="identity",
           position='stack') +
  scale_fill_manual(values=c("darkseagreen2","lightblue3")) +
  theme_light (base_size = 12, base_family = "") +
  facet_grid( ~ Treatment) +
  labs(title= "Retrieved plaster balls NMB", x= "Meters", y="number of retrieved plaster balls")

countnmb

dev.off()




#____________________________________________________________________________________________________________-
#linear mixed model 
#library(lme4)
#library(lmerTest)

#mean(d$percentageLoss)
#length(d$percentageLoss)
#sd(d$percentageLoss)
#names(d)
#d
#newdata <- data.frame(d$Treatment,d$Meters,d$Plot, d$percentageLoss)
#newdata
#lmsouth <- newdata[newdata$d.Treatment %in% c('Mu','Control'),]
#lmsouth
#names(lmsouth)
#lmsouth$proportionLoss <- lmsouth$percentageLoss/100
#lmsouth$arcprop <- asin(sqrt(lmsouth$proportionLoss))
#fit1 <- brm(formula = percentageLoss ~ Meters * Treatment
#          + (1|Plot),
#         data = lmsouth, family = gaussian(),
#        prior = c(set_prior("normal(0,1)", class = "b", coef= "Meters"),
#                 set_prior("cauchy(0,2)", class = "sd", coef="Plot")),
#      warmup = 1000, iter = 2000, chains = 4,
#     control = list(adapt_delta = 0.95))

#library(rethinking)
#lmsouth$sinperc <- asin(sqrt(lmsouth$d.percentageLoss))
#lmsouth$d.Plot<- coerce_index(lmsouth$d.Plot)  #factor --> integer
#lmsouth$d.Treatment <- ifelse(lmsouth$d.Treatment=="Mu",1,0)
#names(lmsouth)
#colnames(lmsouth) <- c("Treatment", "Meters", "Plot", "percentageLoss")
#names(lmsouth)
#m1 <- map2stan(
# alist(
#  percentageLoss ~ dnorm(mu,sigma) , 
#    mu <- bm*Meters + bt*Treatment + bmt*Meters*Treatment + a_plot[Plot],
#    a_plot[Plot] ~ dnorm(a,sigma_plot),
#    a ~ dnorm (0,10),
#    sigma_plot ~ dcauchy (0,2),
#    bm ~ dnorm(0,10),
#    bt ~ dnorm(0,10),
#    bmt ~ dnorm(0,10),
#    sigma ~ dcauchy (0,2)
#  ),
#  data=lmsouth, warmup=500, iter= 2000, chains=1
#) 

#m1
#depth==2
#precis(m1,depth=2)
##lmnmb <- newdata[newdata$d.Treatment %in% c('NMB','ControlNM'),]
#lmnmb
#library(brms)
#south data normality 
#shapiro.test(lmsouth$percentageLoss)
#qqnorm(lmsouth$percentageLoss)
#qqline(lmsouth$percentageLoss)
#hist(lmsouth$percentageLoss, prob=TRUE)
#data is not normal --> reciprocal transformation just barely works 
#lmsouth$recperc <- 1/(lmsouth$percentageLoss)
#shapiro.test(lmsouth$recperc)
#qqnorm(lmsouth$recperc)
#qqline(lmsouth$recperc)
#hist(lmsouth$recperc, prob=TRUE)
#soooo I'll run a penalized quasi-likelihood method 
#library(MASS)
#PQL <- glmmPQL(percentageLoss ~ Meters + Treatment, ~1 | Plot, family = inverse.gaussian(link = "1/mu^2"),
#               data = lmsouth, verbose = FALSE)
#summary(PQL)

#lm1south <- lmer(percentageLoss~Meters+Treatment+(1|Plot),data=lmsouth)
#summary(lm1south)
#coef(lm1south)
#residuals linearity 
#residuals(lm1south)
#plot(fitted(lm1south),residuals(lm1south))#

#allow for random plot slopes to check their effect
#lm2south <- lmer(percentageLoss~Meters+Treatment+(1+Meters|Plot),data=lmsouth, REML=FALSE)
#summary(lm2south)
#coef(lm2south)

#lm3south <- lmer(percentageLoss~Meters+Treatment+(1+Treatment|Plot),data=lmsouth, REML=FALSE)
#summary(lm3south)
#coef(lm3south)


#NMB 

#shapiro.test(lmnmb$percentageLoss)
#qqnorm(lmnmb$percentageLoss)
#qqline(lmnmb$percentageLoss)
#hist(lmnmb$percentageLoss, prob=TRUE)

#data is not normal --> reciprocal transformation just barely works 
#lmnmb$recperc <- 1/(lmnmb$percentageLoss)

#shapiro.test(lmnmb$recperc)
#qqnorm(lmnmb$recperc)
#qqline(lmnmb$recperc)
#hist(lmnmb$recperc, prob=TRUE)

#again just barely 

#PQL <- glmmPQL(percentageLoss ~ Meters + Treatment, ~1 | Plot, family = inverse.gaussian(link = "1/mu^2"),
#               data = lmnmb, verbose = FALSE)

#summary(PQL)
#data is not normally distributed 
#lm1nmb <- lmer(percentageLoss~Meters+Treatment+(1|Plot),data=lmnmb)
#summary(lm1nmb)
#plot in grid
#g <- ggplot(d, aes(d$Position, d$Lost_weight))
#g + geom_point()+ facet_grid(. ~ d$Treatment)

#__________________________________________________________________________________
#Comparing the data of the natural mussel bed and the BESE in the SOUTH 

#dcomp
#names(dcomp)

#transforms data 
#dcomp <- transform(dcomp, lossperhour=ifelse(Treatment==c("Control"), percentageLoss/35,
#                                    ifelse(Treatment==c("Mu"), percentageLoss/35, percentageLoss/42.5)))
#dcomp
#dcomp$lossperhour
#dcomp <- dcomp[-which(is.na(dcomp$percentageLoss)),]
#shapiro.test(dcomp$lossperhour)
#par(mar = rep(2, 4))
#qqnorm(dcomp$lossperhour)
#qqline(dcomp$lossperhour)
#hist(dcomp$lossperhour, prob=TRUE)
#compsumm <- dcomp %>% group_by(Meters, Treatment) %>%
#  summarize(ymin = min(lossperhour),
#            ymax = max(lossperhour),
#            ymean = mean(lossperhour))
#compsumm
#open pdf file to save all percentage loss plots
#pdf("FULLcomparison.pdf", width=6 , height=4 , paper='special')
#plots
#ggplot(compsumm, aes(x = Meters, y = ymean, color = Treatment, shape = Treatment)) +
#  geom_point(size = 3) +
#  geom_errorbar(aes(ymin = ymin, ymax = ymax)) + 
#  scale_color_manual(values=c("navy","seagreen3", "indianred2","magenta4"))+
#  theme_light (base_size = 12, base_family = "") + 
#  labs(title= "Comparison BESE, NMB, Controls", subtitle= "standard deviation", x= "distance in meters", y="percentage/hour", color="Treatment", shape="Treatment")

#ggplot(compsumm, aes(x = Meters, y = ymean, color = Treatment, shape = Treatment)) +
#  geom_point(size = 3) +
#  geom_errorbar(aes(ymin = ymin, ymax = ymax)) + 
#  scale_x_continuous(limits = c(-5, 25)) +
#  scale_color_manual(values=c("navy","seagreen3", "indianred2","magenta4"))+
#  theme_light (base_size = 12, base_family = "") + 
#  labs(title= "Comparison BESE, NMB, Controls", subtitle= "-5 ->25 m., standard deviation", x= "distance in meters", y="percentage/hour", color="Treatment", shape="Treatment")


#compsumm2 <- compsumm[compsumm$Treatment %in% c('Mu', 'NMB','Control'),]
#compsumm2

#ggplot(compsumm2, aes(x = Meters, y = ymean, color = Treatment, shape = Treatment)) +
#  geom_point(size = 3) +
#  geom_errorbar(aes(ymin = ymin, ymax = ymax)) + 
#  scale_color_manual(values=c("navy","seagreen3", "indianred2"))+
#  theme_light (base_size = 12, base_family = "") + 
#  labs(title= "Comparison BESE, NMB, Control", subtitle= "standard deviation", x= "distance in meters", y="percentage/hour", color="Treatment", shape="Treatment")

#ggplot(compsumm2, aes(x = Meters, y = ymean, color = Treatment, shape = Treatment)) +
#  geom_point(size = 3) +
#  geom_errorbar(aes(ymin = ymin, ymax = ymax)) + 
#  scale_x_continuous(limits = c(-5, 25)) +
#  scale_color_manual(values=c("navy","seagreen3", "indianred2"))+
#  theme_light (base_size = 12, base_family = "") + 
#  labs(title= "Comparison BESE, NMB, Control", subtitle= "-5 ->25 m., standard deviation", x= "distance in meters", y="percentage/hour", color="Treatment", shape="Treatment")#

#compsumm3 <- compsumm[compsumm$Treatment %in% c('ControlNM', 'Control'),]
#compsumm3
#
#ggplot(compsumm3, aes(x = Meters, y = ymean, color = Treatment, shape = Treatment)) +
#  geom_point(size = 3) +
#  geom_errorbar(aes(ymin = ymin, ymax = ymax)) + 
#  scale_color_manual(values=c("navy","seagreen3", "indianred2"))+
#  theme_light (base_size = 12, base_family = "") + 
#  labs(title= "Comparison Controls", subtitle= "standard deviation", x= "distance in meters", y="percentage /hour", color="Treatment", shape="Treatment")

#ggplot(compsumm3, aes(x = Meters, y = ymean, color = Treatment, shape = Treatment)) +
#  geom_point(size = 3) +
#  geom_errorbar(aes(ymin = ymin, ymax = ymax)) + 
#  scale_x_continuous(limits = c(-5, 25)) +
#  scale_color_manual(values=c("navy","seagreen3", "indianred2"))+
#  theme_light (base_size = 12, base_family = "") + 
#  labs(title= "Comparison Controls", subtitle= "-5 ->25 m., standard deviation", x= "distance in meters", y="percentage/hour ", color="Treatment", shape="Treatment")
#dev.off()
#___________________________________________________________________________________                  
