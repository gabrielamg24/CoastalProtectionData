#clear workspace 
rm(list=ls())
library(ggplot2)
library(dplyr)
#set working directory and read file 
setwd("C:/Users/Gabriela/Desktop/RUG/Project 1/DATA/sediment sticks/")
d <- read.csv("sedsticks.csv", header=T)
#get names of columns
names(d)
#get a review of your data 
str(d)

#test data for normality
shapiro.test(d$difference)
qqnorm(d$difference)
qqline(d$difference)
hist(d$difference, prob=TRUE)


#if data is not normally distributed, try transforming data and check for normality 
d$transfdifference <- log(1+d$difference)

shapiro.test(d$transfdifference)
qqnorm(d$transfdifference)
qqline(d$transfdifference)
hist(d$transfdifference, prob=TRUE)

#omit Not Available values from the column d$difference (shows how much the stick was digged in-cm.)
d <- d[-which(is.na(d$difference)),]
d

xid <- data.frame(xintercept = c(0.23,3.23, 6.23,9.23), lty=factor(1))


#plots____________________________________________________________________________________
#open pdf file to save all following plots with std. deviation 
pdf("sedsticksPLOTSstddev.pdf", width=6 , height=4 , paper='special')

#plotting with error bars 
#get  max & min values for the variable $difference 
#(function needs dplyr, called at the beginning of code)
d.summary <- d %>% group_by(meters, treatment) %>%
  summarize(ymin = min(difference),
            ymax = max(difference),
            ymean = mean(difference))
d.summary

errorplots <- ggplot(d.summary, aes(x = meters, y = ymean, color = treatment, shape = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) + 
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=0.4) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")+
  labs(title= "Sediment erosion", subtitle= "standard deviation", x= "distance (meters)", y="erosion (cm)", color="Treatment", shape="Treatment")
errorplots

#up to 25 meters, for better resolution  
errorplots25 <- ggplot(d.summary, aes(x = meters, y = ymean, color = treatment, shape = treatment)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) + 
  scale_x_continuous(limits = c(-5, 25)) +
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=1) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")+
  labs(title= "Sediment erosion", subtitle= "-5 -> 25 m., standard deviation", x= "distance (meters)", y="erosion (cm)", color="Treatment", shape="Treatment")
errorplots25 

#AVERAGED PLOTS
#get the average of the response variable, use function aggregate 
#for each position per treatment (make plots clearer)
d.ave<- aggregate(difference ~ meters + treatment, data= d, mean)
d.ave
names (d.ave)

#get an averaged plot of the data 
erosion.ave <- ggplot(d.ave, aes(x=d.ave$meters, y=d.ave$difference, color = d.ave$treatment, shape= d.ave$treatment)) +
  geom_point(size=3) + 
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=0.4) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")+
  labs(title= "Sediment erosion", subtitle= "plots averaged per distance and treatment", x= "distance (meters)", y="erosion (cm)", color="Treatment", shape="Treatment")

erosion.ave

#get a plot of averaged data up to 25 meters for better overview 
erosion.ave25 <- ggplot(d.ave, aes(x=d.ave$meters, y=d.ave$difference, color = d.ave$treatment, shape= d.ave$treatment)) + 
  geom_point(size=3) + 
  scale_x_continuous(limits = c(-5, 25)) + 
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=1) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")+
  labs(title= "Sediment erosion",subtitle= "-5->25 meters, plots averaged per distance and treatment", x= "distance (meters)", y="erosion (cm)", color="Treatment", shape="Treatment")

erosion.ave25

#CONSISTENT PLOTS 
#keeping only the rows that have a measurement for same position in BESE & control 
#useful for clarity 
keep <- c("A","C", "J", "N", "Q", "AC", "AK")
d.consistent <- subset(d, position %in% keep)
d.consistent

#with error bars
dcons.summary <- d.consistent %>% group_by(meters, treatment) %>%
  summarize(ymin = min(difference),
            ymax = max(difference),
            ymean = mean(difference))

dcons.summary 

ggplot(dcons.summary, aes(x = meters, y = ymean, color = treatment, shape = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) + 
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=0.4) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")+
  labs(title= "Sediment erosion", subtitle= "standard deviation, consistent measurements BESE & Control", x= "distance (meters)", y="erosion (cm)", color="Treatment", shape="Treatment")

#up to 25 meters 
ggplot(dcons.summary, aes(x = meters, y = ymean, color = treatment, shape = treatment)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) + 
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  scale_x_continuous(limits = c(-5, 25)) +
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=1) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")+
  labs(title= "Sediment erosion", subtitle= "-5 -> 25  meters; standard deviation; consistent measurements BESE & Control", x= "distance (meters)", y="erosion (cm)", color="Treatment", shape="Treatment")

#close pdf file with all plots 
dev.off()

#one plot per treatment 
#g <- ggplot(d, aes(x=d$meters, y=d$difference))
#g + geom_point() + facet_grid(. ~ d$treatment)
#_______________________________________________________________________________________________________
#open pdf file to save all following plots with std. deviation 
pdf("sedsticksPLOTSstderror.pdf", width=6 , height=4 , paper='special')

names(d)
sticks <- aggregate(d$difference,
                    by = list(meters = d$meters, treatment = d$treatment),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),
                                        n = length(x)))
sticks <- do.call(data.frame, sticks)
sticks$se <- sticks$x.sd / sqrt(sticks$x.n)

colnames(sticks) <- c("meters", "treatment", "mean", "sd", "n", "se")

sticks$names <- c(paste(sticks$meters, "meters /",
                        sticks$treatment, " treatment"))

limitssticks <- aes(ymax = sticks$mean + sticks$se,
              ymin = sticks$mean - sticks$se)


errorplots <- ggplot(sticks, aes(x = meters, y = mean, color = treatment, shape = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(limitssticks) + 
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=0.4) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")+
  labs(title= "Sediment erosion", subtitle= "standard error", x= "distance (meters)", y="erosion (cm)", color="Treatment", shape="Treatment")
errorplots

#up to 25 meters, for better resolution  


errorplots25 <- ggplot(sticks, aes(x = meters, y = mean, color = treatment, shape = treatment)) +
  geom_point(size = 4) +
  geom_errorbar(limitssticks) + 
  scale_x_continuous(limits = c(-5, 25)) +
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=1) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")+
  labs(title= "Sediment erosion", subtitle= "-5 -> 25 m., standard error", x= "distance (meters)", y="erosion (cm)", color="Treatment", shape="Treatment")
errorplots25 

#AVERAGED PLOTS
#get the average of the response variable, use function aggregate 
#for each position per treatment (make plots clearer)
names(sticks)


#get an averaged plot of the data 
erosion.aveerror <- ggplot(sticks, aes(x=meters, y=mean, color = treatment, shape= treatment)) +
  geom_point(size=3) + 
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=0.4) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")+
  labs(title= "Sediment erosion", subtitle= "plots averaged per distance and treatment", x= "distance (meters)", y="erosion (cm)", color="Treatment", shape="Treatment")

erosion.aveerror

#get a plot of averaged data up to 25 meters for better overview 
erosion.ave25error <- ggplot(sticks, aes(x=meters, y=mean, color = treatment, shape= treatment)) + 
  geom_point(size=3) + 
  scale_x_continuous(limits = c(-5, 25)) + 
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=1) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")+
  labs(title= "Sediment erosion",subtitle= "-5->25 meters, plots averaged per distance and treatment", x= "distance (meters)", y="erosion (cm)", color="Treatment", shape="Treatment")

erosion.ave25error

#CONSISTENT PLOTS 
#keeping only the rows that have a measurement for same position in BESE & control 
#useful for clarity 
keep <- c("A","C", "J", "N", "Q", "AC", "AK")
d.consistenterror <- subset(d, position %in% keep)
d.consistenterror


names(d.consistenterror)
sticksconsistent <- aggregate(d.consistenterror$difference,
                    by = list(meters = d.consistenterror$meters, treatment = d.consistenterror$treatment),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),
                                        n = length(x)))
sticksconsistent <- do.call(data.frame, sticksconsistent)
sticksconsistent$se <- sticksconsistent$x.sd / sqrt(sticksconsistent$x.n)

colnames(sticksconsistent) <- c("meters", "treatment", "mean", "sd", "n", "se")

sticksconsistent$names <- c(paste(sticksconsistent$meters, "meters /",
                                  sticksconsistent$treatment, " treatment"))

limitssticksconsistent <- aes(ymax = sticksconsistent$mean + sticksconsistent$se,
                    ymin = sticksconsistent$mean - sticksconsistent$se)



ggplot(sticksconsistent, aes(x = meters, y = mean, color = treatment, shape = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(limitssticksconsistent) + 
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=0.4) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")+
  labs(title= "Sediment erosion", subtitle= "standard error, consistent measurements BESE & Control", x= "distance (meters)", y="erosion (cm)", color="Treatment", shape="Treatment")

#up to 25 meters 
ggplot(sticksconsistent, aes(x = meters, y = mean, color = treatment, shape = treatment)) +
  geom_point(size = 4) +
  geom_errorbar(limitssticksconsistent) + 
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  scale_x_continuous(limits = c(-5, 25)) +
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=1) +
  scale_linetype_manual(values = 1, name="",label="BESE Structures")+
  labs(title= "Sediment erosion", subtitle= "-5 -> 25  meters; standard error; consistent measurements BESE & Control", x= "distance (meters)", y="erosion (cm)", color="Treatment", shape="Treatment")


#close pdf file with all plots 
dev.off()


#_______________________________________________________________________________________________________
#BAR PLOTS with standard error 
pdf("sedsticksPLOTSbar.pdf", width=6 , height=4 , paper='special')


names(d)
myData <- aggregate(d$difference,
                    by = list(meters = d$meters, treatment = d$treatment),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),
                                        n = length(x)))
myData <- do.call(data.frame, myData)
myData$se <- myData$x.sd / sqrt(myData$x.n)

colnames(myData) <- c("meters", "treatment", "mean", "sd", "n", "se")

myData$names <- c(paste(myData$meters, "meters /",
                        myData$treatment, " treatment"))

limits <- aes(ymax = myData$mean + myData$se,
              ymin = myData$mean - myData$se)

p <- ggplot(data = myData, aes(x = factor(meters), y = mean,
                               fill = factor(treatment)))+
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(limits, position = position_dodge(0.9), width = 0.25) +
  labs(x = "distance (m.)", y = "erosion(cm.)") +
  ggtitle("Erosion by sediment sticks ") +
  theme_light (base_size = 12, base_family = "") + 
  scale_fill_manual(values=c("navy","seagreen3"))

p

#consistent bar plots standar error
names(d.consistent)
barconsistent <- aggregate(d.consistent$difference,
                    by = list(meters = d.consistent$meters, treatment = d.consistent$treatment),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),
                                        n = length(x)))
barconsistent <- do.call(data.frame, barconsistent)
barconsistent$se <- barconsistent$x.sd / sqrt(barconsistent$x.n)

colnames(barconsistent) <- c("meters", "treatment", "mean", "sd", "n", "se")

barconsistent$names <- c(paste(barconsistent$meters, "meters /",
                        barconsistent$treatment, " treatment"))

limits <- aes(ymax = barconsistent$mean + barconsistent$se,
              ymin = barconsistent$mean - barconsistent$se)

ggplot(data = barconsistent, aes(x = factor(meters), y = mean,
                               fill = factor(treatment))) +
  geom_bar(stat = "identity",
             position = position_dodge(0.9)) +
  geom_errorbar(limits, position = position_dodge(0.9),
                width = 0.25) +
  labs(x = "distance (m.)", y = "erosion(cm.)") +
  ggtitle("Erosion by sediment sticks ") +
  theme_light (base_size = 12, base_family = "") + 
  scale_fill_manual(values=c("navy","seagreen3"))
  


#close pdf file with all plots 
dev.off()
