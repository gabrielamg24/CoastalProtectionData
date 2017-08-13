#clear workspace 
rm(list=ls())
#set working directory and read file 
setwd("C:/Users/Gabriela/Desktop/RUG/Project 1/DATA/dGPS/")
d2 <- read.csv("201705_gr_nmb.csv", header=T)
d1 <- read.csv("201704_gr_nmb.csv", header=T)
#get names of columns
names(d1)
names(d2)


#check for outliers 


remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

zout <- remove_outliers(d2$z)
as.vector(zout)
d2<-cbind(d2,zout)
d2 <- d2[-which(is.na(d2$zout)),]
names(d2)
d2 <- d2[,-10] 


names(d1)
d1
zout1 <- remove_outliers(d1$z)
as.vector(zout1)
d1<-cbind(d1,zout1)
d1 <- d1[-which(is.na(d1$zout)),]
d1 <- d1[,-10] 


#merging both data sets 
names(d1)
names(d2)
d <- rbind(d1,d2) 
d
names(d)

#open pdf file to save all following plots
pdf("dgpsPLOTSnmb.pdf", width=6 , height=4 , paper='special')


xid <- data.frame(xintercept = c(0), lty=factor(1))


#plots with std DEVIATION 

d.summary <- d %>% group_by(meters, treatment,time) %>%
  summarize(ymin = min(z),
            ymax = max(z),
            ymean = mean(z))
d.summary

ggplot(d.summary, aes(x = meters, y = ymean, color = time, shape = time)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) + 
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  labs(title= "Sediment accretion in Natural Mussel Beds", subtitle= "standard deviation", x= "distance (meters)", y="sediment height", color="Treatment", shape="Treatment") +
  facet_grid(. ~ d.summary$treatment)

ggplot(d.summary, aes(x = meters, y = ymean, color = treatment, shape = treatment)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax)) + 
  scale_color_manual(values=c("navy","seagreen3"))+
  theme_light (base_size = 12, base_family = "") + 
  labs(title= "Sediment accretion in Natural Mussel Beds", subtitle= "standard deviation", x= "distance (meters)", y="sediment height", color="Treatment", shape="Treatment") +
  facet_grid(. ~ d.summary$time)
#_________________________________________________________________________________
#plots with std ERROR


south <- aggregate(d$z,
                   by = list(meters = d$meters, treatment = d$treatment, time = d$time),
                   FUN = function(x) c(mean = mean(x), sd = sd(x),
                                       n = length(x)))
south <- do.call(data.frame, south)
south$se <- south$x.sd / sqrt(south$x.n)

colnames(south) <- c("meters", "treatment", "time" , "mean", "sd", "n", "se")

south$names <- c(paste(south$meters, "meters /",
                       south$treatment, " treatment /",
                       south$time, "time"))
south

limitsS <- aes(ymax = south$mean + south$se,
               ymin = south$mean - south$se)

ggplot(data = south, aes(x = meters, y = mean, color = time, shape=time)) +
  geom_point(size = 3) +
  geom_errorbar(limitsS) +
  scale_color_manual(values=c("navy","seagreen3"))+
  labs(x = "distance (m.)", y = "altitude") +
  ggtitle("Sediment accretion Natural Mussel Beds ") +
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=0.6) +
  scale_linetype_manual(values = 1, name="",label="NMB")+
  facet_grid(~treatment) 

ggplot(data = south, aes(x = meters, y = mean, color = treatment, shape=treatment)) +
  geom_point(size = 3) +
  geom_errorbar(limitsS) +
  scale_color_manual(values=c("navy","seagreen3"))+
  labs(x = "distance (m.)", y = "altitude") +
  ggtitle("Sediment accretion Natural Mussel Beds") +
  theme_light (base_size = 12, base_family = "") + 
  geom_vline(data=xid, aes(xintercept=xintercept, lty=lty) , color= "bisque3", size=0.6) +
  scale_linetype_manual(values = 1, name="",label="NMB")+
  facet_grid(~time) 

#__________________________________________________________________________________
#BAR PLOTS with error bars
names(d)
dbar <- aggregate(d$z,
                  by = list(meters = d$meters, treatment = d$treatment, time = d$time),
                  FUN = function(x) c(mean = mean(x), sd = sd(x),
                                      n = length(x)))
dbar <- do.call(data.frame, dbar)
dbar$se <- dbar$x.sd / sqrt(dbar$x.n)

colnames(dbar) <- c("meters", "treatment", "time" , "mean", "sd", "n", "se")

dbar$names <- c(paste(dbar$meters, "meters /",
                      dbar$treatment, " treatment /",
                      dbar$time, "time"))
dbar

#SOUTH bar plots 
limits <- aes(ymax = dbar$mean + dbar$se,
                   ymin = dbar$mean - dbar$se)

p <- ggplot(data = dbar, aes(x = factor(meters), y = mean,
                                      fill = factor(time)))

p + geom_bar(stat = "identity",
                  position = position_dodge(0.9)) +
  geom_errorbar(limits, position = position_dodge(0.9),
                width = 0.25) +
  facet_grid(~treatment) +
  labs(x = "distance (m.)", y = "altitude") +
  ggtitle("Sediment accretion Natural Mussel Beds ") +
  theme_light (base_size = 12, base_family = "") + 
  scale_fill_manual(values=c("darkseagreen2","lightblue3"))

dev.off()


##MIXED EFFECTS MODEL 
#names(d)
#shapiro.test(d$z)
#qqnorm(d$z)
#qqline(d$z)
#hist(d$z, prob=TRUE)

#d$z

#d$zt <- exp(d$z)
#shapiro.test(d$zt)
#qqnorm(d$zt)
#qqline(d$zt)
#hist(d$zt, prob=TRUE)

#PQL <- glmmPQL(z ~ meters + treatment, ~1 | plot, family = gaussian(link = "log"),
#               data = d, verbose = FALSE)

#summary(PQL)
#try this on better pc
#library(plot3D)
#par(mfrow = c(1, 1))
#x <- y <- z <- seq(-4, 4, by = 0.2)
#M <- mesh(d$x, d$y, d$z)
#R <- with (M, sqrt(x^2 + y^2 +z^2))
#p <- sin(2*R)/(R+1e-3)
#slice3D(x, y, z, colvar = p,
 #       xs = 0, ys = c(-4, 0, 4), zs = NULL)
#isosurf3D(x, y, z, colvar = p, level = 0, col = "red")

