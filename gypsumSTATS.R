#gypsum sticks STATS 
#clear environment
rm(list=ls())
#set working directory and call file 
setwd("C:/Users/Gabriela/Desktop/RUG/Project 1/DATA/gypsum sticks/")
d <- read.csv("gypsum.csv", header=T)
#get file names and summary
names(d)
str(d)
#create variable "percentage weight loss" by (lost_weight/beforeweight*100)
d$percentageLoss= (d$Lost_weight/d$Before_weigh)*100
d


#mean(d$percentageLoss)
#length(d$percentageLoss)
#sd(d$percentageLoss)


#remove NA (not avaialable points)
d2 <- d[-which(is.na(d$percentageLoss)),]
d2

#subset the data with the columns needed for analysis 
d2 <- data.frame(d2$Treatment,d2$Meters,d2$Plot, d2$percentageLoss)
d2
#change column names
colnames(d2) <- c("Treatment", "Meters", "Plot", "percentageLoss")


#bayesian analysis for interaction and with a random intercept (random effect= plot)
library(rethinking)


#subset the data per site to be analyzed
south <- d2[d2$Treatment %in% c('Mu','Control'),]
south
names(south)

#data exploration 
#test for normality of the data
shapiro.test(south$percentageLoss)
par(mar = rep(2, 4))
qqnorm(south$percentageLoss)
qqline(south$percentageLoss)
hist(south$percentageLoss, prob=TRUE)


nmb <- d2[d2$Treatment %in% c('NMB','ControlNM'),]
nmb
names(nmb)


#data exploration 
#test for normality of the data
shapiro.test(nmb$percentageLoss)
par(mar = rep(2, 4))
qqnorm(nmb$percentageLoss)
qqline(nmb$percentageLoss)
hist(nmb$percentageLoss, prob=TRUE)

#Run bayesian analysis
#south site first 

library(car)
scatterplotMatrix(~percentageLoss+Meters+Treatment, data=south, col=rangi2,smoother=F)

#turn percentages into proportion 
south$proportion <- south$percentageLoss/100
#center the continuous predictor  
mean(south$Meters)
south$MetersC <- south$Meters - mean(south$Meters)
#factor --> integer (for stan)
south$Plot<- coerce_index(south$Plot) 
#dummy variable for categorical predictor 
south$TreatmentMu <- ifelse(south$Treatment=="Mu",1,0)
names(south)
#again keep only columns to be used by map2stan 
south <- data.frame(south$TreatmentMu,south$MetersC,south$Plot,south$percentageLoss)
colnames(south) <- c("TreatmentMu", "Meters", "Plot", "propLoss")
names(south)

m1s <- map2stan(
  alist(
    propLoss ~ dnorm(mu,sigma) , 
    mu <- bm*Meters + bt*TreatmentMu + bmt*Meters*TreatmentMu + a_plot[Plot],
    a_plot[Plot] ~ dnorm(a,sigma_plot),
    a ~ dnorm (0,10),
    sigma_plot ~ dcauchy (0,2),
    bm ~ dnorm(0,10),
    bt ~ dnorm(0,10),
    bmt ~ dnorm(0,10),
    sigma ~ dcauchy (0,2)
  ),
  data=south, warmup=1000, iter= 5000, chains=1
) 

#no interaction 
m2s <- map2stan(
  alist(
    propLoss ~ dnorm(mu,sigma) , 
    mu <- bm*Meters + bt*TreatmentMu + a_plot[Plot],
    a_plot[Plot] ~ dnorm(a,sigma_plot),
    a ~ dnorm (0,10),
    sigma_plot ~ dcauchy (0,2),
    bm ~ dnorm(0,10),
    bt ~ dnorm(0,10),
    sigma ~ dcauchy (0,2)
  ),
  data=south, warmup=1000, iter= 2000, chains=1
) 


#no random intercept 
m3s <- map2stan(
  alist(
    propLoss ~ dnorm(mu,sigma) , 
    mu <- a + bm*Meters + bt*TreatmentMu + bmt*Meters*TreatmentMu,
    a ~ dnorm (0,10),
    bm ~ dnorm(0,10),
    bt ~ dnorm(0,10),
    bmt ~ dnorm(0,10),
    sigma ~ dcauchy (0,2)
  ),
  data=south, warmup=1000, iter= 2000, chains=1
) 



#just treatment and random intercept 
m4s <- map2stan(
  alist(
    propLoss ~ dnorm(mu,sigma) , 
    mu <- bt*TreatmentMu + a_plot[Plot],
    a_plot[Plot] ~ dnorm(a,sigma_plot),
    a ~ dnorm (0,10),
    sigma_plot ~ dcauchy (0,2),
    bt ~ dnorm(0,10),
    sigma ~ dcauchy (0,2)
  ),
  data=south, warmup=1000, iter= 2000, chains=1
) 


#comparing the WAIC values and weights for each model 
south.models <- compare(m1s,m2s,m3s,m4s)
south.models
#model 3 seems the best but let's see-->
#plotting the models values 
plot(south.models,SE=T, dSE=T)
coeftab(m1s,m2s,m3s,m4s)
plot(coeftab(m1s,m2s,m3s,m4s))
#we see no influence of the variable "meters" or is interaction at all 
#let's keep the most complex model 


m1s
#Checking the chains (trace plots, n_eff, Rhat) carefully to ensure they are valid
precis(m1s,depth=2)
plot(precis(m1s,depth=2))
pairs(m1s)
#closer look at pair plots 
pairs(~ propLoss + TreatmentMu + Meters, data=south, col=rangi2)
plot(m1s)
library(ggplot2)
library(ggmcmc) # uses ggplot2
m1s.ggs <- ggs(m1s@stanfit)
ggs_traceplot(m3s.ggs)

#checking for the variance-covariance matrix 
precis(m1s, corr = TRUE)


#plottint the posterior distribution of the estimates for treatments

post <- extract.samples(m1s) 
gamma.Control <- post$a + post$bmt*0
gamma.Mussels <- post$a + post$bt + post$bmt*1

mean( gamma.Control) 
mean( gamma.Mussels)

par(mfrow=c(1,1))
dens( gamma.Control ,
      xlab="posterior distribution" , col=rangi2 )
dens( gamma.Mussels , add=TRUE )

#_________________________________________________________________________
#now NMB

scatterplotMatrix(~percentageLoss+Meters+Treatment, data=nmb, col=rangi2,smoother=F)

#turn percentages into proportion 
nmb$propLoss <- nmb$percentageLoss/100
#center the continuous predictor  
mean(nmb$Meters)
nmb$MetersC <- nmb$Meters - mean(nmb$Meters)
#factor --> integer (for stan)
nmb$Plot<- coerce_index(nmb$Plot)  
#turn treatment into 1, 0 categorical
nmb$TreatmentNMB <- ifelse(nmb$Treatment=="NMB",1,0)
#again keep only columns to be used by map2stan 
nmb <- data.frame(nmb$TreatmentNMB,nmb$MetersC,nmb$Plot,nmb$propLoss)
colnames(nmb) <- c("TreatmentNMB", "Meters", "Plot", "propLoss")
names(nmb)

m1nmb <- map2stan(
  alist(
    propLoss ~ dnorm(mu,sigma) , 
    mu <- bm*Meters + bt*TreatmentNMB + bmt*Meters*TreatmentNMB + a_plot[Plot],
    a_plot[Plot] ~ dnorm(a,sigma_plot),
    a ~ dnorm (0,10),
    sigma_plot ~ dcauchy (0,2),
    bm ~ dnorm(0,10),
    bt ~ dnorm(0,10),
    bmt ~ dnorm(0,10),
    sigma ~ dcauchy (0,2)
  ),
  data=nmb, warmup=1000, iter= 2000, chains=1
) 

#try without intercation 
m2nmb <- map2stan(
  alist(
    propLoss ~ dnorm(mu,sigma) , 
    mu <- bm*Meters + bt*TreatmentNMB + a_plot[Plot],
    a_plot[Plot] ~ dnorm(a,sigma_plot),
    a ~ dnorm (0,10),
    sigma_plot ~ dcauchy (0,2),
    bm ~ dnorm(0,10),
    bt ~ dnorm(0,10),
    sigma ~ dcauchy (0,2)
  ),
  data=nmb, warmup=1000, iter= 2000, chains=1
) 

#try without random intercept 
m3nmb <- map2stan(
  alist(
    propLoss ~ dnorm(mu,sigma) , 
    mu <- a + bm*Meters + bt*TreatmentNMB + bmt*Meters*TreatmentNMB,
    a ~ dnorm (0,10),
    bm ~ dnorm(0,10),
    bt ~ dnorm(0,10),
    bmt ~ dnorm(0,10),
    sigma ~ dcauchy (0,2)
  ),
  data=nmb, warmup=1000, iter= 5000, chains=1
) 


m4nmb <- map2stan(
  alist(
    propLoss ~ dnorm(mu,sigma) , 
    mu <- bt*TreatmentNMB + a_plot[Plot],
    a_plot[Plot] ~ dnorm(a,sigma_plot),
    a ~ dnorm (0,10),
    sigma_plot ~ dcauchy (0,2),
    bt ~ dnorm(0,10),
    sigma ~ dcauchy (0,2)
  ),
  data=nmb, warmup=1000, iter= 2000, chains=1
) 



#comparing the WAIC values and weights for each model 
nmb.models <- compare(m1nmb,m2nmb,m3nmb,m4nmb)
nmb.models
#model 3 seems the best but let's see-->
#plotting the models values 
plot(nmb.models,SE=T, dSE=T)
coeftab(m1nmb,m2nmb,m3nmb,m4nmb)
plot(coeftab(m1nmb,m2nmb,m3nmb,m4nmb))
#we see no influence of the variable "meters" or is interaction at all 
#let's keep the most complex model 


m1nmb
#Checking the chains (trace plots, n_eff, Rhat) carefully to ensure they are valid
precis(m1nmb,depth=2)
plot(precis(m1nmb,depth=2))
pairs(m1nmb)
plot(m1nmb)
library(ggplot2)
library(ggmcmc) # uses ggplot2
m1nmb.ggs <- ggs(m1nmb@stanfit)
ggs_traceplot(m1nmb.ggs)





#checking for the variance-covariance matrix 
precis(m1nmb, corr = TRUE)


#plottint the posterior distribution of the estimates for treatments

post <- extract.samples(m1nmb) 
gamma.Control <- post$a + post$bmt*0
gamma.Mussels <- post$a + post$bt + post$bmt*1

mean( gamma.Control) 
mean( gamma.Mussels)

par(mfrow=c(1,1))
dens( gamma.Control ,
      xlab="posterior distribution" , col=rangi2 )
dens( gamma.Mussels , add=TRUE )






#___________________________________________________________________________________
#COUNT stats get stats for retrieved gypsum sticks

#make a copy of d 
dcounts <- d
names(dcounts)

#dummy variable for retrieved 
dcounts$Retrieved <- ifelse(dcounts$Retrieved=="y",1,0)
#plot as integer
dcounts$Plot<- coerce_index(dcounts$Plot)  #factor --> integer
#center the continuous predictor  
mean(dcounts$Meters)
dcounts$MetersC <- dcounts$Meters - mean(dcounts$Meters)
#frame data with columns that will be used
dcounts <- data.frame(dcounts$Treatment,dcounts$MetersC,dcounts$Plot,dcounts$Retrieved)
colnames(dcounts) <- c("Treatment", "Meters", "Plot", "Retrieved")
names(dcounts)

#First south data 

#subset for SOUTHERN SITE
countsouth <- dcounts[dcounts$Treatment %in% c('Mu','Control'),]
#turn treatment into 1, 0 categorical
countsouth$TreatmentMu <- ifelse(countsouth$Treatment=="Mu",1,0)
#frame data again since we turned treatment into categorical 
countsouth <- data.frame(countsouth$TreatmentMu,countsouth$Meters,countsouth$Plot,countsouth$Retrieved)
colnames(countsouth) <- c("TreatmentMu", "Meters", "Plot", "Retrieved")
names(countsouth)

mYESsouth <- map2stan(
  alist(
    Retrieved ~ dbinom(1,p) , 
    logit(p) <- a[Plot]+ bm*Meters + bt*TreatmentMu + bmt*Meters*TreatmentMu,
    a[Plot] ~ dnorm(a,sigma_plot),
    a ~ dnorm (0,10),
    sigma_plot ~ dcauchy (0,2),
    bm ~ dnorm(0,10),
    bt ~ dnorm(0,10),
    bmt ~ dnorm(0,10)
  ),
  data=countsouth, warmup=1000, iter= 2000, chains=1
) 

mYESsouth2 <- map2stan(
  alist(
    Retrieved ~ dbinom(1,p) , 
    logit(p) <- a + bm*Meters + bt*TreatmentMu + bmt*Meters*TreatmentMu,
    a ~ dnorm (0,10),
    bm ~ dnorm(0,10),
    bt ~ dnorm(0,10),
    bmt ~ dnorm(0,10)
  ),
  data=countsouth, warmup=1000, iter= 2000, chains=1
) 

mYESsouth2
precis(mYESsouth2,depth=2)
logistic(-0.44+1.04)
logistic(-0.44)
logistic(-0.44+1.04)-logistic(-0.44)
pairs(mYESsouth2)
plot(mYESsouth2)

mYESsouth2.ggs <- ggs(mYESsouth2@stanfit)
ggs_traceplot(mYESsouth2.ggs)

#plotting the posterior distribution of the estimates for treatments

post <- extract.samples(mYESsouth2) 
gamma.Control <- logistic(post$a) + logistic(post$bmt*0)
gamma.BESE <- logistic(post$a) + logistic(post$bt) + logistic(post$bmt*1)

mean( gamma.Control) 
mean( gamma.Mussels)

par(mfrow=c(1,1))
dens( gamma.Control ,
      xlab="posterior distribution" , col=rangi2 )
dens( gamma.Mussels , add=TRUE )


#________________________________________________________________
#subset for NMB
countnmb <- dcounts[dcounts$Treatment %in% c('NMB','ControlNM'),]
#turn treatment into 1, 0 categorical
countnmb$TreatmentNMB <- ifelse(countnmb$Treatment=="NMB",1,0)
#frame data again
countnmb <- data.frame(countnmb$TreatmentNMB,countnmb$Meters,countnmb$Plot,countnmb$Retrieved)
colnames(countnmb) <- c("TreatmentNMB", "Meters", "Plot", "Retrieved")
names(countnmb)


#attempt to set an attribute on NULL
mYESn2 <- map2stan(
  alist(
    Retrieved ~ dbinom(1,p) , 
    logit(p) <- a + bm*Meters + bt*TreatmentNMB + bmt*Meters*TreatmentNMB,
    a ~ dnorm (0,10),
    bm ~ dnorm(0,10),
    bt ~ dnorm(0,10),
    bmt ~ dnorm(0,10)
  ),
  data=countnmb, warmup=1000, iter= 2000, chains=1
) 


mYESn2
precis(mYESn2,depth=2)
logistic(2.07)
logistic(2.07+7.61)
logistic(2.07)-logistic(2.07+7.61)
pairs(mYESn2)
plot(mYESn2)
mYESn2.ggs <- ggs(mYESn2@stanfit)
ggs_traceplot(mYESn2.ggs)




#example of the model for nomal data
#linear mixed model 
#library(lme4)
#library(lmerTest)
#m1 <- lmer(percentageLoss~Meters+Treatment+Meters*Treatment+(1|Plot),data=d)
#summary(m1)
#checking for normality of residuals
#resid(m1) #List of residuals
#plot(density(resid(m1))) #A density plot
#qqnorm(resid(m1)) # A quantile normal plot - good for checking normality
#qqline(resid(m1))
#shapiro.test(resid(m1))