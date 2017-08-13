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

#omit Not Available values from the column d$difference (shows how much the stick was digged in-cm.)
d <- d[-which(is.na(d$difference)),]
d

#test data for normality
shapiro.test(d$difference)
qqnorm(d$difference)
qqline(d$difference)
hist(d$difference, prob=TRUE)

#if data is not normally distributed, try transforming data and check for normality 
d$transfdifference <- sqrt(d$difference)

shapiro.test(d$transfdifference)
qqnorm(d$transfdifference)
qqline(d$transfdifference)
hist(d$transfdifference, prob=TRUE)

#data could not be transformed, proceed to BAYESIAN ANALYSIS 
library(rethinking) 

#subset the data with the columns needed for analysis 
d <- data.frame(d$treatment,d$meters,d$plot, d$difference)
d

#change column names
colnames(d) <- c("Treatment", "Meters", "Plot", "Difference")
names(d)

d$Plot<- coerce_index(d$Plot)  #factor --> integer
#center the continuous predictor  
mean(d$Meters)
d$Meters <- d$Meters - mean(d$Meters)
#turn treatment into 1, 0 categorical
d$TreatmentMu <- ifelse(d$Treatment=="Mu",1,0)
d <- data.frame(d$TreatmentMu,d$Meters,d$Plot, d$Difference)
colnames(d) <- c("TreatmentMu", "Meters", "Plot", "Difference")
names(d)

m1 <- map2stan(
  alist(
    Difference ~ dnorm(mu,sigma) , 
    mu <- bm*Meters + bt*TreatmentMu + bmt*Meters*TreatmentMu + a_plot[Plot],
    a_plot[Plot] ~ dnorm(a,sigma_plot),
    a ~ dnorm (0,10),
    sigma_plot ~ dcauchy (0,2),
    bm ~ dnorm(0,10),
    bt ~ dnorm(0,10),
    bmt ~ dnorm(0,10),
    sigma ~ dcauchy (0,2)
  ),
  data=d, warmup=1000, iter= 2000, chains=1
) 

#no random intercept 
m2 <- map2stan(
  alist(
    Difference ~ dnorm(mu,sigma) , 
    mu <- bm*Meters + bt*TreatmentMu + bmt*Meters*TreatmentMu + a ,
    a ~ dnorm (0,10),
    bm ~ dnorm(0,10),
    bt ~ dnorm(0,10),
    bmt ~ dnorm(0,10),
    sigma ~ dcauchy (0,2)
  ),
  data=d, warmup=1000, iter= 2000, chains=1
) 
#no interaction 
m3 <- map2stan(
  alist(
    Difference ~ dnorm(mu,sigma) , 
    mu <- bm*Meters + bt*TreatmentMu + a_plot[Plot],
    a_plot[Plot] ~ dnorm(a,sigma_plot),
    a ~ dnorm (0,10),
    sigma_plot ~ dcauchy (0,2),
    bm ~ dnorm(0,10),
    bt ~ dnorm(0,10),
    sigma ~ dcauchy (0,2)
  ),
  data=d, warmup=1000, iter= 2000, chains=1
) 

#no meters no interaction 
m4 <- map2stan(
  alist(
    Difference ~ dnorm(mu,sigma) , 
    mu <- bt*TreatmentMu + a_plot[Plot],
    a_plot[Plot] ~ dnorm(a,sigma_plot),
    a ~ dnorm (0,10),
    sigma_plot ~ dcauchy (0,2),
    bt ~ dnorm(0,10),
    sigma ~ dcauchy (0,2)
  ),
  data=d, warmup=1000, iter= 2000, chains=1
) 


#comparing the WAIC values and weights for each model 
dgps.models <- compare(m1,m2,m3,m4)
dgps.models
#model 3 seems the best but let's see-->
#plotting the models values 
par(mfrow=c(1,1))
plot(dgps.models,SE=T, dSE=T)
coeftab(m1,m2,m3,m4)
plot(coeftab(m1,m2,m3,m4))


m1
#Checking the chains (trace plots, n_eff, Rhat) carefully to ensure they are valid
precis(m1,depth=2)
plot(precis(m1,depth=2))
pairs(m1)
plot(m1)
library(ggplot2)
library(ggmcmc) # uses ggplot2
m1s.ggs <- ggs(m1s@stanfit)
ggs_traceplot(m3s.ggs)

#checking for the variance-covariance matrix 
precis(m1s, corr = TRUE)


#plottint the posterior distribution of the estimates for treatments

post <- extract.samples(m1) 
gamma.Control <- post$a + post$bmt*0
gamma.Mussels <- post$a + post$bt + post$bmt*1

mean( gamma.Control) 
mean( gamma.Mussels)

dens( gamma.Control ,
      xlab="posterior distribution" , col=rangi2 )
dens( gamma.Mussels , add=TRUE )


