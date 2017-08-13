#clear workspace 
rm(list=ls())
#set working directory and read file 
setwd("C:/Users/Gabriela/Desktop/RUG/Project 1/DATA/dGPS/")
d2 <- read.csv("201705_gr_south.csv", header=T)
d1 <- read.csv("201703_gr_south.csv", header=T)
#get names of columns
names(d1)
names(d2)

#I've only got horizontal position "3" (central transect of plot for both times)
d2 <- d2[d2$horizontal %in% c('3'),]
d2
d1
 

#check for outliers 

boxplot2 <- boxplot(d2$z)
boxplot2$out

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

shapiro.test(d2$z)
qqnorm(d2$z)
qqline(d2$z)
hist(d2$z, prob=TRUE)
#not normal 

boxplot1 <- boxplot(d1$z)
boxplot1$out

zout1 <- remove_outliers(d1$z)
as.vector(zout1)
d1<-cbind(d1,zout1)
d1 <- d1[-which(is.na(d1$zout)),]

shapiro.test(d1$z)
qqnorm(d1$z)
qqline(d1$z)
hist(d1$z, prob=TRUE)
#not normal 

#rest the value of z on t0 from t1

d<-within(merge(d1,d2,by=c("plot","meters","treatment")), {
  z <- z.y - z.x
})[,c("treatment","plot","meters","z")]

d
names(d)


shapiro.test(d$z)
qqnorm(d$z)
qqline(d$z)
hist(d$z, prob=TRUE)


d$plot<- coerce_index(d$plot)  #factor --> integer
#turn treatment into 1, 0 categorical
d$TreatmentMu <- ifelse(d$treatment=="Mu",1,0)
#center the continuous predictor  
mean(d$meters)
d$meters <- d$meters - mean(d$meters)
#again keep only columns to be used by map2stan 
d <- data.frame(d$TreatmentMu,d$meters,d$plot,d$z)
colnames(d) <- c("treatmentmu", "meters", "plot", "z")
names(d)

m1s <- map2stan(
  alist(
    z ~ dnorm(mu,sigma) , 
    mu <- bm*meters + bt*treatmentmu + bmt*meters*treatmentmu + a_plot[plot],
    a_plot[plot] ~ dnorm(a,sigma_plot),
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
m2s <- map2stan(
  alist(
    z ~ dnorm(mu,sigma) , 
    mu <- a+ bm*meters + bt*treatmentmu + bmt*meters*treatmentmu,
    a ~ dnorm (0,10),
    bm ~ dnorm(0,10),
    bt ~ dnorm(0,10),
    bmt ~ dnorm(0,10),
    sigma ~ dcauchy (0,2)
  ),
  data=d, warmup=1000, iter= 2000, chains=1
) 

#no interaction 
m3s <- map2stan(
  alist(
    z ~ dnorm(mu,sigma) , 
    mu <- bm*meters + bt*treatmentmu + a_plot[plot],
    a_plot[plot] ~ dnorm(a,sigma_plot),
    a ~ dnorm (0,10),
    sigma_plot ~ dcauchy (0,2),
    bm ~ dnorm(0,10),
    bt ~ dnorm(0,10),
    sigma ~ dcauchy (0,2)
  ),
  data=d, warmup=1000, iter= 2000, chains=1
) 

#no meters no interaction 
m4s <- map2stan(
  alist(
    z ~ dnorm(mu,sigma) , 
    mu <- bt*treatmentmu + a_plot[plot],
    a_plot[plot] ~ dnorm(a,sigma_plot),
    a ~ dnorm (0,10),
    sigma_plot ~ dcauchy (0,2),
    bt ~ dnorm(0,10),
    sigma ~ dcauchy (0,2)
  ),
  data=d, warmup=1000, iter= 2000, chains=1
) 


#comparing the WAIC values and weights for each model 
dgps.models <- compare(m1s,m2s,m3s,m4s)
dgps.models
#model 3 seems the best but let's see-->
#plotting the models values 
par(mfrow=c(1,1))
plot(dgps.models,SE=T, dSE=T)
coeftab(m1s,m2s,m3s,m4s)
plot(coeftab(m1s,m2s,m3s,m4s))


m1s
#Checking the chains (trace plots, n_eff, Rhat) carefully to ensure they are valid
precis(m1s,digits=4)
plot(precis(m1s,depth=2))
pairs(m1s)
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

dens( gamma.Control ,
      xlab="posterior distribution" , col=rangi2 )
dens( gamma.Mussels , add=TRUE )


#example of the model for nomal data
#linear mixed model 
#library(lme4)
#library(lmerTest)
#names(d)
#m1 <- lmer(z~meters+treatment+meters*treatment+(1|plot),data=d)
#summary(m1)
#par(mfrow= c(2,2))
#plot(m1)
#checking for normality of residuals
#resid(m1) #List of residuals
#plot(density(resid(m1))) #A density plot
#qqnorm(resid(m1)) # A quantile normal plot - good for checking normality
#qqline(resid(m1))
#shapiro.test(resid(m1))
