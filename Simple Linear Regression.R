
library(ISLR)
library(MASS)

?Pingos

# Simple linear regression
Pingos = read.csv('PingosDefectData.csv')
names(Pingos)

plot(quality_percentage~dens_curr,Pingos)

fit1 = lm(quality_percentage~dens_curr , data = Pingos)
fit1
summary(fit1)
?abline
abline(fit1 , col="red")

names(fit1)
?lm()
confint(fit1)


?predict(fit1,data.frame(lstat=Pingos$lstat))

predict(fit1,data.frame(dens_curr= c(1500,1600,1550)),interval = "confidence")
