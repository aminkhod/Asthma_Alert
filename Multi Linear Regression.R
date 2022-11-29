#install.packages('ISLR')
#install.packages('MASS')

library(ISLR)
library(MASS)

daily_data = read.csv('daily data1.csv')
#Multiple linear regression
fit1= lm(quality_percentage~dens_curr, data = daily_data)
summary(fit1)

fit2= lm(quality_percentage~dens_curr+hour,data = daily_data)

summary(fit2)

names(Pingos)

fit3= lm(quality_percentage~.-date,data = Pingos)

summary(fit3)

par(mfrow=c(2,2))

# Ei vs Yi
plot(fit3)

par(mfrow= c(1,1))

?update()
# fit4 = update(fit3,Pingos$crim~.-crim+medv- age-chas)
# summary(fit4)

# fit5=lm(medv~lstat*age ,data = Pingos)
# summary(fit5)
# 
# fit51=lm(medv~lstat:age ,data = Pingos)
# summary(fit51)
# bos=Pingos
# intract=bos$lstat*bos$age
# plot(medv~intract,data = Pingos)
# abline(fit51,Pingos,col="red")

# points(intract,fitted(fit5),col="blue")

fit52=lm(quality_percentage~dens_curr^2 ,data = Pingos)
summary(fit52)

I(c(2,3,3,6))
?I()

fit6 = lm(quality_percentage~dens_curr+ I(dens_curr^2) ,Pingos)
summary(fit6)

plot(quality_percentage~dens_curr,Pingos)
points(Pingos$dens_curr,fitted(fit6),col="red",pch=20,cex=1)

attach(Pingos)

b=poly(Pingos$dens_curr,4)
b

#pch
plot(1:20,1:20 ,pch=1:20 ,cex=3)

#d= Pingos

par(mfrow=c(1,1))
plot(quality_percentage~dens_curr)
points(dens_curr,fitted(fit6),col="red",pch=20,cex=1)

fit7 = lm(quality_percentage~poly(dens_curr,2) )
summary(fit7)

points(dens_curr,fitted(fit7),col="blue",pch=20, cex=2)

#Qualitative predictators
?Carseats
fix(Carseats)
View(Carseats)
names(Carseats)
summary(Carseats)

plot(Carseats$Sales~.,Carseats)

attach(Carseats)

fit8= lm(Sales~.+Income:Advertising+Age:Price,data=Carseats)
summary(fit8)

fit9= lm(Sales~+Income*Advertising+Age*Price)
summary(fit9)

contrasts(Carseats$ShelveLoc)

par(mfrow = c(2,2))
plot(Sales~ .,Carseats)
pairs(Carseats)

par(mfrow = c(1,1))

#writting R functions
regplot1 = function(x,y){
  fit = lm(y~x)
  plot(x,y)
  abline(fit,col= "green")
}


regplot1(Carseats$Price , Carseats$Sales)


regplot2 = function(x,y ,...){
  fit = lm(y~x)
  plot(x,y ,...)
  abline(fit,col= "blue")
}

regplot2(Carseats$Price, Carseats$Sales,ylab="Sale",xlab="Price",col="black",pch=4)

