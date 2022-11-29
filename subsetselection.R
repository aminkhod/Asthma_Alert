#install.packages('caTools')
#library(caTools)
#require(ISLR)
# Installing the package
#install.packages("dplyr")

# Loading package
library(dplyr)

# Loading package
library(caTools)
library(ROCR) 

#library(boot)
Pingos = read.csv('daily data1.csv')
names(Pingos)
# attach(Pingos)
#cor(Pingos[,2:23])

# pairs(Pingos[,2:23],col=Pingos$quality_percentage)


# # split train and test set
# set.seed(88)
# split <- sample.split(Pingos$cultivar_1, SplitRatio = 0.8)
# # get training and test data
# split
pingostrain <- Pingos[1:floor(length(Pingos$temp)*.8), ]
pingostest <- Pingos[floor(length(Pingos$temp)*.8):length(Pingos$temp), ]
#Subset selection for logistic regression
#y=B0
?glm()
lm_null=glm(pingostrain$risk.level~1, data = pingostrain, family = "binomial")
summary(lm_null)

lm_full=glm(pingostrain$risk.level~ . ,data=pingostrain, family = "binomial")
summary(lm_full)

stepwize=step(lm_null,
     scope = list(upper=lm_full),
     direction="both",
     test="Chisq",
     data=pingostrain)
summary(stepwize)
#fitting selected subset model 

lm_selectedSubset = glm(formula = quality_percentage ~ day + turning_speed + dens_reservoir + 
                         shift + month + prod_id + sup_nr_missing + dens_mand + viscosity + 
                         weight_raw + color_id + part_nr_in + dens_curr, data = pingostrain)

summary(lm_selectedSubset)
#predition
lm_probs2=predict(lm_selectedSubset ,newdata=pingostest,
                  type = "response")
# MSE
sum((lm_probs2-pingostest$quality_percentage)^2)/ length(lm_probs2)

# 