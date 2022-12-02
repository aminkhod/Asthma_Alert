#install.packages('caTools')
library(caTools)
require(ISLR)
#library(boot)
Dataset = read.csv('Content.csv',sep = ',')

Dataset = cbind.data.frame(Dataset[,3:55])

names(Dataset)
#attach(Dataset)
cor(Dataset)

cor.test(isDeleted,HasDeleteExpereince)
# pairs(Dataset)

#make train and test set
set.seed(80)
split <- sample.split(Dataset[,1], SplitRatio = 0.8)
#get training and test data
# split
train <- subset(Dataset, split)
test <- subset(Dataset, !split)
#Subset selection for logistic regression
#y=B0
glm_null=glm(train$isDeleted~1,data = train)
summary(glm_null)

glm_full=glm(train$isDeleted~ . ,data=train)
summary(glm_full)

stepwize=step(glm_null,
              scope = list(upper=glm_full,lower=glm_null),
              direction="forward",
              test="Chisq",
              data=train)
stepSum = summary(stepwize)

#fitting selected subset model
######## Both

