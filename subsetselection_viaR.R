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



######## Forward


######## Backward

glm_selectedSubset = glm(formula = train$isDeleted ~ HasDeleteExpereince + like_count + 
                           X2 + X36 + X13 + X37 + X23 + is_mother + X7 + X20 + HasImage + 
                           X6 + comment_count + X5 + privacy_public + X34 + HasCover + 
                           app_version + X1 + X28 + X35 + initial_er + isParagnant + 
                           X22 + X11 + participator_count, 
                         data = train)
summary(glm_selectedSubset)
#predition
glm_probs2=predict(glm_selectedSubset,newdata=train,
                   type = "response")
glm_probs2
glm_pred2=ifelse(glm_probs2>0.57 ,1 ,0)

table(glm_pred2 ,train$isDeleted)
mean(glm_pred2==train$isDeleted)
 
# plot(table(HasDeleteExpereince,isDeleted))
# mean(isDeleted==HasDeleteExpereince)

# boxplot(Dataset[,2]~Dataset[,1])
