# Mike Bonsignore
# CIS 468
# Assignment 4
# 4/23/23

#### imports and data ####
library(caret)
library(NeuralNetTools)
library(kernlab)

bcdata <- read.csv(file = "breast_cancer.csv", stringsAsFactors = T)
names(bcdata)
summary(bcdata)
View(bcdata)
unique(bcdata$diagnosis)
bcdata <- bcdata[,c(-1)]


#### Holdout partitioning ####
set.seed(4567)
trainIndex <- createDataPartition(bcdata$diagnosis, p=.7, list=FALSE, times = 3)
bctrain <- bcdata[trainIndex,]
bctest <- bcdata[-trainIndex,]

#### Logistic Regression ####
trControl <- trainControl(method = 'repeatedcv',
                          number = 10,
                          repeats =  5)

logitFit <- train(diagnosis ~ ., data = bctrain, 
                  method = 'glm',
                  trControl = trControl,
                  family = 'binomial' )

summary(logitFit)

logitPredClass <- predict(logitFit,bctest)
logitPredProbs <- predict(logitFit,bctest,'prob')

# confusion matrix
confusionMatrix(logitPredClass, bctest$diagnosis, mode="everything", positive = "M")
varImp(logitFit,scale=FALSE)

#### Neural Net ####
trControl <- trainControl(method = 'cv', number = 10)

nnetFit <- train(diagnosis ~ ., data = bcdata,
                 method = 'nnet',
                 preProcess = c("center","scale"),
                 trControl = trControl)

# check CV results
plot(nnetFit)
plotnet(nnetFit, alpha = 0.6)

nnetPredClass <- predict(nnetFit,bctest)

# confusion matrix
confusionMatrix(nnetPredClass, bctest$diagnosis, mode="everything", positive = "M")
varImp(nnetFit,scale=FALSE)

#### SVM ####
trControl <- trainControl(method = 'repeatedcv',
                          number = 10,
                          repeats =  5)

svmFit <- train(diagnosis ~ ., data = bctrain, 
                method = 'svmPoly',
                trControl = trControl,
                preProcess = c("center","scale"))



svmPredict <- predict(svmFit, bctest)

# confusion matrix
confusionMatrix(svmPredict, bctest$diagnosis, mode="everything", positive = "M")
varImp(svmFit,scale=FALSE)
