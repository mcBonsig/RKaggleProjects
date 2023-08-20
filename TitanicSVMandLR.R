# Mike Bonsignore, TJ Riesset, Angelo Reppas
# CIS 468
# Group project
# 4/20/23

# imports
library(caret)
library(ggplot2)
library(cluster)
library(fpc)
library(scales)
titanic <- read.csv(file="titanic.csv", stringsAsFactors = TRUE)

names(titanic)

#### cleaning ####
summary(titanic)
View(titanic)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Embarked <- as.factor(titanic$Embarked)
titanic$SibSp <- as.factor(titanic$SibSp)
titanic$Survived <- as.factor(titanic$Survived)

summary(titanic)
titanic <- titanic[,c(-1, -4, -9, -11)]
titanic <- titanic[complete.cases(titanic),]
summary(titanic)


# survival rate by class
class1 <- subset(titanic, Pclass == 1)
summary(class1)

class2 <- subset(titanic, Pclass == 2)
summary(class2)

class3 <- subset(titanic, Pclass == 3)
summary(class3)

# subset age
AgeYoung1 <- subset(titanic, Age <= 10 & Pclass == 1)
summary(AgeYoung1)

FemaleSurvive1 <- subset(titanic, Sex == "female" & Pclass == 1)
summary(FemaleSurvive1)

AgeYoung2 <- subset(titanic, Age <= 10 & Pclass == 2)
summary(AgeYoung2)

FemaleSurvive2 <- subset(titanic, Sex == "female" & Pclass == 2)
summary(FemaleSurvive2)

AgeYoung3 <- subset(titanic, Age <= 10 & Pclass == 3)
summary(AgeYoung3)

FemaleSurvive3 <- subset(titanic, Sex == "female" & Pclass == 3)
summary(FemaleSurvive3)

FemaleSurvive <- subset(titanic, Sex == "female")
summary(FemaleSurvive)

MaleSurvive <- subset(titanic, Sex == "male")
summary(MaleSurvive)

#### visualizations ####
ggplot(titanic, aes(x = Sex, fill = Survived)) + geom_bar(position = "fill")
ggplot(titanic, aes(x = Survived, y = Age)) + geom_boxplot()
ggplot(titanic, aes(x = Age)) + geom_density()
ggplot(titanic, aes(x = Fare)) + geom_density()
ggplot(titanic, aes(x = Survived, y = Fare)) + geom_col()
ggplot(titanic, aes(x = Pclass, fill = Survived)) + geom_bar(position = "fill")
ggplot(titanic, aes(x = Pclass, y = Age, color = Survived)) + geom_point()
ggplot(titanic, aes(x = Pclass, y = Fare)) + geom_boxplot()

#### Partitioning ####
set.seed(4567)
trainIndex <- createDataPartition(titanic$Survived, p=.7, list=FALSE, times = 1)
titanic_train <- titanic[trainIndex,]
titanic_test <- titanic[-trainIndex,]

#### Logistic Regression ####
trControl <- trainControl(method = 'repeatedcv',
                          number = 10,
                          repeats =  5)

logitFit <- train(Survived ~ ., data = titanic_train, 
                  method = 'glm',
                  trControl = trControl,
                  family = 'binomial' )

summary(logitFit)

logitPredClass <- predict(logitFit, titanic_test)
logitPredProbs <- predict(logitFit, titanic_test,'prob')

#Now evaluate the classifier using the confusionMatrix() function
confusionMatrix(logitPredClass, titanic_test$Survived, mode="everything", positive = "1")

# check which variables are most important
varImp(logitFit, scale=FALSE, positive = "1")


#### SVM ####
trControl <- trainControl(method = 'repeatedcv',
                          number = 10,
                          repeats =  5)

svmFit <- train(Survived ~ ., data = titanic_train, 
                method = 'svmPoly',
                trControl = trControl,
                preProcess = c("center","scale"))



svmPredict <- predict(svmFit, titanic_test)

#Now evaluate the classifier using the confusionMatrix() function
confusionMatrix(svmPredict, titanic_test$Survived, mode="everything", positive = "1")

# check which variables are most important
varImp(svmFit,scale=FALSE)

#### Neural Net ####
trControl <- trainControl(method = 'cv', number = 10)

nnetFit <- train(Survived ~ ., data = titanic_train, method = 'nnet', preProcess = c("center","scale"), trControl = trControl)

#Examine the result of the cross validation
plot(nnetFit)

nnetPredClass <- predict(nnetFit,titanic_test)

#Now evaluate the classifier using the confusionMatrix() function
confusionMatrix(nnetPredClass, titanic_test$Survived, mode="everything", positive = "1")

#Plot the neural network with the NeuralNetTools package
library(NeuralNetTools)
plotnet(nnetFit, alpha = 0.6)

varImp(nnetFit,scale=FALSE)


##################
#Cluster use?
#################

titanic_norm <- titanic
titanic_norm <- titanic_norm[,c(-1, -2, -3, -5, -7, -8)]
summary(titanic_norm)
View(titanic_norm)
#titanic_norm$Survived <- rescale(titanic$Survived, to = c(0,1))
#titanic_norm$Pclass <- rescale(titanic$Pclass, to = c(0,1))
#titanic_norm$Sex <- rescale(titanic$Sex, to = c(0,1))
titanic_norm$Age <- rescale(titanic$Age, to = c(0,1))
#titanic_norm$Parch <- rescale(titanic$Parch, to = c(0,1))
titanic_norm$Fare <- rescale(titanic$Fare, to = c(0,1))
#titanic_norm$Cabin <- rescale(titanic$Cabin, to = c(0,1))
#titanic_norm$Embarked <- rescale(titanic$Embarked, to = c(0,1))

pairs(titanic)
pairs(titanic_norm)

summary(car_data_complete$MPG.Highway)
#run k-means with k = 2
km2 <- kmeans(titanic_norm, centers = 2) 
km2

summary(as.factor(km2$cluster))

#visualize results colored by cluster
pairs(titanic_norm, col=km2$cluster)

#run k-means with k = 3
km3 <- kmeans(car_norm, centers = 3) 
km3

summary(as.factor(km3$cluster))

#visualize results colored by cluster
pairs(new_car_data, col=km3$cluster)