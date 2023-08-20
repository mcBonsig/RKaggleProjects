# Mike Bonsignore
# CIS 468
# Assignment 3
# 4/9/23

library(caret)
library(pROC)

#### Exploration ####
weatherdata <- read.csv(file="seattle-weather.csv", stringsAsFactors = T)
summary(weatherdata)
View(weatherdata)

#### Preparations ####
weatherdata <- weatherdata[complete.cases(weatherdata),]
weatherdata <- weatherdata[,c(-1)]
weatherdata$weather <- as.factor(weatherdata$weather)

#### Partitioning ####
set.seed(1234)
trainIndex <- createDataPartition(weatherdata$weather, p=.7, list=FALSE, times = 1)
weatherdata_train <- weatherdata[trainIndex,]
weatherdata_test <- weatherdata[-trainIndex,]

#### Partition testing and training ####
prop.table(table(weatherdata_train$weather)) * 100
prop.table(table(weatherdata_test$weather)) * 100

#Inspect the descriptive statistics for each variable in the training and testing partition
summary(weatherdata_train)
summary(weatherdata_test)
View(weatherdata_train)
View(weatherdata_test)

#### CLASSIFICATIONS ####

  #### K - nearest neighbor #####
  ctrl_none <- trainControl(method="none")
  knnFit_none <- train(weather ~ ., data = weatherdata_train, method = "knn", trControl = ctrl_none, preProcess = "scale")
  knnFit_none
  
  # predict test data
  knnPredict_none <- predict(knnFit_none, newdata = weather_test)
  knnPredict_none
  
  # CONFUSION MATRIX
  confusionMatrix(knnPredict_none, weather_test$weather, mode="everything")

  #### Naive Bayes ####
  ctrl <- trainControl(method="cv")
  nbFit <- train(weather ~ ., data = weatherdata_train, method = "nb", trControl = ctrl, preProcess="scale")
  nbFit
  
  # evaluate performance of model
  nbPredict <- predict(nbFit, newdata = weatherdata_test)
  nbPredict
  
  # CONFUSION MATRIX
  confusionMatrix(nbPredict, weatherdata_test$weather, mode="everything")
  
  # could be more accurate, lets check this thing just in case
  varImp(nbFit,scale=FALSE)

  #### Decision Tree ####
  ctrl <- trainControl(method="cv")
  treeFit <- train(weather ~ ., data = weatherdata_train, method = "rpart", trControl = ctrl)
  treeFit
  summary(treeFit$finalModel)
  
  # checking importance attribute
  varImp(treeFit,scale=FALSE)
  
  # plot the tree
  plot(treeFit$finalModel, uniform=T)
  text(treeFit$finalModel, all=T, cex=.7)
  
  # use rplot for final tree to make it look prettier
  library(rpart.plot)
  rpart.plot(treeFit$finalModel)
  
  # predictions loading...
  treePredict <- predict(treeFit, newdata = weatherdata_test)
  treePredict
  
  # CONFUSION MATRIX
  confusionMatrix(treePredict, weatherdata_test$weather, mode="everything")
  
#### descriptive statistics ####
max(weatherdata$precipitation)
max(weatherdata$temp_max)  
min(weatherdata$temp_min)
max(weatherdata$wind)
summary(weatherdata$weather)

# visualizations
library(ggplot2)
library(gridExtra)
density1 <- ggplot(weatherdata, aes(x = precipitation, fill = weather)) + geom_density()
density2 <- ggplot(weatherdata, aes(x = wind, fill = weather)) + geom_density() 
density3 <- ggplot(weatherdata, aes(x = temp_max, fill = weather)) + geom_density()

grid.arrange(density1, density2, density3, ncol = 1)
