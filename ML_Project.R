library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(corrplot)

trainRaw  <- read.csv('pml-training.csv',header=T)
head(trainRaw)
testRaw  <- read.csv('pml-testing.csv',header=T)
head(testRaw )
dim(trainRaw);dim(testRaw )


sum(complete.cases(trainRaw))

trainRaw <- trainRaw[, colSums(is.na(trainRaw)) == 0]
testRaw <- testRaw[, colSums(is.na(testRaw)) == 0]

classe <- trainRaw$classe
trainRemove <- grepl("^X|timestamp|window", names(trainRaw))
trainRaw <- trainRaw[, !trainRemove]
trainCleaned <- trainRaw[, sapply(trainRaw, is.numeric)]
trainCleaned$classe <- classe
testRemove <- grepl("^X|timestamp|window", names(testRaw))
testRaw <- testRaw[, !testRemove]
testCleaned <- testRaw[, sapply(testRaw, is.numeric)]


set.seed(22519) # For reproducibile purpose
inTrain <- createDataPartition(trainCleaned$classe, p=0.70, list=F)
trainData <- trainCleaned[inTrain, ]
testData <- trainCleaned[-inTrain, ]


controlRf <- trainControl(method="cv", 5)
modelRf <- train(classe ~ ., data=trainData, method="rf", trControl=controlRf, ntree=250)
modelRf


predictRf <- predict(modelRf, testData)
confusionMatrix(testData[['classe']], predictRf)


accuracy <- postResample(predictRf, testData$classe)
accuracy

oose <- 1 - as.numeric(confusionMatrix(testData$classe, predictRf)$overall[1])
oose


result <- predict(modelRf, testCleaned[, -length(names(testCleaned))])
result
