library(caret)
library(kernlab) # for getting spam dataset
data(spam)

head(spam)
dim(spam)

# Create Train and Test partition
rows <- createDataPartition(y=spam$type, p=0.75, list = F)
train <- spam[rows,]
test <- spam[-rows,]
dim(train)

# Model creation
set.seed(32343)
model <- train(type~., data=train, method='glm')
model
model$finalModel
summary(model)

# Predict on Test set
pred <- predict(model, newdata = test)
pred

# Confusion Matrix
confusionMatrix(pred, test$type)
########################################################################################
#Data Slicing

# Cross Validation - create folds
set.seed(32323)
folds <- createFolds(y=spam$type, k=10, list=T, returnTrain = T)
sapply(folds, length)

folds <- createFolds(y=spam$type, k=10, list=T, returnTrain = F)
sapply(folds, length)

folds <- createResample(y=spam$type, times=10, list=T)
sapply(folds, length)

# Time Slices
set.seed(32323)
time <- 1:1000
folds <- createTimeSlices(y=time, initialWindow = 20, horizon = 10)
names(folds)
folds$train[[1]]
folds$test[[1]]
folds$train[[2]]
folds$test[[2]]

########################################################################################
# Plotting Predictors

install.packages('ISLR')
library(ISLR)
library(ggplot2)
library(caret)

data(Wage)
summary(Wage)
head(Wage)
dim(Wage)

# Create Train and Test partition
rows <- createDataPartition(y=Wage$wage, p=0.7, list = F)
train <- Wage[rows,]
test <- Wage[-rows,]
dim(train) ;dim(test)  

featurePlot(x = train[,c('age','education','jobclass')],y=train$wage, plot='pairs')
qplot(age, wage, data=train, colour=jobclass) 
# Add Regression SMoothers
qplot_1 <- qplot(age, wage, data=train, colour=education) 
qplot_1 <- qplot_1 + geom_smooth(method='lm', formula=y~x)
qplot_1

qplot_2 <- qplot(age, wage, data=train, colour=jobclass) 
qplot_2 <- qplot_2 + geom_smooth(method='lm', formula=y~x)
qplot_2

# Making Factors: cut2 function
library(Hmisc)
cutWage <- cut2(train$wage, g=3)
table(cutWage)

# Boxplot
boxplot_v1 <- qplot(cutWage, age, data=train, fill=cutWage, geom=c('boxplot'))
boxplot_v1

boxplot_v2 <- qplot(cutWage, age, data=train, fill=cutWage, geom=c('boxplot','jitter'))
boxplot_v2
library(grid)
library(gridExtra)
grid.arrange(boxplot_v1,boxplot_v2, ncol=2)
grid.arrange(boxplot_v1,boxplot_v2, ncol=1)

# Table
t1 <- table(cutWage,train$jobclass)
t1 # As number
prop.table(t1) # As proportion

# Density Plot
qplot(wage, colour=education, data=train, geom='density')
 
########################################################################################
# Preprocess

# Create Train and Test partition
rows <- createDataPartition(y=spam$type, p=0.75, list = F)
train <- spam[rows,]
test <- spam[-rows,]
dim(train)

hist(train$capitalAve, main='', xlab = 'ave. capital, run length')

mean(train$capitalAve)
sd(train$capitalAve)

trainCapAve <- train$capitalAve
trainCapAve_s <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAve_s)
sd(trainCapAve_s)

testCapAve <- test$capitalAve
testCapAve_s <- (testCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(testCapAve_s)
sd(testCapAve_s)

# Using caret package
preObj <- preProcess(train[,-58],method=c('center','scale'))
trainCapAveS <- predict(preObj,train[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)
# IMP: while processing the test set, use the preprocess object generated for train
testCapAveS <- predict(preObj,test[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)

hist(trainCapAveS)
qqnorm(trainCapAveS)

# BoxCox transform - continous data: make look like normalised data
preObj_v2 <- preProcess(train[,-58],method=c('BoxCox'))
trainCapAveS <- predict(preObj_v2,train[,-58])$capitalAve
hist(trainCapAveS)
qqnorm(trainCapAveS)


# Imputing Data
set.seed(13343)
train$capAve <- train$capitalAve
slectNA <- rbinom(dim(train)[1],size=1, prob=0.05) == 1
train$capAve[slectNA] <- NA

summary(train$capAve)

# Impute and Standardize
#install.packages('RANN')
library(RANN)

preObj <- preProcess(train[,-58], method=c('knnImpute'))
capAve <- predict(preObj, train[,-58])$capAve
summary(capAve)

# Standardize True Values
capAveTruth <- train$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)


# COVARIATE creation
# Create Train and Test partition
rows <- createDataPartition(y=Wage$wage, p=0.7, list = F)
train <- Wage[rows,]
test <- Wage[-rows,]
dim(train) ;dim(test) 

table(train$jobclass)

dummies <- dummyVars(wage~jobclass, data=train,fullRank = T)
head(predict(dummies, newdata=train))

nsv <- nearZeroVar(train, saveMetrics = T)
nsv
