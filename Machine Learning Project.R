# 1. Importing Data
data <- read.table("calibration.csv",sep = ',',header = TRUE,na.strings = c("",NA)) # Replacing blanks with NA
head(data)
str(data)
nrow(data)
ncol(data)

# 2. Preprocessing Data
selCols <- read.table("selCols.csv",sep=',',header = TRUE) # Selecting columns based on judgment and analysis of dataset
nrow(selCols)

data_v1 <- data[names(data) %in% selCols$Col.Names] # DataFrame with selected columns
names(data_v1)
str(data_v1)
nrow(data_v1)
ncol(data_v1)

colSums(is.na(data_v1))
colSums(is.na(data_v1[,colSums(is.na(data_v1)) > 0]))
rowsToSelect <- which(rowMeans(is.na(data_v1))<0.1) # Selecting rows with < 10% missing values
colsToSelect <- which(colMeans(is.na(data_v1))<0.1) # Selecting cols with < 10% missing values
data_v2 <- data_v1[rowsToSelect,colsToSelect]
nrow(data_v2)
ncol(data_v2)

colSums(is.na(data_v2)) # No ofmissing records columnwise
colSums(is.na(data_v2[,colSums(is.na(data_v2)) > 0]))
colToImp <- names(data_v2[colSums(is.na(data_v2))>0]) # Column names to impute
colToImp

# Imputing missing values
for (i in colToImp){
    if (class(data_v2[,i]) == 'character' || class(data_v2[,i]) ==  'factor'){
        data_v2[,i][is.na(data_v2[,i])] = names(which.max(summary(as.factor(data_v2[,i]))))
    }
    else{
        mean_val = mean(data_v2[,i],na.rm = TRUE)
        data_v2[,i][is.na(data_v2[,i])] = mean_val
    }
}

summary(as.factor(data_v2$age1))[1]
data_v3=data_v2[!names(data_v2) %in% "age1"] # age1 has 28% records with value as 0. Hence removing
colSums(is.na(data_v3[,colSums(is.na(data_v3)) > 0]))

str(data_v3)

# 3. Model Building
### 3.1: MODEL 1: LOGISTIC REGRESSION
# Creating Dummy Variables using caret package
library(caret)
dummy <- dummyVars("~.",data_v3,fullRank = T)
data_dummy <- data.frame(predict(dummy,newdata=data_v3))
names(data_dummy)
data_dummy$churn = as.factor(data_dummy$churn)

# Checking for Multicollinearity - VIF using car package
library(car)
vif(lm(churn~.,data=data_v3)) 
# VIF > 5. Moderate to High Multi Collinearity present --> PCA to be done

# Data partitioning using caret package
library(caret)
set.seed(42) # Roll No.
row = createDataPartition(data_dummy$churn,p=0.7,list=F) #70% records in Training, 30% in Validation
dtrain_lr = data_dummy[row,]
dval_lr = data_dummy[-row,]

# Principal Component Analysis using caret package. Data centered and scaled
rot = preProcess(dtrain_lr[,!names(dtrain_lr) %in% 'churn'],method=c('scale','center','pca'))
names(rot)
rot$rotation # Rotation matrix of PCA

dtrain_pca = predict(rot,dtrain_lr[,!names(dtrain_lr) %in% 'churn']) # Transforming Training set
head(dtrain_pca)
dtrain_pca$churn <- dtrain_lr$churn # Adding back outcome variable

# LOGISTIC REGRESSION MODEL - Iteration 1
lr_v1<-glm(churn~.,data=dtrain_pca,family="binomial")
print(summary(lr_v1))
anova(lr_v1,test="Chisq") # ANOVA summary

# Removing variables that are not significant in model: lr_v1
lr_v1_col = read.table("lr_v1.csv",sep=',',header = TRUE) # csv files with selected columns

dtrain_pca_v2 <- dtrain_pca[names(dtrain_pca)%in%lr_v1_col$Selected.PC] # Removing insignificant cols
dtrain_pca_v2$churn = dtrain_lr$churn

# LOGISTIC REGRESSION MODEL - Iteration 2
lr_v2<-glm(churn~.,data=dtrain_pca_v2,family="binomial")
print(summary(lr_v2))
anova(lr_v2,test="Chisq")

# Predicting Outcome in Validation dataset
# PCA transformation of Validation dataset using Rotation matrix
dval_pca = predict(rot,dval_lr[,!names(dval_lr) %in% 'churn'])
dval_pca <- dval_pca[names(dval_pca) %in% lr_v1_col$Selected.PC]
names(dval_pca)

lr_val <- predict(lr_v2,newdata=dval_pca, type="response") # Prediction on validation Dataset

# Draw ROC Curve
library("ROCR") # ROCR package to generate ROC curves 
pred<-prediction(lr_val,dval_lr$churn)
perf<-performance(pred,"tpr","fpr")
plot(perf,main="ROC Curve",colorize=T)


predcton<-ifelse(lr_val>0.5,1,0) #Fixing cutoff probability as 0.5 based on ROC curve
print("At cutoff=0.50")
tab_v1 <- table(dval_lr$churn,predcton)
acc_v1=(tab_v1[1,1]+tab_v1[2,2])/nrow(dval_lr) # Accuracy of Model: TP + TN / Total rows
print('Accuracy at 0.5 Cutoff probability');acc_v1  # 58%

predcton<-ifelse(lr_val>0.51,1,0) #Fixing cutoff probability as 0.5 based on ROC curve
print("At cutoff=0.51")
tab_v2 <- table(dval_lr$churn,predcton)
acc_v2=(tab_v2[1,1]+tab_v2[2,2])/nrow(dval_lr) # Accuracy of Model: TP + TN / Total rows
print('Accuracy at 0.51 Cutoff probability');acc_v2

predcton<-ifelse(lr_val>0.49,1,0) #Fixing cutoff probability as 0.5 based on ROC curve
print("At cutoff=0.49")
tab_v3 <- table(dval_lr$churn,predcton)
acc_v3=(tab_v3[1,1]+tab_v3[2,2])/nrow(dval_lr) # Accuracy of Model: TP + TN / Total rows
print('Accuracy at 0.49 Cutoff probability');acc_v3  


### 3.2: MODEL 2: RANDOM FOREST
dtrain_rf = data_v3[row,] # Training data with 70% records
dval_rf = data_v3[-row,] # Validation data with 30% records

library(randomForest)
dtrain_rf$churn <- as.factor(dtrain_rf$churn)

# RANDOM FOREST MODEL
rf_v1 <- randomForest(churn~.,dtrain_rf)
varImpPlot(rf_v1)

# Predicting Outcome in Validation dataset
rf_val <- predict(rf_v1,newdata=dval_rf)
rf_val

conf_tab = table(dval_rf$churn,rf_val)
conf_tab

rf_acc = (conf_tab[1,1]+conf_tab[2,2])/nrow(dval_rf)
print('Accuracy of model through Random Forest Algorithm'); rf_acc # 63%


### 3.3: MODEL 3 k Nearest Neighbours
dtrain_knn <- data_dummy[row,]
dval_knn <- data_dummy[-row,]

dtrain_knn_scaled <- scale(dtrain_knn[!names(dtrain_knn) %in% 'churn']) 
dval_knn_scaled <- scale(dval_knn[!names(dval_knn) %in% 'churn']) 

# kNN MODEL
library(class)
knn_v1 <- knn(dtrain_knn_scaled,dval_knn_scaled,dtrain_knn[,names(dtrain_knn) %in% 'churn'],k=3)
print("k=3")
print(table(dval_knn[,names(dval_knn) %in% 'churn'],knn_v1))

tab_kNN=table(dval_knn[,names(dval_knn) %in% 'churn'],knn_v1)
acc_knn=(tab_kNN[1,1]+tab_kNN[2,2])/nrow(dval_knn)
acc_knn # 53%

knn_v2 <- knn(dtrain_knn_scaled,dval_knn_scaled,dtrain_knn[,names(dtrain_knn) %in% 'churn'],k=101)
print("k=101")
print(table(dval_knn[,names(dval_knn) %in% 'churn'],knn_v2))

tab_kNN_2=table(dval_knn[,names(dval_knn) %in% 'churn'],knn_v2)
acc_knn_2=(tab_kNN_2[1,1]+tab_kNN_2[2,2])/nrow(dval_knn)
acc_knn_2 # 56%

## Calculating the TopDecileLift and Gini Coefficient
library(lift)
TopDecileLift(rf_val,dval_rf['churn'])
library(MLmetrics)
Gini(y_pred= rf_val,y_true =dval_rf[,'churn'])

#########
#Current Score and Future Score
#########

#Loading the current score and future score dataset
data_current = read.table("currrent_score.csv",sep = ',',header = TRUE,na.strings = c("",NA))
data_future = read.table("future_score.csv",sep = ',',header = TRUE,na.strings = c("",NA))


#Preprocessing the 2 new Datasets
data_c1 <- data_current[names(data_current) %in% names(data_v3)] # DataFrame with selected columns
data_f1 <- data_future[names(data_future) %in% names(data_v3)]


colSums(is.na(data_c1[,colSums(is.na(data_c1)) > 0]))
colSums(is.na(data_f1[,colSums(is.na(data_f1)) > 0]))

colSums(is.na(data_c1)) # No ofmissing records columnwise
colToImp_c <- names(data_c1[colSums(is.na(data_c1))>0]) # Column names to impute

colSums(is.na(data_f1)) # No ofmissing records columnwise
colToImp_f <- names(data_f1[colSums(is.na(data_f1))>0]) # Column names to impute


# Imputing missing values
for(i in colToImp_c){
    if (class(data_c1[,i]) == 'character' || class(data_c1[,i]) ==  'factor'){
        data_c1[,i][is.na(data_c1[,i])] = names(which.max(summary(as.factor(data_c1[,i]))))
    }
    else{
        mean_val = mean(data_c1[,i],na.rm = TRUE)
        data_c1[,i][is.na(data_c1[,i])] = mean_val
    }
}

for(i in colToImp_f){
    if (class(data_f1[,i]) == 'character' || class(data_f1[,i]) ==  'factor'){
        data_f1[,i][is.na(data_f1[,i])] = names(which.max(summary(as.factor(data_f1[,i]))))
    }
    else{
        mean_val = mean(data_f1[,i],na.rm = TRUE)
        data_f1[,i][is.na(data_f1[,i])] = mean_val
    }
}

data_c2=data_c1[!names(data_c1) %in% "age1"] # age1 has 28% records with value as 0. Hence removing
colSums(is.na(data_c2)) 
str(data_c2)
data_f2=data_f1[!names(data_f1) %in% "age1"] # age1 has 28% records with value as 0. Hence removing
colSums(is.na(data_f2)) 
str(data_c2)
names(data_f2)
names(data_c2)

nrow(data_c2) # Rows in Current Score dataset
ncol(data_c2) # Cols in Current Score dataset
nrow(data_f2) # Rows in Future Score dataset
ncol(data_f2) # Cols in Future Score dataset

# Predicting Outcome for both  dataset using Random Forest Model
rf_val_c <- predict(rf_v1,newdata=data_c2)
rf_val_c
rf_val_f <- predict(rf_v1,newdata=data_f2)
rf_val_f
#Getting Error as type of predictors donot match. Hence running a workaround obtained from stackoverflow
xtest <- rbind(dtrain_rf[1,!names(dtrain_rf)=='churn'] , data_f2)
xtest <- xtest[-1,]

rf_val_f <- predict(rf_v1,newdata=xtest)
rf_val_f

names(rf_val_f)=seq(1, nrow(data_f2))# edit row names to match earlier values

cout=cbind(data_current,outcome = rf_val_c)
c_data = data.frame(cout)
names(c_data)
write.csv(c_data,"CurrentScore_Outcome.csv",row.names = F)

fout=cbind(data_future,outcome = rf_val_f)
f_data = data.frame(fout)
names(f_data)
write.csv(f_data,"FutureScore_Outcome.csv",row.names = F)

