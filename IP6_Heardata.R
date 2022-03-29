# Importing the dataset
rm(list = ls())
dataset=read.csv('heart.data (1).csv')
#Displaying the count of null values per column
colSums(is.na(dataset))

hist(dataset$biking)
hist(dataset$smoking)
hist(dataset$heart.disease)


# Missing data
#na. rm = TRUE to exclude missing values
dataset$biking[is.na(dataset$biking)]<-median(dataset$biking, na.rm=TRUE)
dataset$smoking[is.na(dataset$smoking)]<-median(dataset$smoking, na.rm=TRUE)
dataset$heart.disease[is.na(dataset$heart.disease)]<-median(dataset$heart.disease, na.rm=TRUE)
colSums(is.na(dataset))
#Create multiple copies of the dataset with no missing data
dataset1<-dataset
dataset2<-dataset
dataset3<-dataset
dataset4<-dataset

##################################################################
## Multiple Linear Regression

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
split=sample.split(dataset$heart.disease,SplitRatio = 0.8)
training_set=subset(dataset,split==TRUE)
testing_set=subset(dataset,split==FALSE)

# Fitting Multiple Linear Regression to the Training set
regressor_mlr<-lm(formula=heart.disease~.,data = training_set)



# Predicting the Validation set results
y_pred=predict(regressor_mlr,newdata= testing_set)

new1<-data.frame(biking=45.0972, smoking=21.3856)
predict(regressor_mlr,newdata = new1)
new2<-data.frame(biking=8.27974, smoking=6.42372)
predict(regressor_mlr,newdata = new2)
new3<-data.frame(biking=42.3459, smoking=20.7413)
predict(regressor_mlr,newdata = new3)
new4<-data.frame(biking=30.7743, smoking=23.6102)
predict(regressor_mlr,newdata = new4)
#RMSE
library(caret)
RMSE(testing_set$heart.disease,y_pred)

########################################################
#Support Vector Regressor
# Splitting the dataset into the Training set and Test set

# Fitting SVR to the dataset
library(e1071)
regressor_SVR=svm(formula=heart.disease~.,data=training_set, 
                  type='eps-regression',
                  kernal='radial')

# Predicting the Validation set results
y_pred=predict(regressor_SVR,newdata = testing_set)

new1<-data.frame(biking=45.0972, smoking=21.3856)
predict(regressor_SVR,newdata = new1)
new2<-data.frame(biking=8.27974, smoking=6.42372)
predict(regressor_SVR,newdata = new2)
new3<-data.frame(biking=42.3459, smoking=20.7413)
predict(regressor_SVR,newdata = new3)
new4<-data.frame(biking=30.7743, smoking=23.6102)
predict(regressor_SVR,newdata = new4)

RMSE(testing_set$heart.disease,y_pred)
########################################################
#Decision Tree Regressor
# Splitting the dataset into the Training set and Test set

# Fitting to the dataset
library(rpart)
regressor_DT=rpart(formula=heart.disease~., data=training_set)

# Predicting the Validation set results
y_pred=predict(regressor_DT, newdata = testing_set)

new1<-data.frame(biking=45.0972, smoking=21.3856)
predict(regressor_DT,newdata = new1)
new2<-data.frame(biking=8.27974, smoking=6.42372)
predict(regressor_DT,newdata = new2)
new3<-data.frame(biking=42.3459, smoking=20.7413)
predict(regressor_DT,newdata = new3)
new4<-data.frame(biking=30.7743, smoking=23.6102)
predict(regressor_DT,newdata = new4)

RMSE(testing_set$heart.disease,y_pred)


########################################################
#Random Forest Regressor
# Splitting the dataset into the Training set and Test set

# Fitting to the dataset
library(randomForest)
set.seed(1234)

regressor_RF=randomForest(x=training_set[,1:2], y=training_set$heart.disease,ntree=20)

# Predicting the Validation set results
y_pred=predict(regressor_RF,newdata=testing_set)

new1<-data.frame(biking=45.0972, smoking=21.3856)
predict(regressor_RF,newdata = new1)
new2<-data.frame(biking=8.27974, smoking=6.42372)
predict(regressor_RF,newdata = new2)
new3<-data.frame(biking=42.3459, smoking=20.7413)
predict(regressor_RF,newdata = new3)
new4<-data.frame(biking=30.7743, smoking=23.6102)
predict(regressor_RF,newdata = new4)

RMSE(testing_set$heart.disease,y_pred)

