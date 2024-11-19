# Add your own version of the dataset so it calls the right file on your system, don't delete other people's dataset.

diabetes_data <-read.csv("/home/evo/Mcmaster/4m03/R code/diabetes_dataset.csv") # Tony's dataset
diabetes_data <-read.csv("C:/Users/msafi/OneDrive/Documents/GitHub/4m_final_project/diabetes_dataset.csv") # Safi's dataset

library(ggplot2)
library(dplyr)# Library for the Shapiro-Wilk test



#PCA # To be merged into EDA?
#################################################################################################################################
diabetes_pca<-prcomp(diabetes_data,scale=TRUE)
diabetes_pca

summary(diabetes_pca) # 80 percent rules suggests that we should take 5 principle components.
sum(diabetes_pca$sdev^2)

eigenvals <- diabetes_pca$sdev^2
eigenvals

cumsum(eigenvals)/sum(eigenvals) # same as last line in summary(heptathlon_pca2) "Cumulative Proportion"

plot(eigenvals,xlab="Principal Component",ylab="Eigenvalue",main="Eigenvalue vs. Principal) Component",type ="l" ) #Eigenvalues suggest that we should take 3 principle components.
#################################################################################################################################



Normality Test
#################################################################################################################################
# Normality test for factor analysis. 

# This is some text

shapiro.test(diabetes_data$Pregnancies) # Not normal, p= 2.2e-16

shapiro.test(diabetes_data$Glucose) # Not normal

shapiro.test(diabetes_data$BloodPressure) # Not normal

shapiro.test(diabetes_data$SkinThickness) # Not normal

shapiro.test(diabetes_data$Insulin) # Not normal

shapiro.test(diabetes_data$BMI) # Not normal

shapiro.test(diabetes_data$Age) # Not normal

# Using the normal QQ-Plot to confirm that the dataset is not normally distributed
qqnorm(diabetes_data[,1])
qqline(diabetes_data[,1])
#################################################################################################################################



Random Forest Classifier
#################################################################################################################################
set.seed(2024118)
train = sample (1: nrow(diabetes_data), nrow(diabetes_data)*0.75)
test = diabetes_data[-train,"Outcome"]

rf_tune = tune.randomForest(Outcome~., data = diabetes_data[train,], mtry = 1:8, ntree = 100*1:5, tunecontrol = tune.control(sampling = "cross",cross=5))
rf_tune

rf1=randomForest(Outcome~.,data=diabetes_data,subset=train,mtry=2, ntree = 200, importance=TRUE)
rf1
prediction1 = predict(rf1,diabetes_data[-train,],type="response")
prediction1 = round(prediction1, 0)
ConfusionMatrix<-table(test, prediction1)
ConfusionMatrix
MCR <- 1 - sum(diag(ConfusionMatrix)) / sum(ConfusionMatrix)
MCR

varImpPlot(rf1, main = "Variable Importance in Predicting Diabetes")
#################################################################################################################################


#################################################################################################################################

#Testing all the Supervised Leanring Analysis Methods With Our Diabetes Data Set

##############################################################################################################################3##

library(randomForest)
library(MASS)
library(tree)
library(e1071)
library(class)
library(rpart)
library(rattle )
library(gbm)
library(DAAG)
library(caret)

set.seed(2024118)

diabetes_data <-read.csv("C:/Users/msafi/OneDrive/Documents/GitHub/4m_final_project/diabetes_dataset.csv") # Safi's dataset
diabetes_data[,-9] <- scale(diabetes_data[,-9])
diabetes_data$Outcome<-factor(diabetes_data$Outcome)

set.seed(2024118)

train.index <- createDataPartition(diabetes_data$Outcome, p=.75, list = FALSE)
# train.index <- createDataPartition(diabetes_data$Outcome, p=.80, list = FALSE) # Try 80 percent? apparently this is a more common approach.
y <- diabetes_data[train.index,9]
x <- diabetes_data[train.index,-9]
y <- as.factor(y)


# Classification Tree

set.seed(2024118)
diabetes_tree <- rpart(Outcome ~.,data = diabetes_data,subset=train.index,method="class")
fancyRpartPlot(diabetes_tree)

diabetes_tree_pred=predict(diabetes_tree,diabetes_data[-train.index,],type="class")
diabetes_tab_tree<-table(diabetes_data[-train.index,9],diabetes_tree_pred)
diabetes_tab_tree

# KNN Classification
# 5-fold CV

set.seed(2024118)

diabetes_cv <- tune.knn(x, y, k = 1:10, tunecontrol = tune.control(sampling = "cross",cross=5))

summary(diabetes_cv)
plot(diabetes_cv)

set.seed(2024118)
diabetes_knn<-knn(train=x, test=diabetes_data[-train.index,-9], cl=y, k=3, prob=TRUE)
diabetes_knn
# ais_knn_pred=predict(ais_knn,ais[-train.index,],type="class")
tab_diabetes.knn<-table(diabetes_data[-train.index,9],diabetes_knn)
tab_diabetes.knn

# Bagging 

set.seed(2024118)
diabetes_bag=randomForest(Outcome~.,data=diabetes_data,subset=train.index,mtry=8,importance=TRUE,type="class")
diabetes_bag
diabetes_bag_pred=predict(diabetes_bag,diabetes_data[-train.index,],type="class")
diabetes_tab_bag<-table(diabetes_data[-train.index,9],diabetes_bag_pred)
diabetes_tab_bag

varImpPlot(diabetes_bag)
# random forests 

# Now use cross-validate 
set.seed(2024118)
diabetes_rf = tune.randomForest(Outcome~., data = diabetes_data[train.index,], mtry = 1:8,ntree=100*1:5,tunecontrol = tune.control(sampling = "cross",cross=5))
summary(diabetes_rf)
plot(diabetes_rf)

set.seed(2024118)
rf_diabetes=randomForest(Outcome~.,data=diabetes_data,subset=train.index,mtry=4,ntree=200,importance=TRUE,type="class")
rf_diabetes
rf_diabetes_pred=predict(rf_diabetes,diabetes_data[-train.index,],type="class")
tab_diabetes.rf<-table(diabetes_data[-train.index,9],rf_diabetes_pred)
tab_diabetes.rf

#Compare all use MCR??

# Classification Tree
1-classAgreement(diabetes_tab_tree)$diag

# k-Nearest Neighbours (kNN)
1-classAgreement(tab_diabetes.knn)$diag 

# Bagging 
1-classAgreement(diabetes_tab_bag)$diag

# Random Rorest
1-classAgreement(tab_diabetes.rf)$diag 


#Compare all use ARI??
#tree
classAgreement(diabetes_tab_tree)$crand

#knn
classAgreement(tab_diabetes.knn)$crand
#bagging 
classAgreement(diabetes_tab_bag)$crand
# random forest
classAgreement(tab_diabetes.rf)$crand
#boosting 










#################################################################################################################################





# Need to add tab.stuff to compare are the Supervised Learning Analysis Methods



