# Add your own version of the dataset so it calls the right file on your system, don't delete other people's dataset.

diabetes_data <-read.csv("/home/evo/Mcmaster/4m03/R code/diabetes_dataset.csv") # Tony's dataset
diabetes_data <-read.csv("C:/Users/msafi/OneDrive/Documents/GitHub/4m_final_project/diabetes_dataset.csv") # Safi's dataset

library(ggplot2)
library(dplyr)# Library for the Shapiro-Wilk test
library(tidyverse)


#################################################################################################################################

#Libraries Needed For Supervised Learning Analysis 

#################################################################################################################################
library(randomForest)
library(MASS)
library(tree)
library(e1071)
library(class)
library(rpart)
library(rattle )
library(gbm)
library(caret)
library(xgboost)
library(pROC)
library(ggplot2)
#################################################################################################################################
#################################################################################################################################

#Libraries Needed For Unupervised Learning (Clustering) Analysis 

#################################################################################################################################
library(pgmm)
library(dendextend)
library(mixture)
library(mclust)
library(MixGHD)
library(teigen)
library(ggplot2)
library(GGally)
library(cluster)
#################################################################################################################################

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



# Random Forest Classifier
# #################################################################################################################################
# set.seed(2024118)
# train = sample (1: nrow(diabetes_data), nrow(diabetes_data)*0.75)
# test = diabetes_data[-train,"Outcome"]

# rf_tune = tune.randomForest(Outcome~., data = diabetes_data[train,], mtry = 1:8, ntree = 100*1:5, tunecontrol = tune.control(sampling = "cross",cross=5))
# rf_tune

# rf1=randomForest(Outcome~.,data=diabetes_data,subset=train,mtry=2, ntree = 200, importance=TRUE)
# rf1
# prediction1 = predict(rf1,diabetes_data[-train,],type="response")
# prediction1 = round(prediction1, 0)
# ConfusionMatrix<-table(test, prediction1)
# ConfusionMatrix
# MCR <- 1 - sum(diag(ConfusionMatrix)) / sum(ConfusionMatrix)
# MCR

# varImpPlot(rf1, main = "Variable Importance in Predicting Diabetes")
# #################################################################################################################################


#################################################################################################################################

#Testing Supervised Leanring Analysis Methods With Our Diabetes Data Set

##############################################################################################################################3##

rm(list=ls())

set.seed(2024118)

diabetes_data <-read.csv("C:/Users/msafi/OneDrive/Documents/GitHub/4m_final_project/diabetes_dataset.csv") # Safi's dataset
summary(diabetes_data)
diabetes_data[,-9] <- scale(diabetes_data[,-9])
diabetes_data$Outcome<-factor(diabetes_data$Outcome)

set.seed(2024118)

train.index <- createDataPartition(diabetes_data$Outcome, p=.75, list = FALSE)
# train.index <- createDataPartition(diabetes_data$Outcome, p=.80, list = FALSE) # Try 80 percent? apparently this is a more common approach.
y <- diabetes_data[train.index,9]
x <- diabetes_data[train.index,-9]
y <- as.factor(y)


# k-Nearest Neighbours Classification Using 5-Fold Cross Validation

set.seed(2024118)

diabetes_cv <- tune.knn(x, y, k = 1:10, tunecontrol = tune.control(sampling = "cross",cross=5))
summary(diabetes_cv)
plot(diabetes_cv)

set.seed(2024118)
diabetes_knn<-knn(train=x, test=diabetes_data[-train.index,-9], cl=y, k=3, prob=TRUE)
diabetes_knn
tab_diabetes.knn<-table(diabetes_data[-train.index,9],diabetes_knn)
tab_diabetes.knn

# Random Forest Classification  

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

varImpPlot(rf_diabetes)

# Boosting

str(data)
summary(data)

data$Outcome <- as.factor(data$Outcome)

set.seed(123)
trainIndex <- createDataPartition(data$Outcome, p = 0.7, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

train_matrix <- as.matrix(train_data[, -ncol(train_data)])  # Exclude Outcome
train_label <- as.numeric(as.character(train_data$Outcome))  # Convert Outcome to numeric
test_matrix <- as.matrix(test_data[, -ncol(test_data)])  # Exclude Outcome
test_label <- as.numeric(as.character(test_data$Outcome))

dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
dtest <- xgb.DMatrix(data = test_matrix, label = test_label)

params <- list(
  objective = "binary:logistic",  # Binary classification
  eval_metric = "auc",           # Evaluation metric: AUC
  max_depth = 6,                 # Maximum depth of a tree
  eta = 0.1,                     # Learning rate
  nthread = 2                    # Number of threads
)

set.seed(123)
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,               # Number of boosting rounds
  watchlist = list(train = dtrain, test = dtest),
  early_stopping_rounds = 10   # Early stopping
)

importance <- xgb.importance(model = xgb_model)
print(importance)

xgb.plot.importance(importance_matrix = importance)

pred_probs <- predict(xgb_model, dtest)
pred_classes <- ifelse(pred_probs > 0.5, 1, 0)

conf_matrix <- confusionMatrix(as.factor(pred_classes), as.factor(test_label))
auc_value <- auc(test_label, pred_probs)

print(conf_matrix)
cat("AUC:", round(auc_value, 4), "\n")

roc_curve <- roc(test_label, pred_probs)
plot(roc_curve, col = "blue", main = "ROC Curve for Boosting Model")




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

# Performing Cluster Analysis Using Various Methods

#################################################################################################################################


# Perform cluster analysis using model based clustering I: fit Gaussian parsimonious mixture with G=1:5, k-means initialization

diabetes_data[,-9] <- scale(diabetes_data[,-9]) #Scales all columns of the data except column 9: Outcome, which is categorical 
true.label <- diabetes_data[,9] # gets the true labels for the diabetes dataset

set.seed(2024118)

diabetes_data <- as.matrix(diabetes_data) # specify z to be a matrix for GPCM

# pairs(diabetes_data,col=true.label) 
# ggpairs(diabetes_data, columns = 2:4,  aes(color = Outcome, alpha = 0.5))

set.seed(2024118)

### Fitting Gaussian mixture using gpcm(...)

gpcm.out <- gpcm(diabetes_data,G=1:8, start = 2) # now gpcm fits.
summary(gpcm.out)
outcome.predict.gpcm <- gpcm.out$map # gets the vector of classifications for each observation
tab_gpcm <- table(true.label,outcome.predict.gpcm )
classAgreement(tab_gpcm)$crand
best <- get_best_model(gpcm.out)
best

### Fitting  t Parsimonious mixture using tpcm(...) from mixture

tpar = tpcm(diabetes_data, G=1:8,  start=2)
summary(tpar)
outcome.predict.tpar <- tpar$map # gets the vector of classifications for each observation
tab_t<- table(true.label,outcome.predict.tpar)
classAgreement(tab_t )$crand
best <- get_best_model(tpar)
best


### Fitting  mixtures of multivariate-t distributions using teigen(...) to the  data

t.out <- teigen(x=diabetes_data,Gs=1:5,init="kmeans") # fits mixtures of multivariate-t distributions to the data
outcome.predict.t <- t.out$classification # gets the vector of classifications for each observation
tab_mt<- table(true.label,outcome.predict.t)
classAgreement(tab_mt )$crand
best <- summary(t.out)
best

### compare performance using ARI ??
classAgreement(tab_gpcm )$crand
classAgreement(tab_t )$crand
classAgreement(tab_mt )$crand




