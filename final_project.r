# Add your own version of the dataset so it calls the right file on your system, don't delete other people's dataset.

diabetes_data <-read.csv("/home/evo/Mcmaster/4m03/R code/diabetes_dataset.csv") # Tony's dataset
diabetes_data <-read.csv("C:/Users/msafi/OneDrive/Documents/GitHub/4m_final_project/diabetes_dataset.csv") # Safi's dataset
diabetes_data <- read.csv("/Users/xinyichen/Desktop/diabetes_dataset.csv") #Xiyi's dataset
diabetes_data <- read.csv("C:/Users/10106/Downloads/diabetes_dataset.csv") #Zensen's dataset

library(ggplot2)
library(dplyr)# Library for the Shapiro-Wilk test
library(tidyverse)

###############################################################################################################################

#Libraries Needed For Supervised Learning Analysis 

###############################################################################################################################
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
###############################################################################################################################
###############################################################################################################################

#Libraries Needed For Unupervised Learning (Clustering) Analysis 

###############################################################################################################################
library(pgmm)
library(dendextend)
library(mixture)
library(mclust)
library(MixGHD)
library(teigen)
library(ggplot2)
library(GGally)
library(cluster)
###############################################################################################################################

set.seed(2024118)

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

str(diabetes_data)
summary(diabetes_data)

diabetes_data$Outcome <- as.factor(diabetes_data$Outcome)

scaled_data <- diabetes_data
scaled_data[, -ncol(diabetes_data)] <- scale(diabetes_data[, -ncol(diabetes_data)])

set.seed(2024118)  # Correct seed
trainIndex <- createDataPartition(scaled_data$Outcome, p = 0.75, list = FALSE)
trainData <- scaled_data[trainIndex, ]
testData <- scaled_data[-trainIndex, ]

# Prepare data for XGBoost
trainMatrix <- model.matrix(Outcome ~ . - 1, data = trainData)
testMatrix <- model.matrix(Outcome ~ . - 1, data = testData)
trainLabel <- as.numeric(as.character(trainData$Outcome))
testLabel <- as.numeric(as.character(testData$Outcome))

# Train the XGBoost model
xgbModel <- xgboost(
  data = trainMatrix,
  label = trainLabel,
  max_depth = 6,
  eta = 0.1,
  nrounds = 100,
  objective = "binary:logistic",
  eval_metric = "auc",
  verbose = 0
)

# Make predictions on the test set
testPred <- predict(xgbModel, testMatrix)
testPredBinary <- ifelse(testPred > 0.5, 1, 0)

# Evaluate model performance
confMat <- confusionMatrix(as.factor(testPredBinary), as.factor(testLabel))
print(confMat)

# Compute Misclassification Rate (MCR)
mcr <- 1 - sum(diag(confMat$table)) / sum(confMat$table)
mcr

# Calculate AUC
rocCurve <- roc(testLabel, testPred)
aucValue <- auc(rocCurve)
print(paste("AUC:", aucValue))

# Plot ROC Curve
plot(rocCurve, main = "ROC Curve for Boosting Model")

# Feature importance
importance <- xgb.importance(model = xgbModel, feature_names = colnames(trainMatrix))
print(importance)
xgb.plot.importance(importance, main = "Feature Importance Plot")

#Comparing All Methods Using MCR

# Classification Tree
1-classAgreement(diabetes_tab_tree)$diag

# k-Nearest Neighbours (kNN)
1-classAgreement(tab_diabetes.knn)$diag 

# Bagging 
1-classAgreement(diabetes_tab_bag)$diag

# Random Rorest
1-classAgreement(tab_diabetes.rf)$diag 

# Boosting (Add Boosting MCR Here)
mcr <- 1 - sum(diag(confMat$table)) / sum(confMat$table)
mcr

###############################################################################################################################

# Performing Binary Logistic Regression

###############################################################################################################################

# Scale data
predictors <- c("Pregnancies", "Glucose", "BloodPressure", "SkinThickness", "Insulin", "BMI", "DiabetesPedigreeFunction","Age")
diabetes_data_scaled <- diabetes_data
diabetes_data_scaled[predictors] <- scale(diabetes_data[predictors])

# Splitting dataset
set.seed(2024118)
train = sample (1: nrow(diabetes_data_scaled), nrow(diabetes_data_scaled)*0.75)
diabetes_data.train= diabetes_data_scaled[train,]
diabetes_data.test=diabetes_data_scaled[-train,"Outcome"]
# fit bin. log. reg
diabetes_logreg <- glm(Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, data = diabetes_data.train, family = binomial("logit"))
summary(diabetes_logreg)

# Predict and convert to binary outcomes
class.pred <- ifelse(predict(diabetes_logreg, newdata = diabetes_data[-train, ], type = "response") > 0.5, 1, 0)

# Calculate confusion matrix, MCR, and CR
conf_matrix <- table(diabetes_data.test, class.pred)
MCR <- 1 - sum(diag(conf_matrix)) / sum(conf_matrix)
MCR
CR <- sum(diag(conf_matrix)) / sum(conf_matrix)
CR


#Remove BloodPressure, SkinThickness, Insulin bcs it has highest p-values
set.seed(2024118)
train = sample (1: nrow(diabetes_data), nrow(diabetes_data)*0.75)
diabetes_data.train= diabetes_data[train,]
diabetes_data.test=diabetes_data[-train,"Outcome"]
# fit bin. log. reg
diabetes_logreg <- glm(Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction + Age, data = diabetes_data.train, family = binomial("logit"))
summary(diabetes_logreg)
# Predict and convert to binary outcomes
class.pred <- ifelse(predict(diabetes_logreg, newdata = diabetes_data[-train, ], type = "response") > 0.5, 1, 0)

# Calculate confusion matrix, MCR, and CR
conf_matrix <- table(diabetes_data.test, class.pred)
MCR <- 1 - sum(diag(conf_matrix)) / sum(conf_matrix)
MCR
CR <- sum(diag(conf_matrix)) / sum(conf_matrix)
CR

#Remove Age, bcs it has highest p-values
set.seed(2024118)
train = sample (1: nrow(diabetes_data), nrow(diabetes_data)*0.75)
diabetes_data.train= diabetes_data[train,]
diabetes_data.test=diabetes_data[-train,"Outcome"]
# fit bin. log. reg
diabetes_logreg <- glm(Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction, data = diabetes_data.train, family = binomial("logit"))
summary(diabetes_logreg)
# Predict and convert to binary outcomes
class.pred <- ifelse(predict(diabetes_logreg, newdata = diabetes_data[-train, ], type = "response") > 0.5, 1, 0)

# Calculate confusion matrix, MCR, and CR
conf_matrix <- table(diabetes_data.test, class.pred)
MCR <- 1 - sum(diag(conf_matrix)) / sum(conf_matrix)
MCR
CR <- sum(diag(conf_matrix)) / sum(conf_matrix)
CR

###############################################################################################################################

#EDA Preperation For the Data

###############################################################################################################################

library(dplyr)
library(dendextend)
library(RColorBrewer)
library(reshape2)
library(ggthemes)
#install.packages("ggthemes") will need to run if you get error message "Error in library(ggthemes) : there is no package called 'ggthemes'"
library(GGally)
library(hdrcde)
#install.packages("hdrcde") will need to run if you get error message "Error in library(ggthemes) : there is no package called 'hdrcde'"
library(KernSmooth)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(wordcloud)
#install.packages("wordcloud") will need to run if you get error message "Error in library(ggthemes) : there is no package called 'wordcloud'"
library(wordcloud2)
#install.packages("wordcloud2") will need to run if you get error message "Error in library(ggthemes) : there is no package called 'wordcloud2'"
library(wesanderson)
#install.packages("wesanderson") will need to run if you get error message "Error in library(ggthemes) : there is no package called 'wesanderson'"
library(plotly)
library(kernlab)
library(vscc)
library(caret)

df <- diabetes_data

# quick look
head(df)
# Viewing variable/data types
str(df)
# Dimensions of the data
dim(df)
# Summary statistics
summary(df)
# Finds the count of missing values 
sum(is.na(df))
# Finds duplicated rows
sum(duplicated(df))
# Pair plot 
ggpairs(df[,-c(9)], aes(colour=as.factor(Outcome), alpha=0.4),lower=list(continuous="points"),
        axisLabels="none", switch="both")

# Plot of Outcomes 
# One variable, Tells us how many people do and do not have diabetes
outcomes <- df$Outcome
outcomes <- as.factor(outcomes)
plot(outcomes, xlab = "Diabetes", ylab = "Number of Individuals", 
     main = "Diabetes Dataset Outcomes Distribution", col = "lightgreen")

# Age distribution
ages <- df$Age
hist(ages, xlab = "Ages", main = "Age Distribution in Diabetes Dataset", col="skyblue")

# Correlations 
data2<-df[,-c(9)]
corr1<-cor(data2)
corrplot(corr1, bg = "white", 
         type="lower", tl.cex = 0.75, 
         tl.col="black", tl.srt = 45)

# Normality Test for Factor Analysis. 
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

###############################################################################################################################