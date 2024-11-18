# Add your own version of the dataset so it calls the right file on your system, don't delete other people's dataset.

diabetes_data <-read.csv("/home/evo/Mcmaster/4m03/R code/diabetes_dataset.csv") # Tony's dataset
diabetes_data <-read.csv("C:/Users/msafi/OneDrive/Documents/GitHub/4m_final_project/diabetes_dataset.csv") # Safi's dataset

library(ggplot2)
library(dplyr)# Library for the Shapiro-Wilk test



PCA # To be merged into EDA?
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

rf_tune = tune.randomForest(Outcome~., data = diabetes_data[train,], mtry = 1:8, tunecontrol = tune.control(sampling = "cross",cross=5))
rf_tune

rf1=randomForest(Outcome~.,data=diabetes_data,subset=train,mtry=2,importance=TRUE)
rf1
prediction1 = predict(rf1,diabetes_data[-train,],type="response")
prediction1 = round(prediction1, 0)
ConfusionMatrix<-table(test, prediction1)
ConfusionMatrix
MCR <- 1 - sum(diag(ConfusionMatrix)) / sum(ConfusionMatrix)
MCR

varImpPlot(rf1)
#################################################################################################################################





# Need to add tab.stuff to compare are the Supervised Learning Analysis Methods



