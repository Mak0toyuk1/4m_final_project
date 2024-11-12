# Add your own version of the dataset so it calls the right file on your system, don't delete other people's dataset.

diabetes_data <-read.csv("/home/evo/Mcmaster/4m03/R code/diabetes_dataset.csv") # Tony's dataset
diabetes_data <-read.csv("C:/Users/msafi/OneDrive/Documents/GitHub/4m_final_project/diabetes_dataset.csv") # Safi's dataset

library(ggplot2)
library(dplyr)# Library for the Shapiro-Wilk test


diabetes_pca<-prcomp(diabetes_data,scale=TRUE)
diabetes_pca

summary(diabetes_pca) # 80 percent rules suggests that we should take 5 principle components.
sum(diabetes_pca$sdev^2)

eigenvals <- diabetes_pca$sdev^2
eigenvals

cumsum(eigenvals)/sum(eigenvals) # same as last line in summary(heptathlon_pca2) "Cumulative Proportion"

plot(eigenvals,xlab="Principal Component",ylab="Eigenvalue",main="Eigenvalue vs. Principal) Component",type ="l" ) #Eigenvalues suggest that we should take 3 principle components.

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

# This is some text






## FA, we use 3 factors by the eigenvalue method.
# par(mfrow = c(1,3))
fa1<-factanal(diabetes_data, factors = 3, rotation = "none")
fa1
# plot(fa1$loadings[,1],fa1$loadings[,2], xlab = "U1", ylab = "U2", 
#      ylim = c(-1,1),xlim = c(-1,1), main="no rotation")
# text(fa1$loadings[,1]-0.1, fa1$loadings[,2]+0.1,colnames(food),col="red")
# abline(h = 0, v = 0)

fa2<-factanal(diabetes_data, factors = 3, rotation = "varimax")
fa2
# plot(fa2$loadings[,1],fa2$loadings[,2], xlab = "U1", ylab = "U2", 
#      ylim = c(-1,1),xlim = c(-1,1),main="with varimax")
# text(fa2$loadings[,1]-0.1, fa2$loadings[,2]+0.1,colnames(food),col="red")
# abline(h = 0, v = 0)

fa3<-factanal(diabetes_data, factors = 3, rotation = "promax")
fa3
# plot(fa3$loadings[,1],fa3$loadings[,2], xlab = "U1", ylab = "U2", 
#      ylim = c(-1,1),xlim = c(-1,1), main="with promax")
# text(fa3$loadings[,1]-0.1, fa3$loadings[,2]+0.1,colnames(food),col="red")
# abline(h = 0, v = 0)




