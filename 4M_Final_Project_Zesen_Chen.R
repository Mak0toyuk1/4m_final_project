data <- read.csv("C:/Users/10106/Downloads/diabetes_dataset.csv")

library(xgboost)
library(caret)
library(pROC)
library(ggplot2)

str(data)
summary(data)

data$Outcome <- as.factor(data$Outcome)

scaled_data <- data
scaled_data[, -ncol(data)] <- scale(data[, -ncol(data)])

set.seed(2024118)
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
print(paste("Misclassification Rate (MCR):", round(mcr, 4)))

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