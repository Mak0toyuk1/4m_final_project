data <- read.csv("C:/Users/10106/Downloads/diabetes_dataset.csv")

library(xgboost)
library(caret)
library(pROC)
library(ggplot2)

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