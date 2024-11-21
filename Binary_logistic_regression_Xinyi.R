diabetes_data <- read.csv("/Users/xinyichen/Desktop/diabetes_dataset.csv")
head(diabetes_data)
#scale data
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


