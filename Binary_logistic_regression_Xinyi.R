diabetes_data <- read.csv("/Users/xinyichen/Desktop/diabetes_dataset.csv")
head(diabetes_data)
#diabetes_data$rank <- factor(diabetes_data$rank)
# Splitting dataset
set.seed(114)
train = sample (1: nrow(diabetes_data), nrow(diabetes_data)*0.75)
diabetes_data.train= diabetes_data[train,]
diabetes_data.test=diabetes_data[-train,"Outcome"]
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


#Remove SkinThickness, bcs it has highest p-values
set.seed(114)
train = sample (1: nrow(diabetes_data), nrow(diabetes_data)*0.75)
diabetes_data.train= diabetes_data[train,]
diabetes_data.test=diabetes_data[-train,"Outcome"]
# fit bin. log. reg
diabetes_logreg <- glm(Outcome ~ Pregnancies + Glucose + BloodPressure + Insulin + BMI + DiabetesPedigreeFunction + Age, data = diabetes_data.train, family = binomial("logit"))
summary(diabetes_logreg)
# Predict and convert to binary outcomes
class.pred <- ifelse(predict(diabetes_logreg, newdata = diabetes_data[-train, ], type = "response") > 0.5, 1, 0)

# Calculate confusion matrix, MCR, and CR
conf_matrix <- table(diabetes_data.test, class.pred)
MCR <- 1 - sum(diag(conf_matrix)) / sum(conf_matrix)
MCR
CR <- sum(diag(conf_matrix)) / sum(conf_matrix)
CR

#Remove Insulin, bcs it has highest p-values
set.seed(114)
train = sample (1: nrow(diabetes_data), nrow(diabetes_data)*0.75)
diabetes_data.train= diabetes_data[train,]
diabetes_data.test=diabetes_data[-train,"Outcome"]
# fit bin. log. reg
diabetes_logreg <- glm(Outcome ~ Pregnancies + Glucose + BloodPressure + BMI + DiabetesPedigreeFunction + Age, data = diabetes_data.train, family = binomial("logit"))
summary(diabetes_logreg)
# Predict and convert to binary outcomes
class.pred <- ifelse(predict(diabetes_logreg, newdata = diabetes_data[-train, ], type = "response") > 0.5, 1, 0)

# Calculate confusion matrix, MCR, and CR
conf_matrix <- table(diabetes_data.test, class.pred)
MCR <- 1 - sum(diag(conf_matrix)) / sum(conf_matrix)
MCR
CR <- sum(diag(conf_matrix)) / sum(conf_matrix)
CR
