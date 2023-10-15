# Install and load the "mlbench" package
install.packages("mlbench")
library(mlbench)
library(caret)
library(e1071)
# Load the Pima Indians Diabetes dataset
data("PimaIndiansDiabetes")

# Check the structure of the dataset
str(PimaIndiansDiabetes)

# Example: Split the dataset into training and testing sets
set.seed(123)  # Set a random seed for reproducibility


# Assuming "diabetes" is the target variable
inTrain <- createDataPartition(y = PimaIndiansDiabetes$diabetes, p = 0.6, list = FALSE)
training_data <- PimaIndiansDiabetes[inTrain, ]
testing_data <- PimaIndiansDiabetes[-inTrain, ]



# Create a Naive Bayes classifier using the Pima Indians Diabetes dataset
pima_nb_model <- e1071::naiveBayes(diabetes ~ pregnant + glucose + pressure + triceps + insulin + mass + pedigree + age, data = PimaIndiansDiabetes)

# Check the summary of the Naive Bayes model
summary(pima_nb_model)

# Assuming you have already trained the Naive Bayes model (pima_nb_model) and have a testing dataset (testing_data)
predictions_nb_caret <- predict(pima_nb_model, newdata = testing_data[, c("pregnant", "glucose", "pressure", "triceps", "insulin", "mass", "pedigree", "age")])


# Calculate the confusion matrix
confusion_matrix <- confusionMatrix(predictions_nb_caret, testing_data$diabetes)
print(confusion_matrix)

# Create a plot based on the confusion matrix
plot(confusion_matrix$table)

## 2. Train a linear regression model (for regression) ----

### 2.a. Bootstrapping train control
# Assuming you have already split your data into training and testing sets
train_control <- trainControl(method = "boot", number = 500)

pima_lm_model <- caret::train(diabetes ~
                                              pregnant + glucose + pressure + triceps + insulin +
                                              mass + pedigree + age,
                                            data = training_data,
                                            trControl = train_control,
                                            na.action = na.omit, method = "glm", metric = "Accuracy")
# 1. Test the trained linear regression model using the testing dataset
predictions_lm <- predict(pima_lm_model, newdata = testing_data[, c("pregnant", "glucose", "pressure", "triceps", "insulin", "mass", "pedigree", "age")])

# 2. View the RMSE
rmse <- sqrt(mean((testing_data$diabetes - predictions_lm)^2))
cat("RMSE:", rmse, "\n")

#View the predicted values for the observations
print(predictions_lm)


# Assuming you have already split your data into training and testing sets

# Binary Classification: Logistic Regression with 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

pima_lr_model <- caret::train(diabetes ~ .,
                              data = training_data,
                              trControl = train_control,
                              na.action = na.omit,
                              method = "glm", 
                              family = "binomial", 
                              metric = "ROC")

# Test the trained logistic regression model using the testing dataset
predictions_lr <- predict(pima_lr_model, newdata = testing_data[, c("pregnant", "glucose", "pressure", "triceps", "insulin", "mass", "pedigree", "age")])

# Print the model and predicted values
print(pima_lr_model)
print(predictions_lr)
