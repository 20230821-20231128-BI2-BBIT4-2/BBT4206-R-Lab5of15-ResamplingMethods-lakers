# Install and load the "mlbench" package
install.packages("mlbench")
library(mlbench)

# Load the Pima Indians Diabetes dataset
data("PimaIndiansDiabetes")

# Check the structure of the dataset
str(PimaIndiansDiabetes)

# Example: Split the dataset into training and testing sets
set.seed(123)  # Set a random seed for reproducibility
library(caret)

# Assuming "diabetes" is the target variable
inTrain <- createDataPartition(y = PimaIndiansDiabetes$diabetes, p = 0.6, list = FALSE)
training_data <- PimaIndiansDiabetes[inTrain, ]
testing_data <- PimaIndiansDiabetes[-inTrain, ]

library(e1071)

# Create a Naive Bayes classifier using the Pima Indians Diabetes dataset
pima_nb_model <- e1071::naiveBayes(diabetes ~ pregnant + glucose + pressure + triceps + insulin + mass + pedigree + age, data = PimaIndiansDiabetes)

# Check the summary of the Naive Bayes model
summary(pima_nb_model)

# Assuming you have already trained the Naive Bayes model (pima_nb_model) and have a testing dataset (testing_data)
predictions_nb_caret <- predict(pima_nb_model, newdata = testing_data[, c("pregnant", "glucose", "pressure", "triceps", "insulin", "mass", "pedigree", "age")])


# Load the required packages
library(e1071)
library(caret)

# Assuming you have already split your data into training and testing sets
# Train a Naive Bayes classifier using the training dataset
pima_nb_model <- naiveBayes(diabetes ~ pregnant + glucose + pressure + triceps + insulin + mass + pedigree + age, data = training_data)

# Make predictions using the testing dataset
predictions <- predict(pima_nb_model, newdata = testing_data)

# Calculate the confusion matrix
confusion_matrix <- confusionMatrix(predictions, testing_data$diabetes)
print(confusion_matrix)

# Create a plot based on the confusion matrix
plot(confusion_matrix$table)





