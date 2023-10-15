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
