# Author: Katherine Shagalov
# CWID: 10463818
# Assignment: Homework 6 RF C5



# install and load packages
install.packages(c("C50", "caret", "randomForest"))
library(C50)
library(caret)
library(randomForest)

breast_cancer_data <- read.csv("C:/Users/kshag/OneDrive/Desktop/classes/CS 513 - Knowledge Discovery and Data Mining/breast-cancer-wisconsin.csv")

colnames(breast_cancer_data)

feature_columns <- c("F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9")

for (col in feature_columns) {
  breast_cancer_data[[col]] <- as.factor(breast_cancer_data[[col]])
}

breast_cancer_data$Class <- as.factor(breast_cancer_data$Class)

set.seed(123)  # for reproducibility
split_index <- createDataPartition(breast_cancer_data$Class, p = 0.7, list = FALSE)
train_data <- breast_cancer_data[split_index, ]
test_data <- breast_cancer_data[-split_index, ]



# 6.1 Use the C5.0 methodology to develop a classification model for the Diagnosis.
c5_model <- C5.0(Class ~ ., data = train_data)

summary(c5_model)

predictions <- predict(c5_model, newdata = test_data)

confusion_matrix <- table(predictions, test_data$Class)
print(confusion_matrix)





#6.2 Use the Random Forest methodology to develop a classification model for the Diagnosis and identify important features.

rf_model <- randomForest(Class ~ ., data = train_data, ntree = 500)

print(rf_model)

predictions_rf <- predict(rf_model, newdata = test_data)

confusion_matrix_rf <- table(predictions_rf, test_data$Class)
print(confusion_matrix_rf)

importance <- importance(rf_model)
print(importance)
varImpPlot(rf_model, main = "Random Forest - Feature Importance")