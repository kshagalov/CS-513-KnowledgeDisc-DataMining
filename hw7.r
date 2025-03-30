# Author: Katherine Shagalov
# CWID: 10463818
# Assignment: HW 7 SVM


# install and load packages
install.packages(c("e1071", "caret"))
library(e1071)
library(caret)

# load data
breast_cancer_data <- read.csv("C:/Users/kshag/OneDrive/Desktop/classes/CS 513 - Knowledge Discovery and Data Mining/wisc_bc_ContinuousVar.csv")

breast_cancer_data$diagnosis <- as.factor(breast_cancer_data$diagnosis)

# spliting data into training/testing sets
set.seed(123)  # for reproducibility
split_index <- createDataPartition(breast_cancer_data$diagnosis, p = 0.7, list = FALSE)
train_data <- breast_cancer_data[split_index, ]
test_data <- breast_cancer_data[-split_index, ]

# building the SVM model
svm_model <- svm(diagnosis ~ ., data = train_data, kernel = "linear")

predictions_svm <- predict(svm_model, newdata = test_data)

confusion_matrix_svm <- table(predictions_svm, test_data$diagnosis)
print(confusion_matrix_svm)

accuracy_svm <- sum(diag(confusion_matrix_svm)) / sum(confusion_matrix_svm)
print(paste("Accuracy:", accuracy_svm))