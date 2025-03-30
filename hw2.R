#  Course          : Data Mining
#  First Name      : Katherine
#  Last Name       : Shagalov
#  Id              : 10463818
#  purpose         : HW 2 EDA Analysis

## Step 0 clean up!!!
## remove all objects
rm(list=ls())


##1-Load the “breast-cancer-wisconsin.data.csv” from canvas into R and perform the EDA analysis by:
#Load the dataset and attach it
getwd()
setwd("C:/Users/kshag/OneDrive/Desktop/classes/CS 513 - Knowledge Discovery and Data Mining/")
data <- read.csv("breast-cancer-wisconsin.csv")
View(data)

# I. Summarizing each column (e.g. min, max, mean )
summary(data)
# II. Identifying missing values
# fixing F6 to recognize as number values
data$F6 <- as.numeric(data$F6)
missing_values <- is.na(data)
# III. Replacing the missing values with the “mean” of the column.
col_means <- colMeans(data, na.rm = TRUE)
# Replace missing values with column means
for (i in 1:ncol(data)) {
  data[is.na(data[, i]), i] <- col_means[i]
}
summary(data)

#IV. Displaying the frequency table of “Class” vs. F6
frequency_table <- table(data$Class, data$F6)

# Display the frequency table
print(frequency_table)
#V. Displaying the scatter plot of F1 to F6, one pair at a time
for (i in 1:6) {
  for (j in 1:6) {
    if (i != j) {
      plot(data[, i], data[, j], main = paste("F", i, " vs F", j), xlab = paste("F", i), ylab = paste("F", j))
    }
  }
}
#VI. Show histogram box plot for columns F7 to F9
selected_columns <- data[, 7:9]

# Plot histogram for each column (F7 to F9)
for (i in 1:3) {
  hist(selected_columns[, i], main = paste("Histogram for F", i+6), xlab = paste("F", i+6), breaks = 20)
}

# Plot box plot for each column (F7 to F9)
boxplot(selected_columns, main = "Boxplot for F7 to F9", names = c("F7", "F8", "F9"))



#2- Delete all the objects from your R- environment. Reload the “breast-cancer-wisconsin.data.csv” from
#canvas into R. Remove any row with a missing value in any of the columns.
# deleting everything
rm(list = ls())

# reloading
setwd("C:/Users/kshag/OneDrive/Desktop/classes/CS 513 - Knowledge Discovery and Data Mining/")
data <- read.csv("breast-cancer-wisconsin.csv")
View(data)
# fixing F6 to recognize as number values
data$F6 <- as.numeric(data$F6)  
data_no_missing <- data[complete.cases(data), ]
View(data_no_missing)



