install.packages(c("ggplot2", "dplyr", "tidyr", "readr", "reshape2"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(reshape2)
titanic_data <- read.csv("train.csv")
head(train.csv)

# Set the working directory to the folder where train.csv is located
setwd("C:/Users/sanyam/Desktop/titanic_data")  # Make sure the path uses forward slashes or double backslashes

# Check if the file exists in the working directory
if (file.exists("train.csv")) {
  # Load the Titanic dataset into R
  titanic_data <- read.csv("train.csv")
  
  # View the first few rows of the dataset
  head(titanic_data)
} else {
  cat("File 'train.csv' not found in the specified directory!")
}
getwd()

# Check the structure of the dataset
str(titanic_data)

# Summary of the dataset
summary(titanic_data)

# Check for missing values
colSums(is.na(titanic_data))

# Histogram for Age
ggplot(titanic_data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = 'lightblue', color = 'black') +
  labs(title = "Age Distribution", x = "Age", y = "Frequency")

# Pclass Distribution
ggplot(titanic_data, aes(x = factor(Pclass))) +
  geom_bar(fill = 'lightcoral', color = 'black') +
  labs(title = "Pclass Distribution", x = "Pclass", y = "Count")

# Sex Distribution
ggplot(titanic_data, aes(x = Sex)) +
  geom_bar(fill = 'lightpink', color = 'black') +
  labs(title = "Sex Distribution", x = "Sex", y = "Count")

# Embarked Distribution
ggplot(titanic_data, aes(x = Embarked)) +
  geom_bar(fill = 'lightblue', color = 'black') +
  labs(title = "Embarked Distribution", x = "Embarked", y = "Count")

# Boxplot for Age
ggplot(titanic_data, aes(x = "", y = Age)) +
  geom_boxplot(fill = 'lightblue') +
  labs(title = "Age Boxplot", y = "Age")

# Identifying Age outliers using IQR
age_IQR <- IQR(titanic_data$Age, na.rm = TRUE)
age_lower_bound <- quantile(titanic_data$Age, 0.25, na.rm = TRUE) - 1.5 * age_IQR
age_upper_bound <- quantile(titanic_data$Age, 0.75, na.rm = TRUE) + 1.5 * age_IQR
age_outliers <- titanic_data$Age[titanic_data$Age < age_lower_bound | titanic_data$Age > age_upper_bound]
print(age_outliers)

# Boxplot for Fare
ggplot(titanic_data, aes(x = "", y = Fare)) +
  geom_boxplot(fill = 'lightgreen') +
  labs(title = "Fare Boxplot", y = "Fare")

# Identifying Fare outliers using IQR
fare_IQR <- IQR(titanic_data$Fare, na.rm = TRUE)
fare_lower_bound <- quantile(titanic_data$Fare, 0.25, na.rm = TRUE) - 1.5 * fare_IQR
fare_upper_bound <- quantile(titanic_data$Fare, 0.75, na.rm = TRUE) + 1.5 * fare_IQR
fare_outliers <- titanic_data$Fare[titanic_data$Fare < fare_lower_bound | titanic_data$Fare > fare_upper_bound]
print(fare_outliers)


# Boxplot for Age vs. Survival
ggplot(titanic_data, aes(x = factor(Survived), y = Age)) +
  geom_boxplot(fill = c('lightgreen', 'lightcoral')) +
  labs(title = "Age vs Survival", x = "Survived", y = "Age")

# Boxplot for Fare vs. Survival
ggplot(titanic_data, aes(x = factor(Survived), y = Fare)) +
  geom_boxplot(fill = c('lightgreen', 'lightcoral')) +
  labs(title = "Fare vs Survival", x = "Survived", y = "Fare")

# Scatter plot for Age vs. Pclass
ggplot(titanic_data, aes(x = Age, y = factor(Pclass))) +
  geom_point(aes(color = factor(Survived))) +
  labs(title = "Age vs Pclass", x = "Age", y = "Pclass")

# Save the Age vs. Survival plot as an image (png format)
ggsave("Age_vs_Survival.png")
