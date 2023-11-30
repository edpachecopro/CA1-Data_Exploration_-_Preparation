# Installing  Libraries if you don't have it 

#install.packages("readr")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("DT")
#install.packages("caret")


# Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(DT)
library(caret)

# Load the dataset
data_full <- read_csv("Crimes-2017.csv")
data_full <- read_csv("Crimes-2017-full.csv")



# Print the first 10 lines
datatable(head(data, 8000))


# Check variable types

# Check variable types
cat_vars <- c("ID", "Case Number", "Primary Type", "Description", "Location Description", "Arrest", "Domestic", "FBI Code")
discrete_vars <- c("Beat", "District", "Ward", "Community Area", "Year")
continuous_vars <- c("IUCR")

# Visualize missing values
missing_values <- data %>%
  summarise_all(~ sum(is.na(.))) %>%
  gather(variable, count)

ggplot(missing_values, aes(x = variable, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Missing Values by Variable", x = "Variable", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# Cleaning Data

# Check for missing values and remove rows with any missing value
data <- data_full %>%
  na.omit()

# Display the number of rows and columns before and after removing missing values
cat("Before removing missing values:", nrow(data_full), "rows\n")
cat("After removing missing values:", nrow(data), "rows\n")



