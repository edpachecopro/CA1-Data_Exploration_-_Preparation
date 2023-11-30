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
data <- read_csv("Crimes-2017.csv")
#data_full <- read_csv("Crimes-2017-test.csv")



# Print the first 10 lines
datatable(head(data, 8000))

#TASK A

# Check variable types

# Check variable types
cat_vars <- c("ID", "Case Number", "Primary Type", "Description", "Location Description", "Arrest", "Domestic", "FBI Code")
discrete_vars <- c("Beat", "District", "Ward", "Community Area", "Year")
continuous_vars <- c("IUCR")

# Visualize missing values Before Cleaning data 
missing_values <- data %>%
  summarise_all(~ sum(is.na(.))) %>%
  gather(variable, count)

ggplot(missing_values, aes(x = variable, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Missing Values by Variable", x = "Variable", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Cleaning Data

# Check for missing values and remove rows with any missing value
data_clean <- data %>%
  na.omit(data)

# Remove rows with any NA values across all columns
data_cleaned <- data[complete.cases(data), ]

# Display the number of rows and columns before and after removing missing values
cat("Before removing missing values:", nrow(data), "rows\n")
cat("After removing missing values:", nrow(data_clean), "rows\n")

# Visualize missing values After Cleaning data
missing_values <- data %>%
  summarise_all(~ sum(is.na(.))) %>%
  gather(variable, count)

ggplot(missing_values, aes(x = variable, y = count)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Missing Values by Variable", x = "Variable", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#TASK B

# Calculate statistical parameters
stat_parameters <- data %>%
  summarise(
    mean_IUCR = mean(IUCR, na.rm = TRUE),
    median_IUCR = median(IUCR, na.rm = TRUE),
    min_IUCR = min(IUCR, na.rm = TRUE),
    max_IUCR = max(IUCR, na.rm = TRUE),
    sd_IUCR = sd(IUCR, na.rm = TRUE)
  )

str(stat_parameters)


#TASK C
# Select numerical columns for normalization and standardization
numerical_cols <- c("IUCR")

# Min-Max Normalization
data_normalized <- data
data_normalized[numerical_cols] <- preProcess(data[numerical_cols], method = c("range"))$x

# Z-score Standardization
data_standardized <- data
data_standardized[numerical_cols] <- preProcess(data[numerical_cols], method = c("center", "scale"))$x

# Robust Scalar
data_robust_scaled <- data
data_robust_scaled[numerical_cols] <- preProcess(data[numerical_cols], method = c("center", "scale", "range"))$x



