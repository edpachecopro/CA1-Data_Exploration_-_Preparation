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
data <- read_csv("covid-19-EU.csv")


#data_full <- read_csv("Crimes-2017-test.csv")



# Print the first 10 lines
datatable(head(data, 8000))



# Cleaning Data

# Selecting only numeric columns
Hospitalized <- sapply(data, is.numeric)

# Filtering out rows with non-numeric values in numeric columns
cleaned_data <- data[complete.cases(data[Hospitalized]), ]

# Viewing the resulting cleaned dataset
datatable(head(cleaned_data, 30))


# Remove rows with any NA values across all columns
data_cleaned <- data[complete.cases(data), ]

# Display the number of rows and columns before and after removing missing values
cat("Before removing missing values:", nrow(data), "rows\n")
cat("After removing missing values:", nrow(data_clean), "rows\n")

data_cleaned <- data_cleaned[!is.na(as.numeric(data_cleaned$Hospitalized)), ]

#TASK A

# Check variable types


# Check variable types
cat_vars <- c("iso3", "CountryName", "Region", "EUcountry", "EUCPMcountry")
discrete_vars <- c("Date")
continuous_vars <- c("lat", "lon", "CumulativePositive", "CumulativeDeceased", "CumulativeRecovered", "CurrentlyPositive", "Hospitalized", "IntensiveCare")

# Visualize missing values Before Cleaning data 
missing_values <- data %>%
  summarise_all(~ sum(is.na(.))) %>%
  gather(variable, count)

ggplot(missing_values, aes(x = variable, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Missing Values by Variable", x = "Variable", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#TASK B

# Calculate statistical parameters
stat_parameters <- data_cleaned %>%
  summarise(
    MEAN = mean(Hospitalized, na.rm = TRUE,),
    MEDIAN = median(Hospitalized, na.rm = TRUE),
    MIN = min(Hospitalized, na.rm = TRUE),
    MAX = max(Hospitalized, na.rm = TRUE),
    SD = sd(Hospitalized, na.rm = TRUE)
  )

stat_parameters


#TASK C



# Select numerical columns for normalization and standardization
numerical_cols <- c("CumulativePositive", "CumulativeDeceased", "CumulativeRecovered", 
                    "CurrentlyPositive", "Hospitalized", "IntensiveCare")

# Min-Max Normalization
data_normalized <- data_cleaned
data_normalized[numerical_cols] <- preProcess(data_cleaned[numerical_cols], method = c("range"))$x

# Z-score Standardization
data_standardized <- data_cleaned
data_standardized[numerical_cols] <- preProcess(data_cleaned[numerical_cols], method = c("center", "scale"))$x

# Robust Scalar
data_robust_scaled <- data_cleaned
data_robust_scaled[numerical_cols] <- preProcess(data_cleaned[numerical_cols], method = c("center", "scale", "range"))$x



