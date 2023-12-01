# Installing  Libraries if you don't have it 

#install.packages("readr")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("DT")
#install.packages("caret")
#install.packages("reshape2")


# Load necessary libraries
library(readr)
library(dplyr) 
library(tidyr)
library(ggplot2)
library(DT)
library(caret)
library(reshape2)


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
numerical_cols <- c( "CurrentlyPositive", "Hospitalized", "IntensiveCare")

# Min-Max Normalization
data_normalized <- data_cleaned
data_normalized[numerical_cols] <- preProcess(data_cleaned[numerical_cols], method = c("range"))$x

head(data_normalized)


# Z-score Standardization
data_standardized <- data_cleaned
data_standardized[numerical_cols] <- preProcess(data_cleaned[numerical_cols], method = c("center", "scale"))$x

head(data_standardized)

# Robust Scalar
data_robust_scaled <- data_cleaned
data_robust_scaled[numerical_cols] <- preProcess(data_cleaned[numerical_cols], method = "range")$x

head(data_robust_scaled)

# Select columns for scaling
numerical_cols <- c("CumulativePositive")

# Assuming data_robust_scaled contains the scaled values
# Create a scatter plot to visualize the relationship between original and scaled values

plot(data_cleaned$Hospitalized, data_robust_scaled$Hospitalized, 
     xlab = "Original Values (CumulativePositive)", ylab = "Scaled Values (CumulativePositive)",
     main = "Range Scaling: Original vs Scaled", type = "p", col = "blue")





#TASK D

# Calculate correlation matrix for COVID dataset
correlation_matrix_covid <- cor(data_cleaned[, c("CumulativePositive", "CumulativeDeceased", "CumulativeRecovered", "CurrentlyPositive", "Hospitalized")], use = "complete.obs")

# Line plot for correlation
plot(correlation_matrix_covid, type = "l")

# Scatter plot matrix
pairs(data_cleaned[, c("CumulativePositive", "CumulativeDeceased", "CumulativeRecovered", "CurrentlyPositive", "Hospitalized")])

# Heatmap for correlation
cor_melted_covid <- melt(correlation_matrix_covid)
ggplot(cor_melted_covid, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "COVID Correlation Heatmap")


#TASK F

# Basic exploratory analysis for COVID dataset
summary(data_cleaned)
table(data_cleaned$CountryName)

# Subgroups analysis
# For example, subgroups based on EUcountry and EUCPMcountry variables
eu_analysis <- data_cleaned %>%
  group_by(CountryName, Region) %>%
  summarise(case_count = n())

total_rows <- nrow(eu_analysis)


# To print the first 80 rows of eu_analysis on datatable
datatable(head(eu_analysis,80))

# To print the first 20 rows of eu_analysis on console
print(head(eu_analysis, 20))
print(paste("Here is 20 rows of ", total_rows, "rows of total"))


