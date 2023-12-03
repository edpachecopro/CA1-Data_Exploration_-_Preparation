# Installing  Libraries if you don't have it 

#install.packages("readr")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("DT")
#install.packages("caret")
#install.packages("reshape2")
#install.packages("princomp")
#install.packages("tidyverse")


# Load necessary libraries
library(readr)
library(dplyr) 
library(tidyr)
library(ggplot2)
library(DT)
library(caret)
library(reshape2)
library(princomp)
library(tidyverse)

###############################################################################################################
# Load the dataset
###############################################################################################################

#loading dataset
data <- read_csv("covid-19-EU.csv")
nrow(data) #print the number of rows in the dataset

# Print the first 10 lines
datatable(head(data, 8000))

###############################################################################################################
# CLEANING DATA
###############################################################################################################

# Selecting only numeric columns
numerical_cols <- c("CumulativePositive", "CumulativeDeceased", "CumulativeRecovered", "CurrentlyPositive", "Hospitalized", "IntensiveCare")

# Replace NA values with mean for each numeric column
for (col in numerical_cols) {
  data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
}

# remove rows where only some values are NA:
data %>% filter_all(all_vars(!is.na(.)))
data %>% filter_all(all_vars(complete.cases(.)))  


# Min-Max Normalization for numerical columns
cleaned_data <- data
cleaned_data[numerical_cols] <- preProcess(data[numerical_cols], method = c("range"))$x

# Display the number of rows before and after removing missing values
cat("Before removing missing values:", nrow(data), "rows\n")
cat("After removing missing values:", nrow(cleaned_data), "rows\n")



###############################################################################################################
#TASK A
###############################################################################################################

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


###############################################################################################################
#TASK B
###############################################################################################################


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

###############################################################################################################
#TASK C
###############################################################################################################


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

plot(data_cleaned$CumulativePositive, data_robust_scaled$CumulativePositive, 
     xlab = "Original Values (CumulativePositive)", ylab = "Scaled Values (CumulativePositive)",
     main = "Range Scaling: Original vs Scaled", type = "p", col = "blue")

#print plot to visualise the Positive grouped by iso

data_cleaned <-  data_cleaned %>% group_by(iso3) %>% filter(sum(CumulativePositive)>0)


plot1 <- ggplot(data_cleaned, aes(x = iso3, y = CumulativePositive)) +
  geom_col(aes(fill = iso3)) +
  scale_y_continuous(name = "Cumulative Positive", scale())


#print plot to visualise the CumulativeRecovered grouped by CountryName

plot1<-  data_cleaned %>% group_by(CountryName) %>% filter((CumulativeRecovered)>0)
CumulativeRecovered <- ggplot(data_cleaned, aes(x = CountryName, y= CumulativeRecovered)) 
CumulativeRecovered + geom_col(aes(fill=CountryName)) 


#print plot to visualise the Hospitalized grouped by Region

plot2<-  data_cleaned %>% group_by(Region) %>% filter((Hospitalized)>0)
Hospitalized <- ggplot(data_cleaned, aes(x = Region, y= Hospitalized)) 
Hospitalized + geom_col(aes(fill=Region)) +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



###############################################################################################################
#TASK D
###############################################################################################################


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




###############################################################################################################
#TASK F
###############################################################################################################


# Basic exploratory analysis for COVID dataset
summary(data_cleaned)
table(data_cleaned$CountryName)

# Subgroups analysis
# For example, subgroups based on EUcountry and EUCPMcountry variables
eu_analysis <- data_cleaned %>%
  group_by(CountryName, Region) %>%
  summarise(case_count = n())

total_rows <- nrow(eu_analysis)

# To print the first 20 rows of eu_analysis on console
print(head(eu_analysis, 20))
print(paste("Here is 20 rows of ", total_rows, "rows of total"))


###############################################################################################################
#TASK G
###############################################################################################################

# Select columns for PCA (numerical variables)
pca_cols <- c("CumulativePositive", "CumulativeDeceased", "CumulativeRecovered", "CurrentlyPositive", "Hospitalized", "IntensiveCare")
pca_data <- data_cleaned[, pca_cols]

# Apply PCA
pca_result <- prcomp(pca_data, scale. = TRUE)

# Profile of the first few components
summary(pca_result)



###############################################################################################################
#TASK H
###############################################################################################################


# Handling missing values (if any)
# Replace missing values in numerical columns with the mean
numerical_cols <- c("CumulativePositive", "CumulativeDeceased", "CumulativeRecovered", "CurrentlyPositive", "Hospitalized", "IntensiveCare")

for (col in numerical_cols) {
  data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
}

# Min-Max Normalization for numerical columns
data_normalized <- data
data_normalized[numerical_cols] <- preProcess(data[numerical_cols], method = c("range"))$x

# Displaying first few rows of normalized data
head(data_normalized)

###############################################################################################################
#TASK G
###############################################################################################################

" 
Dimensionality reduction aims to simplify complex datasets by reducing the number of features or 
variables while retaining essential information. This process is beneficial in scenarios where 
datasets have high dimensionality, containing numerous features that might lead to increased 
computational complexity, overfitting, or noise.

Benefits of dimensionality reduction:
1. Computational Efficiency: Reducing dimensions can speed up computations and analyses, making complex 
algorithms more efficient.
2. Visualization: It helps in visualizing data in lower dimensions, making it easier to interpret and 
understand relationships.
3. Overfitting Mitigation: High-dimensional data can cause models to overfit. Reducing dimensions 
can mitigate this issue by focusing on essential information.
4. Noise Reduction: Eliminating irrelevant features can filter out noise, enhancing the 
signal-to-noise ratio in the data.
5. Feature Engineering: Dimensionality reduction can aid in feature selection or extraction, 
creating more robust and interpretable models.

Situations benefiting from dimensionality reduction include high-dimensional datasets like image 
recognition, genomics, natural language processing, and sensor data analysis. In these cases, 
reducing dimensions can enhance model performance, interpretability, and overall analysis efficiency.
"
