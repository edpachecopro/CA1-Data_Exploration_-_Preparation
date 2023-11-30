# Installing  Libraries if you don't have it 

#install.packages("readr")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("DT")


# Load the dataset
data <- read_csv("/Users/edgardpacheco/Library/CloudStorage/GoogleDrive-edgardpsilva@gmail.com/My Drive/01-CCT/Year-4/03 Tuesday - Mohamad - DataAnali/CA1/Crimes-2017.csv")


# Print the first 10 lines
datatable(head(data, 8000))


# Check variable types
cat_vars <- c("ID", "Case Number", "Primary Type", "Description", "Location Description", "Arrest", "Domestic", "FBI Code")
discrete_vars <- c("Beat", "District", "Ward", "Community Area", "Year")
continuous_vars <- c("IUCR")

