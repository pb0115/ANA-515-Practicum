# ANA-515-Practicum
Assignment

# Setting the working directory to the folder 
setwd("C:\\Users\\Dell\\Downloads")

# Installing and loading the necessary packages
install.packages(c("tidyverse", "readxl", "ggplot2"))
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggplot2)
library(lubridate)

# Reading the first data file which is sheet1
data1 <- read_excel("C:\\Users\\Dell\\Downloads\\survey.xlsx", sheet = 1)

# Reading the second data file which is sheet2
data2 <- read_excel("C:\\Users\\Dell\\Downloads\\survey.xlsx", sheet = 2)

# Replacing 0 values in Age column with NA in both data frames
data1$Age[data1$Age == 0] <- NA
data2$Age[data2$Age == 0] <- NA

# Combining the two data files into one data frame
merged_data <- rbind(data1, data2)

# Converting the numeric timestamps to character format
merged_data$Timestamp <- as.character(as.POSIXct(merged_data$Timestamp, origin = "1970-01-01"))

# Handling missing values
merged_data[is.na(merged_data)] <- NA  # Replacing NA values with NA

# Replacing empty strings with NA in character columns
character_cols <- sapply(merged_data, is.character)
merged_data[character_cols] <- lapply(merged_data[character_cols], function(x) ifelse(x == "", NA, x))

# Handling formatting and spelling errors
# Applying necessary transformations or corrections to specific columns here

# Cleaning the Gender column
merged_data$Gender <- ifelse(grepl("m|male", tolower(merged_data$Gender)), "Male",
                             ifelse(grepl("f|female", tolower(merged_data$Gender)), "Female", "Other/Unknown"))

# Cleaning country column
merged_data$Country <- recode(merged_data$Country, "US" = "United States", "UK" = "United Kingdom")

# Cleaning state column
merged_data$state <- recode(merged_data$state, "Texas" = "TX", "Newyork" = "NY", "California" = "CA")

# Cleaning the Treatment column
merged_data$treatment <- ifelse(grepl("y|yes", tolower(merged_data$treatment)), "Yes",
                                ifelse(grepl("n|no", tolower(merged_data$treatment)), "No", merged_data$treatment))

# Cleaning the no_employees column
merged_data$no_employees <- ifelse(merged_data$no_employees %in% c("05-Jan", "25-Jun", "44201", "44372"), NA, merged_data$no_employees)

# Cleaning the remote_work column
merged_data$remote_work <- ifelse(merged_data$remote_work %in% c("1", "2", "-"), NA, merged_data$remote_work)

# Cleaning the Tech_Company column
merged_data$tech_company <- ifelse(grepl("yes", tolower(merged_data$tech_company)), "Yes",
                                   ifelse(grepl("no", tolower(merged_data$tech_company)), "No", merged_data$tech_company))
# Exploring the structure of the merged data
str(merged_data)

# Identifying the missing values
sum(is.na(merged_data))

# Handling missing values
# In this example, I am imputing missing values in numeric columns with the mean
numeric_cols <- sapply(merged_data, is.numeric)
merged_data[numeric_cols] <- lapply(merged_data[numeric_cols], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# Identifying and handling outliers
# I am using boxplots to identify outliers in numeric columns and replace them with the median
numeric_cols <- sapply(merged_data, is.numeric)
outlier_threshold <- 1.5
for (col in names(merged_data)[numeric_cols]) {
  median_val <- median(merged_data[[col]], na.rm = TRUE)
  iqr <- IQR(merged_data[[col]], na.rm = TRUE)
  lower_bound <- median_val - outlier_threshold * iqr
  upper_bound <- median_val + outlier_threshold * iqr
  merged_data[[col]][merged_data[[col]] < lower_bound | merged_data[[col]] > upper_bound] <- median_val
}

# Handling formatting and spelling errors
# It involves renaming columns, correcting misspelled values, reformatting dates, etc.

# Visualizations
# Creating a histogram for the Age variable
ggplot(merged_data, aes(x = Age)) +
  geom_histogram(fill = "maroon", color = "black") +
  labs(title = "Age Histogram", x = "Age", y = "Frequency")

# Creating a bar plot for the Treatment variable
ggplot(merged_data, aes(x = treatment)) +
  geom_bar(fill = "yellow", color = "black") +
  labs(title = "Treatment Options", x = "Treatment", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Saving the cleaned dataset
write.csv(merged_data, file = "cleaned_data.csv", row.names = FALSE)

