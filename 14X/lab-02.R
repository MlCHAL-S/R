# Install necessary libraries if not already installed
if (!require(dplyr)) install.packages("dplyr")
if (!require(magrittr)) install.packages("magrittr")
if (!require(ggplot2)) install.packages("ggplot2")

# Load the libraries
library(dplyr)      # for data manipulation
library(magrittr)   # for using %>% pipes
library(ggplot2)    # for plotting

setwd("/home/michal/Desktop/R/14X")

data <- read.csv("wiek.csv")

# 1. Sort the data

sorted_data <- data %>%
  arrange(wiek)

print("Ex. 1")
print(sorted_data)

# 2. Define a function to calculate the following: minimum, maximum, mean, 
#    standard deviation (sd), variance (var), median, mode (dominanta), 
#    interval range using the 3-sigma rule, and check for outliers.

descriptive_stats <- function(x) {
  min_val <- min(x)
  max_val <- max(x)
  mean_val <- mean(x)
  
  # standard deviation - the average of the squared differences from the mean
  sd_val <- sd(x)
  
  # variance - the square root of the variance
  var_val <- var(x)
  
  # the middle value when all values are sorted
  median_val <- median(x) 
  
  # the most frequent value in the dataset
  # table() counts how often each value appears
  # sort - it sorts heh
  # as.numeric - converts it to a number
  mode_val <- as.numeric(names(sort(table(x), decreasing = TRUE)[1]))
  
  # 3-sigma rule: mean +- 3 * sd
  lower_bound <- mean_val - 3 * sd_val
  upper_bound <- mean_val + 3 * sd_val
  
  # Identify outliers (values outside 3-sigma interval)
  outliers <- x[x < lower_bound | x > upper_bound]
  
  list(
    min = min_val,
    max = max_val,
    mean = mean_val,
    sd = sd_val,
    variance = var_val,
    median = median_val,
    mode = mode_val,
    lower_bound = lower_bound,
    upper_bound = upper_bound,
    outliers = outliers
  )
}

stats <- descriptive_stats(data$wiek)
print(stats)


# 3. plot the Distribution
ggplot(data, aes(x = wiek)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Age Distribution of Employees", x = "Age", y = "Count")

# Plotting the Cumulative Distribution Function
ggplot(data, aes(x = wiek)) +
  stat_ecdf(geom = "step", color = "red") +
  labs(title = "Cumulative Distribution Function of Age", x = "Age", y = "ECDF")


# 4. Percentage of Employees Aged 31-40
age_31_40 <- data %>%
  filter(wiek >= 31 & wiek <= 40)

percentage_31_40 <- (nrow(age_31_40) / nrow(data)) * 100
print(paste("Percentage of employees aged 31-40: ", round(percentage_31_40, 2), "%"))

# age less than 40
age_le_40 <- data %>%
  filter(wiek <= 40)

percentage_le_40 <- (nrow(age_le_40) / nrow(data)) * 100
print(paste("Percentage of employees aged ≤ 40: ", round(percentage_le_40, 2), "%"))

