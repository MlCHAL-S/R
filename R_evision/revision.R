# ============================================================================
# Combined R Script for Data Analysis and Statistical Learning
# ============================================================================

# =======================
# Session 1: 11XI
# =======================
# Setting the working directory (adjust according to your system)
# This ensures the script can access the necessary files.
setwd('Desktop/R/12XI')

# Load necessary libraries
# These libraries are used for statistical testing, data manipulation, and analysis.
library(nortest)    # For normality testing (Anderson-Darling test)
library(agricolae)  # Statistical analysis in agriculture (not used directly here)
library(car)        # Companion for applied regression
library(dplyr)      # Data manipulation
library(ExpDes)     # Experimental design and data analysis

# Load the dataset for analysis
srczas <- read.csv('SrCzasAlgorytmy.csv', sep = ';')  # Algorithm execution times
print(srczas)

# Sorting the data by the 'Algorytm' column
srczassort <- srczas[order(srczas[,'Algorytm']),]
print(srczassort)

# Perform Anderson-Darling test for normality on segments of data
adsrczasN <- c(
  ad.test(srczassort[1:20, 3])$p.value,
  ad.test(srczassort[21:40, 3])$p.value,
  ad.test(srczassort[41:60, 3])$p.value,
  ad.test(srczassort[61:80, 3])$p.value,
  ad.test(srczassort[81:100, 3])$p.value
)
print(adsrczasN)

# Create a function for applying Anderson-Darling test on segmented data
adsrczas <- function(y) {
  adsrcz <- rep(0, 5)  # Initialize a vector for storing p-values
  for (i in 1:length(adsrcz)) {
    adsrcz[i] <- ad.test(y[(1 + 20 * (i - 1)):(20 * i), 3])$p.value
  }
  print(adsrcz)
}

# Example usage:
adsrczas(srczassort)

# Load another dataset for analysis
spalanie <- read.csv('SrSpalanie.csv', sep = ';')  # Fuel consumption data
print(spalanie)

# =======================
# Session 2: 14X
# =======================
# Setting the working directory
setwd('/home/michal/Desktop/R/14X')

# Load the necessary libraries
if (!require(dplyr)) install.packages("dplyr")
if (!require(magrittr)) install.packages("magrittr")
if (!require(ggplot2)) install.packages("ggplot2")

library(dplyr)      # Data manipulation
library(magrittr)   # Pipe operators for cleaner syntax
library(ggplot2)    # Data visualization

# Load the dataset for analysis
data <- read.csv("wiek.csv")  # Employee ages
print(data)

# Sorting the data by the 'wiek' column
sorted_data <- data %>% arrange(wiek)
print("Sorted data:")
print(sorted_data)

# Define a function for descriptive statistics
descriptive_stats <- function(x) {
  min_val <- min(x)
  max_val <- max(x)
  mean_val <- mean(x)
  sd_val <- sd(x)  # Standard deviation
  var_val <- var(x)  # Variance
  median_val <- median(x)
  mode_val <- as.numeric(names(sort(table(x), decreasing = TRUE)[1]))  # Most frequent value
  lower_bound <- mean_val - 3 * sd_val  # 3-sigma lower bound
  upper_bound <- mean_val + 3 * sd_val  # 3-sigma upper bound
  outliers <- x[x < lower_bound | x > upper_bound]  # Outliers
  
  # Return a list of all statistics
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

# Apply the descriptive statistics function
stats <- descriptive_stats(data$wiek)
cat("Descriptive Statistics:\n")
cat("Min:", stats$min, "\n",
    "Max:", stats$max, "\n",
    "Mean:", stats$mean, "\n",
    "SD:", stats$sd, "\n",
    "Variance:", stats$variance, "\n",
    "Median:", stats$median, "\n",
    "Mode:", stats$mode, "\n",
    "Lower Bound (3-sigma):", stats$lower_bound, "\n",
    "Upper Bound (3-sigma):", stats$upper_bound, "\n",
    "Outliers:", ifelse(length(stats$outliers) > 0, paste(stats$outliers, collapse = ", "), "None"), "\n")

# Visualize data distribution
ggplot(data, aes(x = wiek)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Age Distribution", x = "Age", y = "Count")

# Plot cumulative histogram
h <- hist(data$wiek, breaks = c(10, 20, 30, 40, 50, 60), prob = T)
h$counts <- cumsum(h$counts)
plot(h, main = "Cumulative Histogram", xlab = "Age", ylab = "Cumulative Count")

# Cumulative distribution function plot
ggplot(data, aes(x = wiek)) +
  stat_ecdf(geom = "step", color = "red") +
  labs(title = "Cumulative Distribution Function of Age", x = "Age", y = "ECDF")

# Percentage of employees aged 31-40
age_31_40 <- data %>% filter(wiek >= 31 & wiek <= 40)
percentage_31_40 <- (nrow(age_31_40) / nrow(data)) * 100
print(paste("Percentage of employees aged 31-40:", round(percentage_31_40, 2), "%"))

# Percentage of employees aged ≤ 40
age_le_40 <- data %>% filter(wiek <= 40)
percentage_le_40 <- (nrow(age_le_40) / nrow(data)) * 100
print(paste("Percentage of employees aged ≤ 40:", round(percentage_le_40, 2), "%"))

# =======================
# Session 3: 18XI
# =======================
# Setting the working directory
setwd('Desktop/R/18XI')

# Load necessary libraries (already loaded earlier)
# No additional libraries introduced here.

# Reloading `SrCzasAlgorytmy.csv` for further analysis
srczas <- read.csv('SrCzasAlgorytmy.csv', sep = ';')  # Algorithm execution times
print(srczas)

# Sorting the dataset
srczassort <- srczas[order(srczas[, 'Algorytm']),]
print("Sorted dataset by Algorytm:")
print(srczassort)

# Reusing the Anderson-Darling testing function (defined earlier)
adsrczas(srczassort)

# Reloading `SrSpalanie.csv` for analysis of fuel consumption data
spalanie <- read.csv('SrSpalanie.csv', sep = ';')
print("Fuel Consumption Data:")
print(spalanie)

# =======================
# Session 4: 21X
# =======================
# Setting the working directory
setwd('Desktop/R/21X')

# Load the new dataset `WM.csv`
wm_data <- read.csv('WM.csv', sep = ';')  # Assumed to be a dataset for analysis
print("Loaded WM Dataset:")
print(wm_data)

# Perform preliminary analysis on `WM.csv`
# Since no specific operations are given, let’s assume:
# - Checking basic statistics
summary(wm_data)  # Summary statistics of the dataset

# =======================
# Session 5: 25XI
# =======================
# Setting the working directory
setwd('Desktop/R/25XI')

# Load datasets for advanced statistical and survey analysis
ancz10zm <- read.csv('AnCz10zm.csv', sep = ';')  # Example dataset (10 variables)
anczkw <- read.csv('AnCzKwestionariusz.csv', sep = ';')  # Survey data
platnosci <- read.csv('platnosci.csv', sep = ';')  # Payments/Financial data
probitlogit <- read.csv('ProbitLogit.csv', sep = ';')  # Probit and logistic regression dataset
serwer <- read.csv('serwer.csv', sep = ';')  # Server-related dataset

# Print dataset previews
print("AnCz10zm Dataset:")
print(head(ancz10zm))

print("Survey Data (AnCzKwestionariusz):")
print(head(anczkw))

print("Payments Data (Platnosci):")
print(head(platnosci))

print("Probit and Logit Data:")
print(head(probitlogit))

print("Server Data:")
print(head(serwer))

# Placeholder for specific statistical modeling (Probit/Logit)
# Example: Probit Model using `glm`
if (!require(stats)) install.packages("stats")
library(stats)

# Assume `dependent` and `independent` are column names in the `probitlogit` dataset
# Replace with actual column names if known
# Example Probit Regression:
probit_model <- glm(dependent ~ independent, family = binomial(link = "probit"), data = probitlogit)
summary(probit_model)

# Example Logistic Regression:
logit_model <- glm(dependent ~ independent, family = binomial(link = "logit"), data = probitlogit)
summary(logit_model)

# =======================
# Session 6: 28X
# =======================
# Setting the working directory
setwd('Desktop/R/28X')

# Load `Trendy.csv` for trend analysis
trendy <- read.csv('Trendy.csv', sep = ';')
print("Trend Data:")
print(head(trendy))

# Plotting trends over time
# Assuming `date` and `value` are columns in `Trendy.csv`
ggplot(trendy, aes(x = date, y = value)) +
  geom_line(color = "blue") +
  labs(title = "Trend Analysis", x = "Date", y = "Value") +
  theme_minimal()

# Load and analyze `WM.csv` again (if necessary)
wm_data <- read.csv('WM.csv', sep = ';')
print("WM Dataset (Revisited):")
print(head(wm_data))

# =======================
# Session 7: 4XI
# =======================
# Setting the working directory
setwd('Desktop/R/4XI')

# Reloading `Trendy.csv` to reinforce concepts
trendy <- read.csv('Trendy.csv', sep = ';')
print("Trend Data (Revisited):")
print(head(trendy))

# Revisit trend analysis from session 6
ggplot(trendy, aes(x = date, y = value)) +
  geom_line(color = "green") +
  labs(title = "Trend Analysis (Revisited)", x = "Date", y = "Value") +
  theme_minimal()

# =======================
# Summary: Key Lessons and Reusable Functions
# =======================
# 1. Anderson-Darling Test for Normality
#    Function `adsrczas()` defined and reused.
# 2. Descriptive Statistics
#    Function `descriptive_stats()` for min, max, mean, SD, variance, mode, etc.
# 3. Data Manipulation
#    Sorting, filtering, and summarizing datasets using `dplyr`.
# 4. Visualization
#    Created histograms, cumulative distributions, and line charts with `ggplot2`.
# 5. Advanced Modeling
#    Examples of Probit and Logistic regression models with `glm()`.

# End of Script
