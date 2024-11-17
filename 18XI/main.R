setwd('Desktop/R/18XI')

# Load data from the CSV file
srczas <- read.csv2("SrCzasAlgorytmy.csv", sep = ";", dec = ".")

# Load necessary libraries
library(nortest)    # For normality test (ad.test)
library(agricolae)  # For SNK test
#library(car)        # For Levene's test
library(dplyr)      # For data manipulation (%>% and group_by)
library(ExpDes)     # For SNK (Student-Newman-Keuls) test

# Sort the dataset by the "Algorytm" column
srczasort <- srczas[order(srczas[, "Algorytm"]),]

# Perform Anderson-Darling normality test for each group of 20 rows
adsrczasN <- c(
  ad.test(srczasort[1:20, 3])$p.value,
  ad.test(srczasort[21:40, 3])$p.value,
  ad.test(srczasort[41:60, 3])$p.value,
  ad.test(srczasort[61:80, 3])$p.value,
  ad.test(srczasort[81:100, 3])$p.value
)

# Define a function to perform Anderson-Darling normality test
adsrczas <- function(y) {
  adsrcz <- rep(0, 5) # Initialize a vector for results
  for (i in 1:length(adsrcz)) {
    adsrcz[i] <- ad.test(y[(1 + 20 * (i - 1)):(20 * i), 3])$p.value
  }
  print(adsrcz)
}


adsrczas(srczasort)

# Define a function to calculate means for each group
srcz <- function(y) {
  sredniczas <- rep(0, 5) # Initialize a vector for means
  for (i in 1:length(sredniczas)) {
    sredniczas[i] <- mean(y[(1 + 20 * (i - 1)):(20 * i), 3])
  }
  print(sredniczas)
}


srcz(srczasort)

# Define a function to calculate variances for each group
varsrczas <- function(y) {
  varsrcz <- rep(0, 5) # Initialize a vector for variances
  for (i in 1:length(varsrcz)) {
    varsrcz[i] <- var(y[(1 + 20 * (i - 1)):(20 * i), 3])
  }
  print(varsrcz)
}


varsrczas(srczasort)

# Perform one-way ANOVA to compare means across groups
aovsrczas <- aov(srczasort$Czas ~ srczasort$Algorytm)
anova(aovsrczas)

# Perform Kruskal-Wallis test (non-parametric alternative to ANOVA)
kruskal.test(srczasort$Czas, srczasort$Algorytm)

# Perform post-hoc tests
# Tukey's Honest Significant Difference (assumes equal group sizes)
TukeyHSD(aovsrczas)

# Prepare for SNK test
dfErrorSrCzas <- df.residual(aovsrczas)                  # Degrees of freedom
SSerrorSrCzas <- sum(aovsrczas$residuals^2)             # Sum of squares error
MSerrorSrCzas <- SSerrorSrCzas / dfErrorSrCzas          # Mean square error

# Perform SNK test (Student-Newman-Keuls)
snk(srczasort$Czas, srczasort$Algorytm, dfErrorSrCzas, SSerrorSrCzas)
