library(nortest) #ad.test	
library(agricolae) #SNK.test
library(car)	# Levene
library(dplyr)	# %>% group by ...
library(ExpDes)	# snk

library(nFactors) 
library(corpcor)
library(psych)
library(lavaan)

install.packages("name")
# ============================================================================
#                               LM, predicting values, plotting
# ============================================================================
data = read.csv('/home/michal/Desktop/R/Practice/data.csv')
par(mfrow=c(2,1))

plot(data$x, data$y)
points(data$x, data$y)

model <- lm(y ~ x, data = data)
summary(model)
coef(model)

abline(model)


# new data to predict
new_x <- data.frame(x = c(666))
predicted_y_600 <- predict(model, newdata = new_x)

# Ex 1
Hours_Studied <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
Exam_Score <- c(50, 55, 60, 65, 68, 72, 75, 80, 85, 90)

# Combine into a data frame
study_data <- data.frame(Hours_Studied, Exam_Score)

plot(study_data$Hours_Studied, study_data$Exam_Score)

study_model <- lm(Exam_Score ~ Hours_Studied, data = study_data)
abline(study_model)

new_hours <- data.frame(Hours_Studied = c(12, 13, 14))
exam_ecores <- predict(study_model, newdata = new_hours)



# Ex. 2
Ads_Placed <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
Products_Sold <- c(5, 8, 12, 15, 18, 20, 25, 28, 30, 35)

# Combine into a data frameTukeyHSD(anova_model)
ads_data <- data.frame(Ads_Placed, Products_Sold)
plot(ads_data$Ads_Placed, ads_data$Products_Sold)

ads_model <- lm(Products_Sold ~ Ads_Placed, data = ads_data)
abline(ads_model)

summary(ads_model)

new_ads <- data.frame(Ads_Placed = c(11, 12, 19))
new_ads_y <- predict(ads_model, newdata = new_ads)

# ============================================================================
#                               Trends, NLS (nonlinear least squares)
# R^2 - how well the regression model explains the variability in the data (1 - best, 0 - worst)
# Adjusted R^2 - penalizes the model for including too many predictors that don't significantly improve the fit.
# MSE (Mean Squared Error) - measures the average squared difference between the observed and predicted values.
# residual - diff between actual and predicted value
# ============================================================================
trends = read.csv("/home/michal/Desktop/R/data/Trendy.csv",header=T,sep=";")
par(mfrow = c(2, 2))

# more formulas:
# y ~ a * exp(b * x)
# y ~ a / (1 + b * exp(-c * x))
# y ~ a / (x + b)

### Y1 ###
plot(trends$y1)

model_y1 <- lm(y1 ~ t, data = trends)
lines(trends$t, predict(model_y1), col = 'red', lwd = 2)

model_y1_quad <- lm(y1 ~ t + I(t^2), data = trends)
lines(trends$t, predict(model_y1_quad), col = 'green', lwd = 2)

mse_y1 <- mean(residuals(model_y1)^2)
mse_y1_quad <- mean(residuals(model_y1_quad)^2)
cat('mse_y1: ', mse_y1)
cat('mse_y1_quad: ', mse_y1_quad)

### Y2 ###
plot(trends$y2)

model_y2_exp <- nls(y2 ~ a * exp(b * t), data = trends, start = list(a = 1, b = -0.1))

lines(trends$t, predict(model_y2_exp), col = 'black', lwd = 2)
mse_y2_exp <- mean(residuals(model_y2_exp)^2)

### Y3 ###
plot(trends$y3)

model_y3_hyper <- nls(y3 ~ a / (t + b), data = trends, start = list(a = 10, b = 1))
lines(trends$t, predict(model_y3_hyper), col = 'blue', lwd = 2)

model_y3_quad <- lm(y3 ~ t + I(t^2), data = trends)
lines(trends$t, predict(model_y3_quad), col = 'orange', lwd = 2)

mse_y3_hyper <- mean(residuals(model_y3_hyper)^2)
mse_y3_quad <- mean(residuals(model_y3_quad)^2)

### Y4 ###
plot(trends$y4)

model_y4_logistic <- nls(y4 ~ a / (1 + b * exp(-c * t)), 
                         data = trends, 
                         start = list(a = 100, b = 1, c = 0.1))
lines(trends$t, predict(model_y4_logistic), col = 'purple', lwd = 2)

model_y4_quad <- lm(y4 ~ t + I(t^2), data = trends)
lines(trends$t, predict(model_y4_quad), col = 'pink', lwd = 2)

mse_y4_logistic <- mean(residuals(model_y4_logistic)^2)
mse_y4_quad <- mean(residuals(model_y4_quad)^2)

# Print all metrics
cat("Metrics Summary:\n")
cat(sprintf("Y1 - MSE (Linear): %.3f, MSE (Quadratic): %.3f\n", mse_y1, mse_y1_quad))
cat(sprintf("Y2 - MSE (Exponential): %.3f\n", mse_y2_exp))
cat(sprintf("Y3 - MSE (Hyperbolic): %.3f, MSE (Quadratic): %.3f\n", mse_y3_hyper, mse_y3_quad))
cat(sprintf("Y4 - MSE (Exponential): %.3f, MSE (Quadratic): %.3f, MSE (Logistic): %.3f\n", 
            mse_y4_exp, mse_y4_quad, mse_y4_logistic))


### Test exercise
data <- read.csv("/home/michal/Desktop/R/data/kolokwium_data.csv")

# Podział na grupy
group1 <- subset(data, group == "Group 1")
group2 <- subset(data, group == "Group 2")

model_group1_lm <- lm(y ~ t, data = group1)
model_group1_quad <- lm(y ~ t + I(t^2), data = group1)
model_group1_logistic <- nls(y ~ a / (1 + b * exp(-c * t)), 
                             data = group1, 
                             start = list(a = 100, b = 5, c = 0.7))

# Obliczenie mierników dla Grupy 1
mse_group1_lm <- mean(residuals(model_group1_lm)^2)
mse_group1_quad <- mean(residuals(model_group1_quad)^2)
mse_group1_logistic <- mean(residuals(model_group1_logistic)^2)


r2_group1_lm <- summary(model_group1_lm)$r.squared
r2adj_group1_lm <- summary(model_group1_lm)$adj.r.squared

r2_group1_quad <- summary(model_group1_quad)$r.squared
r2adj_group1_quad <- summary(model_group1_quad)$adj.r.squared

# Second group
model_group2_lm <- lm(y ~ t, data = group2)
model_group2_quad <- lm(y ~ t + I(t^2), data = group2)
model_group2_exp <- nls(y ~ a * exp(b * t), 
                        data = group2, 
                        start = list(a = 10, b = -0.3))


mse_group2_lm <- mean(residuals(model_group2_lm)^2)
mse_group2_quad <- mean(residuals(model_group2_quad)^2)
mse_group2_exp <- mean(residuals(model_group2_exp)^2)


r2_group2_lm <- summary(model_group2_lm)$r.squared
r2adj_group2_lm <- summary(model_group2_lm)$adj.r.squared

r2_group2_quad <- summary(model_group2_quad)$r.squared
r2adj_group2_quad <- summary(model_group2_quad)$adj.r.squared



cat("Grupa 1 - MSE:", mse_group1_lm, mse_group1_quad, mse_group1_logistic, "\n")
cat("Grupa 1 - R^2:", r2_group1_lm, r2_group1_quad, "\n")
cat("Grupa 1 - Adjusted R^2:", r2adj_group1_lm, r2adj_group1_quad, "\n")

cat("Grupa 2 - MSE:", mse_group2_lm, mse_group2_quad, mse_group2_exp, "\n")
cat("Grupa 2 - R^2:", r2_group2_lm, r2_group2_quad, "\n")
cat("Grupa 2 - Adjusted R^2:", r2adj_group2_lm, r2adj_group2_quad, "\n")


# Wizualizacja dla Grupy 1
plot(group1$t, group1$y, main = "Group 1", pch = 16)
lines(group1$t, predict(model_group1_lm), col = 'red', lwd = 2)
lines(group1$t, predict(model_group1_quad), col = 'blue', lwd = 2)
lines(group1$t, predict(model_group1_logistic), col = 'green', lwd = 2)

# Wizualizacja dla Grupy 2
plot(group2$t, group2$y, main = "Group 2", pch = 16)
lines(group2$t, predict(model_group2_lm), col = 'red', lwd = 2)
lines(group2$t, predict(model_group2_quad), col = 'blue', lwd = 2)
lines(group2$t, predict(model_group2_exp), col = 'green', lwd = 2)
# ============================================================================
# VARIANCE ANALYSIS (ANOVA AND RELATED TESTS)
# ============================================================================

# 1. Custom dataset: Simulate data for four groups (A, B, C, D)
# Each group has 10 observations.
group <- rep(c("A", "B", "C", "D"), each = 10)  # 4 groups, 10 values per group
time <- c(rnorm(10, mean = 100, sd = 5),  # Group A: mean 100, sd 5
          rnorm(10, mean = 110, sd = 5),  # Group B: mean 110, sd 5
          rnorm(10, mean = 105, sd = 5),  # Group C: mean 105, sd 5
          rnorm(10, mean = 120, sd = 5))  # Group D: mean 120, sd 5

anova_data <- data.frame(Group = group, Time = time)  # Combine into a data frame

# ============================================================================
# TERMS TO KNOW:
# ----------------------------------------------------------------------------
# - ANOVA: Compares the means of 2+ groups to determine if they are significantly different.
# - Normality Test (Anderson-Darling): Checks if data follows a normal distribution.
# - Homogeneity of Variances (Levene's Test): Checks if group variances are equal.
# - TukeyHSD: Post-hoc test to identify *which* group means differ.
# - SNK Test: Clusters similar groups into homogeneous subsets.
# - Kruskal-Wallis: Non-parametric test for median differences (if assumptions fail).
# ============================================================================

# ============================================================================
# STEP 1: NORMALITY TESTS (CHECKING ASSUMPTIONS BEFORE ANOVA)
# ============================================================================
library(nortest)

# Perform Anderson-Darling normality test for each group
ad.test(anova_data$Time[anova_data$Group == "A"])
ad.test(anova_data$Time[anova_data$Group == "B"])
ad.test(anova_data$Time[anova_data$Group == "C"])
ad.test(anova_data$Time[anova_data$Group == "D"])

# Interpretation:
# If p-value >= 0.05, the group follows a normal distribution.
# If p-value < 0.05, the group does not follow a normal distribution (use non-parametric tests like Kruskal-Wallis).

# ============================================================================
# STEP 2: ANOVA TEST
# ============================================================================
# Fit ANOVA model
anova_model <- aov(Time ~ Group, data = anova_data)
summary(anova_model)

# Interpretation:
# - Df: Degrees of freedom. Group = k-1, Residuals = n-k (where k = number of groups, n = total observations).
# - Sum Sq: Sum of Squares. Group = Between-group variability, Residuals = Within-group variability.
# - Mean Sq: Mean Squares. Sum Sq / Df.
# - F-value: F = Mean Sq(Group) / Mean Sq(Residuals).
# - Pr(>F): p-value for the F-test. If p < 0.05, group means are significantly different.

# ============================================================================
# STEP 3: POST-HOC TESTS
# ============================================================================
# TukeyHSD (pairwise comparisons to determine which groups differ)
TukeyHSD(anova_model)

# SNK Test (Student-Newman-Keuls)
library(agricolae)
df_error <- df.residual(anova_model)            # Residual degrees of freedom
ss_error <- sum(anova_model$residuals^2)       # Residual sum of squares

snk(anova_data$Time, anova_data$Group, df_error, ss_error)

# Interpretation:
# - TukeyHSD: Shows the difference (diff) between groups, confidence intervals (lwr, upr), and p-values (p adj).
# - SNK Test: Groups means into homogeneous subsets.

# ============================================================================
# STEP 4: HOMOGENEITY OF VARIANCES (LEVENES TEST)
# ============================================================================
library(car)
leveneTest(Time ~ Group, data = anova_data)

# Interpretation:
# - If p-value >= 0.05: Variances are equal (homogeneity assumption holds).
# - If p-value < 0.05: Variances are not equal (consider robust ANOVA or Kruskal-Wallis).

# ============================================================================
# STEP 5: KRUSKAL-WALLIS TEST (NON-PARAMETRIC ALTERNATIVE)
# ============================================================================
kruskal.test(Time ~ Group, data = anova_data)


# Interpretation:
# - Kruskal-Wallis tests median differences between groups.
# - Used if normality or homogeneity assumptions are violated.


# Ex. 1
mean_time <- read.csv2("/home/michal/Desktop/R/data/SrCzasAlgorytmy.csv", sep=";", dec=".")
mean_time_sorted <- mean_time %>% arrange(Algorytm)

# means
means <- mean_time_sorted %>%
  group_by(Algorytm) %>%
  summarize(Srednia = mean(Czas))

# var
vars <- mean_time_sorted %>%
  group_by(Algorytm) %>%
  summarize(Var = var(Czas))

# check for normality, if p.value >= 0.05, data follows the normal distribution
ad.test(mean_time_sorted$Czas[mean_time_sorted$Algorytm == 'A1'])$p.value
ad.test(mean_time_sorted$Czas[mean_time_sorted$Algorytm == 'A2'])$p.value
ad.test(mean_time_sorted$Czas[mean_time_sorted$Algorytm == 'A3'])$p.value
ad.test(mean_time_sorted$Czas[mean_time_sorted$Algorytm == 'A4'])$p.value
ad.test(mean_time_sorted$Czas[mean_time_sorted$Algorytm == 'Aref'])$p.value

# anova, if p.value < 0.05, groups are significantly different, proceed
anova_model <- aov(mean_time_sorted$Czas ~ mean_time_sorted$Algorytm, data = mean_time_sorted)
summary(anova_model)

# post-hoc tests
TukeyHSD(anova_model)
snk(mean_time_sorted$Czas, mean_time_sorted$Algorytm, anova_model$df.residual, sum(anova_model$residual^2))

# Ex. 2
data <-read.csv2("/home/michal/Desktop/R/data/SrSpalanie.csv" ,sep=";", dec=".")
data_long <- data.frame(
  Model = rep(c('S1', 'S2', 'S3', 'S4', 'S5'), each = 6),
  Spalanie = c(data$S1, data$S2, data$S3, data$S4, data$S5)
)

# omit NA values
data_long <- na.omit(data_long)

# means
means <- data_long %>%
  group_by(Model) %>%
  summarize(SrSpalanie = mean(Spalanie))
  
# var
vars <- data_long %>%
  group_by(Model) %>%
  summarize(Var = var(Spalanie))

# check bell shape
ad.test(data_long$Spalanie[data_long$Model == 'S1'])$p.value
# we get "Error in ad.test(data_long$Spalanie[data_long$Model == "S1"]) : sample size must be greater than 7"

# this time we use Kruskal-Wallis test if, data does not follow the normal distribution
kruskal_test_results <- kruskal.test(Spalanie ~ Model, data = data_long)
# p - value 0.001637 < 0.05.

# anova
anova_model <- aov(Spalanie ~ Model, data = data_long)
summary(anova_model)


# post-hoc tests
TukeyHSD(anova_model)
snk(data_long$Spalanie, data_long$Model, anova_model$df.residual, sum(anova_model$residuals^2))


# Ex. 3
data <- read.csv2("/home/michal/Desktop/R/data/SrCzasAlgorytmy.csv",sep=";",dec=".")
data_sorted <- data %>% arrange(Algorytm, InspNatura)

# calculate means and vars
means <- data_sorted %>%
  group_by(Algorytm, InspNatura) %>%
  summarize(mean_time = mean(Czas))
  
vars <- data_sorted %>%
  group_by(Algorytm, InspNatura) %>%
  summarize(var_time = var(Czas))
  
# check for normal distribution
normality_results <- data_sorted %>%
  group_by(Algorytm, InspNatura) %>%
  summarize(normal_p = ad.test(Czas)$p.value)

# one test does not have p - value >= 0.05. What then?

# use 2 way anova
anova_model <- aov(Czas ~ Algorytm * InspNatura, data = data_sorted)
summary(anova_model)

# Tukey
TukeyHSD(anova_model)

# snk
df_error <- df.residual(anova_model)            # Residual degrees of freedom
ss_error <- sum(anova_model$residuals^2)        # Residual sum of squares

snk(
  data_sorted$Czas, 
  interaction(
    data_sorted$Algorytm, 
    data_sorted$InspNatura),
  df_error, ss_error)


# ============================================================================
#                               Analiza czynnikowa
# ============================================================================
kwest <- read.csv2("/home/michal/Desktop/R/data/AnCzKwestionariusz.csv", sep=";", dec=".")

summary(kwest)

# Correlation matrix
cor_matrix <- cor(kwest, use = "complete.obs")
round(cor_matrix, 2) # Inspect correlations

# Check KMO (measures sampling adequacy for factor analysis)
kmo_result <- KMO(cor_matrix)
print(kmo_result)

# Perform EFA (e.g., assuming 3 factors)
efa_result <- fa(kwest, nfactors = 3, rotate = "varimax", fm = "ml")
print(efa_result)

# View factor loadings (interpret how variables load onto factors)
fa.diagram(efa_result)

# Example CFA model with 3 factors
model <- '
  Factor1 =~ P1 + P2 + P3
  Factor2 =~ P4 + P5 + P6
  Factor3 =~ P7 + P8 + P9
'

cfa_result <- cfa(model, data = kwest)
summary(cfa_result, fit.measures = TRUE, standardized = TRUE)


# Visualize CFA results
semPaths(cfa_result, "std", layout = "circle", whatLabels = "std")

# Summarize results
print(cfa_result)

