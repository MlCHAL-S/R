# setwd('Desktop/R/18XI')

# Load data from the CSV file
srczas <- read.csv2("SrCzasAlgorytmy.csv", sep = ";", dec = ".")
srczas

# Load necessary libraries
library(nortest)    # For normality test (ad.test)
library(agricolae)  # For SNK test
#library(car)        # For Levene's test
library(dplyr)      # For data manipulation (%>% and group_by)
library(ExpDes)     # For SNK (Student-Newman-Keuls) test

srczasort<-srczas[order(srczas[,"Algorytm"]),]  	
srczasort

#N(*,*) 
adsrczasN<-c(ad.test(srczasort[1:20,3])$p.value,
             ad.test(srczasort[21:40,3])$p.value,
             ad.test(srczasort[41:60,3])$p.value,
             ad.test(srczasort[61:80,3])$p.value,
             ad.test(srczasort[81:100,3])$p.value)
adsrczasN

# lub funkcja
adsrczas<-function(y){
  adsrcz<-rep(0,5)
  for (i in 1:length(adsrcz))
    adsrcz[i]<-ad.test(y[(1+20*(i-1)):(20*i),3])$p.value
  print(adsrcz)}

adsrczas(srczasort)

# mean
srcz<-function(y){
  sredniczas<-rep(0,5)
  for (i in 1:length(sredniczas))
    sredniczas[i]<-mean(y[(1+20*(i-1)):(20*i),3])
  print(sredniczas)}
srcz(srczasort)

# var	
varsrczas<-function(y){
  varsrcz<-rep(0,5)
  for (i in 1:length(varsrcz))
    varsrcz[i]<-var(y[(1+20*(i-1)):(20*i),3])
  print(varsrcz)}
varsrczas(srczasort)

### ANoVA
aovsrczas<-aov(srczasort$Czas~srczasort$Algorytm)
anova(aovsrczas)						

# Kruskal-Wallis - nieparametryczna alternatywa dla ANOVA jeśli np. warunek N() nie jest spełniony
kruskal.test(srczasort$Czas,srczasort$Algorytm)


## TESTY post-hoc:
#Tukey - "uczciwie istotnych różnic", zakłada równoliczność grup
TukeyHSD(aovsrczas)					

#LSD (agricolae) - nie zakłada równoliczności grup

# Test SNK (Student-Newman-Keuls) - podobny do TukeyHSD
dfErrorSrCzas <- df.residual(aovsrczas)
SSerrorSrCzas <- sum(aovsrczas$residuals^2)
MSerrorSrCzas <- SSerrorSrCzas/dfErrorSrCzas
snk(srczasort$Czas,srczasort$Algorytm,dfErrorSrCzas,SSerrorSrCzas)

################################### Spalanie ################################### 

# Load the SrSpalanie data
srspalanie <- read.csv2("SrSpalanie.csv", sep=";", dec=".", header=TRUE)
srspalanie

# Ensure numeric values and handle missing data
srspalanie <- as.data.frame(lapply(srspalanie, function(x) as.numeric(as.character(x))))
srspalanie[is.na(srspalanie)] <- 0  # Replace missing values with 0

# Create a "long format" for analysis (like srczasort)
srspalanie_long <- data.frame(
  Group = rep(colnames(srspalanie), each = nrow(srspalanie)),
  Value = as.vector(as.matrix(srspalanie))
)

# Remove rows with zero values introduced from missing data
srspalanie_long <- srspalanie_long[srspalanie_long$Value > 0, ]

# Sort data by Group
srspalanie_sort <- srspalanie_long[order(srspalanie_long$Group),]

# ANOVA
aov_srspalanie <- aov(Value ~ Group, data = srspalanie_sort)
anova(aov_srspalanie)

# Kruskal-Wallis Test (if needed)
kruskal.test(srspalanie_sort$Value, srspalanie_sort$Group)

# SNK test for SrSpalanie
dfErrorSrSpalanie <- df.residual(aov_srspalanie)
SSerrorSrSpalanie <- sum(aov_srspalanie$residuals^2)
MSerrorSrSpalanie <- SSerrorSrSpalanie / dfErrorSrSpalanie

snk(srspalanie_sort$Value, srspalanie_sort$Group, dfErrorSrSpalanie, SSerrorSrSpalanie)


######################### Natura algo 2 features

# Load data from the CSV file
srczas <- read.csv2("SrCzasAlgorytmy.csv", sep = ";", dec = ".")
srczas

# Sort data by Algorytm
srczasort <- srczas[order(srczas[,"Algorytm"]),]

# Shapiro-Wilk Test (N(*,*)) for Normality
adsrczasN <- c(ad.test(srczasort[1:20,3])$p.value,
               ad.test(srczasort[21:40,3])$p.value,
               ad.test(srczasort[41:60,3])$p.value,
               ad.test(srczasort[61:80,3])$p.value,
               ad.test(srczasort[81:100,3])$p.value)

# Alternatively, define as a function
adsrczas <- function(y) {
  adsrcz <- rep(0, 5)
  for (i in 1:length(adsrcz))
    adsrcz[i] <- ad.test(y[(1 + 20 * (i - 1)):(20 * i), 3])$p.value
  print(adsrcz)
}

adsrczas(srczasort)

# Mean function
srcz <- function(y) {
  sredniczas <- rep(0, 5)
  for (i in 1:length(sredniczas))
    sredniczas[i] <- mean(y[(1 + 20 * (i - 1)):(20 * i), 3])
  print(sredniczas)
}

srcz(srczasort)

# Variance function
varsrczas <- function(y) {
  varsrcz <- rep(0, 5)
  for (i in 1:length(varsrcz))
    varsrcz[i] <- var(y[(1 + 20 * (i - 1)):(20 * i), 3])
  print(varsrcz)
}

varsrczas(srczasort)

# Two-Way ANOVA
# Include both Algorytm and InspNatura as factors
aov_two_way <- aov(Czas ~ Algorytm * InspNatura, data = srczasort)
anova(aov_two_way)

# Kruskal-Wallis test as a non-parametric alternative
kruskal.test(srczasort$Czas ~ interaction(srczasort$Algorytm, srczasort$InspNatura))

## Post-hoc analysis for two-way ANOVA
# TukeyHSD for pairwise comparisons
TukeyHSD(aov_two_way)

# Calculate SNK test manually
dfErrorSrCzas <- df.residual(aov_two_way)
SSerrorSrCzas <- sum(aov_two_way$residuals^2)
MSerrorSrCzas <- SSerrorSrCzas / dfErrorSrCzas
snk(srczasort$Czas, interaction(srczasort$Algorytm, srczasort$InspNatura), dfErrorSrCzas, SSerrorSrCzas)

plot(TukeyHSD(aov_two_way, conf.level = .95), las = 2)

boxplot(Czas ~ Algorytm:InspNatura)

# finish code here
