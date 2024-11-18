# setwd('/home/michal/Desktop/R/25XI')

library(nFactors)
library(corpcor)
library(psych)
library(lavaan)

dane <- read.csv2('AnCz10zm.csv', header = TRUE, row.names = 1)
dane

mkorelacji <- cor(dane)
mkorelacji