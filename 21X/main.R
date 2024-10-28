# setwd("Desktop/R/21X")


mw <- read.csv("WM.csv", header = TRUE, sep = ';')
X <- cbind(1, mw$W)

X_transposed <- t(X)
X_squared <- X_transposed %*% X
X_inversed <- solve(X_squared)

M <- as.matrix(mw$M)

teta <- X_inversed %*% X_transposed %*% M

library(ggplot2)
intercept <- teta[1, 1]
slope <- teta[2, 1]

ggplot(mw, aes(x = W, y = M)) +
  geom_point(color = "blue", size = 3) +
  geom_abline(intercept = intercept, slope = slope, color = "red", linetype = "dashed", size = 1) + 
  labs(title = "Linear Regression: M vs. W",
       x = "W",
       y = "M"
  )


alfa <- c(-100, 1, 1)
MNW <- function(alfa, M, W)
  sum(0.5 * log(alfa[3]) + 0.5 * (M - alfa[1]))