# setwd("Desktop/R/28X")


mw <- read.csv("WM.csv", header = TRUE, sep = ';')
X <- cbind(1, mw$W)

X_transposed <- t(X)
X_squared <- X_transposed %*% X
X_inversed <- solve(X_squared)

M <- as.matrix(mw$M)

teta <- X_inversed %*% X_transposed %*% M
teta

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


alfa_pocz <- c(-100, 1, 1)
MNW <- function(alfa, M, W) {
  sum(0.5 * log(alfa[3]) + 0.5 * (M - alfa[1] - alfa[2] * W)^2 / alfa[3])
}

oszacowane_parametry <- optim(alfa_pocz, MNW, M=mw$M, W = mw$W, hessian = TRUE, method = c("Nelder-Mead"))
oszacowane_parametry

summary(lm(mw$M ~ mw$W))



### tak dla jajec chyba
find_zero <- function(f, tol = 1e-6, max_iter = 1000) {

  x1 <- -5
  x2 <- 5
  
  for (i in 1:max_iter) {

    x_mid <- (x1 + x2) / 2
    f_mid <- f(x_mid)
    

    if (abs(f_mid) < tol) {
      return(x_mid)
    }
    

    if (f(x1) * f_mid < 0) {
      x2 <- x_mid
    } else {
      x1 <- x_mid
    }
  }
}

root <- find_zero(function(x) x^2 - 3)
cat("Approximate root:", root, "\n")


#### Trendy ####
trendy <- read.csv("Trendy.csv", header = TRUE, sep = ';')
trendy
# 
t <- 1:20

set.seed(5)
y0 <- c(20:1) + rnorm(20)

plot(t, y0)

model <- lm(y0 ~ t)

predicted_y0 <- predict(model)

lines(t, predicted_y0)

summary(model)

