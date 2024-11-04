# setwd('Desktop/R/4XI')
data <- read.csv('Trendy.csv', sep=';')

plot(data$y2)

# top-left
y <- data$y1
t <- 1:length(y)
plot(y)

# y = at + b
fit_linear <- lm(y ~ t)

# y = a*t^2 + b*t + c
fit_quadratic <- lm(y ~ poly(t, 2, raw = TRUE))

# y = a*t^2 + c
fit_simple_quadratic <- lm(y ~ I(t^2))

lines(t, predict(fit_linear), col = "blue", lwd = 2, lty = 1)
lines(t, predict(fit_quadratic), col = "red", lwd = 2, lty = 2)
lines(t, predict(fit_simple_quadratic), col = "green", lwd = 2, lty = 3)

# jakiś miernik R^2 do dopasowania do danych
adjusted_r2 <- function(model) {
  summary(model)$r.squared
}

r2_linear <- adjusted_r2(fit_linear)
r2_quadratic <- adjusted_r2(fit_quadratic)
r2_simple_quadratic <- adjusted_r2(fit_simple_quadratic)

cat("y = a*t + b: ", r2_linear, "\n")
cat("y = a*t^2 + b*t + c: ", r2_quadratic, "\n")
cat("y = a*t^2 + c:", r2_simple_quadratic, "\n")

max(r2_linear, r2_quadratic, r2_simple_quadratic)

# top-right
y2 <- data$y2
plot(y2)

fit_exp <- lm(log(y2) ~ t)
pred_exp <- exp(predict(fit_exp))

fit_exp_rev <- lm(log(y2) ~ t - 1)
pred_exp_rev <- exp(predict(fit_exp_rev))

fit_hyperbolic <- lm(y2 ~ I(1 / t))
pred_hyperbolic <- predict(fit_hyperbolic)

lines(t, pred_exp, col = "purple", lwd = 2, lty = 1)
lines(t, pred_hyperbolic, col = "orange", lwd = 2, lty = 2)
lines(t, pred_exp_rev, col = "blue", lwd = 2, lty = 2)

adjusted_r2 <- function(model, original_y, predicted_y) {
  tss <- sum((original_y - mean(original_y))^2)
  rss <- sum((original_y - predicted_y)^2)
  1 - rss / tss
}

r2_exp <- adjusted_r2(fit_exp, y, pred_exp)
r2_exp_rev <- adjusted_r2(fit_exp_rev, y, pred_exp_rev)
r2_hyperbolic <- adjusted_r2(fit_hyperbolic, y, pred_hyperbolic)

cat("y = a * exp(-b * t): ", r2_exp, "\n")
cat("y = a / t + c: ", r2_hyperbolic, "\n")
cat("y = a * exp(-b * (t - 1)): ", r2_exp_rev, "\n")

max(r2_exp, r2_hyperbolic, r2_exp_rev)
