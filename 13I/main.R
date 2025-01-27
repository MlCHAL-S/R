# egzamin to dobor metody do danych eg. masz wykres i dobierz dane do wykresu
# mierniki jakosci dopasowania modelu do danych
# 

library(MASS)
library(rpart)
library(tree)

# install.packages('tree')

marki <- read.csv("/home/michal/Desktop/R/data/samochody.csv", header = TRUE, sep = ";")
marki_df <- data.frame(as.factor(marki[, 1]), marki[, 2:19])
names(marki_df) <- names(marki)


# Drzewa klasyfikacyjne (dyskryminacyjne) - podzial na regiony R_i
# Drzewa regresyjne - przypisanie T lub N dla X_i < x_i do regionu R_i
# Y = f(X_1, X_2, ..., X_m) => f(x_i) = sum(k=1)^K \alpha_i I(x_i \in R_I)
# R_1, ..., R_K - segmenty (podprzestrzenie X^m)
# I - indykator
# Kazdy z obszarow R_k okreslany przez granice w X^m
# Ocena jakosci podzialu (homogenicznosc podprzestrzeni):
# Q(R_k) = 1 / N(k) \sum_(x_i \in R_i) (y_i -\alpha_k)^2
# lub (cechy nominalne) - entropia, wsk. Giniego,...
# optymalny wybor: S_\lambda (D) =\sum_{k=1}^K Q(R_k)p_k+\lambda K 
# D-model (drzewo), \lambda-par.zlozonosci


# drzewp
marki_drzewo <- tree(Marka ~ ., data = marki_df)
summary(marki_drzewo)

base::plot(marki_drzewo, main="Klasyfikacja samochodow wzgledem ich cech")
text(marki_drzewo)


# prune
marki_drzewo_prune <- prune.tree(marki_drzewo, best = 3)
# dla 3 budowane na: "Wydluzenie" i "Wyp..."

# plot dla przycietego
plot(marki_df$Wydluzenie, marki_df$WspProporcjiMaxOsi, type = "n",
     xlab = "Wydluzenie", ylab = "WspProporcjiMaxOsi")

text(marki_df$Wydluzenie,
  marki_df$WspProporcjiMaxOsi,
  c("1", "2", "3", "4")[marki_df$Marka]
)

partition.tree(marki_drzewo_prune,
    add = TRUE,
    cex = 0.8
)
# 1 - bus, 2 - open, 3  -saab, 4 - van

# Szukanie wielkosci drzewa: uczacy + testowy
set.seed(114)
liczebnosc <- nrow(marki_df)
test <- sample(1:liczebnosc,
    round(liczebnosc / 3),
    replace = FALSE,
)

# Train data
marki_ucz <- marki_df[-test, ]

# Test data
marki_test <- marki_df[test, ]

# model
marki_drzewo_r_part_ucz <- rpart(
    Marka ~ ., data = marki_ucz,
    control = rpart.control(xval = 100, cp = 0),

)
plot(marki_drzewo_r_part_ucz)
marki_cp <- marki_drzewo_r_part_ucz$cptable


# model optymalny
marki_opt <- which.min(marki_cp[, 4])
marki_cp_opt <- marki_cp[marki_opt, 1]
marki_drzewo_prune_ucz <- prune(
    marki_drzewo_r_part_ucz, cp = marki_cp_opt
)
plot(marki_drzewo_prune_ucz) # dla ostatniej wartosci


# zaleznosci miedzy wielkoscia drzewa a wielkoscia ...
# bl. klasyfikacji
#   a. rel_error <- bl. zastapienia
#   b. cross_validation
plot(marki_cp[, 2] + 1, marki_cp[, 4],
    ylim = c(min(marki_cp[, 3]), 1),
    type = "n",
    xlab = "Wielkosc drzewa",
    ylab = "Blad klasyfikacji")

lines(marki_cp[, 2] + 1, marki_cp[, 4], lty = 1)
lines(marki_cp[, 2] + 1, marki_cp[, 3], lty = 2)
points(marki_cp[, 2] + 1, marki_cp[, 4], lty = 21)
lines(marki_cp[, 2] + 1, marki_cp[, 3], lty = 22)
legend("topright",
    legend = c("Bl. spr krzyzowe", "Blad zastapienia"),
    lty = 1:2,
    pch = 21:22
)



#################################################################################
                              # My Exercise
#################################################################################

data(iris)
head(iris)

iris_tree <- tree(Species ~ ., data = iris)

plot(iris_tree, main = "Classification of Iris Flowers")
text(iris_tree)

# Train and test data
set.seed(42)
test_idx <- sample(1:nrow(iris), nrow(iris) / 3)

iris_train <- iris[-test_idx, ]
iris_test <- iris[test_idx, ]

# Fit model
iris_rpart <- rpart(Species ~ ., data = iris_train, control = rpart.control(xval = 10, cp = 0))

# Isolate complexity parameter table
iris_cp <- iris_rpart$cptable

# Finds the complexity parameter (cp) that minimizes cross-validation error.
# which.min() identifies the row index of the smallest value in column 4
iris_opt_cp <- iris_cp[which.min(iris_cp[, 4]), 1]

# Prunes the tree using the optimal complexity parameter (cp), 
# resulting in a simpler tree that balances accuracy and generalization.
iris_pruned <- prune(iris_rpart, cp = iris_opt_cp)

# Plot the tree
plot(iris_pruned, main = "Pruned Iris Decision Tree")
text(iris_pruned)





plot(iris_cp[, 2] + 1, iris_cp[, 4], type = "n", xlab = "Tree Size", ylab = "Classification Error")
lines(iris_cp[, 2] + 1, iris_cp[, 4], lty = 1)
lines(iris_cp[, 2] + 1, iris_cp[, 3], lty = 2)
legend("topright", legend = c("Cross-Validation Error", "Resubstitution Error"), lty = 1:2)

#################################################################################
#################################################################################
#################################################################################


### procesory
cpu_dane <- read.csv("data/cpudane.csv", header = TRUE, sep = ";")
cpu_df <- data.frame(as.factor(cpu_dane[, 1]), cpu_dane[, 2:4])
names(cpu_df) <- c("nazwa", "mmax", "cache", "perf")

cpu_drzewo <- tree(log10(perf) ~ mmax + cache, data = cpu_df)
plot(cpu_drzewo)
text(cpu_drzewo)

partition.tree(cpu_drzewo)


# zm egzo - numeryczna => 
# perf - silna asymetria => log10

perf_cache_drzewo <- tree(log10(perf) ~ cache, data = cpu_df)
cache_podzial <- seq(min(cpu_df$cache),
    max(cpu_df$cache), 0.5  # cache "zageszczony"
)
perf_pred <- predict(perf_cache_drzewo,
    list(cache = cache_podzial)
)

# graficzna reprezentacja schodkowej regresji
plot(cpu_df$cache, log10(cpu_df$perf))
lines(cache_podzial, perf_pred)




# irysy
# Wczytanie danych
irysy_data <- read.csv("/home/michal/Desktop/R/data/irysy.csv", header = TRUE, sep = ";")
irysy_df <- data.frame(
  gatunek = as.factor(irysy_data$gatunek),
  irysy_data[, -1] # Include all other columns except the first (assumed target is the first column)
)

# Fit a decision tree
irysy_drzewo <- tree(
  gatunek ~ ., # Use all features to predict 'gatunek'
  data = irysy_df
)

# Build the decision tree
plot(irysy_drzewo, main = "Decision Tree for Iris Dataset")
text(irysy_drzewo)

# 1 wykres - Visualizing decision boundaries
# Ensure features 'DlPlatkaZ' and 'SzPlatkaZ' exist in the dataset
plot(
  irysy_df$DlPlatkaZ, irysy_df$SzPlatkaZ,
  type = "n",
  xlab = "DlPlatkaZ",
  ylab = "SzPlatkaZ",
  main = "Decision Boundaries for Iris Data"
)
text(
  irysy_df$DlPlatkaZ, irysy_df$SzPlatkaZ,
  labels = as.numeric(irysy_df$gatunek) # Label points with their class
)
partition.tree(irysy_drzewo, add = TRUE)



# Splitting data into training and test sets for validation
set.seed(123) # For reproducibility
n <- nrow(irysy_df)
test_indices <- sample(1:n, round(n / 3), replace = FALSE)
irysy_train <- irysy_df[-test_indices, ]
irysy_test <- irysy_df[test_indices, ]



# Train a more refined tree using rpart
irysy_rpart <- rpart(
  gatunek ~ ., 
  data = irysy_train,
  control = rpart.control(xval = 10, cp = 0) # Allow full growth initially
)


# Find optimal complexity parameter (cp) using cross-validation
irysy_cp <- irysy_rpart$cptable
optimal_cp <- irysy_cp[which.min(irysy_cp[, 4]), 1]

# Prune the tree based on the optimal cp
irysy_pruned <- prune(irysy_rpart, cp = optimal_cp)

# Visualize the pruned tree
plot(irysy_pruned, main = "Pruned Decision Tree for Iris Dataset")
text(irysy_pruned)


# Evaluate the model on the test set
predictions <- predict(irysy_pruned, irysy_test, type = "class")
confusion_matrix <- table(Predicted = predictions, Actual = irysy_test$gatunek)

# Calculate classification error rate
classification_error <- 1 - sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Classification error rate:", classification_error))



# Blad klasyfikacji graph
plot(
  irysy_cp[, 2] + 1, irysy_cp[, 4],
  type = "n",
  xlab = "Tree Size",
  ylab = "Classification Error",
  main = "Classification Error vs. Tree Size"
)
lines(irysy_cp[, 2] + 1, irysy_cp[, 4], lty = 1) # Cross-validation error
lines(irysy_cp[, 2] + 1, irysy_cp[, 3], lty = 2) # Resubstitution error
legend(
  "topright",
  legend = c("Cross-Validation Error", "Resubstitution Error"),
  lty = 1:2
)