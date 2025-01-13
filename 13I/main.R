# egzamin to dobor metody do danych eg. masz wykres i dobierz dane do wykresu
# mierniki jakosci dopasowania modelu do danych
# 

library(MASS)
library(rpart)
library(tree)

marki <- read.csv("data/samochody.csv", header = TRUE, sep = ";")
marki_df <- data.frame(as.factor(marki[, 1]), marki[, 2:19])
names(marki_df) <- names(marki)


# Drzewa klasyfikacyjne (dyskryminacyjne) - podzial na regiony R_i
# Drzewa regresyjne - przypisanie T lub N dla X_i < x_i do regionu R_i
# Y = f(X_1, X_2, ..., X_m) => f(x_i) = sum(k=1)^K / alpha_i I(x_i /in R_I)
# R_1, ..., R_K - segmenty (podprzestrzenie X^m)
# I - indykator
# Kazdy z obszarow R_k okreslany przez granice w X^m
# Ocena jakosci podzialu (homogenicznosc podprzestrzeni):
# 


# drzewp
marki_drzewo <- tree(Marka ~ ., data = marki_df)
summary(marki_drzewo)

base::plot(marki_drzewo, main="Klasyfikacja samochodow wzgledem ich cech")
text(marki_drzewo)


# prune
marki_drzewo_prune <- prune.tree(marki_drzewo, best = 3)
# dla 3 budowane na: "Wydluzenie" i "Wyp..."

# plot dla przycietego
plot(marki_df$Wydluzenie,
  marki_df$WspProporcjiMaxOsi,
  type = "n",
  xlab = "Wydluzenie",
  ylab = "WspProporcjiMaxOsi",
)

text(marki_df$Wydluzenie,
  marki_df$WspProporcjiMaxOsi,
  c("1", "2", "3", "4")[marki_df$Marka]
)
# 1 - bus, 2 - open, 3  -saab, 4 - van


partition.tree(marki_drzewo_prune,
    add = TRUE,
    cex = 0.8
)


# Szukanie wielkosci drzewa: uczacy + testowy
set.seed(114)
liczebnosc <- nrow(marki_df)
test <- sample(1:liczebnosc,
    round(liczebnosc / 3),
    replace = FALSE,
)
marki_ucz <- marki_df[-test, ]
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
irysy_data <- read.csv("data/irysy.csv", header = TRUE, sep = ";")
irysy_df <- data.frame(as.factor(irysy_data))

irysy_drzewo <- tree(
    gatunek ~ DlPlatkaZ + SzPlatkaZ + DlPlatkaW + SzPlatkaW, 
    data = irysy_df)

plot(irysy_drzewo)
text(irysy_drzewo)

# 1 wykres
partition.tree(irysy_drzewo)

# 2 wykjres, blad klasyfikacji


#. blafd klasyfikacji ma wyjsc 0.6