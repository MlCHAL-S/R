# ============================================================================
# Combined R Script for Data Analysis and Statistical Learning
# ============================================================================

# ============================================================================
#                               Session 1: 7X
# ============================================================================
setwd('/home/michal/Desktop/R/7X')
# Some basics to cover later

# ============================================================================
#                               Session 2: 14X
# ============================================================================
setwd('/home/michal/Desktop/R/14X')
wiek=read.csv("/home/michal/Desktop/R/data/wiek.csv",header=T,sep=";") #wczytanie danych
wiek_sort <- sort(wiek$wiek)

#	2. stworzy� funkcj�, kt�ra wyznaczy: min, max, �rednia, sd, var, mediana, dominanta, 
kra�ce przedzia�u wg regu�y 3sigma oraz sprawdzi�, czy istniej� warto�ci odstaj�ce;
summary <- function(wiek_sort){
  minV <- min(wiek_sort)
  cat("min: ",minV,"\n")
  maxV <- max(wiek_sort)
  cat("max: ",maxV ,"\n")
  meanV <- mean(wiek_sort)
  cat("srednia: ",meanV ,"\n")
  sdV <- sd(wiek_sort) # sigma
  cat("sd: ",sdV ,"\n")
  varV <- var(wiek_sort) # wariancja
  cat("var: ",varV ,"\n")
  medianV <- median(wiek_sort)
  cat("mediana: ",medianV ,"\n")
  
  #TODO: dominanta poprawic
  modeV <- as.numeric(names(sort(table(wiek_sort), decreasing = T) [1]))
  cat("mode: ",modeV ,"\n")
  
  lower_bound <- meanV - 3 * sdV
  upper_bound <- meanV + 3 * sdV
  
  cat("przedzial 3 sigma: (",lower_bound,"; ",upper_bound,")\n")
  
  outliers <- (minV < lower_bound | maxV > upper_bound)
  cat("wartosci odstajace: ",outliers ,"\n")
  
  
  #	3. wyznaczy� rozk�ad i dystrybuant� zmiennej "wiek" oraz przedstawi� graficznie wynik;
  
  # rozklad wieku
  tabela <- table(wiek_sort)
  # histogram
  h = hist(wiek$wiek,breaks=c(10,20,30,40,50,60),prob=T)
  h$counts
  
  h$counts <- cumsum(h$counts)
  plot(h)
  
  N <- length(wiek$wiek)
  plot(c(20,30,40,50,60),h$counts/N,type="b")
  plot(ecdf(wiek$wiek), main = "Dystrybuanta empiryczna")
  
  #	4. Jaki % wszystkich pracownik�w stanowi� osoby: a) w wieku 31-40 lat oraz b) nie przekroczy�y 40 lat?
  
  # Procent pracowników w wieku 31-40 lat
  procent31a40 <- 100 * sum(wiek$wiek >= 31 & wiek$wiek <= 40) / N
  cat("Procent pracowników w wieku 31-40 lat:", procent31a40, "%\n")
  
  # Procent pracowników, którzy nie przekroczyli 40 lat
  procent_do40 <- 100 * sum(wiek$wiek <= 40) / N
  cat("Procent pracowników, którzy nie przekroczyli 40 lat:", procent_do40, "%\n")
}

summary <- summary(wiek_sort)


# ============================================================================
#                               Session 1: 21X
# ============================================================================
setwd('/home/michal/Desktop/R/21X')

plik=read.csv("/home/michal/Desktop/R/data/WM.csv",header=T,sep=";")
plik

m <- cbind(plik$M)
x <- cbind(1, plik$W)
xt <- t(x)

xSqared <- xt %*% x
xInv <- solve(xSqared)

p <- xInv %*% xt %*% m
p[1]

mTeor <- p[1] + p[2] * plik$W

plot(plik$W, plik$M, col="blue", pch=1, xlim=c(2000, 4300), ylim=c(1900, 4300))
lines(plik$W, mTeor, col="green")
legend("bottomright", c("M empiryczne", "Mt teoretyczne"), col=c("blue","green"), pch=c(1,NA), lty=c(NA,1))


alfa_poczatkowe <- c(-100,1,1)

MNW <- function(alfa, M, W){
  sum(0.5*log(alfa[3])+0.5*log(M-alfa[1]-alfa[2]*W)^2/alfa[3])}
# ============================================================================
#                               Session 1: 28X
# ============================================================================
setwd('/home/michal/Desktop/R/28X')


# ============================================================================
#                               Session 1: 4XI
# ============================================================================
setwd('/home/michal/Desktop/R/4XI')
trendy = read.csv("/home/michal/Desktop/R/data/Trendy.csv",header=T,sep=";")

par(mfrow=c(2,2))

plot (trendy$y1)
plot (trendy$y2)
plot (trendy$y3)
plot (trendy$y4)

y <- trendy$y1
t <- 1:length(y)

plot(y)

M <- cbind(trendy$y1)
x <- cbind(1, trendy$t)
xt <- t(x)

xSqared <- xt %*% x
xInv <- solve(xSqared)

p <- xInv %*% xt %*% M

mTeorLin <- p[1] + p[2] * trendy$t

X <- cbind(1, trendy$t, trendy$t^2)
Xt <- t(X)
XSquared <- Xt %*% X
XInv <- solve(XSquared)
P <- XInv %*% Xt %*% M
mTeorQuad <- P[1] + P[2] * trendy$t + P[3] * trendy$t^2

simpleQuad <- lm(y ~ I(t^2))

lines(trendy$t, mTeorLin , col="red")
lines(trendy$t, mTeorQuad , col="blue")

lines(trendy$t, predict(simpleQuad), col="green")


y2 <- trendy$y2
t2 <- 1:length(y2)

plot(y2)

expModel <- lm(log(y2) ~ t)
expFitted <- exp(predict(expModel))
lines(t, expFitted, col = "green")

expModel <- lm(log(y2) ~ t-1)
expFitted <- exp(predict(expModel))
lines(t, expFitted, col = "red")

hyperModel <- lm(y2 ~ I(1/t))
hyperFitted <- predict(hyperModel)
lines(t, hyperFitted, col = "blue")
# ============================================================================
#                               Session 2: 12XI
# ============================================================================
setwd('/home/michal/Desktop/R/12XI')
srczas<-read.csv2("/home/michal/Desktop/R/data/SrCzasAlgorytmy.csv",sep=";",dec=".")

library(nortest) #ad.test	
library(agricolae) #SNK.test
library(car)	# Levene
library(dplyr)	# %>% group by ...
library(ExpDes)	# snk

srczasort<-srczas[order(srczas[,"Algorytm"]),]  	

#N(*,*) 
adsrczasN<-c(ad.test(srczasort[1:20,3])$p.value,
             ad.test(srczasort[21:40,3])$p.value,
             ad.test(srczasort[41:60,3])$p.value,
             ad.test(srczasort[61:80,3])$p.value,
             ad.test(srczasort[81:100,3])$p.value)

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

#============spalanie===========

# Create the new dataset
data <-read.csv2("SrSpalanie.csv",sep=";",dec=".")

data_long <- data.frame(
  Algorithm = rep(c("S1", "S2", "S3", "S4", "S5"), each = 6),
  Time = c(data$S1, data$S2, data$S3, data$S4, data$S5)
)

# Remove NA values
data_long <- data_long[!is.na(data_long$Time), ]

# Calculate means
means <- data_long %>%
  group_by(Algorithm) %>%
  summarize(mean_time = mean(Time))

print(means)

# Calculate variances
variances <- data_long %>%
  group_by(Algorithm) %>%
  summarize(variance_time = var(Time))

print(variances)

# ANOVA
aov_results <- aov(Time ~ Algorithm, data = data_long)
summary(aov_results)

# Kruskal-Wallis test
kruskal_test_results <- kruskal.test(Time ~ Algorithm, data = data_long)
print(kruskal_test_results)

# Tukey HSD
tukey_results <- TukeyHSD(aov_results)
print(tukey_results)

# SNK test
df_error <- df.residual(aov_results)
ss_error <- sum(aov_results$residuals^2)
ms_error <- ss_error / df_error
snk_results <- snk(data_long$Time, data_long$Algorithm, df_error, ss_error)



#===============czas-uwzglednienie dwoch cech=====================

srczas<-read.csv2("SrCzasAlgorytmy.csv",sep=";",dec=".")

srczasort <- srczas[order(srczas$Algorytm, srczas$InspNatura),]	

ad_test_results <- srczasort %>%
  group_by(Algorytm, InspNatura) %>%
  summarize(p_value = ad.test(Czas)$p.value)

print(ad_test_results)

# Calculate means
srcz <- function(y){
  sredniczas <- y %>%
    group_by(Algorytm, InspNatura) %>%
    summarize(mean_time = mean(Czas))
  print(sredniczas)
}

srcz(srczasort)

# Calculate variances
varsrczas <- function(y){
  varsrcz <- y %>%
    group_by(Algorytm, InspNatura) %>%
    summarize(variance_time = var(Czas))
  print(varsrcz)
}

varsrczas(srczasort)

# Two-Way ANOVA
aovsrczas <- aov(Czas ~ Algorytm * InspNatura, data = srczasort)
summary(aovsrczas)	

reszty<-aovsrczas$residuals

hist(reszty,main ="Histogram-reszty",xlab="Reszty")				

# Tukey HSD
tukey_results <- TukeyHSD(aovsrczas)
print(tukey_results)

plot(tukey_results, las=2)

# SNK test
dfErrorSrCzas <- df.residual(aovsrczas)
SSerrorSrCzas <- sum(aovsrczas$residuals^2)
MSerrorSrCzas <- SSerrorSrCzas / dfErrorSrCzas
snk_results <- snk(srczasort$Czas, interaction(srczasort$Algorytm, srczasort$InspNatura), dfErrorSrCzas, SSerrorSrCzas)

# ============================================================================
#                               Session 2: 18XI
# ============================================================================
setwd('/home/michal/Desktop/R/18XI')

library(nFactors)
library(corpcor)
library(psych)
library(lavaan)

kwest<-read.csv2("/home/michal/Desktop/R/data/AnCzKwestionariusz.csv",sep=";",dec=".")
zm<-read.csv2("/home/michal/Desktop/R/data/AnCz10zm.csv",sep=";",dec=".")

# ============================================================================
#                               Session 2: 25XI
# ============================================================================
setwd('/home/michal/Desktop/R/25XI')

dane<-read.csv2("/home/michal/Desktop/R/data/ProbitLogit.csv",sep=";",dec=".")

CzestEmp<-dane[,6]/dane[,2]
SrWiek<-(dane$WiekDo-dane$WiekOd)/2+dane$WiekOd
SrStaz<-dane$SrStazPracy
X<-as.matrix(cbind(rep(1,10),SrWiek,SrStaz))
XT<-t(X)

#PROBIT
ProbEmp<-qnorm(CzestEmp)+5

odwVi<-1/(CzestEmp* (1-CzestEmp)/(dane[,2]*(dnorm(CzestEmp))^2))
odwV<-diag(odwVi)

temp<-XT %*% odwV %*% X
tempOdw<-solve(temp)
Beta<-tempOdw %*% XT %*% odwV %*% ProbEmp

ProbitTeor<-X%*%Beta
piTeorProb<-pnorm(ProbitTeor-5)
probitR2<-1-sum((CzestEmp-piTeorProb)^2)/sum((CzestEmp-mean(CzestEmp))^2)




#wiek 30 staz 10
#wiek 35 staz 15

wiek1<-30
wiek2<-35
staz1<-10
staz2<-15

X1 <- c(1, wiek1, staz1)
X2 <- c(1, wiek2, staz2)

ProbitTeor1 <- sum(X1 * Beta)
piTeorProb1 <- pnorm(ProbitTeor1 - 5)

ProbitTeor2 <- sum(X2 * Beta)
piTeorProb2 <- pnorm(ProbitTeor2 - 5)






# Wczytanie danych
dane <- read.csv2("/home/michal/Desktop/R/data/ProbitLogit.csv", sep=";", dec=".")

# Obliczenia
CzestEmp <- dane[,6] / dane[,2]
SrWiek <- (dane$WiekDo - dane$WiekOd) / 2 + dane$WiekOd
SrStaz <- dane$SrStazPracy
X <- as.matrix(cbind(rep(1, 10), SrWiek, SrStaz))
XT <- t(X)

# LOGIT
LogitEmp <- log(CzestEmp / (1 - CzestEmp))

odwVi <- dane[,2]*CzestEmp*(1-CzestEmp)
odwV <- diag(odwVi)

temp <- XT %*% odwV %*% X
tempOdw <- solve(temp)
Beta <- tempOdw %*% XT %*% odwV %*% LogitEmp

LogitTeor <- X %*% Beta
piTeorLogit <- 1 / (1 + exp(-(LogitTeor)))
logitR2 <- 1 - sum((CzestEmp - piTeorLogit)^2) / sum((CzestEmp - mean(CzestEmp))^2)






















# wiek 30, staż 10
wiek1 <- 30
staz1 <- 10
X1 <- c(1, wiek1, staz1)
LogitTeor1 <- sum(X1 * Beta)
piTeorLogit1 <- 1 / (1 + exp(-(LogitTeor1 - 5)))

# wiek 35, staż 15
wiek2 <- 35
staz2 <- 15
X2 <- c(1, wiek2, staz2)
LogitTeor2 <- sum(X2 * Beta)
piTeorLogit2 <- 1 / (1 + exp(-(LogitTeor2 - 5)))

# Wyniki
piTeorLogit1
piTeorLogit2



# End of Script

