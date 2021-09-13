health <- read.csv("heart_failure_clinical_records_dataset.csv")
sum(is.na(health[,c('age')]))
sum(is.na(health[,c('anaemia')]))
sum(is.na(health[,c('creatinine_phosphokinase')]))
sum(is.na(health[,c('diabetes')]))
sum(is.na(health[,c('ejection_fraction')]))
sum(is.na(health[,c('high_blood_pressure')]))
sum(is.na(health[,c('platelets')]))
sum(is.na(health[,c('serum_creatinine')]))
sum(is.na(health[,c('serum_sodium')]))
sum(is.na(health[,c('sex')]))
sum(is.na(health[,c('smoking')]))
sum(is.na(health[,c('time')]))
sum(is.na(health[,c('DEATH_EVENT')]))
sum(health$DEATH_EVENT)
#There is no data missing from any of the columns. This data is very clean
#does not need to be modified.

library(MVA)
library(mvnmle)
library(pkgbuild)

death <- which(colnames(health) == "DEATH_EVENT")
v <- cor(health)
v

x <- health[,c("serum_sodium", "ejection_fraction")]
bvbox(x, pch=" ", xlab = "Serum Sodium", ylab = "Ejection Fraction")
text(x$serum_sodium, x$ejection_fraction, labels = rownames(x), cex=.7)

#List of numbers (18, 127, 5, 200, 20, 226, 65, 237)

alpha <- match(lab3 <- c(18, 127, 5, 200, 20, 226, 65, 237), rownames(health))
cor(health$serum_sodium, health$ejection_fraction)
cor(health$serum_sodium[-alpha], health$ejection_fraction[-alpha])

y <- health[,c("serum_sodium", "serum_creatinine")]
bvbox(y, pch=" ", xlab = "Serum Sodium", ylab = "Serum_creatinine")
text(y$serum_sodium, y$serum_creatinine, labels = rownames(y), cex = .7)

#List of numbers(200, 5, 20, 127, 226, 115, 18, 66, 283, 62, 83, 32, 11, 229, 132, 10, 218, 29, 49, 125, 36, 184, 168, 248, 151, 282, 204, 218, 118, 57, 39, 40, 105, 26, 188, 196, 130, 83, 237)
beta <- match(lab4 <- c(200, 5, 20, 127, 226, 115, 18, 66, 283, 62, 83, 32, 11, 229, 132, 10, 218, 29, 49, 125, 36, 184, 168, 248, 151, 282, 204, 218, 118, 57, 39, 40, 105, 26, 188, 196, 130, 83, 237), rownames(health))
cor(health$serum_sodium, health$serum_creatinine)
cor(health$serum_sodium[-beta], health$serum_creatinine[-beta])


#Dimensionality
health_pca <- princomp(health, cor = TRUE)
summary(health_pca, loading = TRUE)
health_pca$loadings[,1:3]
biplot(health_pca, col = c("black", "red"), cex = .7)
scale(health)


#Clustering
km <- kmeans(health.s, centers = 2)
table(km$cluster)
table(km, health$DEATH_EVENT)

plot.wgss <- function(mydata, maxc){
  wss <- numeric(maxc)
  for (i in 1:maxc){
    wss[i] <- kmeans(health, centers = i, nstart = 10)$tot.withinss
  }
  plot(1:maxc, wss, type = "b", xlab = "Number of Clusters",
       ylab = "Within Groups Sum of Squares", main = "Scree Plot")
}
plot.wgss(health.s, 20)
round(km$centers, 3)



#CFA
library(sem)
efa <- factanal(health, 3)
efa
print(efa$loadings, cut = .3)