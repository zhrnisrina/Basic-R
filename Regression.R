
setwd("C:/Users/HP/OneDrive - Institut Teknologi Sepuluh Nopember/SEMESTER 4/AED/Setelah UTS")

#RESISTANT LINE

data(mtcars)
head(mtcars)

x <- mtcars$mpg
y <- mtcars$disp

resline = line(x,y, iter=4)

class(resline) # Melihat class
residuals(resline) # Melihat residual
coef(resline) # Melihat coefficient
fitted(resline) # Melihat nilai prediksi

intercept <- resline$coefficients[1]
slope <- resline$coefficients[2]

plot(x,y)
abline(a = intercept, b=slope)

R1 <- line(x,y,iter = 1)
R2 <- line(x,y,iter = 2)
R3 <- line(x,y,iter = 3)
R4 <- line(x,y,iter = 4)

abline(a = R1$coefficient[1], b = R1$coefficient[2], col = "magenta")
abline(a = R2$coefficient[1], b = R2$coefficient[2], col = "darkgreen")
abline(a = R3$coefficient[1], b = R3$coefficient[2], col = "lightblue")
abline(a = R4$coefficient[1], b = R4$coefficient[2], col = "green")


#REGRESSIONLINE

library(MASS)
linearModelA <- lm(Gas~Temp, data=whiteside)
names(linearModelA) # Function yang bisa digunakan

plot(whiteside$Temp, whiteside$Gas, xlab="Temp", 
     ylab="Gas", main="Temp vs Gas")
abline(linearModelA)

pred <- predict(linearModelA)
res <- resid(linearModelA)
sres <- rstandard(linearModelA)

plot(pred, sres, ylim=c(-3,3), 
     xlab="Fitted Value", 
     ylab="Standardized Residuals", 
     main="Fitted Value vs Standardized Residuals")
abline(h=2, lty = 2)
abline(h=-2, lty = 2)

n <- nrow(whiteside)
plot(1:n, sres, type="b", 
     xlab="Observation Order", 
     ylab="Standardized Residuals", 
     main="Observation Order vs Standardized Residuals")

p <- (1:n - 0.5)/n * 100; p
sorted_sres <- sort(sres); sorted_sres
plot(sorted_sres, p,
     xlab="Standardized Residuals",
     ylab="Probability", main="Normal Probability Plot")

plot(pred, whiteside$Gas, 
     xlim=c(1,8), ylim=c(1,8),
     xlab="Fitted Value",
     ylab="Actual Value",
     main="Fitted Value vs Actual Value")
abline(coef=c(0,1))


df <- UScereal[, c("calories", "fat")]
summary(df)

boxplot(df$calories, main="Boxplot Calories", xlab="Calories")
boxplot(df$fat, main="Boxplot Fat", xlab="Fat")

detect_outlier <- function(x){
  n <- length(x)
  Q1 <- quantile(x, 0.25)
  Q2 <- quantile(x, 0.5)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3-Q1
  LB <- Q1-1.5*IQR
  UB <- Q3+1.5*IQR
  index <- c()
  for(i in 1:n){
    if(x[i] >= LB & x[i] <= UB){
      next
    }else(
      index <- append(index, i)
    )
  }
  return(index)
}
index1 <- detect_outlier(df$calories)
index2 <- detect_outlier(df$fat)
data <- df[-c(index1, index2), ]

linearModelB <- lm(calories~fat, data=data)
plot(data$fat, data$calories,
     xlab="Fat", ylab="Calories", main="Fat vs Calories")
abline(linearModelB)

predB <- predict(linearModelB)
resB <- resid(linearModelB)
sresB <- rstandard(linearModelB)

plot(predB, sresB, ylim=c(-3,3), 
     xlab="Fitted Value", 
     ylab="Standardized Residuals", 
     main="Fitted Value vs Standardized Residuals")
abline(h=2, lty = 2)
abline(h=-2, lty = 2)

n <- nrow(data)
plot(1:n, sresB, type="b", 
     xlab="Observation Order", 
     ylab="Standardized Residuals", 
     main="Observation Order vs Standardized Residuals")

p <- (1:n - 0.5)/n * 100; p
sorted_sresB <- sort(sresB); sorted_sresB
plot(sorted_sresB, p,
     xlab="Standardized Residuals",
     ylab="Probability", main="Normal Probability Plot")

plot(pred, whiteside$Gas, 
     xlim=c(1,8), ylim=c(1,8),
     xlab="Fitted Value",
     ylab="Actual Value",
     main="Fitted Value vs Predicted Value")
abline(coef=c(0,1))

#---------------------------------------------------------------------------------------


# 22/4/2022

library(MASS)
df1 <- whiteside[, c("Gas", "Temp")] # Data asli
df2 <- df1
df2[40, ] <- c(9999, 9999) # Data Modifikasi

OLS1 <- lm(Gas~Temp, data=df1)
coef1 <- coefficients(OLS1); coef1
OLS2 <- lm(Gas~Temp, data=df2)
coef2 <- coefficients(OLS2); coef2

par(mfrow=c(1,2))
plot(df1$Temp, df1$Gas, 
     xlab="Temp", ylab="Gas", main = "Data Asli")
abline(OLS1)
plot(df2$Temp, df2$Gas, 
     xlab="Temp", ylab="Gas", main = "Data Modifikasi")
abline(OLS2)
dev.off()


X <- df1$Temp
Y <- df1$Gas
b01 <- coef1[1]; b01
b11 <- coef1[2]; b11
b0 <- c(b01)
b1 <- c(b11)
j <- 2
while(j>=2){
  res1 <- Y-(b0[j-1]+b1[j-1]*X)
  mad1 <- median(abs(res1-median(res1)))
  sigma_hat <- mad1/0.6475
  u <- res1/sigma_hat
  rob1 <- data.frame(res1,u)
  rob1["w"] <- c(1)
  colnames(rob1) <- c("ei", "ui", "wi")
  
  n <- length(res1)
  for(i in 1:n){
    if(rob1$ui[i]  <= 4.685){
      rob1$wi[i] <- (1-(rob1$ui[i]/4.685)^2)^2
    }else{
      rob1$wi[i] <- 0
    }
  }
  W <- diag(rob1$wi)
  MX <- cbind(rep(1,n), X)
  MY <- Y
  XTW <- t(MX) %*% W
  XTWX <- XTW %*% MX
  XTWY <- XTW %*% MY
  est1 <- solve(XTWX) %*% XTWY
  b0 <- append(b0, est1[1])
  b1 <- append(b1, est1[2])
  et0 <- abs((b0[j]-b0[j-1])/b0[j-1])
  et1 <- abs((b1[j]-b1[j-1])/b1[j-1])
  limit <- 0.0000005
  j <- j+1
  if(et0 < limit & et1 < limit){
    break
  }
}
beta0 <- b0[length(b0)]; beta0
beta1 <- b1[length(b1)]; beta1


X <- df2$Temp
Y <- df2$Gas
b01 <- coef2[1]; b01
b11 <- coef2[2]; b11
b0 <- c(b01)
b1 <- c(b11)
j <- 2
while(j>=2){
  res1 <- Y-(b0[j-1]+b1[j-1]*X)
  mad1 <- median(abs(res1-median(res1)))
  sigma_hat <- mad1/0.6475
  u <- res1/sigma_hat
  rob1 <- data.frame(res1,u)
  rob1["w"] <- c(1)
  colnames(rob1) <- c("ei", "ui", "wi")
  
  n <- length(res1)
  for(i in 1:n){
    if(rob1$ui[i]  <= 4.685){
      rob1$wi[i] <- (1-(rob1$ui[i]/4.685)^2)^2
    }else{
      rob1$wi[i] <- 0
    }
  }
  W <- diag(rob1$wi)
  MX <- cbind(rep(1,n), X)
  MY <- Y
  XTW <- t(MX) %*% W
  XTWX <- XTW %*% MX
  XTWY <- XTW %*% MY
  est1 <- solve(XTWX) %*% XTWY
  b0 <- append(b0, est1[1])
  b1 <- append(b1, est1[2])
  et0 <- abs((b0[j]-b0[j-1])/b0[j-1])
  et1 <- abs((b1[j]-b1[j-1])/b1[j-1])
  limit <- 0.0000005
  j <- j+1
  if(et0 < limit & et1 < limit){
    break
  }
}
beta0 <- b0[length(b0)]; beta0
beta1 <- b1[length(b1)]; beta1



#------------------------------------------------------------------------------

Model <- lm(MPG.city ~ Horsepower, data = Cars93)
summary(Model)
attach(Cars93)
plot(Horsepower, MPG.city)
abline(Model)

Model <- lm(MPG.city ~ I(1/Horsepower), data = Cars93)
summary(Model)
a <- I(1/Horsepower)
plot(a, MPG.city)
abline(Model)

Model$residuals


#------------------------------------------------------------------------------------

library(readxl)
library(lattice)
library(lmtest)
library(dplyr)
library(tidyr)

## NOMOR 3
df_3 <- read_xlsx("Nomor 3.xlsx")
Y <- df_3$`IPM 2019 (Y)`
X <- df_3$`Angka Harap Hidup Saat Lahir 2019 (X)`

# Regresi
model <- lm(Y~X)
pred <- predict(model)
res <- resid(model)
sres <- rstandard(model)

# Boxplot
boxplot(X, main = "Boxplot Angka Harap Hidup Saat Lahir 2019")
boxplot(Y, main = "Boxplot IPM 2019")

# Scatter Plot
plot(X,Y, xlab = "Angka Harapan Hidup", ylab = "IPM", main = "Data Provinsi Tahun 2019")
abline(model, col="blue")

# Identik
plot(pred, sres, ylim=c(-3,3), 
     xlab="Fitted Value", 
     ylab="Standardized Residuals", 
     main="Fitted Value vs Standardized Residuals")
abline(h=2, lty = 2)
abline(h=-2, lty = 2)
abline(h=0)
# H0 : Homoskedastisitas; H1 : Heteroskedastisitas
bptest(model, studentize=F) # Error Identik

# Independen
n <- nrow(df_3)
plot(1:n, sres, type="o", pch=16,
     xlab="Observation Order", 
     ylab="Standardized Residuals", 
     main="Observation Order vs Standardized Residuals")
# H0 : Autokorelasi ; H1 : Tidak terjadi autokorelasi
dwtest(model) #Error Tidak independen

#eror berdistribusi normal
p <- (1:n - 0.5)/n * 100; p
sorted_sres <- sort(sres); sorted_sres
plot(sorted_sres, p,
     xlab="Standardized Residuals",
     ylab="Probability", main="Normal Probability Plot")
# H0 : Dist. Normal ; H1 : Tidak Dist. Normal
ks.test(resid(model), y=pnorm) #Error Berdistribusi Normal

# Summary
model$coefficients
summary(model)

# Robust Regression
model2 <- rlm(Y ~ X)
plot(X,Y, xlab = "Angka Harapan Hidup", ylab = "IPM", main = "Data Provinsi Tahun 2019")
abline(model2, col="blue")

model2$coefficients
summary(model2)

# Resistant Line
resline <- line(X,Y, iter=10)
resline

intercept <- resline$coefficients[1]; intercept
slope <- resline$coefficients[2]; slope

plot(X,Y, xlab = "Angka Harapan Hidup", ylab = "IPM", main = "Data Provinsi Tahun 2019")
abline(model, col="blue")

# Pembanding
plot(X,Y, xlab = "Angka Harapan Hidup", ylab = "IPM", main = "Data Provinsi Tahun 2019")
legend("bottomright", 
       legend = c("Regresi", "Regresi Robust", "Resistant"), 
       col = c("red", "blue", "black"), 
       pch = 3,
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F)
abline(model, col="red")
abline(model2, col="blue")
abline(a = intercept, b=slope, col="black")



## NOMOR 4
df_4 <- read_xlsx("Nomor 4.xlsx")
head(df_4)
clean_df_4 <- df_4 %>% gather(key = Jarak, value=Waktu, -Tahun) %>%
  mutate(Tahun = factor(Tahun))
head(clean_df_4)

# Anova tanpa Interaksi
anova1 <- aov(Waktu ~ Tahun + Jarak, data = clean_df_4)
anova1
summary(anova1)

# Anova dengan Interaksi
anova2 <- aov(Waktu ~ Tahun + Jarak + Tahun:Jarak, data = clean_df_4)
anova2
summary(anova2)

# Median Polish
med <- medpolish(df_4, maxiter=3)
med
