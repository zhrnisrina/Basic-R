
setwd("C:/Users/HP/OneDrive - Institut Teknologi Sepuluh Nopember/SEMESTER 4/Komputasi Statistika")

Hubble = read.table("HUbble.txt", sep = "\t", header = TRUE)
Hubble
distance = Hubble$distance
rec.vel = Hubble$recession_velocity

#No intercept
model1 = lm(Hubble$recession_velocity~Hubble$distance-1) 
summary(model1)
anova(model1)
model1$coefficients
coef(model1)

thit = coef(model1)/sqrt(vcov(model1))
thit

#Ho = H is equal to zero
#H1 = H is not equal to zero

#pvalue
dt(thit,23)
#ttabel
qt(0.95, 23)

#tolak Ho jika thit > ttabel
#tolak Ho, H is different from zero

plot(model1$residuals) #ngga keluarr


#With Intercept
model2 = lm(Hubble$recession_velocity~Hubble$distance) 
summary(model2)
anova(model2)
model2$coefficients

thit = coef(model2)/sqrt(vcov(model2))
thit
#ada NAN


library("scatterplot3d")

cereal = read.table("cereal.txt", header = TRUE)

model = lm( cereal$rating ~ cereal$sugars + cereal$fat , data = Hubble )
summary ( model )
anova (model)

#uji serentak

"qt(0.95, 61)

thit1 = coef(model)[1]/sqrt(vcov(model)[1,1])
thit1
thit2 = coef(model)[2]/sqrt(vcov(model)[2,2])
thit2
thit3 = coef(model)[3]/sqrt(vcov(model)[3,3])
thit3"

scatterplot3d(model$fitted.values,cereal$sugars,cereal$fat)
sc3d=scatterplot3d(cereal$sugars,cereal$fat,cereal$rating,pch=16,type="h", highlight.3d = T)
sc3d$plane3d(model, lty.box = "solid",col="blue")



#---------------------------------------------------------------------------
#---------------------------------------------------------------------------

#1. estimasi parameter pendekatan matrix 

X = cbind(rep(1,24),distance)
Xt = t(X)
XtX = Xt%*%X
a <- solve(XtX)

Y = rec.vel
XtY = Xt%*%Y

parameter = a%*%XtY
parameter

I = diag(1, length(distance))
I

J = matrix(1, length(distance), length(distance))
J

H = X%*%solve(Xt%*%X)%*%Xt

#2
SST = t(Y)%*%(I-J/24)%*%Y ;SST
SSR = t(Y)%*%(H-J/24)%*%Y ; SSR
SSE = t(Y)%*%(I-H)%*%Y ; SSE

SSE+SSR

#3
MSR = SSR/1 ; MSR
MSE = SSE/(24-1-1) ; MSE
Fhit = MSR/MSE ; Fhit
pvalue <- pf(Fhit, 1, 22, lower.tail =F) 
pvalue

#4
Rkuadrat = 1-(SSE/SST) ; Rkuadrat

#5 
variance = c(MSE) ; variance
SE = a*variance
SEb0 = sqrt(SE[1,1]) ; SEb0
SEb1 = sqrt(SE[2,2]) ; SEb1

#6
t.test(distance, y =NULL, alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = TRUE)


#7

for(i in (1:24)){
  y = -40.78365 + 454.15844*distance[i]
  print(y)
}

residual = rec.vel - y
residual

#8

hist(residual)
plot(c(1:nrow(Hubble)), residual)
lines(c(1:nrow(Hubble)),residual)
