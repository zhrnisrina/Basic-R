
print ("Hello World")

a = "pommes frites" ; a

b = chartr("oemsp", "eol H", a) ;b 
o jadi e, e jadi o, m jadi l, s jadi spasi, p jadi H
#outputnya "Hello frito"

class(b)
str(b)

c = unlist(strsplit(b, " "))[c(1,3)];c

p = strsplit(b, " ")
class (p)
q = unlist(p)
q
q[c(1,3)]
q[1]
q[3]

d = as.numeric(charToRaw(c[2])) ; d 

as.numeric(charToRaw("hai")) 

e = rawToChar(as.raw(d + c(17 , -3 , 9 , -8 , -11)))
e

e = rawToChar(as.raw(d)) ; e

f = paste(c[1], e, "!!!") ; f

#------------------------------------------------------------

help () #
help ( sin) # help for sin ()
?sin # help for sin
help . start () # HTML help
library () # show installed libraries
library ( help ="<package >") # or without ""
help . search ("sin")
?? sin # same as help . search (" sin ")


#------------------- BASIC R STUFF ---------------------- 

setwd() # set working directory
getwd () # get working directory
save . image () # saves workspace to . Rdata
savehistory () # save command history
load (". Rdata ") # loads workspace
loadhistory () # as the name says
source (" myfile .r") # read commands from file
sink (" output .txt") # write output to file
sink () # output to screen
q(); quit () # quits R


#-------------------------------------------------------------

# download and install
install . packages (" mvtnorm ")
# update local packages
update . packages ()
# show installed libraries
library ()
# load library
library (name ,lib.loc=[ location ])
# load library in functions
require () # return TRUE or FALSE

# show global options
options ()
# set option
options ( prompt =": -)")
# get spec . option
getOption (" prompt ")


#------------------ BASIC CALCULATION ------------------------

a <- pi + 0.5
a # [1] 3.641593
floor (a) # 3
floor ( -a) # -4
ceiling (4.8) # 5
ceiling ( -a) # -3
trunc (3.9) # 3
trunc (-a) # -3
round (a) # 4
round ( -a) # -4


a <- 2
a

sin(pi)
acos(0)
cosh(0)

#--------------------------------------------------------------

search()
ls(2)
objects()
rm(list=ls())  # removes all objects

seq (1 ,3)
seq (3) 
seq (1 ,2 , by= 0.1)
seq (1 ,3 ,0.5)
seq (1 ,3 , length.out = 4)
rep (1:4 ,2)
rep (1:4 , each = 2)
rep (c(7 ,9 ,3) , 1:3) 

a <- c(2,3,1,4)
a
length(a)
rev(a)
a[1:2]
a[-1]
a[-c(1,2)]
a[a<3]
which (a == 3)
a >1 

a <- letters [1:3]
a
b <- LETTERS [1:3]
b


#---------------------------- VECTORS ------------------------------

a <- c(1 ,2 ,3 ,4)
a
t(a)
t(t(a))

matrix (1:12 , nrow =3)
matrix (1:12 , nrow =3 , byrow = T)
matrix (1 , nrow =2 , ncol =2)
matrix (1:4 , 2 ,2)

x = 1:3
y = 4:6
rbind (x,y)
cbind (x,y)

x <- matrix (1:10 , 2 , 5)
x
dim(x)
col(x)
row(x)

x[row(x) == col(x) ]


k= matrix (1:10 ,2 ,5)
k

k [1:2 ,3:4]
diag(2)
diag(5,3,3)


b <- matrix (1:20 ,4 ,5)
b
dimnames (b)<- list ( letters [1:4] , letters [1:5])
b["b","b"]

d <- matrix (1:20 ,4 ,5)
d
colnames (d) <- letters [1:5]
rownames (d) <- letters [1:4]

b["b","b"]

x = matrix (1:20 ,4 ,5)
prod (x)
colSums (x)

#--------------------------------------------------------------------------------


myframe = read.table ("myframe.txt", header=T,sep ="")
myframe

class(myframe)

a <- c(10,20,15,43,76,41,25,46)
a
str(a)
dim(a)
dim(as.matrix(a))

b <- c("m", "f","m", "f","m", "f","m", "f")
class(b)

b <- factor(c("m", "f","m", "f","m", "f","m", "f"))
class(b)
str(b)
#tipe data faktor yaitu data bertipe kategori 

c <- c(2,5,8,3,6,1,5,6) 
c
class(c)

z <- c(1,5)
z
zz <- factor(c(1,5))
zz

mydat = cbind(a,b,c) ; mydat
class(mydat)
#tipenya matrix -> semua elemen di dalamnya numerik bukan lg kategori

myframe <- data.frame(a,b,c)
class(myframe)
#tipe datanya ttp dataframeee

colnames(myframe) <- c("Age", "Sex", "Siblings")
myframe

myframe [ ,1] #smua baris kolom 1 
myframe ["Age"]
myframe [c("Age", "Sex")]
myframe $ Age

myframe [3 ,3] <-2 # change value do baris 3 kolom 3 
myframe [ ,-2] # all vars except 2nd atau dihapus kolom keduanya 
myframe [ , -c(1,2)]

str(myframe)
attach(myframe)
Sex
Age
Siblings
detach(myframe)

myframe[myframe$Age > 30, ]
subset ( myframe , myframe $ Age >30) 
#yang umurnya lebih dari 30 

mean ( subset ( myframe $Age , myframe $Sex=="m"))
#hitung rata2 umur yg laki2
mean ( subset ( myframe $Age , myframe $Sex!="m"))
#hitung rata2 umur yg bukan laki2 ((perempuan))

myframe [( myframe $Sex=="m") & ( myframe $Age >30) ,]
#laki2 yg umurnya > 30 
myframe [( myframe $Sex=="m") | ( myframe $Age >30) ,]
#laki2 atau umur >30

myframe [ (myframe$Sex =="m")&
            (myframe$Age >10 &
               myframe$Siblings >2), ]


myframe <- cbind(myframe, "Income(USD)"=
                   factor(c(1700,2100,2300,2050,2800,1450,3400,2000))
               
names(myframe)[names( myframe )==" Income ( USD)"] <-
  " IncomeUSD "
myframe

names ( myframe ) <- gsub ("In","Out",names ( myframe ))
names(myframe)

x = c(2,3,5,2,5,6,7,3)
sort(x)
order(x)
rank(x)

myframe[order(myframe$Age) ,]
#sesuai tahun

myframe [ order ( myframe $Sex , partial = myframe $Age) ,]
myframe

myframe [, -1]
edited.myframe <- edit ( myframe )
fix(myfrme)

Sys.time () # clock time as POSIXct
date () # Date without a time
s = c(" 23.05.1984 ", " 01.01.2000 ", " 03.05.1256 ")
s
d = as.Date (s, "%d.%m.%Y")
difftime (Sys.time () , d)
format (d, "%Y")
dP = as.POSIXlt (d)
months (dP)
weekdays (dP)
quarters (dP)


library("datasets")
data("pressure")
head(pressure)
tail(pressure)
head(pressure,10)

objects(package:datasets)
help(Orange)
data(Orange)
head(Orange)
objects()

#memunculkannnnnn file
#bskbskkkgausahdirun 

setwd ("C:/Users/HP/OneDrive - Institut Teknologi Sepuluh Nopember/SEMESTER 4/Komputasi Statistika")
write.table (Orange , " orange1 .txt")
write.table (Orange , " orange2 .txt", col.names = F,
               row.names = F)
write.table (Orange , " orange3 .txt", sep = "\t")
write.csv2 (Orange , " orange4 .csv ", row.names = F)

data <- read.table(" orange1 .txt", header = T)
head(data)

names ( data ) = c(" Pohon "," Usia "," LingkarPohon ")
head(data)

data <- read.csv(" orange4 .csv", sep = ";")
data2 = data[ , -1]
data2

n=10
sample(n)
sample(10, replace = T)
x = c(5,9,7,1,3)
sample(x)
sample(x, replace = T)
sample(x,2)

sample(x,3, replace = T)
sample(c("Z","K"),10, replace =T)
sample(c("Z","K"),10, replace =T, prob = c(0.1,0.9))


#--------------Univariate Statistics -----------------------------

# density value of N(0 ,1)
dnorm(0)
dnorm(x=1, mean=1, sd=2)
dnorm(1,1,2)
# cumulative density up to 0
pnorm(0) 
# quantile for 0.5
qnorm (0.5)
# vector with 100 random numbers
rnorm (100)

z1= rnorm (100)
hist (z1)
z2= rnorm (100 , mean =10, sd=2)

hist (z2 , col="pink ")

z2 = rnorm (100 , mean =10, sd=2)
hist (z2 , probability =T, col=" yellow ")
#
# KDE using Gaussian kernel
density (z2)
# empirical cdf
ecdf (z2)


#-------------------------------------------- PLOT

library ("datasets")
data("pressure")
help("pressure")
plot(pressure, type = "c")
plot(pressure, type = "l")
plot(pressure, type = "h")
plot(pressure, type = "p")
plot(pressure, type = "b", lwd = 3, col="blue",
     main = "scatter plot", xlab = "suhu", 
     ylab = "tekanan udara", cex = 1.5, cex.lab = 1.3)

with ( pressure , plot ( temperature , pressure ))
with ( pressure , plot ( pressure , temperature ))


png ( filename = " Rplot.png", width = 480 ,
      height = 480 , units = "px", pointsize = 12,
      bg = " white ", res = NA , restoreConsole = TRUE )

plot ( pressure )
dev.off ()

plot(pressure)
text(150, 200, label = "Hello")

a <- 2
bquote (a == a) # [1] a == a
bquote (a == .(a)) # [1] a == 2

x <- seq (-2, 2, l = 200)
plot (x, dnorm (x), type = "l")
mu <- 0
variance <- 1
text (0, 0.2 , pos =1,
        label = bquote ( paste (" normal density , ",
          mu == .( mu), "; ", sigma ^2 == .( variance ))))

data(iris)
pch.vec <- c(16 ,2 ,3)[ iris$Species ]
col.vec <- c(16 ,2 ,3)[ iris $ Species ]
plot ( iris $ Sepal.Length , iris $ Sepal.Width ,
         col = col.vec , pch = pch.vec)


par ( mfrow = c(1, 2)) # two - colum plot
plot ( pressure , type ="l")
plot ( pressure , type ="h")


layout(matrix(c(1,1)))

layout ( matrix (c(1, 2, 3, 4) , 2, 2, byrow = TRUE ))
layout.show (4)

layout ( matrix (c(1, 1, 3, 2) , 2, 2, byrow = TRUE ))
layout.show (3)

boxplot(iris$Sepal.Length)
boxplot ( pressure , data = pressure , col=" lightgray ")
dev.off()

boxplot ( Sepal.Length ~ Species , data =iris , horizontal =
            TRUE , col=c(" red3 "," blue3 "," green3 "))

legend (y=1.5 , x=6.5 , legend =c(" setosa "," versicolor ",
                                    " virginica "), pch=c(2 ,2 ,2) , col=c(" red3 "," blue3 ",
                                                                           "green3 "))

hist ( iris $ Sepal.Length , col=" lightblue ") # Histogram

hist ( iris $ Sepal.Length , freq = FALSE ) # with density

hist ( iris $ Sepal.Length , freq = FALSE ) # with density
seq (min(iris$Sepal.Length ), max( iris $ Sepal.Length ),
       length = 100) -> grid
d1 <- dnorm (grid , mean ( iris $ Sepal.Length ), sd( iris $Sepal.Length ))
lines (grid , d1 , col = " red3 ", lwd = 2)

install.packages("fields")
R = matrix ( runif (15^2 , -1, 1) , 15, 15)
library ("fields") # extra package
image.plot (R, axes = FALSE )

## Bivariate normal distrib . density
install.packages("mvtnorm")
library ("mvtnorm")
x <- y <- seq (-5, 5, length = 50)
f <- function (x, y){ dmvnorm ( cbind (x, y))}
z <- outer (x, y, f)
persp (x, y, z, theta = 10, phi = 20, expand = 0.5 ,
         col = " lightblue ", shade = 0.75)

x <- y <- seq (-5, 5, length = 150)
z <- outer (x, y, f)
contour (x, y, z, nlevels = 20)
contour (x, y, z, nlevels = 20, col = rainbow (20) )
contour (x, y, z, nlevels = 20, col = rainbow (20) ,
           labels = "")

install.packages("scatterplot3d")
library ( scatterplot3d ) # extra package !
x <- matrix ( rnorm (15000) , ncol = 3)
scatterplot3d (x)
scatterplot3d (x, angle = 20)

install.packages("rgl")
## 3D normally distrib . data
library ( rgl) # extra package !
x <- matrix ( rnorm (1500) , ncol = 3)
plot3d (x, col = 4, size = 4)


layout ( matrix (c(1, 2, 3, 3) , 2, 2, byrow = TRUE ))
par (mai = c(0.5 , 0.5 , 0.5 , 0.5) )
x = -6:6; y = x^2

plot (x, y, pch = 19, type = "l")
persp ( outer (x, y, "*"), shade = 0.7 , theta = -30)
contour ( outer (x, y, "*"), col = rainbow (10) , nlevels
            = 10, lwd = 2)

install.packages("lattice")
install.packages("xyplot")
library ("lattice")
x = 1:10 ; y = 1:10
p <- xyplot (x~y)
print (p)
update (p, main = " title ")

install.packages("rpanel")
library(rpanel)

library ( rpanel )
density.draw <- function ( panel ) {
  plot ( density ( panel $x, bw = panel $h))
  panel
  }
panel <- rp.control (x = rnorm (50) )
rp.slider (panel , h, 0.5 , 5, log=TRUE , action = density.draw )


#-----------------------------------------------------------------------------
setwd("C:/Users/HP/OneDrive - Institut Teknologi Sepuluh Nopember/SEMESTER 4/Komputasi Statistika")

dnorm (0) # density value of N(0 ,1)
pnorm (0) # cum . density up to 0
qnorm (0.5) # quantile for 0.5
rnorm (100) # vector with 100 random numbers

density () # KDE using Gaussian kernel
ecdf () # empirical cdf

sample (n) # sample 1:n vector
sample (x) # shuffle the x vector
sample (x, replace = TRUE ) bootstrap x vector
sample (x, n) # draw sample of size n from x
sample (x, n, replace = TRUE ) # bootstrap sample from x

Hubble = read.table("HUbble.txt", sep = "\t", header = TRUE)
head(Hubble)
distance = Hubble$distance
revel = Hubble$recession_velocity

model = lm(revel ~ distance - 1)  #no intercept
#prediktor distance, revel response
lm(revel ~ distance) #dgn intercept
summary(model)
anova(model)

library(car)
qqPlot(res)

res = model$residuals
yhat = model$fitted.values

cbind(revel, yhat, res)

plot(res)
plot(res, type = "b")

data = read.table (" cereal .txt", header =T)
model = lm( rating ~ sugars + fat , data = data )
summary ( model )
anova (model)

model$residuals
resid(model)

model$fitted.values
fitted.values(model)

model$coefficients
coefficients(model)

vcov(model)
qqPlot(model$residuals)

#nonlinear


x <- seq(2,4, l = 1000)
y <- x^(3+exp(-0.5*x))+rnorm(1000)
data.xy = data.frame(x,y)
plot(x[501:1000],y[501:1000])

nonlinfit <- nls(y~x^(a+exp(b*x)), 
                 start=list(a=1, b=0.1))

install.packages("tseries")
library(tseries)
library(datasets)
data(LakeHuron)
adf.test(LakeHuron)
kpss.test(LakeHuron)


#-----------------------------------------------------------------------

a <- 1:3
b <- 2:6
a
b
a %in% b
b %in% a
a <- c("A","B") 
b <- LETTERS[2:6]
a %in% b
b %in% a

# simple if
x <- 1
if (x==2) print ("x=2")

if(x==2){
  print("x==2")
  print("your answer is correct")
} else{
  print("x!=2")
  print("your answer is not correct")
  }
}

for (i in 1:4) print (i)
for (i in letters [1:4]) { print (i) }
for (i in LETTERS [1:4]) { print (i) }

# generate empty a of length 400
a <- numeric (400)
# fill a with 1:400 ,
# it takes much longer than a <- 1:400
for (i in 1:400) { a[i]=i }
a

i <- 0
while(i<4){
  i <- i+1
  print(i)
}

for(i in 1:100) print(paste("i = ", i))

i <- 0
repeat{
  i <- i+1 ; 
  print(i) ;
  if(i==4) break
}

x <- c(6: -4)
x

sqrt(x)
sqrt ( ifelse (x >= 0, x, NA) )

x <- c (1:5000000)
system.time (for (i in 1:5000000) {x[i] <- rnorm (1)})

system.time (x <- rnorm (5000000) )

matrix (1:10 , nrow =2) ->a
a
mean(a[1,])
apply (a ,1 , mean )
apply (a ,2 , mean )
mean(a)

#MARGIN = 1 indicates row
apply(a, MARGIN = 1, mean)
#MARGIN = 2 indicates column
apply(a, MARGIN = 2, mean)
     
a = matrix(2:11, nrow = 2)
b = matrix(1:10, nrow = 2)
c = list(a,b)
c
ls()

lapply(c, mean) #outputnya list
sapply(c, mean) #outputnya matrix
mapply(rep, pi, 3:1) #buat multivariate

data(iris) 
head(iris)
tapply ( iris$Sepal.Width , iris$Species , mean ) #mean sepal width berasarkann spesiesnya 

factor(iris$Species)

pairs(iris[c("Sepal.Length","Sepal.Width",
             "Petal.Length","Petal.Width")], 
      main="Matrix plot", pch=c(21,23,25),
      bg=c("red", "yellow", "blue")
      [unclass(iris$Species)])

dev.off()

getAnywhere(apply)

myfun <-  function(x){
  return(x*x)
}
myfun(2)

#kalo gaada return outputnya operasional di baris terakhir
myfun <-  function(x){
  x*x
  x
}
myfun(2)

myfun <- function(x,a){
  r <- a*sin(x)
  return(r)
}

myfun(pi/2,2)


myfun2 <- function (x, a=1){ 
  a*sin(x)
  }
myfun2 (pi /2 ,2)
myfun2 (pi /2)

myfun3 <- function(x,a=NULL){
  if(! is.null(a)){
    a*sin(x)
  } else{
    cos(x)
  }
}
myfun3(pi/2,2)
myfun3(pi/2)

myfun4 <- function(x, a=1){
  r1 <- a*sin(x)
  r2 <- a*cos(X)
  return(list(r1,r2))
}

rootsquare <- function(x,type){
  switch(type,
         square=x*x,
         root=sqrt(x))
} #typenya ada 2
rootsquare(10,"square")
rootsquare(10,"root")


#---------------------------------------------------------------------------------

x <- c(8.26, 6.33, 10.4, 5.27, 5.35, 5.61, 6.12, 6.19, 
       5.2,7.01, 8.74, 7.78, 7.02, 6, 6.5, 5.8, 5.12, 
       7.41, 6.52, 6.21,12.28, 5.6, 5.38, 6.6, 8.74)

CV <- function(x) sqrt(var(x))/mean(x)

CV(x)

sample(x,replace=T)
CV(sample(x,replace=T))

sum(x)
mean(x)
var(x)
length(x)
sum((x-mean(x))^ 2)


boot <- numeric(1000)
for (i in 1:1000) {
  boot[i] <- CV(sample(x,replace=T))
}

boot
mean(boot)
var(boot)
hist(boot)
#x nya CV y nya koef korelasi ?


UB = quantile(boot,0.975)
LB = quantile(boot,0.025)

print(paste("CI 95% = ", format(round(LB,3)) , "-", format(round(UB,3))))

#Unbiased Estimator : E(estimator) = parameter
#E(estimator) + bias(estimator) = parameter

bias <- mean(boot) - CV(x)
CV(x) - bias
 
CV(x) - bias - 1.96*sqrt(var(boot))
CV(x) - bias + 1.96*sqrt(var(boot))


jack <- numeric(length(x)-1)
length(jack)

pseudo <- numeric(length(x))

for (i in 1:length(x)){ 
  for (j in 1:length(x)){
    if(j < i) jack[j] <- x[j] 
    else if(j > i) jack[j-1] <- x[j]
  }
  pseudo[i] <- length(x)*CV(x) -(length(x)-1)*CV(jack)}                

mean(pseudo)
var(pseudo)
hist(pseudo)

var(pseudo)/length(x)

mean(pseudo) + qt(0.975,length(x)-1)*sqrt(var(pseudo)/length(x))
mean(pseudo) - qt(0.975,length(x)-1)*sqrt(var(pseudo)/length(x))


data("mtcars")
mpg = mtcars$mpg
n = length(mpg)
print(mean(mpg))
hist(x = mpg, probability = TRUE, xlab = "MPG", main = "Histogram of MPG")

B = 1000 ## number of bootstraps
results = numeric(B) ## vector to hold results
for(b in 1:B){
  i = sample(x = 1:n, size = n, replace = TRUE) ## sample indices
  bootSample = mpg[i] ## get data
  thetaHat = mean(bootSample) ## calculate the mean for bootstrap sample
  results[b] = thetaHat ## store results
}
hist(x = results, probability = TRUE,
     main = "Bootstrapped Samples of Mean_mpg",
     xlab = "theta estimates")

#PAIRED BOOSTRAPPING

library(ggplot2, quietly = TRUE) ## for graphics
mtcars$am <- as.factor(mtcars$am) ## Transmission (0 = automatic, 1 = manual
fit = lm(formula = mpg ~ wt + am, data = mtcars)
data.frame(coefficients = coefficients(fit), CI = confint(fit), check.names = FALSE)

qplot(x = as.factor(am), y = mpg, data = mtcars, geom = "boxplot",
      main = "Boxplot: MPG ~ AM", ylab = "MPG", xlab = "AM",
      colour = am)

qplot(x = wt, y = mpg, data = mtcars, geom = c("point", "smooth"),
      main = "Boxplot: MPG ~ Weight", ylab = "MPG", xlab = "Weight",
      method = "lm", formula = y~x)

## save coefficients
beta_int = coefficients(fit)[1]
beta_wt = coefficients(fit)[2]
beta_am = coefficients(fit)[3]
n = dim(mtcars)[1] ## number of obs in data
B = 1000 ## number of bootstrap samples
results = matrix(data = NA, nrow = B, ncol = 3,
                 dimnames = list(NULL, c("Intercept", "wt", "am")))
## begin bootstrap for-loop
for(b in 1:B){
  i = sample(x = 1:n, size = n, replace = TRUE) ## sample indices
  temp = mtcars[i,] ## temp data set
  temp_model = lm(formula = mpg ~ wt + am, data = temp) ## train model
  coeff = matrix(data = coefficients(temp_model), ncol = 3) ## get coefficients
  results[b,] = coeff ## save coefficients in matrix
}
results <- data.frame(results, check.names = FALSE)
summary(results) ## take a look at the samples
boot_int = results[,"Intercept"]
boot_wt = results[,"wt"]
boot_am = results[,"am"]
par(mfrow = c(2,2))
hist(boot_int, main = "Bootstrapped Coefficients for Intercept",
     xlab = "Coefficients for Intercept", probability = TRUE)
abline(v = coefficients(fit)[1], col = "black", lty=2)
hist(boot_wt, main = "Bootstrapped Coefficients for Weight",
     xlab = "Coefficients for Weight", probability = TRUE)
abline(v = coefficients(fit)[2], col = "blue", lty=2)
hist(boot_am, main = "Bootstrapped Coefficients for AM = 1",
     xlab = "Coefficients for Automatic Transmission", probability = TRUE)
abline(v = coefficients(fit)[3], col = "green", lty=2)


#---------------------------------------------------------------------------
#---------------------------------------------------------------------------


i = rep(seq(1:12), 10) ; i
m = rep(month.abb[1:12], 10) ; m
y = rep(2001 : 2020, each = 6) ; y
t = seq(1:length(i)) ; t
Yt = 100 + 10*t + 50 *sin(2*pi*t/12) + rnorm(length(t), mean = 0, sd = 4) ; Yt

data = data.frame(i,m,y,t,Yt) ; data 
colnames(data) <- c("IndexMonth", "Month", "Year", "Time", "Yt")
data

attach(data)

png ( filename = " plot.png", width = 480 ,
      height = 480 , units = "px", pointsize = 12 ,
      bg = " white ", res = NA , restoreConsole = TRUE )

plot(Time, Yt, type = "l", lwd = 3, col = "red", xlab = "Time", ylab = "Yt")

dev.off()
 
model1 <- lm(Yt ~ Time)
model1$coefficients
model1$fitted.values 

model2 <- lm(Yt ~ Time - 1)
model2$coefficients
model2$fitted.values 

plot(Time, model1$fitted.values , type = "l", lwd = 3, col = "maroon", xlab = "Time", ylab = "Yt'")
plot(Time, model2$fitted.values , type = "l", lwd = 3, col = "magenta", xlab = "Time", ylab = "Yt'")


#----------------------------------------------------------------------------------------


