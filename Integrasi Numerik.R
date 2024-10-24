#RIEMANN

riemann <- function(y,n,a,b)
{
  h <- (b-a)/n
  x = a
  hasil = y(a)
  for ( i in 1:n ){
    x = x+h
    hasil = hasil+y(x)
  }
  hasil = h*hasil
  print(hasil)
}
riemann(function(x){0.2+25*x-200*x^2+675*x^3-900*x^4+400*x^5}, n=8, a=0, b=0.8)


#TRAPEZOIDAL

trapezoidal <- function(y,n,a,b)
{
  h <- (b-a)/n
  x = a 
  hasil = y(a)+y(b)
  for ( i in 1:(n-1) ){
    x = x+h
    hasil = hasil+2*y(x)
  }
  hasil = hasil*h/2
  print(hasil)
}
trapezoidal(function(x){0.2+25*x-200*x^2+675*x^3-900*x^4+400*x^5}, n=8, a=0, b=0.8)


#SIMPSON 1/3

simpson1 <- function(y,n,a,b)
{
  h <- (b-a)/n
  x = a 
  hasil = y(a)+y(b)
  for ( i in 1:(n-1) ){
    x = x+h
    if(i%%2 != 0){
      hasil = hasil+4*y(x)
    }else{
      hasil = hasil+2*y(x)
    }
  }
  hasil = hasil*h/3
  print(hasil)
}
simpson1(function(x){0.2+25*x-200*x^2+675*x^3-900*x^4+400*x^5}, n=8, a=0, b=0.8)



#SIMPSON 3/8

simpson2 <- function(y,n,a,b)
{
  h <- (b-a)/n
  x = a 
  hasil = y(a)+y(b)
  for ( i in 1:(n-1) ){
    x = x+h
    if(i%%3 != 0){
      hasil = hasil+3*y(x)
    }else{
      hasil = hasil+2*y(x)
    }
  }
  hasil = hasil*h*3/8
  print(hasil)
}
simpson2(function(x){0.2+25*x-200*x^2+675*x^3-900*x^4+400*x^5}, n=8, a=0, b=0.8)



#Integral Lipat 2

lipat2 <- function(fx,fy,n,m,a,b,c,d)
{
  h <- (b-a)/n
  k <- (d-c)/m
  y = c
  hasil1 = fy(c)+fy(d)
  for ( j in 1:(m-1)){
    y = y+k
    if(j%%2 != 0){
      hasil1 = hasil1+4*fy(y)
    }else{
      hasil1 = hasil1+2*fy(y)
    }
  }
  x = a
  hasil2 = fx(a) + fx(b)
  for ( i in 1:(n-1)){
    x = x+h
    if(i%%2 != 0){
      hasil2 = hasil2+4*fx(x)
    }else{
      hasil2 = hasil2+2*fx(x)
    }
  }
  hasil = hasil1*hasil2*h*k/9
  print(hasil)
}
lipat2(function(x){(x^2)},function(y){y^2}, n=4, m=4, a=0, b=2, c=1, d=5)


#z-table

trap <- function (f,a,b,n)
{
  h <- (b-a)/n
  hasil <- f(a)+f(b)
  for ( i in 1 : (n-1)){
    x <- a+i*h
    hasil <- hasil + 2*f(x)
  }
  hasil = h/2*hasil
  return(hasil)
  }
  f = function(x){dnorm(x,0,1)}
  alpha <- 0.05
  a <- -5
  et <- 1
  while(et>0.0001){
    luas = trap(f,a,4,100)
    et <- abs(luas-alpha)
    a<- a+0.001
  }
  ztabel <- a
  ztabel 

  
#p-value
  
simpson1 <- function(f,a,b,n)
{
  h <- (b-a)/n
  hasil <- f(a)+f(b)
  for(i in 1:(n-1)){
    x <- a+i*h
    if(i%%2==0){
      hasil <- hasil + 2*f(x)
    }else{
      hasil <- hasil + 4*f(x)
    }
  }
  hasil <- h/3*hasil
  return(hasil)
}
pvalue <- 1/sqrt(2*pi)*simpson1(function(x){exp(-x^2/2)}, -5, -1.645, 10000)
pvalue


#cara lain

simpson1 <- function(f,a,b,n)
{
  h <- (b-a)/n
  hasil <- f(a)+f(b)
  for(i in 1:(n-1)){
    x <- a+i*h
    if(i%%2==0){
      hasil <- hasil + 2*f(x)
    }else{
      hasil <- hasil + 4*f(x)
    }
  }
  hasil <- h/3*hasil
  return(hasil)
}
erf <- function(x)
{
  a<- simpson1(function(x){exp(-x^2)},0,x,100)
  erf <- 2/sqrt(pi)*a
  return(erf)
}
cdfnorm <- function(x){
  1/2*(1+erf(x/sqrt(2)))
}
pvalue <- cdfnorm(-1.93)
pvalue


#cara satu lagi 

xbar <- 7.8
miu <- 8
s <- 0.5
n <- 50

z <- (xbar-miu)/(s/sqrt(n))
z

pvalue <- 1/sqrt(2*pi)*simpson1(function(x){exp(-x^2/2)}, -5, -z, 1000)
pvalue

pvalue <- 1/sqrt(2*pi)*simpson1(function(x){exp(-x^2/2)}, -5, z, 1000)
1-pvalue

pvalue <- pnorm (-abs(z))
pvalue


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#latihannnnnnnnnnn

simpson2 <- function(y,n,a,b)
{
  h <- (b-a)/n
  x = a 
  hasil = y(a)+y(b)
  for ( i in 1:(n-1) ){
    x = x+h
    if(i%%3 != 0){
      hasil = hasil+3*y(x)
    }else{
      hasil = hasil+2*y(x)
    }
  }
  hasil = hasil*h*3/8
  print(hasil)
}
simpson2(function(x){0.2+25*x-200*x^2+675*x^3-900*x^4+400*x^5}, n=4, a=0, b=0.8)

truevalue = integrate(function(x){0.2+25*x-200*x^2+675*x^3-900*x^4+400*x^5},lower=0,upper=0.8)
truevalue


#EKSPEKTASI

EX <- function(y,n,a,b)
{
  h <- (b-a)/n
  x = a 
  hasil = y(a)+y(b)
  for ( i in 1:(n-1) ){
    x = x+h
    if(i%%2 != 0){
      hasil = hasil+4*y(x)
    }else{
      hasil = hasil+2*y(x)
    }
  }
  hasil = hasil*h/3
  print(hasil)
}
EX(function(x){x*(32/(x+4)^3)}, n=1000, a=0, b=100)


#VARIANSI

#E(X^2)
EX2 <- function(y,n,a,b)
{
  h <- (b-a)/n
  x = a 
  hasil = y(a)+y(b)
  for ( i in 1:(n-1) ){
    x = x+h
    if(i%%2 != 0){
      hasil = hasil+4*y(x)
    }else{
      hasil = hasil+2*y(x)
    }
  }
  hasil = hasil*h/3
  print(hasil)
}
EX2(function(x){x^2*(32/(x+4)^3)}, n=100, a=0, b=100)

EX2(function(x){x^2*(32/(x+4)^3)}, n=100, a=0, b=100)-(EX(function(x){x*(32/(x+4)^3)}, n=100, a=0, b=100))^2

#-------------------------------------------------------------------------------------------------


EX<- simpson1(function(x){x*2*x^3}, n=100, a=0, b=1)
EY<- simpson1(function(x){x*(2*x-2*x^3)}, n=100, a=0, b=1)
EXY<- lipat2(function(x){(4*x^2)},function(y){y^2}, n=100, m=100, a=0, b=1, c=0, d=1)
EX2<- simpson1(function(x){x^2*2*x^3}, n=100, a=0, b=1)
EY2<- simpson1(function(x){x^2*(2*x-2*x^3)}, n=100, a=0, b=1)

varx = EX2 - (EX)^2
varx

vary = EY2 - (EY)^2
vary

covxy = EXY - EX - EY
covxy


#-----------------------------------KUISSSSSSSSSSSSSS----------------------------


#Integral Lipat 2

lipat2 <- function(fx,fy,n,m,a,b,c,d)
{
  h <- (b-a)/n
  k <- (d-c)/m
  y = c
  hasil1 = fy(c)+fy(d)
  for ( j in 1:(m-1)){
    y = y+k
    if(j%%2 != 0){
      hasil1 = hasil1+4*fy(y)
    }else{
      hasil1 = hasil1+2*fy(y)
    }
  }
  x = a
  hasil2 = fx(a) + fx(b)
  for ( i in 1:(n-1)){
    x = x+h
    if(i%%2 != 0){
      hasil2 = hasil2+4*fx(x)
    }else{
      hasil2 = hasil2+2*fx(x)
    }
  }
  hasil = hasil1*hasil2*h*k/9
  print(hasil)
}
lipat2(function(x){(4*x)},function(y){y}, n=100, m=100, a=0, b=1, c=0, d=1)
#EX
lipat2(function(x){(x*4*x)},function(y){y}, n=100, m=100, a=0, b=1, c=0, d=1)
#EY
lipat2(function(x){(4*x)},function(y){y^2}, n=100, m=100, a=0, b=1, c=0, d=1)
#EXY
lipat2(function(x){(x*4*x)},function(y){y*y}, n=100, m=100, a=0, b=1, c=0, d=1)

#PELUANG
lipat2(function(x){(4*x)},function(y){y}, n=100, m=100, a=0, b=0.5, c=0.25, d=0.5)
