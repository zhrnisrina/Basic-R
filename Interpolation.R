#INTERPOLASI LINIER

newton <- function(xx,x,y)
{
  n=length(x)
  if (length(y)!=n)
  {warning("x and y must be the same length")}
  y1=y[1]+((y[2]-y[1])/(x[2]-x[1])*(xx-x[1]))
}
yest=newton(2,c(1,6),c(0,1.791759))
yest
yfact=0.693147
error=(abs(yfact-yest)/yfact)*100
error

#General Form of Newton's Interpolating Polynomials

newton_interpolation <- function(x,y,xx)
{
  n=length(x)
  if(length(y)!=n)
  {warning('x and y must be the same length')}
  b <- matrix (0, nrow = n, ncol = n)
  b[,1] = y

  for(j in 2:n){
    for(i in 1:(n-j+1)){
      b[i,j] = (b[i+1,j-1]-b[i,j-1])/(x[i+j-1]-x[i])
    }
  }
  xt <- 1
  yint = b[1,1]
  for(j in 1:(n-1)){
    xt = xt*(xx-x[j])
    yint = yint + b[1,j+1]*xt
  }
  print(b)
  print(xt)
  print(yint)
}
yest <- newton_interpolation(xx=2, x=c(1,4,5,6), y=c(0,1.386294,1.609438,1.791759))
yest
yfact=0.6931471806
error=(abs(yest-yfact)/yfact)*100
error

#LANGRANGE 

x = c(8,12)
y = c(0.90309, 1.079181)
yreal = 1
Lagrange <- function(x,y,xx)
{
  n = length(x)
  if(length(y) != n)
  {
    warning("panjang x dan y harus sama !!")
  }
  yint = 0
  for(i in 1 :n){
    p = y[i]
    for(j in 1:n){
      if(i != j){
        p = p*((xx-x[j])/(x[i]-x[j]))
      }
    }
    yint = yint+p
  }
  print(yint)
}
Lagrange(x,y,10)
yest <- Lagrange(x,y,10)
error = abs(yest-yreal)/yreal*100
error

#LINEAR SPLINE

linear_spline <- function (x,y,xx)
{
  n = length(x)
  if(xx<x[1]|xx>x[n])
  {error('out of x range !')
  }
  for(i in 1 : (n-1))
  {
    if(xx>=x[i] && xx<=x[i+1])
    {
      m=i
    }
  }
  f=y[m]+((y[m+1]-y[m])/(x[m+1]-x[m])*(xx-x[m]))
  print(f)
}
x <- c(3,4.5,7,9)
y <- c(2.5,1,2.5,0.5)
linear_spline(x,y,5)


#QUADRATIC SPLINE

#matriks
library(matlib)
A <- matrix (c(1.5,0,0,0,0,
               0,2.5,0,6.25,0,
               0,0,2,0,4,
               1,-1,0,0,0,
               0,1,-1,5,0), nrow=5, ncol=5, byrow = T) ; A
b <- c(-1.5, 1.5, -2, 0,0) ; b
Solve(A,b)

#kuadratik

kuadratik_spline <- function (x,y,xx)
{
  n = length(x)
  if(xx<x[1]|xx>x[n])
  {
    error('out of x range!')
  }
  for( i in 1 : (n-1))
  {
    if(xx>x[i] && xx<x[i+1])
    {
      print(i)
    }
    s=y[i]+b[i]*(xx-x[i])+c[i]*(xx-x[i])^2
    print(s)
  }
}
x <- c(3,4.5,7,9)
y <- c(2.5,1,2.5,0.5)
b <- c(-1,-1,2.2)
c <- c(0,0.64,-1.6)

kuadratik_spline(x,y,5)

#------------------------------------------------------------------------


#TUGAS 3

#LINEAR
newton <- function(xx,x,y)
{
  n=length(x)
  if (length(y)!=n)
  {warning("x and y must be the same length")}
  y1=y[1]+((y[2]-y[1])/(x[2]-x[1])*(xx-x[1]))
}
yest=newton(4,c(0,5),c(0,0.0337))
yest
yfact=0.073262556
error=(abs(yfact-yest)/yfact)*100
error


#KUADRATIK
newton_interpolation <- function(x,y,xx)
{
  n=length(x)
  if(length(y)!=n)
  {warning('x and y must be the same length')}
  b <- matrix (0, nrow = n, ncol = n)
  b[,1] = y
  
  for(j in 2:n){
    for(i in 1:(n-j+1)){
      b[i,j] = (b[i+1,j-1]-b[i,j-1])/(x[i+j-1]-x[i])
    }
  }
  xt <- 1
  yint = b[1,1]
  for(j in 1:(n-1)){
    xt = xt*(xx-x[j])
    yint = yint + b[1,j+1]*xt
  }
  print(b)
  print(xt)
  print(yint)
}
yest <- newton_interpolation(xx=4, x=c(0,5,6), y=c(0,0.033689735,0.014872513))
yest
yfact=0.073262556
error=(abs(yest-yfact)/yfact)*100
error


#KUBIK
newton_interpolation <- function(x,y,xx)
{
  n=length(x)
  if(length(y)!=n)
  {warning('x and y must be the same length')}
  b <- matrix (0, nrow = n, ncol = n)
  b[,1] = y
  
  for(j in 2:n){
    for(i in 1:(n-j+1)){
      b[i,j] = (b[i+1,j-1]-b[i,j-1])/(x[i+j-1]-x[i])
    }
  }
  xt <- 1
  yint = b[1,1]
  for(j in 1:(n-1)){
    xt = xt*(xx-x[j])
    yint = yint + b[1,j+1]*xt
  }
  print(b)
  print(xt)
  print(yint)
}
yest <- newton_interpolation(xx=4, x=c(0,2,5,6), y=c(0,0.270670566,0.033689735,0.014872513))
yest
yfact=0.073262556
error=(abs(yest-yfact)/yfact)*100
error
