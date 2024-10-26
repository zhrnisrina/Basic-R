# graphic metode 
#cara1

graphic <- function(f1, f2,x)
{
  f1 = f1(x)
  f2 = f2(x)
  my.frame = data.frame(x, f1, f2)
  print(my.frame)
  plot(x,f1,type = "l")
  lines(x,f2,type = "l")
}
graphic(function(x){0.5*x+3}, function(x){(34-x)/6},1:10)

#cara2

f1 = function(x){0.5*x+3}
f2 = function(x){(34-x)/6}

graphic <- function(f1, f2,x)
{
  f1 = f1(x)
  f2 = f2(x)
  my.frame = data.frame(x, f1, f2)
  print(my.frame)
  plot(x,f1,type = "l")
  lines(x,f2,type = "l")
}
graphic(f1,f2,1:10)

#MATRIKS
#berdasarkan kolom
m1 <- matrix(data = c(1,3,4,6), nrow = 2, ncol = 2 )
m1
#berdasarkan baris
m1 <- matrix(data = c(1:4), nrow = 2, ncol = 2, byrow = TRUE)
m1

ms<- matrix(data = 1, nrow = 5, ncol = 5)
ms

#Mengubah Data Menjadi Matriks

a <- c(5:8)
a
matrix(data=a, nrow=2, ncol=2)
matrix(a,2,2)

#Merubah nama baris dan kolom pada matriks

m2 <- matrix(data = a, nrow = 2, ncol = 2,
             dimnames = list(c("row1", "row2"),c("col1","col2")))
m2

#Operasi Matriks

#Penjumlahan
m1+m2
#Pengurangan
m1-m2

#Perkalian indeks
m1*m2
#Perkalian Matriks
m1%*%m2

#Transpose Matriks
b = c(1:10)
m5 = matrix(data = b, nrow = 10)
m5
t(m5)
t(b)

#Inverse Matriks
m1 <- matrix(c(1:4),2,2)
solve(m1)

#Determinan matriks
det(m1)

A <- matrix(c(1,3,5,7,2,4,6,8,2),3,3)
A
det(A)

#Matriks Diagonal
A <- matrix(c(1,3,5,7,2,4,6,8,2),3,3)
diag(A)
diag(diag(A))

#Matriks Identitas
md1 <- diag(c(1,1,1))
md1


#SPL using R

install.packages("matlib")
library(matlib)

A <- matrix(c(8,2,-2,10),2,2, byrow = TRUE)
A
b <- c(2,4)
b

showEqn(A,b) #membuat persamaan linier
Solve(A,b)   #menyelesaikan persamaan linear
plotEqn(A,b) #membuat plot penyelesaian, khusus matriks 3 x 3 

B <- matrix(c(0.3,0.52,1,0.5,1,1.9,0.1,0.3,0.5),3,3, byrow = TRUE)
B
c <- c(-0.01, 0.67, -0.44)

showEqn(B,c)
Solve(B,c)
plotEqn3d(B,c)

C <- matrix(c(2,3,5,4,2,4,4,3,3,5,6,4,6,6,2),4,4, byrow = TRUE)
C
d<- c(16,10,12,24)

showEqn(C,d)
Solve(C,d)

#-------------------------------------------------------------------------

#f1 = 2x1 + x2 = 6
#f2 = x1 - 2x2 = 8

#grafis

graphic <- function(f1, f2,x)
{
  f1 = f1(x)
  f2 = f2(x)
  my.frame = data.frame(x, f1, f2)
  print(my.frame)
  plot(x,f1,type = "l")
  lines(x,f2,type = "l")
}
graphic(function(x){6-2*x}, function(x){(8-x)/-2},2:5)

#matlib

library(matlib)

A <- matrix(c(2,1,1,-2),2,2, byrow = TRUE)
A
b <- c(6,8)
b

showEqn(A,b) #membuat persamaan linier
Solve(A,b)   #menyelesaikan persamaan linear


#ELIMINASI GAUSS
library(matlib)

A <- matrix (c(10,-5,2,-2,9,-2,-2,-3,10),3,3,byrow=TRUE)
A

b <- c(-21,25,13)

echelon(A,b,reduced=FALSE) #HANYA MENAMPILKAN HASIL
echelon(A,b,reduced=FALSE, verbose=TRUE) #DENGAN STEP
echelon(A,b,reduced=FALSE, verbose=TRUE, fraction=TRUE) #DALAM PECAHAN


#ELIMINASI GAUSS JORDAN

A <- matrix (c(10,-5,2,-2,9,-2,-2,-3,10),3,3,byrow=TRUE)
A

b <- c(-21,25,13)

echelon(A,b,reduced=T) #HANYA MENAMPILKAN HASIL
gaussianElimination(A,b) #LANGSUNG HASIL
gaussianElimination(A,b, verbose = T) #BIAR ADA STEP
gaussianElimination(A,b, verbose = T, fractions =T) #BENTUK PECAHAN


#using PRACMA

library(pracma)

<- matrix (c(10,-5,2,-2,9,-2,-2,-3,10),3,3,byrow=TRUE)
b <- c(-21,25,13) 
rref(cbind(A,b))

#cara manual 

gauss_jordan <- function(A, b)
{
  n <- nrow(A)
  c <- matrix(cbind(A, b), nrow = n)
  x <- matrix(rep(0,n),  ncol = 1)
  
  for(i in 1:(n-1)){
    for(j in (i+1) : n){
      pivot <- c[j,i]/c[i,i]
      for(k in 1 : (n+1)){
        c[j,k] <- c[j,k] - pivot*c[i,k]
      }
    }
  }
  
  for(i in n:2){
    for(k in (i-1):1){
      pivot <- c[k,i]/c[i,i]
      c[k,i] <- c[k,i] - pivot*c[i,i]
      c[k,(n+1)] <- c[k,(n+1)] - pivot*c[i,(n+1)]
    }
  }
  
  for(i in 1:n){
    x[i] <- c[i,(n+1)]/c[i,i]
    print(x[i])
  }
}
gauss_jordan(A, b)
