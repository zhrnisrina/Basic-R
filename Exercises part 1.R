
#1
a = "slumdog"
b = "millionaire"

c = paste(a,b) ;c
d = unlist(strsplit(c, " "))
d

#2
s <- c(1,3,5,7,11,13,17,19)
a = t(s)
a

#3
b <- c(2^(1:8))
b

#4
c <- c(1:8)^2
c

#5
which(b==c)

#6
M.c <- cbind(b,c)
M.c

#7
M.r <- rbind(a,b)
colnames(M.r) <- c("S","T","U","V","W","X","Y","Z")
M.r

#8
M.r <- M.r[,-5]
M.r


#9
M.r[M.r >12]

#10
y = exp(-x)
x = seq(-3,3,0.5)
y

#11
d <- seq(1,100)
d
e <- rep(7,100)
e

#12
D <- matrix(1:100, ncol = 10) ;D
o <- c(1/(1:100)) ; o
E <- matrix(o, ncol = 10, byrow =T) 
E

#13
sum = D+E ; sum
dif = D-E ; dif 
P = D%*%E ; P

#14
diag(P)

#15
for (x in 1:10){
  y1 = sum(x^(5:0))
  print(y1)
}
for (x in 1:10){
  y2 = (1+x*(1+x*(1+x*(1+x*(1+x)))))
  print(y2)
}

#16
a = seq(0, 2*pi, 0.5)
b <- sin(a)
c <- cos(a)
d <- tan(a)

cal = b/c-d 
cal

#17
prices <- c(2,3,5,3,2,5,7,4,2,5)
diff(log(prices))

a <- length(prices)
for (i in (1:a-1)){
  print(prices[i+1]-prices[i])
}


#19
x = b[b>0];x
y = b[b<0];y
length(x)/length(y)


#23
B <- matrix(c(1,1,1,1,2,3,1,3,6),3,3)
B
C =matrix(1:9, nrow = 3, ncol = 3)
C
D=B
B[upper.tri(B, diag = F)] = C[upper.tri(C, diag =F)]
B
C[upper.tri(C, diag = F)] = C[upper.tri(D, diag =F)]
C


B[1,2]<- 4
B[1,3]<- 7
B[2,1]<- 2
B[3,1]<- 3
B[2,2]<- 5
B[2,3]<- 8
B[3,2]<- 6
B[3,3]<- 9
C = B*1
C 
  
  
#18

a <- c(1,1,1,1,1,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,6,6,6,6,6)
which(diff(a)!=0)

#20
A<-matrix(c(-1.00, 3.71, 2.80, 0.01, 1.19,0.40, -1.80, -1.96, 1.84, 1.74,-4.30, 1.71, 0.68, 0.11, 3.44, 0.03, 3.9, 0.41, 0.02, 1.05, 0.24, -0.01, 2.10, 2.87, -3.57),5,5)

#21
det(A)
#22
Ai = solve(A)
A%*%Ai




