
Land <- c("Belgium", "Denmark", "France", "GB", "Irenland", 
          "Italy", "Luxembourg", "Holland", "Portugal", "Spain",
          "USA", "Japan", "Deutschland")
x <- c(2.8, 1.2, 2.1, 1.6, 1.5, 4.6, 3.6, 2.1, 
       6.5, 4.6, 3.0, 1.3, 4.2)
y <- c(9.4, 10.4, 10.8, 10.5, 18.4, 11.1, 2.6, 
       8.8, 5.0, 21.5, 6.7, 2.5, 5.6)

df <- data.frame(Land,x,y)
colnames(df) <- c("Land", "increase of the index(x)", "unemployment(y)")
df

#1
maxx <- c(max(df$"increase of the index(x)"),
          Land[which(x == max(df$"increase of the index(x)"))])
maxx
minx <- c(min(df$"increase of the index(x)"),
          Land[which(x == min(df$"increase of the index(x)"))])
minx
maxy <- c(max(df$"unemployment(y)"),
          Land[which(y == max(df$"unemployment(y)"))])
maxx
minx <- c(min(df$"unemployment(y)"),
          Land[which(y == min(df$"unemployment(y)"))])
minx

#2
range <- max(df$"increase of the index(x)")-min(df$"increase of the index(x)")
range

urut <- df[order(df$"increase of the index(x)"), ]
urut[13,2]-urut[1,2]

#3
urut.x <- df[order(df$"increase of the index(x)"), ]
datax <- urut.x[ ,2]
quantile(datax)

urut.y <- df[order(df$"unemployment(y)"), ]
datay <- urut.y[ ,2]
quantile(datay)

#4
median(datax)
median(datay)

med <- (length(datax)+1)/2 ; med
medx <- datax[7]
medx


#5
Q1 <- (length(datax)+1)/4 
Q1
Q3 <- (length(datax)+1)*3/4
Q3
difx <- Q3-Q1
difx


#6
mean(df$"increase of the index(x)")
meanx <- sum(datax)/length(datax)
meanx
mean(df$`unemployment(y)`)
meany <- mean(df$`unemployment(y)`)
meany


#7 
a <- sort(abs(datax[seq(1:13)]-medx))
median(a)

#8
var(datax)
var(datay)
varx <- sum((datax[seq(1:13)]-meanx)^2)/(length(datax)-1)
varx

#9
sqrt(varx)
stdx <- sqrt(sum((datax[seq(1:13)]-meanx)^2)/ (length(datax)-1)) 
stdx

#10
boxplot(df$"increase of the index(x)", 
        main = "Increase of the Index")
boxplot(df$"unemployment(y)", main =
          "Boxplot of unemployement")
boxplot(df[, -1], main ="Boxplot of Increase of the Index and Unemployment")

hist(df$"increase of the index(x)", main = 
       "Increase of the index", xlab = "index")
hist(df$"unemployment(y)", main = "unemployement",
     xlab = "count")

kdex = density(df$"increase of the index(x)",kernel ="gaussian")
plot(kdex)

kdey = density(df$"unemployment(y)", kernel = "gaussian")
plot(kdey)


#11
cov(df$"increase of the index(x)",df$"unemployment(y)")
cov(df[,2:3])

a = 0
for(i in (1:13)){
    a = a + (df$"increase of the index(x)"[i]*df$"unemployment(y)"[i])
}
a

cov <- ((1/(12))*a)-(13/12*meanx*meany)
cov
    
#12
cor(df$"increase of the index(x)",df$"unemployment(y)")
cor(df[,2:3])


#13
rankx = rank(df$"increase of the index(x)")
rankx

ranky = rank(df$"unemployment(y)")
ranky

#14
cor(rankx, ranky)


#15
#qnorm = nilai z tabel 
alpa = 0.05
n = 13
#varians diketahui
KIn = c(meanx - qnorm(1-alpa/2)*stdx/sqrt(n),
        meanx + qnorm(1-alpa/2)*stdx/sqrt(n))
KIn

#varians tidak diketahui
KI = c(meanx - qt(1-alpa/2, df=(n-1))*stdx/sqrt(n),
                  meanx + qt(1-alpa/2, df=(n-1))*stdx/sqrt(n))
KI


#16
#miu o = 3
T = (meanx-0)/stdx*sqrt(n)
T
t <- qt(1-alpa, df =(n-1))
KIt1 = meanx-(1/sqrt(n))*t
KIt1

t.test(df$`increase of the index(x)`, y =NULL, alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = TRUE)

#criticalvalue
cv = qt(0.95, (n-1))
cv

#kalo abs(T)<cv maka H0 gagal tolak

KI = c((mean(df$`increase of the index(x)`))-qt(1-alpa,df=(n-1))*sd(df$`increase of the index(x)`)/sqrt(n),Inf)
KI

#17

T1 = (meanx-meany)/((sqrt((n+n)/(n*n))*((n-1)*var(df$`increase of the index(x)`)+(n-1)*var(df$`increase of the index(x)`))/(n+n-2)))
T1

ttabel = qt(1-alpa/2, df =(n+n-2))
ttabel
-ttabel


#18

T2 = var(datax)/var(datay)

qf.vartest = c(qf(alpa/2, n-1,  n-1), qf(1-alpa/2, n-1,  n-1)) ; qf.vartest
var.test(df$`increase of the index(x)`,df$`unemployment(y)`)



