setwd("C:/Users/HP/OneDrive - Institut Teknologi Sepuluh Nopember/SEMESTER 4/Komputasi Statistika")

i = rep(seq(1,12),15) ; i
m = rep(month.name[1:12], 15) ; m
y = rep(2006:2020, each = 12) ; y
t = seq(1:(length(i))) ; t

data = data.frame(i, m, y, t) ; data
colnames(data) <- c("Index", "Month", "Year", "Time")
data

set.seed(158)

Yt = 100 + 10*t +50*sin((2*pi*t)/12) + rnorm(length(t), mean = 0, sd = 4)

Yt

data <- cbind(data, Yt) ; data

plot(data$Year, data$Yt, type = "l", col = "red",
     ylab = "Yt", xlab = "Time", lwd = "3", xaxt = "n", yaxt = "n")

abline(v=seq(2006,2020,1), h=seq(0,2000,100),
       lty="dotted",col="grey")
axis(1,seq(2006,2020),cex.axis=0.6)
axis(2,seq(0,2000,100),cex.axis=0.6, las = 1)

model <- lm(Yt ~ t - 1) #no intercept
summary(model)
anova(model)
model1 <- lm(Yt ~ t) #with intercept
summary(model1)
anova(model1)


world = read.csv("latian.csv") ; world

AA <- cbind(world$mlifeexp, world$flifeexp) ; AA
BB <- cbind(world$Babymort05, world$Babymort10) ; BB

CC <- AA%*%t(BB) ; CC

layout(matrix(c(1,2)))





