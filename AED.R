
getwd()
setwd("C:/users/HP/OneDrive - Institut Teknologi Sepuluh Nopember/SEMESTER 4/AED/Praktikum")# <- ngeset tempat ngesavenya 

list.files()

# isi dari fungsi = argument

df = read.table("01 Sample csv data no column.csv" ) # <- masukkin file yang mau dibacaa 

df2 = read.table(file = "01 Sample csv data no column.csv")
# masi eroer df2 = read.table(header=TRUE)

data = read.csv("01 Sample csv data no column.csv")

barplot(GNP ~ Year, data = longley)

require(grDevices)
pie(rep(1, 24), col = rainbow(24), radius = 0.9)

pie.olympic <- c(16/32,6/32,3/32,2/32,1/32)
names(pie.olympic) <- c("Europe", "North America",
                      "Asia", "Australia", "South America")
pie(pie.olympic)


pie.edu <- c(12.1/100, 30.5/100, 19.5/100, 8.7/100, 22.6/100, 6.6/100)
names(pie.edu) <- c("Not HS grad", "HS grad",
                        "some college", "Associate", "Bachelor's", "Advanced")
pie(pie.edu)

boxplot(c(108, 96, 96, 84, 108, 240, 84))

#-------------------------------------------------------------------------------

df = read.table('02 Most Followed Instagram Account Clean Data.csv', sep = ',',
                header = TRUE, quote = '"', check.names=FALSE)
head(df)

hist(df$'Followers (M)', main = 'Distribusi Followers Top 100 Account di Instagram',
     xlab = 'Jumlah Followers (Juta)')

hist(df$'Engagement Rate',
     main = 'Distribusi Engagement Rate Top 100 Account di Instagram',
     xlab = 'Engagement Rate (Persen)')

mahasiswa_df = read.table('02 Asal Daerah Mahasiswa.csv', sep = ',',
                          header = TRUE, col.names = c('Pulau', 'Jumlah Mahasiswa'),
                          check.names = FALSE)
head(mahasiswa_df)

pie(mahasiswa_df$'Jumlah Mahasiswa')

pie(mahasiswa_df$"Jumlah Mahasiswa", main = 'Pie Chart Asal Daerah Mahasiswa',
    labels = mahasiswa_df$"Pulau",radius = 0.85,clockwise = TRUE,
    col = c("red","orange","yellow","blue"))

persentase <- round(mahasiswa_df$`Jumlah Mahasiswa`/sum(mahasiswa_df$`Jumlah Mahasiswa`)*100, 2)

pie(mahasiswa_df$`Jumlah Mahasiswa`, main = 'Pie Chart Asal Daerah Mahasiswa (%)',
    labels = paste(persentase, '%', sep=""), radius = 0.85, clockwise = TRUE, 
    col = c("red","orange","yellow","blue"))
legend('bottomleft', 
       legend = mahasiswa_df$Pulau,
       fill = c("red","orange","yellow","blue"))

pie(mahasiswa_df$'Jumlah Mahasiswa',
    main = 'Pie Chart Asal Daerah Mahasiswa',
    labels = paste(mahasiswa_df$'Jumlah Mahasiswa', 'Mahasiswa', sep = ' '),
    radius = 0.85,
    clockwise = TRUE,
    col = c("#264653","#2a9d8f","#e9c46a","#f4a261"))
legend('bottomleft',
       legend = mahasiswa_df$Pulau,
       fill = c("#264653","#2a9d8f","#e9c46a","#f4a261"))

stem(as.integer(df$'Followers'))

bar = barplot(height = mahasiswa_df$`Jumlah Mahasiswa`,
              names = mahasiswa_df$'Pulau',
              xlab = 'Asal Daerah',
              col = c("#264653","#2a9d8f","#e9c46a","#f4a261"),
              border = FALSE,
              ylim = range(0, 50))

text(x = bar,
     y = mahasiswa_df$`Jumlah Mahasiswa`+3,
     labels = mahasiswa_df$`Jumlah Mahasiswa`,
)

#----------------------------------------------------------------------------------------------


install.packages('gapminder')
library(gapminder)
gapminder

gapminder_2007 = gapminder[gapminder$year == 2007, ]
head(gapminder_2007)

gapminder_indonesia = gapminder[gapminder$country == "Indonesia", ]
gapminder_indonesia

x = gapminder_indonesia$year
y = gapminder_indonesia$pop
plot(x, y, type="b")

gapminder_indonesia["Populasi (Juta)"] = gapminder_indonesia["pop"]/1000000
# gapminder_indonesia$year =  as.factor(gapminder_indonesia$year)
gapminder_indonesia

gapminder_malaysia = gapminder[gapminder$country == "Malaysia", ]
gapminder_malaysia

x = gapminder_indonesia$year
y1 = gapminder_indonesia$gdpPercap
y2 = gapminder_malaysia$gdpPercap

plot(x, y1, type="b", col = "red", ylim = c(1,13000), xaxt='n', xlab="Tahun", ylab="GDP per Cap")
lines(x, y2, col = "blue", type = "b")
axis(1, at = seq(1952, 2007, by=5), labels=as.character(x), cex.axis = 0.8)
legend("topleft", legend=c("Indonesia", "Malaysia"),
       col=c("red", "blue"), lty = 1:1, cex=1.2)


#---------------------------------------------------------------------------------------------------------------

View(ToothGrowth)
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
str(ToothGrowth$dose)

install.packages("ggplot2")
install.packages("RColorBrewer")

library(ggplot2)

dev.off()      
p <- ggplot(ToothGrowth, aes(x=dose, y=len)) + geom_violin()
p

p + coord_flip

ggplot(ToothGrowth, aes(x=dose, y=len)) + geom_violin(trim=FALSE)


ggplot(mtcars, aes(x=drat, y=mpg)) + geom_point()

View(midwest)

options(scipen=999)
library(ggplot2)
theme_set(theme_bw())
data("midwest", package = "ggplot2")

gg <- ggplot (midwest, aes(x=area, y=poptotal)) + 
    geom_point(aes(col = state, size = popdensity)) +
    geom_smooth(method="loess", se = F) +
    xlim(c(0, 0.1)) +
    ylim(c(0, 500000)) + 
    labs(subtitle = "Area Vs Population",
         y = "Population", 
         x = "Area", 
         title = "Scatterplot", 
         caption = "Source : midwest")
plot(gg)


#------------------------------------------------------------------------


install.packages("ggplot2") # Main plot library
install.packages("ggExtra") # Marginal Plot
install.packages("vioplot") # Violin Plot
install.packages("dplyr") # Data Preparation
install.packages("ggthemes") # Additional theme
install.packages("ggdark") # Additional dark theme
install.packages("gridExtra") # Add grid
devtools::install_github("psyteacher/introdataviz")

library("ggplot2")
library("ggExtra")
library("vioplot")
library("dplyr")
library("ggthemes")
library("ggdark")
library("gridExtra")

data <- read.csv("heart.csv")
data
summary(data)


data <- data %>% mutate(Sex = ifelse(Sex == "F", "Female", "Male"))
data <- data %>% mutate(HeartDisease = ifelse(HeartDisease == "0", "None", "Heart Disease"))
data <- data %>% mutate(ExerciseAngina = ifelse(ExerciseAngina == "N", "No", "Yes"))
summary(data)

df <- within(data,{
    Sex <- factor(Sex)
    ChestPainType <- factor(ChestPainType)
    FastingBS <- factor(FastingBS)
    RestingECG <- factor(RestingECG)
    ExerciseAngina <- factor(ExerciseAngina)
    ST_Slope <- factor(ST_Slope)
    HeartDisease <- factor(HeartDisease)
})
summary(df)

p1 <- ggplot(df, aes(x=ExerciseAngina,y=Cholesterol))+geom_violin()
p1

#fill warna berdasarkan ExerciseAngina
p2 <- ggplot(df, aes(x=ExerciseAngina,y=Cholesterol, fill=ExerciseAngina)) + geom_violin() +
    labs(title="Violin Plot of Cholesterol", 
         subtitle="Based on Angina Experience",
         x="Angina Experience", y="Cholesterol") + scale_fill_manual(values=c("#FFA392", "#8582DD"))
p2

p3 <- ggplot(df, aes(x=ExerciseAngina,y=Cholesterol, fill=ExerciseAngina)) + geom_violin() +
    labs(title="Violin Plot of Cholesterol", 
         subtitle="Based on Angina Experience",
         x="Angina Experience", y="Cholesterol") + scale_fill_manual(values=c("#FFA392", "#8582DD")) +
    geom_boxplot(width=0.1, fill="white")
p3

#jitter buat liat densitynya 
p4 <- ggplot(df, aes(x=ExerciseAngina,y=Cholesterol, col=ExerciseAngina)) + geom_violin() +
    labs(title="Violin Plot of Cholesterol", subtitle="Based on Angina Experience",
         x="Angina Experience", y="Cholesterol") + scale_fill_manual(values=c("#FFA392", "#8582DD")) +
    geom_jitter(aes(ExerciseAngina), position=position_jitterdodge())
p4

p5 <- ggplot(df, aes(x=ExerciseAngina,y=Cholesterol, fill=ExerciseAngina)) + geom_violin() +
    labs(title="Violin Plot of Cholesterol", subtitle="Based on Angina Experience",
         x="Angina Experience", y="Cholesterol") + scale_fill_manual(values=c("#FFA392", "#8582DD")) +
    geom_boxplot(width=0.05, fill="white")+ facet_grid(cols=vars(Sex)) + theme_light()
p5


p7 <- ggplot(df, aes(x=ExerciseAngina,y=Cholesterol, fill=ExerciseAngina))+geom_violin()+
    labs(title="Violin Plot of Cholesterol", subtitle="Based on Angina Experience",
         x="Angina Experience", y="Cholesterol", caption="Source : [https://www.kaggle.com](https://www.kaggle.com/)") +
    scale_fill_manual(values=c("#EC5E68", "#805A93")) +
    geom_boxplot(width=0.05, fill="white", outlier.shape = NA) + facet_grid(cols=vars(Sex)) + 
    dark_theme_gray() + theme(axis.title = element_text(size=10),
                              plot.title = element_text(face="bold", size=15),
                              plot.subtitle = element_text(face="italic", size=10),
                              plot.caption = element_text(face="italic", size=8))
p7


p8 <- ggplot(df, aes(x=ExerciseAngina,y=Cholesterol, fill=Sex)) +
    introdataviz::geom_split_violin(alpha=0.6, trim=F) + geom_boxplot(width=0.3, outlier.shape = NA) +
    labs(title="Violin Plot of Cholesterol", subtitle="Based on Angina Experience",
         x="Angina Experience", y="Cholesterol", caption="Source : [https://www.kaggle.com](https://www.kaggle.com/)") +
    scale_fill_manual(values=c("#FDFB7D", "#54BD92")) + dark_theme_gray() +
    stat_summary(fun=mean, geom="point", position=position_dodge(0.3)) +
    theme(axis.title = element_text(size=10),
          plot.title = element_text(face="bold", size=15),
          plot.subtitle = element_text(face="italic", size=10),
          plot.caption = element_text(face="italic", size=8))

p8

install.packages("devtools")

df <- df %>% filter(Cholesterol > 0)

q1 <- ggplot(df,aes(x=Cholesterol, y=MaxHR)) + geom_point(col="black") + theme_bw()
q2 <- ggMarginal(q1, type="density")
q2

q3 <- ggplot(df,aes(x=Cholesterol, y=MaxHR, color=Sex)) + geom_point(alpha=0.3) + theme_bw() +
    labs(title="Marginal Plot of Cholesterol and Heart Rate", x="Cholesterol", 
         y="Max Heart rate", caption="Source : https://www.kaggle.com")
q4 <- ggMarginal(q3, type="histogram")
q4

q5 <- ggplot(df,aes(x=Cholesterol, y=MaxHR, color=Sex)) + geom_point(alpha=0.3) +
    theme_bw() + labs(title="Marginal Plot of Cholesterol and Heart Rate", x="Cholesterol",
                      y="Max Heart rate",   caption="Source : https://www.kaggle.com")
q6 <- ggMarginal(q5, type="boxplot", col="black", fill="gray")
q6

q7 <- ggplot(df,aes(x=Cholesterol, y=MaxHR, color=Sex)) + scale_color_manual(values=c("#F78132", "#BF568B"))+
    geom_point(alpha=0.3, size=4) + labs(title="Marginal Plot of Cholesterol and Heart Rate", 
                                         x="Cholesterol", y="Max Heart rate",
                                         caption="Source : https://www.kaggle.com")  + 
    theme_minimal() + theme(axis.title = element_text(size=10, colour = "#BF568B"),
                            plot.title = element_text(face="bold", size=15, colour = "#BF568B"),
                            plot.caption = element_text(face="italic", size=8),
                            legend.position = "bottom")
q8 <- ggMarginal(q7, type="densigram", col="black", fill="#F78132")
q8

grid.arrange(p8,q8)


df = ToothGrowth
print(head(df))
print(summary(df))
print(nrow(df))

df$dose = as.factor(df$dose)

p = ggplot(df, aes(x=dose, y=len)) +
    geom_violin() ; p

# Dijadikan horizontal
p + coord_flip()

#tidak memotong ujung violin plot
p = ggplot(df, aes(x=dose, y=len)) +
    geom_violin(trim=FALSE)
p

# Hanya memilih beberapa kategori
#p + scale_x_discrete(limits=c("0.5", "2")) 

# Mengubah urutan variabel kategori
p + scale_x_discrete(limits=c("2","0.5","1"))


#----------------------------------------------------------------

install.packages("psych")
install.packages("DescTools")
install.packages("vcd")

data1 <- data.frame(
    "Male" = c(68,32),
    "FeMale" = c(23,57),
    row.names = c("Urban","Suburban"),
    stringsAsFactors = F
)
data1

prop.table(data1)
a <- chisq.test(data1, correct=F) ;a
b <- a $expected ;b

# Pembuktian Manual
selisih <- (data1-b)^2/b
selisih
chisq_manual <- sum(selisih)
chisq_manual
p_val <- pchisq(chisq_manual, df=1, lower.tail=F)
p_val

# CramerV
library("DescTools")
CramerV(data1)

# AssocsStats
library("vcd")
assocstats(data1)

#From data
df <- mtcars
df_tab <- xtabs(~df$am + df$vs)
df_tab

library("psych")
phi(df_tab)
CramerV(df_tab)
assocstats(df_tab)


#--------------------------------------------------------------------

library(MASS)
df <- ChickWeight
summary(df)

#index brp yg time nya = 12
index12 <- which(df$Time == 12) ; index12
data <- df[index12, ] ; data
wts12 <- data$weight

#Histogram
par(mfrow=c(1,2))
hist(wts12, main="From Hist()", 
     xlab="Chick Weight at 12 days")
truehist(wts12, main="From truehist()",
         xlab="Chick Weight at 12 days")

dev.off()


truehist(wts12, main = "From truehist()",
         xlab = "Chick Weight at 12 days")
mu <- mean(wts12)
sigma <- sd(wts12)
x <- seq(0,250,2)
px <- dnorm(x, mean=mu, sd=sigma)
lines(density(wts12))
lines(x,px, type="p")

library(car)
qqPlot(wts12, main="qqPlot of WTS12") # Data without outlier
qqPlot(UScereal$fibre, main="qqPlot of Fibre") # Data with outlier

data <- UScereal; data
data_no_outlier <- data$fibre[-c(1:3)]
qqPlot(data_no_outlier)

#Three-Sigma Rules
mean <- mean(data$potassium)
sd <- sd(data$potassium)
plot(1:nrow(data),data$potassium, main="Three Sigma Rules",
     xlab = "Cereal, k", ylab = "Potassium concentration per serving",
     ylim=c(0,1000), cex.axis=0.8)
abline(h=mean, lty=2)
abline(h=mean+3*sd, lty=3)

#Hampel Identifier
median <- median(data$potassium)
mad <- mad(data$potassium) # Median Absolute Deviation
mad1 <- 1.4826*median(abs(data$potassium-median)) # Manual Median Absolute Deviation
plot(1:nrow(data),data$potassium, main="Hampel Identifier",
     xlab = "Cereal, k", ylab = "Potassium concentration per serving",
     ylim=c(0,1000), cex.axis=0.8)
abline(h=median, lty=2)
abline(h=median+3*mad, lty=3)

#Manual Function Using Winsorize
quantile <- quantile(data$potassium, c(0.25, 0.5, 0.75)); quantile
Q1 <- quantile[1]; Q1
Q2 <- quantile[2]; Q2
Q3 <- quantile[3]; Q3
IQR <- Q3-Q1; IQR
plot(1:nrow(data),data$potassium, main="Winsorize",
     xlab = "Cereal, k", ylab = "Potassium concentration per serving",
     ylim=c(0,1000), cex.axis=0.8)
abline(h=Q2, lty=2)
abline(h=Q2+1.5*IQR, lty=3)

#BoxPlot
detect_outlier <- function(x){
    n <- length(x)
    Q1 <- quantile(x, 0.25)
    Q2 <- quantile(x, 0.5)
    Q3 <- quantile(x, 0.75)
    IQR <- Q3-Q1
    LB <- Q2-1.5*IQR
    UB <- Q2+1.5*IQR
    lower <- c()
    upper <- c()
    for(i in 1:n){
        ifelse(x[i] > UB, upper <- append(upper, i),
               ifelse(x[i] < LB, lower <- append(lower,i), next))
    }
    return(list("lower"=lower, "upper"=upper))
}
result <- detect_outlier(data$potassium); result
clean_data1 <- data$potassium
clean_data1[result$upper] <- quantile(clean_data1, 0.75)
clean_data1[result$lower] <- quantile(clean_data1, 0.25)
plot(1:length(clean_data1),clean_data1, main="Plot of Potassium",
     xlab = "Cereal, k", ylab = "Potassium concentration per serving",
     ylim=c(0,1000), cex.axis=0.5)
abline(h=Q2, lty=2)
abline(h=Q2+1.5*IQR, lty=3)


#COMPARE
par(mfrow=c(1,3))
# Before
quantile <- quantile(data$potassium, c(0.25, 0.5, 0.75)); quantile
Q1 <- quantile[1]; Q1
Q2 <- quantile[2]; Q2
Q3 <- quantile[3]; Q3
IQR <- Q3-Q1; IQR
plot(1:nrow(data),data$potassium, main="Plot of Potassium",
     xlab = "Cereal, k", ylab = "Potassium concentration per serving",
     ylim=c(0,1000), cex.axis=0.5)
abline(h=Q2, lty=2)
abline(h=Q2+1.5*IQR, lty=3)


# After Trimmed
plot(1:length(clean_data),clean_data, main="Plot of Potassium After Trimmed",
     xlab = "Cereal, k", ylab = "Potassium concentration per serving",
     ylim=c(0,1000), cex.axis=0.5)
abline(h=Q2, lty=2)
abline(h=Q2+1.5*IQR, lty=3)

dev.off()

#HANDLING MISSING VALUES

#Generating Data
A1 <- sample(1:60, size=200, replace=T); A1
a1 <- sample(1:200, size=sample(1:20, size=1)); a1
A1[a1] <- NA
A1
hist(A1)
ks.test(A1, "pnorm")

#Check Missing Value
is.na(A1)
sum(is.na(A1))
percent_NA <- sum(is.na(A1))/length(A1); percent_NA

#Delete Missing Value
no_NA <- na.omit(A1); no_NA
length(no_NA)


#IMPUTING WITH MEAN
A2 <- A1; A2
A2[is.na(A2)] <- mean(A2, na.rm=T)
A2
mean(A1, na.rm=T)
mean(A2)

#IMPUTING WITH MEDIAN
A2 <- A1; A2
A2[is.na(A2)] <- median(A2, na.rm=T)
A2
median(A1, na.rm=T)
median(A2)

#IMPUTING WITH RANDOM VALUES
# Mengganti dengan angka random
A2 <- A1; A2
A2[is.na(A2)] <- sample(min(A2, na.rm=T):max(A2, na.rm=T), size=length(A2[is.na(A2)]))
A2
median(A1, na.rm=T)
median(A2)
mean(A1, na.rm=T)
mean(A
     
     
#------------------------------------------------------------------------------------------ 

library(car)
library(MASS)
install.packages("mlbench")     
library(mlbench)     

df <- PimaIndiansDiabetes
head(df)
summary(df)
is.na(df)

df2 <- PimaIndiansDiabetes2
head(df2)
summary(df)

qqPlot(df$pressure, main = "qqPlot pressure data 1")
qqPlot(df2$pressure, main = "qqPlot pressure data 1")
is.na(df2)

library(dplyr)
#Mengubah 0 menjadi NA
na_if(df, 0)
df[df==0] <- NA
df
head(df)
summary(df)
is.na(df)

pressure1 <- df$pressure
pressure1
qqPlot(df$pressure, main = "qqPlot pressure data 1")
par(mfrow = c(1,2))

dev.off()

df3 <- Cars93
summary(df3)
qqPlot(df3$Price, main = "qqPlot Price data Cars ")


truehist(df3$Price, main="From truehist()")
mu <- mean(df3$Price)
sigma <- sd(df3$Price)
x <- seq(0,250,2)
px <- dnorm(x, mean=mu, sd=sigma)
lines(density(df3$Price))
lines(x,px, type="p")

df <- df3$Price[-c(40:60)]
qqPlot(df)

#-------------------------------------------------------------------------------------


data <- mtcars
summary(data)
attach(data)
str(cyl)
str(gear)
str(carb)

cyl <- as.ordered(cyl)
gear <- as.ordered(gear)
carb <- as.ordered(carb)

str(vs)

vs <- factor(vs, labels = c("V", "S"))
am <- factor(am, labels = c("automatic", "manual"))

library(dplyr)
data <- data %>% mutate(vs = ifelse(vs == 0, "V", "S"))
data <- data %>% mutate(am = ifelse(am == 0, "automatic", "manual"))

str(vs)
str(am)
