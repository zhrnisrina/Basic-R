library(readxl)

df <- read_xlsx("C:/Users/HP/OneDrive - Institut Teknologi Sepuluh Nopember/SEMESTER 6/Analisis Survival/Sebelum ETS/Data.xlsx")

install.packages("survival")
library(survival)
survdiff(formula = Surv(Time, Censor) ~ Treatment, data = df,
         rho = 0)

fit1 <- survfit(Surv(Time, Censor) ~ Treatment, data = df)
fit1

df <- read_xlsx("C:/Users/HP/OneDrive - Institut Teknologi Sepuluh Nopember/SEMESTER 6/Analisis Survival/Sebelum ETS/Data.xlsx", sheet = "Lembar3")
df

fit1 <- survfit(Surv(Time, Censor) ~ Treatment, data = df)
fit1

survdiff(formula = Surv(Time, Censor) ~ Treatment, data = df,
         rho = 0)

plot(fit1, col=1:2, lwd=2, 
     xlab="Kelangsungan Hidup (Hari)", ylab="Survival")
legend(2,0.2 , c("Obat Baru","Placebo"),
       col=1:2, lwd=2, bty='n')

