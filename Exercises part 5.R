n = 100
x = rnorm(n, mean = 0, sd = 2)
y = array(0, n+1)

for(i in (1:n)){
  if(x[i]>=0){
    y[i+1] = y[i]+x[i]
  
  }else if(x[i] <= -1){
    y[i+1] = y[i]-x[i]
    
  }else{
    y[i+1] = y[i]-2*x[i]
  }
}

y
plot(y)

tapply(iris$Sepal.Length,iris$Species,max)
tapply(iris$Sepal.Length,iris$Species,min)

delNAfun = function(data){
  data = data[-which(apply(data,1,function(x) any(is.na(x)))), ]
}

install.packages("tcltk")
library(tcltk)
tkmessageBox(message="Have you finished all the exercises?",
             icon = "question", type = "yesno", default = "yes")

name <- c("Sue", "Eva", "Henry", "Jan")
sex <- c("f", "f", "m", "m")
years <- c(21,31,29,19)

df <- data.frame(name, sex, years)

df$male.teen = ifelse(df$sex == "n" &
                        df$years <20, "T", "F")

df

for(i in 1:4){
  for(j in 1:10){
    print(j+i-1)
  }
}

data = matrix(ncol = 10, nrow = 100)
randomr <- rnorm(10)

reps <- 1
