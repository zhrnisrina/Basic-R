
findiff <- function(f,x,h, method = NULL){
  if(is.null(method)){
    warning("please select a method")
    }else{
      if(method=="forward"){
        return((f(x+h)-f(x))/h)
      }else if(method=="backward"){
        return((f(x)-f(x-h))/h)
      }else if(method=="central"){
        return((f(x+h)-f(x-h))/(2*h))
    }
  }
}

#Soal 1
soal1 = findiff(function(x)exp(x)*sin(2*x)+1, x=2, h=0.5, method = "central")
print(soal1)

#Soal 2
soal2 = findiff(function(x)1/(1+x^2), x=3, h=0.05, method = "forward")
print(soal2)


#Nilai turunan sebenarnya

#Soal 1
f <- function(x){exp(-2*x)-x}
x <- 2
h <- 0.05
xvec <- seq(x-h, x+h, h)

#turunan pertama
asli = diff(f(xvec), lag=2)/(2*0.5)
asli

#error
asli = 1.043049
error = ((abs(asli-soal1))/abs(asli))*100
error

