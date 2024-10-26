
#Second Order RK

second <- function(x0, y0, n, xn, f){
  h <- (xn-x0)/n
  x <- seq(x0, xn, by=h)
  y <- array(dim = length(x))
  y[1] = y0
  for (i in 1:n){
    k1 <- f(x[i], y[i])
    k2 <- f(x[i]+h, y[i]+k1)
    y[i+1] <- y[i]+0.5*(k1+k2)*h
  }
  h
}

fn <- function(x,y){exp(3*x)/3)-y*x}

second(0,1.5,15,0.1,fn)



#Third Order RK

third <- function(x0, y0, n, xn, f){
  h <- (xn-x0)/n
  x <- seq(x0, xn, by=h)
  y <- array(dim = length(x))
  y[1] = y0
  for (i in 1:n){
    k1 <- f(x[i], y[i])
    k2 <- f(x[i]+0.5*h, y[i]+k1*h)
    k3 <- f(x[i]+h, y[i]-h*k1+2*k2*h)
    y[i+1] <- y[i]+(1/6)*(k1+4*k2+k3)*h
  }
  h
}

fn <- function(x,y){y*(x^2)-1.1*y}

third(0,1,4,2,fn)




#Fourth Order RK

fourth <- function(x0, y0, n, xn, f){
  h <- (xn-x0)/n
  x <- seq(x0, xn, by=h)
  y <- array(dim = length(x))
  y[1] = y0
  for (i in 1:n){
    k1 <- f(x[i], y[i])
    k2 <- f(x[i]+0.5*h, y[i]+k1*h)
    k3 <- f(x[i]+0.5*h, y[i]+k2*h)
    k4 <- f(x[i]+h, y[i]+h*k3)
    y[i+1] <- y[i]+(1/6)*(k1+2*k2+2*k3+k4)*h
  }
  h
}

fn <- function(x,y){y*(x^2)-1.1*y}

fourth(0,1,4,2,fn)


library(rmutil)
fn1 <- function(y,x){y*(x^2)-1.1*y}
runge.kutta(fn1, 1, seq(0,2,by=0.5))


#Euler

euler <- function(f, x0,xt, y0, h){
  x <- x0
  y <- y0
  n <- length (seq(x0,xt,by = h))
  for(i in 1 :(n-1)){
    y0 <- y0+h*f(x0,y0)
    x0 <- x0 + h
    x <- c(x,x0)
    y <- c(y, y0)
  }
  return(data.frame(x=x, y=y))
}

#metode numerik
f1 <- function (x,y){(exp(3*x)/3)-y*x}
num <- euler (f1, x0=0, xt=1.5, y0=1, h=0.1)
num


#metode analitik

analitik <- function(f, x0, xt, y0, h){
  x <- x0
  y <- y0
  n <- length(seq(x0, xt, by = h))
  for ( i in 1 : (n-1)){
    y0 <- f(x0+h)
    x0 <- x0 + h
    x <- c(x,x0)
    y <- c(y,y0) 
  }
  return(data.frame(x=x, y=y))
}
f <- function(x){exp((x^3)/3-1.1*x)}
analitik <- analitik (f, 0, 2, 1, 0.5)
analitik


#midpoint

midpoint <- function(f, x0, xt, y0, h){
  x <- x0
  y <- y0
  n <- length(seq(x0, xt, by = h))
  for ( i in 1 : (n-1)){
    s1 <- y0 + f(x0,y0)*h/2
    s2 <- h*f(x0+h/2), s1
    y0 <- y0+s2
    x0 <- x0 + h
    x <- c(x,x0)
    y <- c(y,y0) 
  }
  return(data.frame(x=x, y=y))
}
fmidp <- function(x){exp(3*x)/3)-y*x}
num <- midpoint(fmidp, x0=0, xt=1.5, y0=1, h=0.1)
num

