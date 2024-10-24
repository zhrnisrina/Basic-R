#METODE GRAFIS #METODE GRAFIS #METODE GRAFIS #METODE GRAFIS #METODE GRAFIS 

root_table <- function(f,a,b,N){
  x <- seq(from = a, to = b, length.out = N+1)
  fx <- rep(0, N+1)
  for(i in 1:(N+1)){
    fx[i] <- f(x[i])
  }
  data <- data.frame(x=x, fx=fx)
  return(data)
}  
  #iterasi
  contoh1 <- root_table(f = function(x){exp(-x)-x},
                       a = 0, b = 1, N = 50)
  print(contoh1)
  
  #iterasi 2
  contoh2 <- root_table(f = function(x){exp(-x)-x},
                        a = 0.56, b = 0.58, N = 20)
  print(contoh2)
  
  
  #METODE BISECTION #METODE BISECTION #METODE BISECTION #METODE BISECTION

  root_bisection <- function(f,a,b,tol = 1e-7, N=100){
    iter <- 0 
    fa <- f(a)
    fb <- f(b)
    while(abs(b-a)>tol){
      iter <-iter+1
      if(iter > N){
        warning('Iterations maximum exceeded')
        break
      }
      x <- (a+b)/2 
      fx <- f(x)
      if(fa*fx>0){
        a <- x
        fa <- fx
      }else{
        b <- x
        fb <- fx 
      }
    }
    
    #iterasi nilai x sebagai return value
    root <- (a+b)/2
    return(list(`function`=f, root = root, iter=iter))
  }
  
  consol <- root_bisection(function(x){exp(-x)-x},
                           a = 0, b = 1)
  print(consol)
  
  install.packages('NLRoot')
  library(NLRoot)
  
  func <- function(x){
    exp(x)-3*x^2
  }
  curve(func, xlim = c(2,3), col = 'blue', lwd = 1.5, lty = 2)
  abline(h=0)
  abline(v=0)
  
  func2 <- function(x){
    -0.847*x^2 + 1.75*x + 2.267
  }
  
  curve(func2, xlim = c(-3,5), col = 'blue', lwd = 1.5, lty = 2)
  abline(h=0)
  abline(v=0)
  
  
  #FALSE POSITION  #FALSE POSITION  #FALSE POSITION  #FALSE POSITION 
  
  root_rf <- function(f, a, b, tol = 1e-7, N){
    iter <- 0 
    fa <- f(a)
    fb <- f(b)
    x <- ((fb*a)-(fa*b))/(fb-fa)
    fx <- f(x)
    while (abs(fx)>tol){
      iter <- iter+1
      if(iter>N){
        warning("iterations maximum exceeded")
        break
      }
      if(fa*fx>0){
        a <- x
        fa <- fx
      } else {
        b <- x 
        fb <- fx
      }
      x <-((fb*a)-(fa*b))/(fb-fa)
      fx <- f(x)
    } 
      #iterasi nilai x sebagai return value
      root <- x
      return(list(`function`= f, root=root, iter=iter))
    }
    consol1 <- root_rf(function(x){exp(-x)-x}, a = 0, b = 1, N = 100)
    print(consol1)
    
    
    #ITERASI satu titik #ITERASI satu titik #ITERASI satu titik 
    
    root_fpi <- function(f, x0, tol = 1e-7, N){
      iter <- 0
      xlama <- x0
      xbaru <- f(xlama)
      while(abs(xbaru-xlama)>tol){
        iter <- iter+1
        if(iter>N){
          warning('iterations maximum exceeded')
          break
        }
        xlama <- xbaru
        xbaru <- f(xlama)
      }
      root <- xbaru
      return(list(`function`=f, root=root, iter=iter))
    }
  #fx yg dimasukkin yg udah jadi gx, x = 
  consol1 <- root_fpi(function(x){exp(-x)}, x0 = 0, N =100)
  print(consol1)
  
  
  #NEWTON RHAPSON #NEWTON RHAPSON #NEWTON RHAPSON #NEWTON RHAPSON
  
  root_newton <- function(f, fd, x0, tol = 0.00005, N = 100){
    iter <- 0
    xold <- x0
    xnew <- xold + 10*tol
    while(abs(xnew-xold)>0){
      iter <- iter +1
      if(iter>N){
        STOP("No Solutions Found")
      }
      xold <- xnew
      xnew <- xold - f(xold)/fd(xold)
      print(xnew)
    }
    root <- xnew
    return(list(`function`=f, root = root, iter = iter))
  }
  root_newton(function(x){x-exp(-x)}, function(x){1+exp(-x)}, x0 = 0)
  
  
  #METODE SECANT  #METODE SECANT  #METODE SECANT  #METODE SECANT  #METODE SECANT
  
  root_secant <- function(f, x, tol = 0.00005, N= 100){
    iter <- 0 
    xold <- x
    fxold <- f(x)
    x <- xold + 10*tol
    
    while(abs(x-xold)>tol){
      iter <- iter + 1
      if(iter>N){
        stop("No Solutions Found")
      }
      fx <- f(x)
      xnew <- x - fx*((x-xold)/(fx-fxold))
      xold <- x
      fxold <- fx
      x <- xnew
      print(x) #buat iterasi
    }
    root <- xnew
    return(list(`function`=f, root=root, iter=iter))
  }
  root_secant(function(x){x-exp(-x)}, x=0)
  
