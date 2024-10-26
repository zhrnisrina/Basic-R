
# a. f(x) = exp(x)-3*x^2

library(NLRoot)

func <- function(x){
  sin(x)-3*x^2
}

curve(func, xlim = c(0,1), col = 'blue', lwd = 1.5, lty = 2)
abline(h=0)
abline(v=0)


func <- function(x){
  exp(x)-3*x^2
}

curve(func, xlim = c(-1,0), col = 'blue', lwd = 1.5, lty = 2)
abline(h=0)
abline(v=0)


#FALSE POSITION  #FALSE POSITION #FALSE POSITION  #FALSE POSITION 
#AKAR1

root_rf <- function(f, a, b, tol = 1e-7, N){
  iter <- 0 
  fa <- f(a)
  fb <- f(b)
  x <- ((fb*a)-(fa*b))/(fb-fa)
  fx <- f(x)
  while (abs(fx)>tol){
    iter <- iter+1==
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
consol1 <- root_rf(function(x){exp(x)-3*x^2}, a = 0, b = 1, N = 100)
print(consol1)


#AKAR2

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
consol1 <- root_rf(function(x){exp(x)-3*x^2}, a = -1, b = 0, N = 100)
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
consol1 <- root_fpi(function(x){sqrt(exp(x)/3)}, x0 = 0, N =100)
print(consol1)


#NEWTON RHAPSON #NEWTON RHAPSON #NEWTON RHAPSON #NEWTON RHAPSON

root_newton <- function(f, fd, x0, tol = 0.00005, N = 100){
  iter <- 0
  xold <- x0
  xnew <- xold + 10*tol
  while(abs(xnew-xold)>0){
    iter <- iter +1
    if(iter>N){
      stop("No Solutions Found")
    }
    xold <- xnew
    xnew <- xold - f(xold)/fd(xold)
    print(xnew)
  }
  root <- xnew
  return(list(`function`=f, root = root, iter = iter))
}
root_newton(function(x){exp(x)-3*x^2}, function(x){exp(x)-6*x}, x0 = 2)


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
root_secant(function(x){exp(x)-3*x^2}, x=2)

#AKAR2

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
root_secant(function(x){exp(x)-3*x^2}, x=0)


# b. f(x) = sin(x)-exp(x)

library(NLRoot)

func <- function(x){
  sin(x)-exp(x)
}

curve(func, xlim = c(-5,-3), col = 'blue', lwd = 1.5, lty = 2)
abline(h=0)
abline(v=0)


func <- function(x){
  sin(x)-exp(x)
}

curve(func, xlim = c(-1.5,-1), col = 'blue', lwd = 1.5, lty = 2)
abline(h=0)
abline(v=0)


#FALSE POSITION  #FALSE POSITION #FALSE POSITION  #FALSE POSITION 

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
consol1 <- root_rf(function(x){sin(x)-exp(x)}, a = -5, b = -3, N = 100)
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
consol1 <- root_fpi(function(x){sin(x)-exp(x)+x}, x0 = 0, N =100)
print(consol1)


#NEWTON RHAPSON #NEWTON RHAPSON #NEWTON RHAPSON #NEWTON RHAPSON

root_newton <- function(f, fd, x0, tol = 0.00005, N = 100){
  iter <- 0
  xold <- x0
  xnew <- xold + 10*tol
  while(abs(xnew-xold)>0){
    iter <- iter +1
    if(iter>N){
      stop("No Solutions Found")
    }
    xold <- xnew
    xnew <- xold - f(xold)/fd(xold)
    print(xnew)
  }
  root <- xnew
  return(list(`function`=f, root = root, iter = iter))
}
root_newton(function(x){sin(x)-exp(x)}, function(x){cos(x)-exp(x)}, x0 = -4)


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
root_secant(function(x){sin(x)-exp(x)}, x=-4)
