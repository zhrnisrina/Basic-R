reg <- function(x,y,alfa,k,beta,Ea)
{
  n=length(x)
  b=c(alfa,k,beta)
  z0=c(n,1)
  z1=c(n,1)
  z2=c(n,1)
  d=c(n,1)
  fx=c(n,1)
  e=matrix(1,2,1)
  while(TRUE){
    for(i in 1:n){
      fx[i]= b[1]/(1+exp(b[2]*(x[i]-b[3])))
      z0[i]= 1/(1+exp(b[2]*(x[i]-b[3])))
      z1[i]= (b[1]*exp(b[2]*(x[i]-b[3])))*(x[i]-b[3])
      z2[i]= (b[1]*exp(b[2]*(x[i]-b[3]))*b[2])/(1+exp(1/(1+exp(b[2]*(x[i]-b[3])))))^2
      d[i] = y[i]-fx[i]
    }
    z = cbind(z0,z1,z2)
    zt= t(z)
    zi= solve(zt%*%z)
    deltab = zi%*%(zt%*%d)
  
    b = b+deltab
    e = abs(deltab/b)
    
    if (e[1, ]< Ea | e[2, ]< Ea) break
  }
  return(list(b=b, e=e))
}
x = c(13410, 884, 147874, 360, 150, 230, 5842, 784, 330, 17714)
y = c(74.7, 55.6, 77.4, 64.7, 45, 46.8, 73.7, 62.5, 54.3, 75.1)
reg(x,y,70,-0.0001,0.00001)

