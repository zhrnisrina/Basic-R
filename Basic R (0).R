a = 13
b = 12
c = "tes"

print(a)
print(b)
print(c)

d = c(1,2,3,4,5,6)
print(d)
e = c(1:6)
print(e)

f = seq(from = 1, to = 10, by = 0.05)
print(f)
length(f)

vektor = c(1:6)
for ( i in vektor ){
  print(vektor[i]*2)
}

awal = 1
while(awal <= 6){
  print(awal*2)
  awal = awal +1
}

a = c(2,4,6,8,10,12,14)
for(i in a){
  if(i>8){
    break
  }
  print(i)
}

a = 8
b = 9

while(a < 15){
  print(a)
  a = a + 1
  if( b + a > 20){
    break
  }
}

perkalian <- function(a,b,c,d){
  kali = a*b*c/d 
  return(kali)
}
perkalian(4,3,2,1)

fungsieksponen <- function(x){
  fx = exp(-x)-x
  return(fx)
}
fungsieksponen(4)
