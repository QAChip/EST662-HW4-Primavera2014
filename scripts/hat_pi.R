# ---- g(x) ----
dg=function(i){
  (2/3)*(2-x)
}
# ---- -x(x-4)/3=u ----
#u \sim U(0,1)
#Generador de va X por el método de inversión

randx=function(n){
  u=runif(n)
  x1=2-sqrt(4-3*u)
  #x2=(2+sqrt(4-3*u)) No se usa porque X2>1 y 0<X<1
  return(x1)
}

randx.anti=function(n){
  u=runif(n)
  x1=2-sqrt(4-3*u)
  x2=2-sqrt(4-3*(1-u))
  #x2=(2+sqrt(4-3*u)) No se usa porque X2>1 y 0<X<1
  x=matrix(c(x1,x2), ncol=2, byrow=FALSE)
  return(x)
}

# ---- Plot g(x) ----
x=matrix(ncol=1, seq(0,1,by=0.001) )
plot(x,apply(x,2,g), type="l")


#---- Pi ----
PI=function(i){
  4/((1+i)^2)
  #6/((2-x)*(1+i)^2) #Dividiendo entre la densidad g(x)
}

PI.anti=function(i,j){
  # la variable i \sim U(0,1)
  # la variable j=1-i \sim U(0,1)
  ( (4/((1+i)^2))+(4/((1+j)^2)) )/2
}
# ---- \teta{Pi} ----
x=randx.anti(100000)
Teta=PI.anti(i=x[,1],j=x[,2])
