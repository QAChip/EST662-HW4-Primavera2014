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
  #4/(1+i^2) Se desactivó en branch pi-test
  6/((2-x)*(1+i^2)) # f(x)/g(x)
}

PI2=function(i){
  24/((1+i^2)^2)*(2-i)
}

PI.anti=function(i,j){
  # la variable i \sim U(0,1)
  # la variable j=1-i \sim U(0,1)
  ( (6/((2-i)*(1+i^2)))+
    (6/((2-j)*(1+j^2)))
  ) /2
}

PI2.anti=function(i,j){
  # la variable i \sim U(0,1)
  # la variable j=1-i \sim U(0,1)
  ( 24/((1+i^2)^2)*(2-i)+
      24/((1+j^2)^2)*(2-j)
  ) /2
}
# ---- \teta{Pi} ----
x=randx(1000)
Teta=PI(i=x)

# ---- teta{sigma2}.anti----
Teta2=PI2(i=x)
s2.teta=mean(Teta2)-mean(Teta)^2

# ---- plot(Teta,m) ----
Teta.n=cumsum(Teta)
m=seq(1:length(Teta.n))
En.Teta=Teta.n/m

# ---- Plot ----
plot(m, En.Teta, type="l", ylim=c(3.11,3.19))
abline(h=pi, col="yellow")

# ---- \teta{Pi}.anti ----
x=randx.anti(5000)
Teta.anti=PI.anti(i=x[,1],j=x[,2])

# ---- teta{sigma2}.anti----
Teta2.anti=PI2.anti(i=x[,1],j=x[,2])
s2.teta.anti=mean(Teta2.anti)-mean(Teta.anti)^2

# ---- plot(Teta.anti,m) ----
Teta.n.anti=cumsum(Teta)
m=seq(1:length(Teta.n.anti))
En.Teta.anti=Teta.n.anti/m

# ---- Plot.anti ----
plot(m,En.Teta.anti,type="l", ylim=c(3.11,3.19))
abline(h=pi, col="yellow")
