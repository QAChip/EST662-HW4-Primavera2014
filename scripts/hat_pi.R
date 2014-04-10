# ---- g(x) ----
dg=function(i){
  (2/3)*(2-x)
}

# ---- f(x) ----

df=function(x){
  y=4/(1+x^2)
}

# ----Soporte ----
x=matrix(ncol=1, seq(0,1,by=0.001) )

# ---- Plotg(x)f(x) ----
plot(x,apply(x,2,df),type="l", col="blue",xlab=expression(x),
     ylim=c(0,4), xlim=c(0,1), ylab=expression(list(f(x), g(x))))
lines(x,apply(x,2,dg), type="l", col="red")
text(0.8,3,expression(f(x)==frac(4,(1+x^2))) )
text(0.8,1.2, expression(g(x)==frac(2*(2-x),3)), ylim=c(0,3.6))


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
# ---- N muestra ----
n=10000

# ---- hat(Pi) ----
x=randx(n)
Teta=PI(i=x)

# ---- hat(sigma2)----
Teta2=PI2(i=x)
s2.teta=mean(Teta2)-mean(Teta)^2

# ---- En(Teta) ----
Teta.n=cumsum(Teta) #Sucesión de sumas parciales
m=seq(1:length(Teta.n)) #Denominador
En.Teta=Teta.n/m #Sucesión E_n[teta] m=1,...,n

# ---- Plot ----
plot(m, En.Teta, type="l", ylim=c(3.11,3.19),
     ylab=expression(y==E[n](hat(pi))))
abline(h=pi, col="yellow")

# ---- hat(Pi.anti) ----
x=randx.anti(n)
Teta.anti=PI.anti(i=x[,1], j=x[,2])

# ---- hat(sigma2.anti)----
Teta2.anti=PI2.anti(i=x[,1],j=x[,2])
s2.teta.anti=mean(Teta2.anti)-mean(Teta.anti)^2

# ---- En(Teta.anti) ----
Teta.n.anti=cumsum(Teta.anti) #Sucesión de sumas parciales
m=seq(1:length(Teta.n.anti))#Denominador
En.Teta.anti=Teta.n.anti/m #Sucesión E_n[teta.anti], m=1,...,n

# ---- Plot.anti ----
plot(m, En.Teta.anti,type="l", ylim=c(3.11,3.19),
     ylab=expression(y==E[n](hat(pi))))
abline(h=pi, col="yellow")
