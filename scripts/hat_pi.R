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

# ---- Plotgf ----
plot(x,apply(x,2,df),type="l", col="blue",xlab=expression(x),
     ylim=c(0,4), xlim=c(0,1), ylab=expression(list(f(x), g(x))))
lines(x,apply(x,2,dg), type="l", col="red")
text(0.8,3,expression(f(x)==frac(4,(1+x^2))) )
text(0.8,1.2, expression(g(x)==frac(2*(2-x),3)), ylim=c(0,3.6))


# ---- randx ----
#u \sim U(0,1)
#Generador de va X por el método de inversión

randx=function(n){
  u=runif(n)
  x1=2-sqrt(4-3*u)
 #x2=(2+sqrt(4-3*u)) No se usa porque X2>1 y 0<X<1
  return(x1)
}

# ----randx.anti ----
randx.anti=function(n){
  u=runif(n)
  x1=2-sqrt(4-3*u)
  x2=2-sqrt(4-3*(1-u))
  #x2=(2+sqrt(4-3*u)) No se usa porque X2>1 y 0<X<1
  x=matrix(c(x1,x2), ncol=2, byrow=FALSE)
  colnames(x)=c("X1","X2")
  return(x)
}

#---- Pi ----
PI=function(i){
  6/((2-x)*(1+i^2)) # f(x)/g(x)
}

# ---- Pi2 ---- 
#No es necesaria esta función
PI2=function(i){
  24/((1+i^2)^2)*(2-i)
}

# ---- Pi.anti ----
PI.anti=function(i,j){
  # la variable i \sim U(0,1)
  # la variable j=1-i \sim U(0,1)
  ( (6/((2-i)*(1+i^2)))+
    (6/((2-j)*(1+j^2)))
  ) /2
}

# ----Pi2.anti
#no es necesaria esta función
PI2.anti=function(i,j){
  # la variable i \sim U(0,1)
  # la variable j=1-i \sim U(0,1)
  ( 24/((1+i^2)^2)*(2-i)+
    24/((1+j^2)^2)*(2-j)
  ) /2
}

# ---- hat.sigma2 ----
s2=function(Teta){
  m=seq(1:length(Teta)) #Denominador
  (cumsum(Teta-cumsum(Teta)/m)^2)/m
}

# ---- hat.sigma2.anti ----
s2.anti=function(x){
  #requiere una muestra matriz x n \times 2
  i=PI(x)
  m=seq(1:length(i[,1])) #Denominador
  cumsum((apply(i,1,mean)-cumsum(apply(i,1,mean))/m)^2)/m
}

# ---- N muestra ----
n=1000

# ---- hat(Pi) ----
x=randx(n)
Teta=PI(i=x)

# ---- En(Teta) ----
Teta.n=cumsum(Teta) #Sucesión de sumas parciales
m=seq(1:length(Teta.n)) #Denominador
En.Teta=Teta.n/m #Sucesión E_n[teta] m=1,...,n
s2.teta=s2(Teta)
q=qnorm(0.975)*sqrt(s2.teta/m)
# ---- Plot ----
plot(m, En.Teta, type="l", ylim=c(3.11,3.19),
     ylab=expression(y==E[n](hat(pi))))
abline(h=pi, col="yellow")
lines(m, En.Teta+q, col="red")
lines(m, En.Teta-q, col="red")


# ---- hatPi.anti ----
x=randx.anti(n)
Teta.anti=PI.anti(x[,1],x[,2])

# ---- hat(sigma2.anti)----
#Teta2.anti=PI2.anti(i=x[,1],j=x[,2]) #No es necesaria esta función
#s2.teta.anti=mean(Teta2.anti)-mean(Teta.anti)^2 #No es necesaria esta función

# ---- En(Teta.anti) ----
Teta.n.anti=cumsum(Teta.anti) #Sucesión de sumas parciales
m=seq(1:length(Teta.n.anti))#Denominador
En.Teta.anti=Teta.n.anti/m #Sucesión E_n[teta.anti], m=1,...,n
s2.teta.anti=s2.anti(x)
q=q=qnorm(0.975)*sqrt(s2.teta.anti/m)
# ---- Plot.anti ----
plot(m, En.Teta.anti,type="l", ylim=c(3.11,3.19),
     ylab=expression(y==E[n](hat(pi))))
abline(h=pi, col="yellow")
lines(m, En.Teta.anti+q, col="red")
lines(m, En.Teta.anti-q, col="red")

