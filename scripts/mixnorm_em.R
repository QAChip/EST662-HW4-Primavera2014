# ---- EM MixNorm ----
em.mixnorm=function(x,n){
  #n es el número total de iteraciones
   pr=0.5 #Valor inicial
   s1 = s2 = sd(x)
   m1=quantile(x, probs=0.25)
   m2=quantile(x, probs=0.75)
   X=matrix(NA, ncol=5, nrow=n)
   colnames(X)=c("Pr","m1","m2","s1","s2")
   for(i in 1:n){
     z1=(x-m1)/s1
     z2=(x-m2)/s2
     p=pr*dnorm(z1)/(pr*dnorm(z1)+(1-pr)*dnorm(z2)*s1/s2)
     s=sum(p)
     n=length(p)
     pr=s/n
     m1=sum(p*x)/s
     m2=sum((1-p)*x)/(n-s)
     s1=sqrt(sum(p*(x-m1)^2)/s)
     s2=sqrt(sum((1-p)*(x-m2)^2)/(n-s))
     X[i,]=c(pr,m1,m2,s1,s2)
     teta<<-list(pr=pr,mu1=m1,mu2=m2,s1=s1,s2=s2)
   }# Cierra for(i in 1:n){
   #print(X)
   return(teta)
 }#Cierra function(x,n)

# ---- Faithful ----
attach(faithful)

# ----PlotDB ----
plot(faithful)

# ---- pdf mixnorm ----
dmixnorm=function(Teta,i){
  x=Teta$pr*(dnorm(i,Teta$mu1,Teta$s1))+
    (1-Teta$pr)*(dnorm(i,Teta$mu2,Teta$s2))
}

# ---- Soporte.pi ----
soporte=function(i,k){
  #Para i se puede selecciona 1 o 2 para las columnas de la DB,
  #k es el número de puntos a evaluar
  seq( range(faithful[,i])[1],
             range(faithful[,i])[2],
             by=(range(faithful[,i])[2]-range(faithful[,i])[1])/k)
}

# ---- HistFaithful1 ----
teta=em.mixnorm(faithful[,1],n=50)
a=soporte(i=1,k=100)
y=dmixnorm(Teta=teta,i=a)

# ---- HistFaithful2 ----
teta=em.mixnorm(faithful[,2],n=50)
b=soporte(i=2,k=100)
z=dmixnorm(Teta=teta,i=b)

# ---- PlotFaithful ----
par(mfrow=c(1,2))
hist(faithful[,1],nclass=21,col="peachpuff",main="",xlab=names(faithful)[1],plot=TRUE,freq=F)
lines(a,y)
hist(faithful[,2],nclass=21,col="darkslategray2",main="",xlab=names(faithful)[2],plot=TRUE, freq=F)
lines(b,z)
