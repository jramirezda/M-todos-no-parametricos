

# Funciones Kernel

x=seq(-1,1, length=10000)
x1=seq(-3,3, length=10000)
y1=(1/(sqrt(2*pi)))*exp((-x1^2)/2) #Gaussiano
y2=rep(0.5, 10000)                #Uniforme
y3= 1-abs(x)
y4=(3/4)*(1-x^2)
par(mfrow=c(2,2))
plot(x1,y1, type="l", xlab="x", col=2, main="Gaussiano", ylab="Densidad")
plot(x,y2, type="l", col=2, main="Uniforme", ylab="Densidad")
plot(x,y3, type="l", col=2, main="Triangular", ylab="Densidad")
plot(x,y4, type="l", col=2, main="Epanechnikov", ylab="Densidad")


#?density
datos=rnorm(100000, 20,2)
par (mfrow=c(2,2))
hist(datos, freq=FALSE, xlab="Datos simulados", 
     ylab="", main="", ylim=c(0,0.20))
plot(density(datos,  kernel="rectangular"), 
     xlab="", ylab="Densidad", main="Uniforme", col=1)
plot(density(datos,  kernel="triangular"),  
     xlab="", ylab="Densidad", main="Triangular",col=2)
plot(density(datos,  kernel="epanechnikov"),   
     xlab="", ylab="Densidad", main="Epanechnikov", col=4)
