## Parcial 1 Jhon Alejandro Ramirez Daza  
## c.c. 1000118906

#1
#simulacion de los datos.
n1=1500
mu1=1
sigma1=2
set.seed(n1)
datos=rnorm(n1,mu1,sigma1)


h_0<-(1.06*min(var(datos),(IQR(datos)/1.349))*n1^(-1/5));h_0


#asumiendo kernel gausiano 
sigmak=1
RK=1/(2*sigmak*sqrt(pi))
Rfpp = 3/(8*sqrt(pi)*(min(var(datos),(IQR(datos)^5))))
AMISE=(4/5) * (sigmak^4*RK)^(4/5)*Rfpp^(1/5)*n1^(-4/5)
AMISE

RK/(n1*h_0) + ((sigmak^4)*(h_0^4))*Rfpp/4

f_h = density(datos,bw = h_0,kernel = "gaussian")
approx(f_h$x,f_h$y,xout=min(datos))

LI=(sqrt(0.003347213)-(qnorm(1-0.05/2)*(sqrt((1/sigmak)/(4*n1*h_0)))))^2
LS=(sqrt(0.003347213)+(qnorm(1-0.05/2)*(sqrt((1/sigmak)/(4*n1*h_0)))))^2
LI;LS


h_0=1.059*sigma1*n1^(-1/5);h_0
mue=mean(datos);mue
sigmae=sd(datos);sigmae
maximo=max(datos);maximo



fmaxe=dnorm(maximo,mue,sqrt(h_0^2+sigmae^2))


fmax=dnorm(maximo,mu1,sigma1)

eee=(fmax-fmaxe)^2
eee


E<-dnorm(mu1+sigma1,mue,sqrt(h_0^2+sigmae^2))
V<-(dnorm(mu1+sigma1,mu1,sigma1)*RK)/(n1*h_0)
V
E-2.575*sqrt(V)
E+2.575*sqrt(V)

library(sm)
?nise
test=nise(datos)
test


# 5.
n2=1500
alpha=1
beta=2
set.seed(n2)
datos2=rweibull(n2,alpha,beta)


# Realiza la prueba de K-S
ks_result <- ks.test(datos, datos2)

# Imprime el resultado
print(ks_result)

#el p valor no rechasa la hipotesis nula.