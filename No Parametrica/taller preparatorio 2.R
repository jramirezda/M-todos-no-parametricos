rm(list=ls())
library(MASS)
library(sm)
library(locfit)
library(ISLR)
library(FNN)
library(RColorBrewer)
library(sp)
library(KernSmooth)
set.seed(777)
mu=6
eta=10
sigma=4
rho=0.5
tao=9
x=seq(5,15, by=0.1)

sample_meanvector <- c(mu, eta)
sample_covariance_matrix <- matrix(c(sigma, sqrt(sigma*tao)*rho, sqrt(sigma*tao)*rho, tao),ncol = 2)


sample_distribution <- mvrnorm(n = 100,mu = sample_meanvector,Sigma = sample_covariance_matrix)
x<-sample_distribution[,1]
y<-sample_distribution[,2]

reg<-(lm(y~x))
summary(reg)

regkernel1=sm.regression(x, y, eval.points = 10, model="none", col = 2, lwd=2)
regkernel1






#######Miguel
library (sm)
library(mv)
set.seed(33)
mu=50
eta=163
sigma =5
rho=0.9
tao=10
x=seq(40,60,len=100)

# #D ----------------------------------------------------------------------

set.seed(33)
x<-seq(5,15,len=100)
eta=163
mu=50
tao=10
sigma=5
rho=0.9
E_y_x=eta+((rho*tao)/sigma)*(x-mu)
V_y_x=(1-rho^2)*(tao^2)
y=rnorm(100,E_y_x,V_y_x)
plot(x,y,main="Estimacion modelo de regresion")
abline(lm(y~x), lty=1,col=36)

lm(y~x)

# #E ----------------------------------------------------------------------

kernel=sm.regression(x,y, model="none",
                     xlab="X",ylab="Y",
                     col = 4, lwd=1,
                     eval.points=seq(5,15,by=0.1))
points(10,kernel$estimate[51],col=46,pch=19)



# #G ----------------------------------------------------------------------

sm.regression(x,y,display="se",col=46)


# #H ----------------------------------------------------------------------

regker=sm.regression(y, x, model="no effect", ylab="Y",
                     xlab="X", col = 1, lwd=2)


# #I ----------------------------------------------------------------------

regkerlin=sm.regression(x, y, model="linear", ylab="Y", main= " No Linealidad",
                        xlab="X", col = 1, lwd=2)


# #J ----------------------------------------------------------------------

modloess = loess(y~x,degree=2,span=0.9)
pred = predict(modloess)
a = order(data.frame(x,y)$x)
plot(x,y,type="p",main="Loess",
     xlab="X", ylab="Y")
lines(data.frame(x,y)$x[a],pred[a],
      col="blue",lwd=2)

modlowess = lowess(y~x)
a2 = order(data.frame(x,y)$x)
plot(x,y,type="p", main="Lowess",xlab="X",
     ylab="Y")
lines(x[a2],modlowess$y,col="blue",lwd=2)

#K
datos <- rmvnorm( n =100 , mean = jesus , sigma = xavi )
datos <-as.data.frame( datos )
set.seed(1)
mu <-1.5 #se modifica antes era 1
eta <-3 #se modifica antes era 2
sigma <-0.9
rho <-0.7
tao <-5
jesus <- matrix (c( mu , eta ) )
xavi <- matrix (c( sigma *2 , rho * sigma *tao , rho sigma *tao , tao **2) ,ncol =2)
datos2 <- rmvnorm ( n =100 , mean = jesus , sigma = xavi )
datos2<-as.data.frame( datos2 )
x_total<-c( datos2$V2 , datos2 $V2 )
y_total<-c( datos$V1 , datos2 $V1 )
grupos <-c(rep (0 ,100) ,rep (1 ,100) )
sm.ancova( x_total , y_total , grupos , model = " equal ")

#### punto 3 
ISIT = read.table("G:/Mi unidad/2023-2/Materias/NoParametrica/No Parametrica/12. Regresion No Paramétrica/ISIT.txt", header = TRUE)
Bioluminiscencia<-ISIT$Sources[ISIT$Station==16]
Profundidad<-ISIT$SampleDepth[ISIT$Station==16]


fit2<-smooth.spline(Profundidad,Bioluminiscencia,cv = TRUE)
plot(Profundidad,Bioluminiscencia,col="red", pch=16, xlab="Profundidad (m)",
     ylab="Bioluminiscencia", main="Smoothing splines")
lines(fit2,col="blue",lwd=2)
