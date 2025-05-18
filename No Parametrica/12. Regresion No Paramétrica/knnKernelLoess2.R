
#------------------------------------------------------
# Ejemplos de KNN, Kernel, LOESS, LOWESS, Inferencia
# con Kernel usando sm
#---------------------------------------------------

install.packages("locfit")
install.packages("ISLR")
install.packages("sm")
install.packages("MASS")
install.packages("FNN")
install.packages("RColorBrewer")
install.packages("sp")
install.packages("KernSmooth")

library(locfit)
library(sm)
library(ISLR)
library(MASS)
library(FNN)
library(RColorBrewer)
library(sp)
library(KernSmooth)


ISIT = read.table("ISIT.txt", header = TRUE)
Sources16 = ISIT$Sources[ISIT$Station==16]
Depth16   = ISIT$SampleDepth[ISIT$Station==16]
ISIT      = ISIT[ISIT$Station==16,]
head(ISIT)


# Regresión KNN datos de precios de vivienda en Bostón

??knn.reg
data(Boston)
head(Boston)
?Boston
X_boston = Boston["lstat"]    # lstat: % de la población del barrio en estrato bajo
y_boston = Boston$medv        # medv: Mediana del precio de la vivienda en cada barrio      

lstat_grid = data.frame(lstat = seq(range(X_boston$lstat)[1], range(X_boston$lstat)[2], 
                        by = 0.01))   # Valores de X para hacer estimación (aclarar estimación, predicción)
?knn.reg
pred_100 = knn.reg(train = X_boston, test = lstat_grid, y = y_boston, k = 100)
plot(medv ~ lstat, data = Boston, cex = .8, col = "dodgerblue", 
     main = "Regresión KNN, k = 100", ylab="Logaritmo del precio",  xlab="Porcentaje en estrato bajo")
lines(lstat_grid$lstat, pred_100$pred, col = "darkred", lwd = 0.25)


#Regresion KNN Bioluminiscencia

X=Depth16
Y=Sources16
plot(X,Y)
X_grid=data.frame(depth=seq(range(X)[1], range(X)[2], by =0.01))
pred = knn.reg(train = X, test = X_grid, y = Y, k = 5)
length(X_grid)
plot(Depth16, Sources16, cex = .8, col = "dodgerblue", main="",
                        xlab="Profundidad", ylab="Bioluminiscencia Palágica")
lines(X_grid$depth,pred$pred,col = "darkred", lwd = 0.25)



#regresión kernel
?sm.regression

data(Boston)
attach(Boston)
head(Boston)

# El h óptimo es 3.23
# ver  regkernel1$h

regkernel1=sm.regression(lstat, medv, model="none", ylab="Logaritmo del precio", 
                         xlab="Porcentaje en estrato bajo", col = 2, lwd=2) 


# Regresión Kernel cambiando h
# Cambiando h =1, 2, 5, 10, 20
regkernel1=sm.regression(lstat, medv,  model="none", ylab="Logaritmo del precio", 
                         xlab="Porcentaje en estrato bajo", col = 2, lwd=2)

regkernel1$h




par(mfrow=c(1,3))


# Regresión Kernel

regkernel1=sm.regression(lstat, medv, model="none", ylab="Logaritmo del precio", 
                         xlab="Porcentaje en estrato bajo", col = 2, lwd=2) 

# Regresión Kernel: Hipótesis de No efecto

regkernel1=sm.regression(lstat, medv, model="no effect", ylab="Logaritmo del precio", 
                         xlab="Porcentaje en estrato bajo", col = 2, lwd=2, main="No Efecto") 

#  Regresión Kernel: Hipótesis de Linealidad
regkernel1=sm.regression(lstat, medv, model="linear", ylab="Logaritmo del precio",  main= "Linealidad",
                         xlab="Porcentaje en estrato bajo", col = 2, lwd=2) 

#----------------------------------------
# Ejemplo de bioluminiscencia pelágica
#---------------------------------------

par(mfrow=c(1,3))

regkernel2=sm.regression(Depth16, Sources16, model="none", main="Regresión Kernel",
                        xlab="Profundidad", ylab="Bioluminiscencia Palágica", col=2,lwd=2)


regkernel2=sm.regression(Depth16, Sources16, model="no effect", main="Regresión Kernel",
                        xlab="Profundidad", ylab="Bioluminiscencia Palágica", col=2,lwd=2)


regkernel2=sm.regression(Depth16, Sources16, model="linear", main="Regresión Kernel",
                        xlab="Profundidad", ylab="Bioluminiscencia Palágica", col=2,lwd=2)



# sm ancova

x <- runif(80, 0, 1)
y <- 4*sin(6*x) + rnorm(50)
g <- rbinom(80, 1, 0.5)
sm.ancova(x, y, g,  model = "equal")

#-----------------------------------
# Regresión por polinomios locales
#----------------------------------

# Usando loess
# span: proporción de datos usados alrededor de cada estimación
# span: numeric parameter between 0 and 1 specifying proportion of data to be used in the local regression 
#      moving window. Larger numbers give smoother fits.

par(mfrow=c(2,1))
mloess = loess(Sources16~Depth16,data=ISIT,degree=2,span=0.1)
y.pred = predict(mloess)
rk = order(ISIT$SampleDepth)
plot(Depth16,Sources16,type="p",main="span=0.1", xlab="Profundidad", ylab="Bioluminiscencia")
lines(ISIT$SampleDepth[rk],y.pred[rk],col="red",lwd=2)

mloess = loess(Sources16~Depth16,data=ISIT, degree=2, span=0.1)
 y.pred = predict(mloess)
 rk = order(Depth16)
 plot(Depth16,Sources16,type="p",main="span=0.1")
 lines(ISIT$SampleDepth[rk],y.pred[rk],col="red",lwd=2)

## Lowess (se debe usar locpoly. lowess hace lo mismo de loess)

# ?lowess 
# Nota: lowess hace lo mismo que loess
# Se puede usar locpoly (librería kernel Smooth) en vez de lowess
# se especifica el kernel y el ancho de banda que se quiera.

# ?locpoly
# Local polynomial fitting with a kernel weight is used to estimate either a density, 
# regression function or their derivatives

# Datos de Bioluminiscencia

par(mfrow=c(1,2))
fit1 <- locpoly(Sources16, Depth16, bandwidth = 2)
fit2 <- locpoly(Sources16, Depth16, bandwidth = 4)
fit3 <- locpoly(Sources16, Depth16, bandwidth = 8)
plot(Sources16, Depth16, xlab="Profundidad", ylab="Bioluminiscencia", main="h=2")
lines(fit1, col=2)
plot(Sources16, Depth16, xlab="Profundidad", ylab="Bioluminiscencia", , main="h=8")
lines(fit3, col=4)

# datos de Viviendas en Boston

fit4=locpoly(lstat, medv, bandwidth = 2)
fit5=locpoly(lstat, medv, bandwidth = 5)
par(mfrow=c(1,2))
plot(lstat, medv, ylab="Logaritmo del precio", 
     xlab="Porcentaje en estrato bajo", col = 2, lwd=2, main="h=2")
lines(fit4, col=4, lwd=4)
plot(lstat, medv, ylab="Logaritmo del precio", 
     xlab="Porcentaje en estrato bajo", col = 2, lwd=2, main="h=5")
lines(fit5, col=4, lwd=4)




