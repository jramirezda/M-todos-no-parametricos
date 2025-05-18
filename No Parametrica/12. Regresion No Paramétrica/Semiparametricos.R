
#------------------------------------------------
# Modelo Semiparametrico
# datos de precios de vivienda en Bostón
# Se evalúa la asociación de precios de vivienda
# con calidad del aire del vecindario 
# Respuesta:    medv: precio mediano de la vivienda
# Covariaables: nox (concentración promedio de oxido nítrico, p.p. 10 million)
#               lstat (% de la población en estrato más bajo)
#               dis (distancia ponderada a los 5 centros de empleo de Bostón)
#--------------------------------------------------

rm(list=ls())
library(MASS)
library(sm)
library(ssym)
library(ISLR)
library(gam)


#--------------------------------------
# Ejemplo precios de vivienda en Boston
#--------------------------------------
data(Boston)
?Boston  # variables de la base de datos
head(Boston)
attach(Boston)
par(mfrow=c(1,3))


plot(tax, log(medv), pch=16, xlab="Tasa de impuesto",
     ylab="Logaritmo del precio", col=2)
plot(lstat, log(medv), pch=16, xlab="Porcentaje en estrato bajo",
     ylab="Logaritmo del precio", col=2)
plot(dis, log(medv), pch=16, xlab="Distancia a los centros de empleo",
     ylab="Logaritmo del precio", col=2)


#regresiones Kernel con pruebas de linealidad


reg1<-sm.regression(tax, log(medv), model="linear", xlab="Tasa de impuesto",
     ylab="Logaritmo del precio", col=2)

reg2<-sm.regression(lstat, log(medv), model="linear", xlab="Porcentaje en estrato bajo",
     ylab="Logaritmo del precio", col=2)

reg3<-sm.regression(dis, log(medv), model="linear", xlab="Distancia a los centros de trabajo",
     ylab="Logaritmo del precio", col=2)


#----------------------------
# Regresión semiparamétrica
#----------------------------

# datos de viviendas de Bostón

# Usandso ssym
# ?ssym
# citation(package = "ssym")
regsemi=ssym.l(log(medv)~ tax + psp(lstat), data=Boston, family="Normal")
summary(regsemi)
np.graph(regsemi, which=1, xlab="Porcentaje en estrato bajo",
         ylab="Estimación del log del precio", 
         main="estimación usando ssym", ylim=c(-1,1.5))
envelope(regsemi)

#------------------------
# usando gam
#-------------------------

regsemi2=gam(log(medv)~ tax+ s(lstat))
summary(regsemi2)
par(mfrow=c(1,2))
plot(regsemi2, main="Estimación usando gam", se=TRUE, col="blue", xlab="Porcentaje en estrato bajo",
         ylab="Estimación del log del precio", ylim=c(-1,1))

#------------------------------------------
# Modelo aditivo
#-----------------------------------------

# Datos de viviendas de Bostón

regadi1=gam(log(medv)~  s(lstat)+s(dis))
summary(regadi1)
par(mfrow=c(1,2))
plot(regadi1, main="", se=TRUE, col="blue", 
     ylab="Estimación del log del precio")




# datos de salarios de 300 trabajadores
# http://127.0.0.1:19055/library/ISLR/html/Wage.html

attach(Wage)
?Wage
head(Wage)
aditivo=gam(log(medv)~s(year)+ s(age))
par(mfrow=c(1,2))
plot(aditivo, se=TRUE, col="blue")



