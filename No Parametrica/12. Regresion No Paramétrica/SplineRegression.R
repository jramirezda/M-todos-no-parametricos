#-----------------------------------
# Regression and smoothing splines
# Datos de Bioluminiscencia Pel?gica
#------------------------------------
install.packages("ggplot")
library(splines)
library(tidyverse)
library(ggplot2)
library(caret)
library(MASS)
library(sm)
library(ssym)  # Vanegas y Paula: Modelos semiparam?tricos
theme_set(theme_classic())
require(ISLR)
require(graphics)
require(stats)



#------------------------------
# Datos de Bioluminiscencia
#------------------------------

# ISIT = read.table("ISIT.txt", header = TRUE)
library(readxl)
ISIT <- read_excel("12. Regresion No Paramétrica/ISIT.xlsx", 
                   col_types = c("numeric", "numeric", "text", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "text", "numeric", 
                                 "numeric", "text", "text", "numeric"))
View(ISIT)


Bioluminiscencia = ISIT$Sources[ISIT$Station==16]
Profundidad = ISIT$SampleDepth[ISIT$Station==16]
base   = data.frame(Bioluminiscencia,Profundidad)
plot(Profundidad,Bioluminiscencia, lty=1, main = "Datos de Zuur et al., 2009",
     ylab = "Bioluminiscencia", xlab = "Profundidad", pch=16)


# Definici?n de los nudos 

nudos  = quantile(Profundidad, p = c(0.15,0.30, 0.45, 0.60, 0.75, 0.9))          
nudos2 = quantile(Profundidad, p = c(0.25, 0.50, 0.75))                          



#-------------------------------------------------------
#             Regresi?n Splines C?bica
#-------------------------------------------------------

# bs(x, df = NULL, knots = NULL, degree = 3, intercept = FALSE,
# Boundary.knots = range(x))

fm1 = lm(Bioluminiscencia ~ bs(Profundidad, knots = nudos), data = base)  # Regresion splines usando 6 nudos
summary(fm1)

# Note que se estiman 10 par?metros (por qu??)
# Ver diapositiva 15 de Regresi?n en Statistical Learning Sesi?n 3

# Curva ajustada usando los seis nudos

plot(Profundidad,Bioluminiscencia, col=2, main="Regresión splines Cúbica con Seis nudos", pch=16,
     xlab ="Profundidad (m)", ylab = "Bioluminiscencia")               # Nube de puntos de Bioluminiscencia vs Profundidad
ht <- seq(500, 4900, length.out = 100)                                 # Puntos para hacer la predicci?n
lines(ht, col=4, lwd=2, predict(fm1, data.frame(Profundidad = ht)))    # Curva de Predicci?n 
for (i in 1:6)
      {
       abline(v=nudos[[i]], lty=2)
      }

# Regresi?n Splines c?bica usando 3 nudos


fm2 = lm(Bioluminiscencia ~ bs(Profundidad, knots = nudos2), data = base)  # Regresion splines usando 6 nudos
summary(fm2)

plot(Profundidad,Bioluminiscencia, col=2, main="Regresión Splines Cúbica con Tres nudos", 
     pch=16, xlab ="Profundidad (m)", ylab = "Bioluminiscencia")
summary(fm2 <- lm(Bioluminiscencia ~ bs(Profundidad, knots = nudos2), data = base))
ht <- seq(500, 5000, length.out = 100)
lines(ht, col=4, lwd=2, predict(fm2, data.frame(Profundidad = ht)))
for (i in 1:3)
      {
        abline(v=nudos[[i]], lty=2)
       }




# Sintaxis usando ggplot y 3 nodos

ggplot(base, aes(Profundidad, Bioluminiscencia) ) +
geom_point() +
stat_smooth(method = lm, formula = y ~ splines::bs(x, knots = nudos2))

# Sintaxis usando ggplot y 6 nodos

ggplot(base, aes(Profundidad, Bioluminiscencia) ) +
geom_point() +
stat_smooth(method = lm, formula = y ~ splines::bs(x, knots = nudos))


# Revisar c?mo definen en ggplot las bandas de variabilidad de la curav ade regresi?n splines


#----------------------------
# Smoothing splines
#-----------------------------

proflims<-range(Profundidad)
prof.grid<-seq(from=proflims[1], to = proflims[2])
fit1<-smooth.spline(Profundidad,Bioluminiscencia,df=16) #16 degrees of freedom
plot(Profundidad,Bioluminiscencia,col="red", pch=16, xlab="Profundidad (m)",ylab="Bioluminiscencia")
lines(fit1,col="red",lwd=2)
proflims<-range(Profundidad)
prof.grid<-seq(from=proflims[1], to = proflims[2])
fit2<-smooth.spline(Profundidad,Bioluminiscencia,cv = TRUE)
fit2  # ver df estimado: Nota: df es la traza de la matriz de suavizado
gl=fit2$df
fit2<-smooth.spline(Profundidad,Bioluminiscencia, df=gl) #gl degrees of freedom
plot(Profundidad,Bioluminiscencia,col="red", pch=16, xlab="Profundidad (m)",
ylab="Bioluminiscencia", main="Smoothing splines")
lines(fit2,col="blue",lwd=2)
legend("topright",("df=15.11, seleccionado por VC"),col="blue",lwd=2)
gl



#---------------------------------------------
# Datos de Ingreso (wage) vs Edad (age)
# Ingresos y edad de 205 trabajadores canadienses
# con educaci?n hasta grado 13
#----------------------------------------------


#loading the Splines Packages

require(splines)
require(ISLR)
attach(Wage) #attaching Wage dataset
?Wage        #for more details on the dataset
agelims<-range(age)
age.grid<-seq(from=agelims[1], to = agelims[2])

fit<-lm(wage ~ bs(age,knots = c(25,40,60)),data = Wage )
summary(fit)

#Plotting the Regression Line to the scatterplot   
plot(age,wage,col="grey",xlab="Age",ylab="Wages")
points(age.grid,predict(fit,newdata = list(age=age.grid)),col="darkgreen",lwd=2,type="l")
#adding cutpoints
abline(v=c(25,40,60),lty=2,col="darkgreen")

#fitting smoothing splines using smooth.spline(X,Y,df=...)

fit1<-smooth.spline(age,wage,df=16) #16 degrees of freedom
#Plotting both cubic and Smoothing Splines 
plot(age,wage,col="grey",xlab="Age",ylab="Wages")
points(age.grid,predict(fit,newdata = list(age=age.grid)),col="darkgreen",lwd=2,type="l")
#adding cutpoints
abline(v=c(25,40,60),lty=2,col="darkgreen")
lines(fit1,col="red",lwd=2)
legend("topright",c("Smoothing Spline with 16 df",
       "Cubic Spline"),col=c("red","darkgreen"),lwd=2)

fit2<-smooth.spline(age,wage,cv = TRUE)
gl=fit2$df
gl

fit2<-smooth.spline(age,wage,df=gl) #gl degrees of freedom
#Plotting both cubic and Smoothing Splines 
plot(age,wage,col="grey",xlab="Age",ylab="Wages")
points(age.grid,predict(fit,newdata = list(age=age.grid)),col="darkgreen",lwd=2,type="l")
lines(fit1,col="red",lwd=2)
legend("topright",c("Smoothing Spline with 6.8 df",
       "Cubic Spline"),col=c("red","darkgreen"),lwd=2)



#------------------------------------
# Ejemplo mcycle
# Regresi?n p-splines
#---------------------------------------


# base mcycle: Aceleraci?n de la cabeza en un accidente de moto simulado.

data("mcycle", package="MASS")
mcycle
?mcycle
fit <- ssym.l(accel ~ psp(times), data=mcycle, family="Normal")
summary(fit)
np.graph(fit, which=1, simul=FALSE, obs=TRUE,  
         main=" Experimento simulado de accidentes de moto", 
         ylab="Aceleraci?n de la cabeza", xlab="Tiempo despu?s del impacto (ms)")








