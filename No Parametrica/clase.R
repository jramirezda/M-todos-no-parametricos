library(locfit)
library(sm)
library(ISLR)
library(MASS)
library(FNN)
library(RColorBrewer)
library(sp)
library(KernSmooth)
data(Boston)
head(Boston)


set.seed(8)
n=100
x=seq (10, 20, length=n)
beta_0=2
beta_1=0.05
media=0
desv=2
y=beta_0 + beta_1*x + rnorm(n,media,desv)
par(mfrow=c(1,2))
plot(x,y, pch=16)
regkernel1=sm.regression(x, y, model="no effect",col = 2,
                         lwd=2, main="Datos simulados")


?rnorm


set.seed(8)
n=100
x=seq (10, 20, length=n)
beta_0=5
beta_1=-1.8
media=0
desv=2
y=beta_0 + beta_1^2*x + rnorm(100,0,10)
par(mfrow=c(1,2))
plot(x,y, pch=16)
regkernel1=sm.regression(x, y, model="no effect",col = 2,
                         lwd=2, main="Datos simulados")



?lm
summary(lm(y~x))



par(mfrow=c(1,2))
# Regresión Kernel: Hipótesis de no efecto
regkernel1=sm.regression(lstat, medv, model="no effect", ylab="Logaritmo del precio",
                         xlab="Porcentaje en estrato bajo", col = 2, lwd=2, main="No Efecto")
## Test of no effect model: significance = 0
# Regresión Kernel: Hipótesis de linealidad
regkernel1=sm.regression(lstat, medv, model="linear",
                         ylab="Logaritmo del precio",
                         main= "Linealidad",xlab="Porcentaje en estrato bajo", col = 2, lwd=2)




?lm

set.seed(8)
error=rnorm(100,0,5)
x=seq(0,20,length=100)
y=100.9-2.52*x+error


plot(x,y)
modelolineal=lm(y~x)
summary(modelolineal)


x2=x-5
x3=x-10
x4=x-15

plot(x2,y)
modelolineal2=lm(y~x2)
summary(modelolineal2)

plot(x3,y)
modelolineal3=lm(y~x3)
summary(modelolineal3)

plot(x4,y)
modelolineal4=lm(y~x4)
summary(modelolineal4)




##graficos nuevos
set.seed(8)
error <- rnorm(100, 0, 5)
x <- seq(0, 20, length = 100)
y <- 100.9 - 2.52 * x + error

# Crear una disposición de 2 filas y 2 columnas para los gráficos
par(mfrow = c(2, 2))

# Gráfico 1
plot(x, y)
abline(modelolineal, col = "red")  # Línea de regresión
title(main = "Gráfico 1")

# Gráfico 2
plot(x2, y)
abline(modelolineal2, col = "red")  # Línea de regresión
title(main = "Gráfico 2")

# Gráfico 3
plot(x3, y)
abline(modelolineal3, col = "red")  # Línea de regresión
title(main = "Gráfico 3")

# Gráfico 4
plot(x4, y)
abline(modelolineal4, col = "red")  # Línea de regresión
title(main = "Gráfico 4")

# Restaurar la disposición de gráficos a su estado original
par(mfrow = c(1, 1))

yes=100.66-2.54*5;yes
yes=100.66-2.54*10;yes
yes=100.66-2.54*15;yes
yes=100.66-2.54*5;yes


beta_0=NULL;beta_1=NULL
for (i in (1:1000)) {
  error <- rnorm(100, 0, 5)
  x <- seq(0, 20, length = 100)
  y <- 100.9 - 2.52 * x + error
  
  beta_0[i]=coef(lm(y~x)[1])
  beta_1[i]=coef(lm(y~x))[2]
  
}

?hist
cbind(head(beta_0),head(beta_1))
beta_0
beta_1
par(mfrow=c(1,2))
hist(beta_0,freq = FALSE, xlab= "beta 0")
hist(beta_1,freq = FALSE, xlab= "beta 1 ")




beta_0 <- NULL
beta_1 <- NULL
for (i in 1:1000) {
  error <- rnorm(100, 0, 5)
  x <- seq(0, 20, length = 100)
  y <- 100.9 - 2.52 * x + error
  
  beta_0[i] <- coef(lm(y ~ x))[1]
  beta_1[i] <- coef(lm(y ~ x))[2]
}

par(mfrow = c(1, 2))

# Histograma y curva para beta_0
hist(beta_0, freq = FALSE, xlab = "beta 0", main = "Distribución de beta_0")
curve(dnorm(x, mean = mean(beta_0), sd = sd(beta_0)), col = "blue", add = TRUE, lwd = 2)

# Histograma y curva para beta_1
hist(beta_1, freq = FALSE, xlab = "beta 1", main = "Distribución de beta_1")
curve(dnorm(x, mean = mean(beta_1), sd = sd(beta_1)), col = "blue", add = TRUE, lwd = 2)



data(Boston)
head(Boston)

par(mfrow=c(1,2))
# Regresión Kernel datos de Boston
lstat=X_boston$lstat
medv= Boston$medv
regkernel1=sm.regression(lstat, medv, model="none", ylab="Logaritmo del precio",
                         xlab="Porcentaje en estrato bajo", col = 2, lwd=2)

h=regkernel1$h
h

par(mfrow=c(1,2))
fit4=locpoly(lstat, medv, bandwidth = 2)
fit5=locpoly(lstat, medv, bandwidth = 5)
plot(lstat, medv, ylab="Logaritmo del precio",
     xlab="Porcentaje en estrato bajo", col = 2, lwd=2, main="h=2")
lines(fit4, col=4, lwd=4)
plot(lstat, medv, ylab="Logaritmo del precio",
     xlab="Porcentaje en estrato bajo", col = 2, lwd=2, main="h=5")
lines(fit5, col=4, lwd=4)




n <- 100
x <- seq(0, 1, length.out = n)
beta_0 <- -0.2
beta_1 <- 1.5
beta_2 <- -1.2
y <- beta_0 + beta_1 * x + beta_2 * (x^2) + rnorm(n, mean = 0, sd = 0.05)
plot(x, y)

set.seed(10)
modelo <- lm(y ~ x + I(x^2))  # Se usa I() para indicar que x^2 es independiente
modelo

beta0 <- coefficients(modelo)[1]
beta1 <- coefficients(modelo)[2]
beta2 <- coefficients(modelo)[3]

beta0
beta1
beta2

x_seq <- seq(0, 1, length.out = 100)  # Ajusta el número de puntos según tus necesidades

# Calcular los valores de y correspondientes al modelo
y_modelo <- beta0 + beta1 * x_seq + beta2 * (x_seq^2)

# Crear el diagrama de dispersión
plot(x, y, main = "Diagrama de Dispersión y Modelo", xlab = "x", ylab = "y")

# Agregar la curva del modelo al diagrama
lines(x_seq, y_modelo, col = "red", lwd = 2)




x2=(x-0.6)
summary(x2)
xmas=c(rep(0,60),x2[61:100])
xmas



y <- beta_0 + beta_1 * x + beta_2 * xmas + rnorm(n, mean = 0, sd = 0.05)

set.seed(10)
modelo <- lm(y ~ x + xmas)  # Se usa I() para indicar que x^2 es independiente
modelo

beta0 <- coefficients(modelo)[1]
beta1 <- coefficients(modelo)[2]
beta2 <- coefficients(modelo)[3]

beta0
beta1
beta2

x_seq <- seq(0, 1, length.out = 100)  # Ajusta el número de puntos según tus necesidades

# Calcular los valores de y correspondientes al modelo
y_modelo <- beta0 + beta1 * x_seq + beta2 * (xmas)
y_modelo
# Crear el diagrama de dispersión
points(x, y_modelo, main = "Diagrama de Dispersión y Modelo", xlab = "x", ylab = "y")







