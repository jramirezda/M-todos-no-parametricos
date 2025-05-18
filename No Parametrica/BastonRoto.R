

# Ejercicio clase 19 Octubre

set.seed(8)

#-------------------------------------------
# Simulación de un modelo bastón roto
# y=beta0+beta1*x+b1*(x-a)+error
#-------------------------------------------

a=0.6
beta0=3
beta1=2
b1=8
error=rnorm(100,0, 0.5)
x=seq(0,1, length=100)
x2=ifelse(x<a,0,(x-a))
y= beta0 +beta1*x +b1*x2 +error
plot(x, y)

#------------------------------------------------------------------
# Estimación del modelo de regresión simple (no debe ajustar bien)
# a los datos simulados de un modelo bastón roto
#------------------------------------------------------------------

datos=data.frame(y,x)
modelo= lm(y ~ x, data=datos)
plot(datos$x, datos$y, xlab="x", ylab="y", main="Regresión lineal")
abline(modelo, col = "red")
names(modelo)
sum(modelo$residuals^2)

#------------------------------------------------------------------
# Estimación del modelo bastón roto (debe ajustar bien la curvatura)
#-------------------------------------------------------------------

modelo2=lm(y~x+x2)
beta0_est=modelo2[[1]][[1]]
beta1_est=modelo2[[1]][[2]]
beta2_est=modelo2[[1]][[3]]
y_est=beta0_est+beta1_est*x+beta2_est*x2
plot(x,y, pch=16, main =" Regresión Bastón Roto")
lines(x, y_est, col=2, lwd=2)
sum ((y-y_est)^2)

#--------------------------------------
# ¿Cuál ajusta mejor?
#--------------------------------------

# Suma de cuadrados del ajuste del modelo lineal
sum(modelo$residuals^2)

# Suma de cuiadrados del ajuste del modelo bastón roto
sum ((y-y_est)^2)


#-------------------------------------------
# Simulación de un modelo cuadrático
# y=beta0+beta1*x+beta2*x^2+error 
# con ajuste de un modelo cuadrático y 
# un modelo bastón roto.
#-------------------------------------------

set.seed(8)
x=seq(-4, 1, length=100)
beta0=4
beta1=-3
beta2=2
y=beta0+beta1*x-beta2*x^2+rnorm(100,0,3)
plot(x,y,pch=16)


# Ajuste del modelo cuadrático
fit <- lm(y ~x + I(x^2))
coef(fit)
y_est= coef(fit)[[1]]+ coef(fit)[[2]]*x+ coef(fit)[[3]]* (x^2)
lines (x, y_est,col=2, lwd=2)
sum ((y-y_est)^2)


# Ajuste de un modelo bastón roto a los datos simulados de un modelo
# cuadrático

a=-1.86
x2=ifelse(x<a,0,(x-a))
modelo3=lm(y~x+x2)
beta0_est=modelo3[[1]][[1]]
beta1_est=modelo3[[1]][[2]]
beta2_est=modelo3[[1]][[3]]
y_est=beta0_est+beta1_est*x+beta2_est*x2
plot(x,y, pch=16, main =" Regresión Bastón Roto")
lines(x, y_est, col=2, lwd=2)
sum ((y-y_est)^2)





# Crear un vector para a que varíe de -2 a 0 en incrementos de 0.1
a_values <- seq(-2, 0, by = 0.01)

# Inicializar un vector para almacenar las sumas de errores al cuadrado
sums_of_squared_errors <- numeric(length(a_values))

# Realizar un bucle para calcular las sumas de errores al cuadrado para cada valor de a
for (i in 1:length(a_values)) {
  a <- a_values[i]
  x2 <- ifelse(x < a, 0, (x - a))
  modelo3 <- lm(y ~ x + x2)
  beta0_est <- modelo3[[1]][[1]]
  beta1_est <- modelo3[[1]][[2]]
  beta2_est <- modelo3[[1]][[3]]
  y_est <- beta0_est + beta1_est * x + beta2_est * x2
  sums_of_squared_errors[i] <- sum((y - y_est)^2)
}

# Imprimir una tabla con los resultados para a igual a -1
data_frame <- data.frame(a = a_values, SumOfSquaredErrors = sums_of_squared_errors)
print(data_frame)

# Trama de errores al cuadrado vs. valores de a
plot(a_values, sums_of_squared_errors, type = 'l', xlab = 'Valor de a', ylab = 'Suma de Errores al Cuadrado')







  #--------------------------------------------
# Regresión látigo
#--------------------------------------------

#-----------------------------
# Simulación del modelo
#---------------------------

x=seq(0,1, length=100)
beta0=0.2
beta1=0.5
b1= 1.0
b2=-1.0
b3=-0.5
b4= 0.5
b5= 2.0
b6= -2.0
b7= 1.5
b8= -1.5 
b9= 2.0
b10=-2.
x1=ifelse(x<0.50, 0, (x-0.50))
x2=ifelse(x<0.55, 0, (x-0.55))
x3=ifelse(x<0.60, 0, (x-0.60))
x4=ifelse(x<0.65, 0, (x-0.65))
x5=ifelse(x<0.70, 0, (x-0.70))
x6=ifelse(x<0.75, 0, (x-0.75))
x7=ifelse(x<0.80, 0, (x-0.80))
x8=ifelse(x<0.85, 0, (x-0.85))
x9=ifelse(x<0.90, 0, (x-0.90))
x10=ifelse(x<0.95,0, (x-0.95))

y= beta0 + beta1*x + b1* x1 + b2* x2 + b3*x3  +
                     b4* x4 + b5* x5 + b6*x6  + 
                     b7* x7 + b8* x8 + b9*x9  +
                     b10*x10 + rnorm (100, 0, 0.2)
plot(x, y)


modelo=lm(y~x+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)

beta0_est=modelo[[1]][[1]]
beta1_est=modelo[[1]][[2]]
b1_est=modelo[[1]][[3]]
b2_est=modelo[[1]][[4]]
b3_est=modelo[[1]][[5]]
b4_est=modelo[[1]][[6]]
b5_est=modelo[[1]][[7]]
b6_est=modelo[[1]][[8]]
b7_est=modelo[[1]][[9]]
b8_est=modelo[[1]][[10]]
b9_est=modelo[[1]][[11]]
b10_est=modelo[[1]][[12]]
y_est=beta0_est+beta1_est*x+b1_est*x1+b2_est*x2+b3_est*x3+b4_est*x4+b5_est*x5+b6_est*x6+b7_est*x7+b8_est*x8+b9_est*x9+b10_est*x10
plot(x,y, pch=16, main =" Regresión latigo")
lines(x, y_est, col=2, lwd=2)
sum ((y-y_est)^2)











###################
#ej #######
x=seq(0,1, length=100)
beta0=0.2
beta1=0.5
b1= 1.0
b2=-1.0
b3=-0.5
b4= 0.5
b5= 2.0
b6= -2.0
b7= 1.5
b8= -1.5 
b9= 2.0
b10=-2
b11=-3
b12= 3
b13=-3.5
b14=2.8
b15=1.2
b16=-1.2
b17=2.7
b18=-3.6




x1=ifelse(x<0.10, 0, (x-0.10))
x2=ifelse(x<0.15, 0, (x-0.15))
x3=ifelse(x<0.20, 0, (x-0.20))
x4=ifelse(x<0.25, 0, (x-0.25))
x5=ifelse(x<0.30, 0, (x-0.30))
x6=ifelse(x<0.35, 0, (x-0.35))
x7=ifelse(x<0.40, 0, (x-0.40))
x8=ifelse(x<0.45, 0, (x-0.45))
x9=ifelse(x<0.50, 0, (x-0.50))
x10=ifelse(x<0.55,0, (x-0.55))
x11=ifelse(x<0.60, 0, (x-0.60))
x12=ifelse(x<0.65, 0, (x-0.65))
x13=ifelse(x<0.70, 0, (x-0.70))
x14=ifelse(x<0.75, 0, (x-0.75))
x15=ifelse(x<0.80, 0, (x-0.80))
x16=ifelse(x<0.85, 0, (x-0.85))
x17=ifelse(x<0.90, 0, (x-0.90))
x18=ifelse(x<0.95, 0, (x-0.95))

y= beta0 + beta1*x + b1* x1 + b2* x2 + b3*x3  +
  b4* x4 + b5* x5 + b6*x6  + 
  b7* x7 + b8* x8 + b9*x9  +
  b10*x10 + b11*x11 +b12*x13 +b13*x13 +b14*x14 +b15*x15 +b16*x16 +b17*x17 +b18*x18 +rnorm (100, 0, 0.2)
plot(x, y)


modelo=lm(y~x+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18)

beta0_est=modelo[[1]][[1]]
beta1_est=modelo[[1]][[2]]
b1_est=modelo[[1]][[3]]
b2_est=modelo[[1]][[4]]
b3_est=modelo[[1]][[5]]
b4_est=modelo[[1]][[6]]
b5_est=modelo[[1]][[7]]
b6_est=modelo[[1]][[8]]
b7_est=modelo[[1]][[9]]
b8_est=modelo[[1]][[10]]
b9_est=modelo[[1]][[11]]
b10_est=modelo[[1]][[12]]
b11_est=modelo[[1]][[13]]
b12_est=modelo[[1]][[14]]
b13_est=modelo[[1]][[15]]
b14_est=modelo[[1]][[16]]
b15_est=modelo[[1]][[17]]
b16_est=modelo[[1]][[18]]
b17_est=modelo[[1]][[19]]
b18_est=modelo[[1]][[20]]

y_est=beta0_est+beta1_est*x+b1_est*x1+b2_est*x2+b3_est*x3+b4_est*x4+b5_est*x5+b6_est*x6+b7_est*x7+b8_est*x8+b9_est*x9+b10_est*x10+b11_est*x11+b12_est*x12+b13_est*x13+b14_est*x14+b15_est*x15+b16_est*x16+b17_est*x17+b18_est*x18
plot(x,y, pch=16, main =" Regresión latigo")
lines(x, y_est, col=2, lwd=2)
sum ((y-y_est)^2)
