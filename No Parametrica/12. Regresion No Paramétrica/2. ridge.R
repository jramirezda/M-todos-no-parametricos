


#---------------------------------------------------
# Ejemplo de regresion ridge
# Tomado de Hastie, Tibshirani y Friedman (2009)
#---------------------------------------------------

# 97 pacientes: datos de cáncer de prostata

# * lcavol: volumen log-cancer
# * peso: peso de la próstata logarítmica
# * edad: edad del paciente
# * lbhp: log-cantidad de hiperplasia benigna
# * svi: invasión de vesículas seminales
# * lcp: penetración log-capsular
# * gleason: Puntuación de Gleason, visite http://en.wikipedia.org/wiki/Gleason_Grading_System
#   pgg45: percent of Gleason scores 4 or 5
#   lpsa is the response variable, log-psa


url <- "http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/prostate.data"
cancer <- read.table(url, header=TRUE)
cancer 
summary(cancer)
str(cancer)
library(car)
library(MASS)


# Modelo de regresión lineal
test=subset(cancer, train=="FALSE")
entreno = subset(cancer,train=="TRUE")  # Toma solo los datos de la muestra de entrenamiento (train =TRUE)
modelo_mco <- lm(lpsa~ . , data=entreno[,-c(5,10)]) # Se exluyen las variable categóricas
summary(modelo_mco)


# Diagnóstico de colinealidad
vif(modelo_mco)
# Hay dos variables con VIF > 2.5  
# No es muy alta la colinealidad 
# (algunos autores indican que el VIF > 2.5,  puede indicar colinealidad)


# Regresión Ridge
modelo_contraida <- lm.ridge(lpsa ~ ., data=entreno[,-c(5,10)], lambda = seq(0,10,0.1))
plot(seq(0,10,0.1), modelo_contraida$GCV, main="Busqueda lambda por GCV", type="l",
     xlab=expression(lambda), ylab="GCV")

# Selección de lambda
select(lm.ridge(lpsa ~ ., data=entreno[,-c(5,10)], lambda = seq(0,10,0.1)))
# lambda=4.

# Grafico

matplot(seq(0,10,0.1), coef(modelo_contraida)[,-1], xlim=c(0,11), type="l",xlab=expression(lambda), 
        ylab=expression(hat(beta)), col=2, lty=1, lwd=2, main="Coeficientes en función del sesgo")

text(rep(10, 9), coef(modelo_contraida)[length(seq(0,10,0.1)),-1], colnames(entreno)[-c(5,9,10)], 
     pos=4, col=4)

# Comparación del modelo lineal y la regresión ridge
# Varianza con el modelo lineal usando la muestra test

ajuste_mco <- predict(modelo_mco,test)
sum((test$lpsa-ajuste_mco)^2)

# Para el modelo por mínimos cuadrados obtenemos una suma de cuadrados del error de 17,5 
# con los datos ajustados del conjunto de datos de la muestra test


#Modelo con lambda optimo

modelo_contraida <- lm.ridge(lpsa ~ ., data=entreno[,-c(5,10)], lambda = 4.5)
#estos objetos no admiten predict
coeficientes <- as.vector(coef(modelo_contraida))
coeficientes=as.matrix(coeficientes)
dim(coeficientes)
matriz <- as.matrix(test[,-c(5,9,10)])
matriz <- cbind(rep(1,length=nrow(test)),matriz)
dim(matriz)
ajuste_contraida <- matriz %*% coeficientes
sum((test$lpsa- ajuste_contraida)^2)


# Para el modelo por regresion ridge obtenemos una suma de cuadrados del error de 16,5 
# con los datos ajustados del conjunto de datos de la muestra test


# Conclusión: El modelo ridge ajusta mejor la muestra test que el modelo lineal



