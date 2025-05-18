
#---------------------------------------------------
# Tomado de Marquez,C. 2017. Modelo de Regresión PLS
# Tesis de Grado. Universidad de Sevilla
#---------------------------------------------------

library(pls)
data("gasoline")
head(gasoline)
dim(gasoline)

# Graficos de as curvas de reflectania
matrizNIR=as.matrix(gasoline)
dim(matrizNIR)
indice=matrizNIR[,1]
matrizNIR=matrizNIR[,-1]
dim(matrizNIR)
x=seq(900,1700, by=2)
matplot(x, t(matrizNIR), type="l", ylab="Reflectancia", xlab="Longitud de oonda (nm)")

# Resumen del indice de octanaje

summary(indice)
boxplot(indice, horizontal=TRUE, col=4, xlab="Octanaje")


#--------------------------------------
# Muestra de entrenamiento y test
#------------------------------------

set.seed(1) 
n=nrow(gasoline) 
train=sample(1:n, floor(n*0.75)) 
test=setdiff(1:n,train)

#----------------------------------------
# ACP
#----------------------------------------

#--------------------------------------
# Rgresion por componentes principales
#--------------------------------------

pcr.fit=pcr(octane~., data=gasoline, subset=train, scale=TRUE, validation="CV")
names(pcr.fit)
explvar(pcr.fit) 
cumsum(explvar(pcr.fit))
plot(pcr.fit, plottype = "scores", comps = 1:3)

# Los tres primeros componentes explican más del 94% de la variabilidad
# de la reflectancia: En otras palabras con los tres componentes se resume la información
# de la reflectancia.
# Nota: Estos datos puden ser anlaizados a través de un modelos de 
# regresión funcional con respuesta escalar


# Pesos de cada longitudu de onda en la construcción de cada componente
plot(pcr.fit, "loadings", comps = 1:2, legendpos = "bottomleft", 
labels = "numbers", xlab = "nm") 
abline(h = 0) 
grid() 
colnames(gasoline$NIR)


# Modelo de regresion ACP ajustado con 3 CP
pcr.fit=pcr(octane~., data=gasoline, ncomp=3, subset=train, scale=TRUE)
?MSEP
ECM_PCR=MSEP(pcr.fit, estimate = "all")
ECM_PCR


# Predicción sobre la muestra test

pcr.pred=predict(pcr.fit,gasoline[test,],ncomp=3) 
ECM_test_PCR=mean((pcr.pred-gasoline$octane[test])^2) 
R2_test_PCR=cor(pcr.pred,gasoline$octane[test])^2 
R2_test_PCR
plot(gasoline$octane[test],pcr.pred, xlab="Octanaje",ylab="Predicción de Octanaje") 
abline(a=0,b=1,col="blue")
grid()



#----------------------------------------------------
# Regresión PLS
#--------------------------------------------------

pls.fit=plsr(octane~., data=gasoline,subset=train, scale=TRUE)
plot(pls.fit, plottype = "scores", comps = 1:3)
explvar(pls.fit) 
cumsum(explvar(pls.fit))
# Los 3 primeros componentes explican el 95% de la variabilidad
loadingplot(pls.fit, comps = 1:2, legendpos = "bottomleft", labels = "numbers", xlab = "nm") 
abline(h = 0) 
grid() 
colnames(gasoline$NIR)

