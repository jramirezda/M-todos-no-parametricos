#################################################
# II Congreso Colombiano de Estadística
# Regresión no paramétrica

#################################################

library(splines)

ISIT <- read.table("ISIT.txt", header = TRUE)
Fuentes16<-ISIT$Sources[ISIT$Station==16]
Prof16<-ISIT$SampleDepth[ISIT$Station==16]
plot(Prof16,Fuentes16, lty=1, main = "Diagrama de Puntos", ylab = "Fuentes", xlab = "Profundidad", pch=16)

##########################
#### Diferente kernel ####
##########################
hist(Fuentes16, main = "Histograma de Profundidad", xlab = "Profundidad", ylab = "Frecuencia" )
plot(density(Fuentes16, kernel = c("gaussian")), main = "Densidad de Profundidad
     Kernel Gaussiano")
par(mfrow=c(1,1))
plot(density(Fuentes16, kernel = c("epanechnikov")), main = "Densidad de Profundidad
     Kernel Epanechnikov")
plot(density(Fuentes16, kernel = c("rectangular")), main = "Densidad de Profundidad
     Kernel Rectangular")
plot(density(Fuentes16, kernel = c("triangular")), main = "Densidad de Profundidad
     Kernel Triangular")
plot(density(Fuentes16, kernel = c("biweight")), main = "Densidad de Profundidad
     Kernel Biweight")
plot(density(Fuentes16, kernel = c("cosine")), main = "Densidad de Profundidad
     Kernel Arco Coseno")
plot(density(Fuentes16, kernel = c("optcosine")), main = "Densidad de Profundidad
     Kernel Coseno")

##########################
#### Diferente bandw ####
##########################
par(mfrow=c(2,3))
plot(density(Fuentes16,bw=0.5), main = "Densidad de Profundidad
     BW = 0.5")
plot(density(Fuentes16,bw=1), main = "Densidad de Profundidad
     BW = 1")
plot(density(Fuentes16, bw=3), main = "Densidad de Profundidad
     BW = 3")
plot(density(Fuentes16, bw=5), main = "Densidad de Profundidad
     BW = 5")
plot(density(Fuentes16, bw=10), main = "Densidad de Profundidad
     BW = 10")
plot(density(Fuentes16, bw=50), main = "Densidad de Profundidad
     BW = 50")

############################
#### Regresión Centrada ####
############################

plot(Fuentes16, pch=16)
hist(Fuentes16, main = "Histograma de Profundidad", xlab = "Profundidad", ylab = "Frecuencia" )
plot(density(Fuentes16),col=2, main = "Densidad de Profundidad")

fit <- lm(Fuentes16 ~ Prof16)
summary(fit)
x = Prof16 - mean(Prof16)
fit1 <- lm(Fuentes16 ~ x)
summary(fit1)
plot(Prof16, Fuentes16, pch=16 )
lines(Prof16, fit$fitted.values, col=2)

#########################
### Polinomios Locales ##
#########################

library(KernSmooth)
library(sm)
library(locpol)

deg <- 1
kernel <- EpaK
bw <- .25
xeval <- 0:100/100
locCteSmootherC(Prof16, Fuentes16, xeval, bw, kernel, weig = rep(1, length(Fuentes16)))
locLinSmootherC(Prof16, Fuentes16, xeval, bw, kernel, weig = rep(1, length(Fuentes16)))
locCuadSmootherC(Prof16, Fuentes16, xeval, bw, kernel, weig = rep(1, length(Fuentes16)))


#-------------------------------
# Splines Lineales
#--------------------------------


data <- cbind(Fuentes16, Prof16)
rango <- (max(Prof16) - min(Prof16))/4
nodos <- c(min(Prof16) +rango, min(Prof16) + 2*rango, min(Prof16) + 3*rango)

Prof161 = (Prof16 - nodos[1])
Prof161[Prof161<0] = 0
Prof162 = (Prof16 - nodos[2])
Prof162[Prof162<0] = 0
Prof163 = (Prof16 - nodos[3])
Prof163[ Prof163<0 ] = 0
print(cbind(Prof16, Prof161, Prof162, Prof163))



fit <- lm(Fuentes16 ~ Prof16 + Prof161 + Prof162 + Prof163 )
print( summary( fit ) )
fitted.mean <- predict( fit )
plot(Prof16, Fuentes16, pch=16,xlab = "Profundidad", ylab = "Fuentes", main="Splines Lineales")
abline(v=nodos[1],lty=2)
abline(v=nodos[2],lty=2)
abline(v=nodos[3],lty=2)
lines(Prof16, Fuentes16 , col="red", lwd=2)
lines(Prof16, fitted.mean, col="blue", lwd=2 )
legend(locator(1), col = c("red","blue"), c("Curva Media","Curva Ajustada 
con Spline Lineal"), border="white", fill=c("red","blue"),bty="n")

ksmooth(Prof16, Fuentes16, kernel = c("box", "normal"), bandwidth = 0.5,
        range.x = range(x),
        n.points = max(100L, length(x)))

#-----------------------------
# Splines Cúbicos 
#----------------------------
Prof16.squared = Prof16^2
Prof16.cubed   = Prof16^3
Prof161.cubed  = Prof161^3
Prof162.cubed =  Prof162^3
Prof163.cubed =  Prof163^3

##### Fit the model:

fit <- lm( Fuentes16 ~ Prof16 + Prof16.squared + Prof16.cubed + Prof161.cubed + Prof162.cubed + Prof163.cubed )
print(summary( fit ))
fitted.mean <- predict( fit )
plot(Prof16 , Fuentes16, pch=16)
lines(Prof16 , Fuentes16, col="red", ltw=2)
lines(Prof16 , sort(fitted.mean,decreasing = T), col="blue", ltw=2)
abline(v=nodos[1],lty=2)
abline(v=nodos[2],lty=2)
abline(v=nodos[3],lty=2)

# Usando una función de R
ispl=interpSpline(Prof16, Fuentes16)
plot(ispl, col=2, main="Regresión Splines Cúbicos", xlab="Profundidad", ylab="Bioluminiscencia  pelágica", lwd=2)
points(Prof16, Fuentes16, pch=16)
splineKnots(ispl)



