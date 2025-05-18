#------------------------------
# Ver página 22 libro de Koch
#------------------------------

A <- matrix(c(2.4, -0.5, -0.5, -1), nrow=2, byrow=TRUE)
A
ev=eigen(A)
t(ev$vectors[,1])%*%ev$vectors[,2]  #$U_1%*%U_2=0$
t(ev$vectors[,1])%*%ev$vectors[,1]  #$U_1%*%U_1=1$
t(ev$vectors[,2])%*%ev$vectors[,2]  #$U_1%*%U_1=1$


# Matriz de covarianza

sigma=c(0.1418, 0.0314, 0.0231, -0.1032, -0.0185, 0.0843,
  0.0314, 0.1303, 0.1084,  0.2158,  0.1050, -0.2093,
  0.0231, 0.1084, 0.1633,  0.2841,  0.1300, -0.2405,
 -0.1032, 0.2158, 0.2841,  2.0869,  0.1645, -1.0370,
 -0.0185, 0.1050, 0.1300,  0.1645,  0.6447, -0.5496,
  0.0843, -0.2093, -0.2405, -1.0370, -0.5496, 1.3277)

# vectores y valores propios

sigma=matrix(sigma, nrow=6, ncol=6)
valorespropios=eigen(sigma)$values
vectorespropios=eigen(sigma)$vectors

# Descomposicón espectral

# Sigma=UDV^{t}: U y V ortogonales (con vectores propios de Sigma)
# y D diagonal con los valores propios de sigma

svd(sigma)
round((svd(sigma)$u)%*%t(svd(sigma)$u),2)



#-----------------------------------------
# Ejemplo con datos reales: variables 
# socioeconómicas # de paises de Europa
#------------------------------------------

#------------------------------------------
# Componentes principales paso a paso
#-----------------------------------------


# Matriz de datos

Europa=read.csv("http://www.instantr.com/wp-content/uploads/2013/01/europe.csv", header=TRUE)
head(Europa)
datos=Europa[,2:8]

# Matriz de varianzas y covarianzas

sigma=var(datos)
is.matrix(sigma)

# Vectores y valores propios

eigen(sigma)
valprop=eigen(sigma)$values
vecprop=eigen(sigma)$vectors

# descomposición del valor singular de sigma

svd(sigma)

# Matriz de datos centrada

center_apply <- function(x) {
    apply(x, 2, function(y) y - mean(y))
}
datoscen=center_apply(datos)
summary(datoscen)

#---------------------------
# Componentes principales
#---------------------------

cp=matrix(rep(0, 28*7), nrow=28, ncol=7)
       for (j in 1:7)
            { 
             for (i in 1:7)
                  {
                  cp[,j]=cp[,j]+(vecprop[i,j]*datoscen[,i])
                   }
             }

round(var(cp),2)



#-----------------------------------------------------
# Componentes principales usando prcomp de R
#------------------------------------------------------

acp.cov <- prcomp(datos)
names(acp.cov)
acp.cov$x
round(acp.cov$x[,1]-cp[,1])
round(acp.cov$x[,2]-cp[,2])  # ojo: dan los mismos valores con signos contrarios