
#------------------------------------------------------------------------
# Ejemplo simple de regresion ridge
# http://web.as.uky.edu/statistics/users/pbreheny/764-f11/notes/9-1.pdf
#-----------------------------------------------------------------------

library(MASS)
x1 <- rnorm(20)
x2 <- rnorm(20,mean=x1,sd=.01)   # Colinealidad con x1
x3= x1*4                         # Colinealidad perfecta

# Colinealidad
y <- rnorm(20,mean=3+x1+x2)
lm(y~x1+x2)$coef
lm.ridge(y~x1+x2,lambda=1)

# Colinealidad perfecta (MCO no funciona!)

y <- rnorm(20,mean=3+x1+x3)
lm(y~x1+x3)$coef
lm.ridge(y~x1+x3,lambda=1)


   