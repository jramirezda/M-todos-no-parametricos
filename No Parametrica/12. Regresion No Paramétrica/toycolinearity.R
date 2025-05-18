
#-------------------------------------------
# Ejemplo simple del problema de colinealidad
#-------------------------------------------

X1=rnorm(100, 10, 2)
X2=X1+3
X3=X1+X2
X=cbind(X1,X2,X3)
is.matrix(X)
mat2=t(X)%*%X
dim(mat2)
solve(mat2)