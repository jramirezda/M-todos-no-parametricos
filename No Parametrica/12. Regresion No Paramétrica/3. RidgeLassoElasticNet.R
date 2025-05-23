

# Usar R 3.5.1


#--------------------------------------------
# Ejempo de Ridge, Lasso y Elastic Net tomado 
# de la libreria glmnet 
#--------------------------------------------

library(glmnet)

#-----------------------------
# Ejemplo peque�o
#------------------------------

# Nota: df= n�mero de coeficientes distintos de cero para cda valor de lambda
# ?glmnet
# Nota: alpha=0 es penalidad ridge, 
#       alpha=1 es penalidad lasso
#       alpha=0.5 es penalidad elastic net


# Generaci�n de datos 
set.seed(19875)
x=matrix(rnorm(100*20),100,20)
y=rnorm(100)

# Ajuste de regresi�n ridge

fit1=glmnet(x,y, alpha=0)  # Regresi�n ridge
plot(fit1, xvar="lambda")
print(fit1)
coef(fit1,s=2) # extract coefficients at a single value of lambda
pred1=predict(fit1,newx=x[1:10,],s=2) # make predictions
cbind(y[1:10],pred1)

# Suma de cuadrados de l error de predicci�n ridge (10 primeros valores)

sum1=0
for (i in 1:10)
     {
      sum1=sum1+(y[i]-pred1[i])^2
     }
sum1

# Ajuste de regresi�n lasso

fit2=glmnet(x,y, alpha=1)  # Regresi�n lasso
plot(fit2, xvar="lambda")
print(fit2)
coef(fit2,s=0.12) # extract coefficients at a single value of lambda (5 variables)
pred2=predict(fit2,newx=x[1:10,],s=0.12) # make predictions
cbind(y[1:10],pred2)

# Suma de cuadrados del error de predicci�n lasso (10 primeros valores)

sum2=0
for (i in 1:10)
     {
      sum2=sum2+(y[i]-pred2[i])^2
     }
sum2
sum1



# Ajuste de regresi�n elastic-net

fit3=glmnet(x,y, alpha=0.5)  # Regresi�n elastic-net
plot(fit3, xvar="lambda")
print(fit3)
coef(fit3,s=0.24) # extract coefficients at a single value of lambda (5 variables)
pred3=predict(fit3,newx=x[1:10,],s=0.24) # make predictions

# Suma de cuadrados del error de predicci�n lasso (10 primeros valores)

sum3=0
for (i in 1:10)
     {
      sum3=sum3+(y[i]-pred3[i])^2
     }
sum3
sum2
sum1

# Comparaci�n 

c(sum1,sum2,sum3)

# Con el ejemplo de juguete es mejor elastic-net
# debe partirse la muestra en una de entranamiento y una test!


#------------------------------------------------
# Otro ejemplo con una base de datos grande
#----------------------------------------------


#---------------------
# Generaci�n de datos
#---------------------

set.seed(19875)      # Set seed for reproducibility
n <- 1000            # Number of observations
p <- 5000            # Number of predictors included in model
real_p <- 15         # Number of true predictors
x <- matrix(rnorm(n*p), nrow=n, ncol=p)
head(x)
y <- apply(x[,1:real_p], 1, sum) + rnorm(n)
head(y)

cor(y,x[,1:15])  #Hay correlaci�n de y con las primeras 15 variables
cor(y, x[, 50:75]) # no hay correlaci�n de y con las otras

#---------------------------------
# Muestras de entrenamiento y test
#----------------------------------

# Se parten los datos en los conjuntos entrenamiento(2/3) y test (1/3) 
train_rows <- sample(1:n, .66*n)
x.train <- x[train_rows, ]
x.test <- x[-train_rows, ]
y.train <- y[train_rows]
y.test <- y[-train_rows]

#-----------------------------------------------
# Ajuste de regresi�n Ridge, Lasso y Elastic Net
#-----------------------------------------------

# Fit models 
# (For plots on left):

fit.lasso <- glmnet(x.train, y.train, family="gaussian", alpha=1)
fit.ridge <- glmnet(x.train, y.train, family="gaussian", alpha=0)
fit.elnet <- glmnet(x.train, y.train, family="gaussian", alpha=.5)



# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
# (For plots on Right)

for (i in 0:10) {
    assign(paste("fit", i, sep=""), cv.glmnet(x.train, y.train, type.measure="mse", alpha=i/10,family="gaussian"))
}


# Gr�fico de Soluciones


par(mfrow=c(3,2))
# For plotting options, type '?plot.glmnet' in R console
#par(mfrow=c(1,2))
plot(fit.lasso, xvar="lambda")
plot(fit10, main="LASSO")

#par(mfrow=c(1,2))
plot(fit.ridge, xvar="lambda")
plot(fit0, main="Ridge")

#par(mfrow=c(1,2))
plot(fit.elnet, xvar="lambda")
plot(fit5, main="Elastic Net")

fit.lasso
fit.ridge
fit.elnet


#-------------------------------------------
# SCAD 
#-------------------------------------------


library(ncvreg)  
data(Prostate) 
?Prostate
head(Prostate)

# Ejemplo de la librer�a 
X <- Prostate$X  # hay un error del ejemplo: svi es categ�rica (0,1)
y <- Prostate$y  # 
op <- par(mfrow=c(2,2))
fit <- ncvreg(X, y)
plot(fit, main=expression(paste(gamma,"=",3)))
fit <- ncvreg(X, y, gamma=10)
plot(fit, main=expression(paste(gamma,"=",10)))
fit <- ncvreg(X, y, gamma=1.5)
plot(fit, main=expression(paste(gamma,"=",1.5)))
fit <- ncvreg(X, y, penalty="SCAD")
plot(fit, main=expression(paste("SCAD, ",gamma,"=",3)))

# Modificaci�n del ejemplo sacando svi

X <- Prostate$X[,-5]  
y <- Prostate$y  # 
op <- par(mfrow=c(2,2))
fit <- ncvreg(X, y)
plot(fit, main=expression(paste(gamma,"=",3)))
fit <- ncvreg(X, y, gamma=10)
plot(fit, main=expression(paste(gamma,"=",10)))
fit <- ncvreg(X, y, gamma=1.5)
plot(fit, main=expression(paste(gamma,"=",1.5)))
fit <- ncvreg(X, y, penalty="SCAD")
plot(fit, main=expression(paste("SCAD, ",gamma,"=",3)))

fit




#-------------------------------------
# nonnegative garrote
#-------------------------------------

# Librer�a?







