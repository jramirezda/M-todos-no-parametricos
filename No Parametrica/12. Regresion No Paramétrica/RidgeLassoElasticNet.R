

# Usar R 3.5.1


#--------------------------------------------
# Ejempo de Ridge, Lasso y Elastic Net tomado 
# de la libreria glmnet 
#--------------------------------------------

library(glmnet)

#---------------------
# Generación de datos
#---------------------

set.seed(19875)      # Set seed for reproducibility
n <- 1000            # Number of observations
p <- 5000            # Number of predictors included in model
real_p <- 15         # Number of true predictors
x <- matrix(rnorm(n*p), nrow=n, ncol=p)
y <- apply(x[,1:real_p], 1, sum) + rnorm(n)

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
# Ajuste de regresión Ridge, Lasso y Elastic Net
#-----------------------------------------------

# Fit models 
# (For plots on left):
fit.lasso <- glmnet(x.train, y.train, family="gaussian", alpha=1)
fit.ridge <- glmnet(x.train, y.train, family="gaussian", alpha=0)
fit.elnet <- glmnet(x.train, y.train, family="gaussian", alpha=.5)


# 10-fold Cross validation for each alpha = 0, 0.1, ... , 0.9, 1.0
# (For plots on Right)
for (i in 0:10) {
    assign(paste("fit", i, sep=""), cv.glmnet(x.train, y.train, type.measure="mse", 
                                              alpha=i/10,family="gaussian"))
}


# Gráfico de Soluciones


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


#-------------------------------------------
# SCAD 
#-------------------------------------------


library(ncvreg)
data(Prostate)
X <- Prostate$X
y <- Prostate$y
op <- par(mfrow=c(2,2))
fit <- ncvreg(X, y)
plot(fit, main=expression(paste(gamma,"=",3)))
fit <- ncvreg(X, y, gamma=10)
plot(fit, main=expression(paste(gamma,"=",10)))
fit <- ncvreg(X, y, gamma=1.5)
plot(fit, main=expression(paste(gamma,"=",1.5)))
fit <- ncvreg(X, y, penalty="SCAD")
plot(fit, main=expression(paste("SCAD, ",gamma,"=",3)))














