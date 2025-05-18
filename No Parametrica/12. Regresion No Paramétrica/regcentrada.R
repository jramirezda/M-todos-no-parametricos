#-----------------------------------------------------------
# Código R para diapositivas de regresión no paramétrica
#---------------------------------------------------------


set.seed(8)
par(mfrow=c(2,2))
# Regresión Simple
x=seq(0, 20, length=100)
y= 100-2.5*x+rnorm(length(x),0,5)
reg=lm(y~x)
names(reg)
reg$coefficients
plot(x,y, main= expression(hat(y)==99.76-2.52*x))
lines(x, reg$fitted.values, col=2, lwd=2)

# Regresión simple centrada en x=media

x1=x-mean(x)
reg2=lm(y~x1)
names(reg2)
reg2$coefficients
plot(x1,y, main=expression(hat(y)==74.5-2.52*(x-bar(x))),xlab = expression(x-bar(x)))
lines(x1, reg2$fitted.values, col=2, lwd=2)
abline(v=0, col=4)
abline(h=mean(y), col=4)

# Regresión simple centrada en x=5

x2=x-5
reg3=lm(y~x2)
names(reg3)
reg3$coefficients
plot(x2,y, main=expression(hat(y)==87.14-2.52*(x-5)),xlab = expression(x-5))
lines(x2, reg3$fitted.values, col=2, lwd=2)
abline(v=0, col=4)
abline(h=87.14, col=4)


# Regresión simple centrada en x=15

x3=x-15
reg4=lm(y~x3)
names(reg4)
reg4$coefficients
plot(x3,y, main=expression(hat(y)==61.91-2.52*(x-15)),xlab = expression(x-15))
lines(x3, reg4$fitted.values, col=2, lwd=2)
abline(v=0, col=4)
abline(h=61.91, col=4)




