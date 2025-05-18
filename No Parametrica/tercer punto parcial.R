# Establecer una semilla para la reproducibilidad
set.seed(123)
library(sm)

# Generar datos de entrada X de forma aleatoria
X <- runif(100, 0, 10)

# Generar la variable de respuesta Y con una relación cuadrática y errores normales
beta0 <- 2  # Término constante
beta1 <- 2.5  # Coeficiente lineal
beta2 <- -2.3  # Coeficiente cuadrático

# Generar Y con una relación cuadrática y errores normales
Y <- beta0 + beta1 * X + beta2 * X^2 + rnorm(100, mean = 0, sd = 2)

# Ajustar una regresión spline
spline_model <- sm.regression(x = X, y = Y, poly.index = 3)

# Resumen del modelo
summary(spline_model)

# Ajustar una regresión kernel
kernel_model <- sm.regression(x = X, y = Y, model = "linear", ylab = "Y", main = "Linealidad", xlab = "X", col = 2, lwd = 2)

# Crear un gráfico de dispersión
plot(X, Y, col = "blue", pch = 19, xlab = "X", ylab = "Y", main = "Regresión Kernel vs Regresión Splines Cúbica")

# Agregar la curva estimada de splines penalizados en rojo
lines(X, fitted(spline_model), col = "red", lwd = 2)

# Agregar la curva estimada de regresión kernel en verde
lines(X, fitted(kernel_model), col = "green", lwd = 2)

# Crear una leyenda
legend("topleft", legend = c("Regresión Splines Cúbica", "Regresión Kernel (Lineal)"), col = c("red", "green"), lwd = 2, cex = 0.8)

# Mostrar el gráfico

