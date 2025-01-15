# F = y
# v = x
# Datos observados
x <- c(10, 20, 30, 40, 50, 60, 70, 80)
y <- c(25, 70, 380, 550, 610, 1220, 830, 1450)

# Transformación logarítmica de F
log_y <- log(y)

# Ajuste del modelo lineal
model <- lm(log_y ~ x)

# Resumen del modelo
summary(model)

# Coeficientes del modelo transformado
B0 <- coef(model)[1]  # Intercepto del modelo lineal en la escala transformada
B1 <- coef(model)[2]  # Pendiente del modelo lineal en la escala transformada
B0
B1

# Revertimos la transformación para obtener los coeficientes originales del modelo exponencial
a <- exp(B0)  # Intercepto del modelo exponencial
b <- B1  # Pendiente del modelo exponencial
a
b

# Predicciones del modelo exponencial
y_fit <- a * exp(b * x)
y_fit

# Visualización de los datos y el ajuste exponencial
plot(x, y, main = "Ajuste de Regresión Exponencial", xlab = "Velocidad (m/s)", ylab = "Fuerza (N)", pch = 19)
lines(x, y_fit, col = "red")
legend("topleft", legend = c("Datos originales", "Ajuste exponencial"), col = c("black", "red"), pch = 19, lty = 1, lwd = 2)



# Datos observados
x <- c(10, 20, 30, 40, 50, 60, 70, 80)
y <- c(25, 70, 380, 550, 610, 1220, 830, 1450)

# Transformación inversa
inv_y <- 1 / y
inv_x <- 1 / x

# Ajuste del modelo lineal a la transformación inversa
model_saturation <- lm(inv_y ~ inv_x)

# Resumen del modelo
summary(model_saturation)

# Coeficientes del modelo transformado
coef_saturation <- coef(model_saturation)
coef_saturation
B0 <- coef_saturation[1]# Intercepto del modelo lineal en la escala transformada
B1 <- coef_saturation[2]  # Pendiente del modelo lineal en la escala transformada
B0
B1


# Revertimos la transformación para obtener los coeficientes originales del modelo de saturación
a <- 1 / B0  # Coeficiente del numerador
b <- B1 / B0  # Coeficiente del denominador
a
b

# Predicciones del modelo de saturación
y_fit_saturation <- a * x / (b + x)

# Visualización de los datos y el ajuste del modelo de saturación
plot(x, y, main = "Ajuste del Modelo de Saturación", xlab = "Velocidad (m/s)", ylab = "Fuerza (N)", pch = 19)
lines(x, y_fit_saturation, col = "red")
legend("topleft", legend = c("Datos originales", "Ajuste de saturación"), col = c("black", "red"), pch = 19, lty = 1, lwd = 2)

# Mostrar los coeficientes
cat("Coeficiente alpha3:", a, "\n")
cat("Coeficiente beta3:", b, "\n")

