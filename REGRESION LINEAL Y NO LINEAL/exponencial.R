# Datos
dias <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
n_casos <- c(7.97, 3.12, 13.23, 25.36, 12.85, 20.44, 49.96, 58.93, 72.19, 120.76, 168.36, 254.84, 391.66, 564.73, 858.54, 1308.06, 1960.39, 2958.93, 4424.60, 6636.39)

# Transformación logarítmica de n_casos
log_y <- log(n_casos)

# Ajuste del modelo lineal
model <- lm(log_y ~ dias)  # Aquí se ajusta el modelo lineal a los datos transformados

# Resumen del modelo
summary(model)

# Coeficientes del modelo transformado
B0 <- coef(model)[1]  # Intercepto del modelo lineal en la escala transformada
B1 <- coef(model)[2]  # Pendiente del modelo lineal en la escala transformada
B0
B1

# Revertimos la transformación 
#para obtener los coeficientes originales del modelo exponencial

a <- exp(B0)  # Intercepto del modelo exponencial
b <- B1  # Pendiente del modelo exponencial
a
b

# Predicciones del modelo exponencial
y_fit <- a * exp(b * dias)  # Aquí se calculan las predicciones del modelo exponencial
y_fit

# Visualización de los datos y el ajuste exponencial
plot(dias, n_casos, main = "Ajuste de Regresión Exponencial", xlab = "Días", ylab = "Número de casos", pch = 19)
lines(dias, y_fit)
legend("topleft", legend = c("Datos originales", "Ajuste exponencial"), col = c("black", "red"), pch = 19, lty = 1, lwd = 2)



# Estimación y visualización del modelo exponencial
y_est <- a * exp(b * dias)
plot(dias, y_est )
points(dias, n_casos, pch = 19)

# Resumen del modelo ajustado
summary(lm(y_est ~ dias))

