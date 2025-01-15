# Cargar librerías necesarias
library(ggplot2)
library(dplyr)

# Cargar los datos modificados desde un archivo CSV
data <- read.csv("enfermedades.csv")

# Ajustar el modelo de regresión exponencial
fit <- nls(sbp ~ beta0 * exp(beta1 * age), data = data, start = list(beta0 = 10, beta1 = 0.2))

# Resumen del modelo
summary(fit)

# Obtener los coeficientes ajustados
coef(fit)

# Crear un rango de valores para age para la predicción
age_new <- seq(min(data$age), max(data$age), length.out = 100)

# Predecir valores de sbp utilizando el modelo ajustado
sbp_pred <- predict(fit, newdata = list(age = age_new))

# Crear un nuevo dataframe para las predicciones
pred_data <- data.frame(age = age_new, sbp = sbp_pred)

# Graficar los datos originales y la curva ajustada
ggplot(data, aes(x = age, y = sbp)) +
  geom_point(color = "red") +
  geom_line(data = pred_data, aes(x = age, y = sbp), color = "black") +
  labs(title = "Modelo Exponencial Ajustado con Datos Modificados",
       x = "Edad",
       y = "Presión Arterial Sistólica") +
  theme_minimal()

# Obtener los residuos del modelo
residuos <- residuals(fit)

# Graficar el Q-Q plot de los residuos usando ggplot2
qqplot <- ggplot(data.frame(residuos), aes(sample = residuos)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot de los Residuos")

# Mostrar el Q-Q plot
print(qqplot)