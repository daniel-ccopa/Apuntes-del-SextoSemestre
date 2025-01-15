# Cargar la biblioteca neuralnet
library(neuralnet)
library(readr)
library(dplyr)
library(ggplot2)
library(GGally)

# Cargar el conjunto de datos
data <- read_csv("SAHeart.csv")

# Visualizar las primeras filas del conjunto de datos
head(data)

# Resumen estadístico de las variables
summary(data)



# Eliminar filas con valores faltantes
data_clean <- na.omit(data)

# Convertir la variable famhist a binaria
data_clean$famhist <- ifelse(data_clean$famhist == "Present", 1, 0)

# Verificar la limpieza de los datos
summary(data_clean)



# Matriz de gráficos de dispersión
ggpairs(data_clean, aes(color = as.factor(chd)))




# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(123)
training_index <- sample(1:nrow(data_clean), 0.7 * nrow(data_clean))
training_data <- data_clean[training_index, ]
testing_data <- data_clean[-training_index, ]

# Asegurarse de que las columnas numéricas están en formato numérico
training_data <- training_data %>% mutate_if(is.character, as.numeric)
testing_data <- testing_data %>% mutate_if(is.character, as.numeric)

# Ajustar los parámetros stepmax y threshold para mejorar la convergencia
nn_model <- neuralnet(chd ~ sbp + tobacco + ldl + adiposity + famhist + typea + obesity + alcohol + age, 
                      data = training_data, hidden = c(5,3), linear.output = FALSE, stepmax = 1e6, threshold = 0.01)

# Evaluar el modelo en el conjunto de prueba
test_data_features <- subset(testing_data, select = -chd)
test_data_features <- as.data.frame(lapply(test_data_features, as.numeric))

# Asegurarse de que las columnas coinciden con las del modelo entrenado
if(!all(colnames(test_data_features) %in% colnames(training_data))){
  stop("Las columnas de los datos de prueba no coinciden con las del modelo entrenado.")
}

# Evaluar el modelo en el conjunto de prueba
predictions <- compute(nn_model, test_data_features)

# Convertir los resultados de la predicción en clases
predicted_classes <- ifelse(predictions$net.result > 0.5, 1, 0)

# Calcular métricas de evaluación
confusion_matrix <- table(predicted_classes, testing_data$chd)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
sensitivity <- confusion_matrix[2,2] / sum(confusion_matrix[2, ])
specificity <- confusion_matrix[1,1] / sum(confusion_matrix[1, ])

cat("Accuracy: ", accuracy, "\n")
cat("Sensitivity: ", sensitivity, "\n")
cat("Specificity: ", specificity, "\n")
