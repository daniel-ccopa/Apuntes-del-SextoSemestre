##########################
### CLASE DEL 20/06/24 ###
##########################

data = read.csv("/home/daniel/Descargas/cdh.csv", sep=";")
#View(data)

# Preparar las variables
X = as.matrix(cbind(1, data$AGE))  # Agregar una columna de 1s para el intercepto
y = data$CHD

# Función para calcular P(y|X, beta)
predict_prob = function(X, beta) {
  return(1 / (1 + exp(-X %*% beta)))
}

# Inicialización de beta
beta = matrix(0, ncol = 1, nrow = ncol(X))

# Parámetros del algoritmo de Newton-Raphson
max_iter = 100
tol = 1e-6

# Iteraciones del algoritmo de Newton-Raphson
for (i in 1:max_iter) {
  p = predict_prob(X, beta)
  W = diag(as.vector(p * (1 - p)))
  z = X %% beta + solve(W) %% (y - p)
  H = t(X) %% W %% X
  grad = t(X) %*% (y - p)
  beta_new = beta + solve(H) %*% grad
  if (sum(abs(beta_new - beta)) < tol) {
    beta = beta_new
    break
  }
  beta = beta_new
}

# Coeficientes obtenidos manualmente
beta

ind = data$AGE
dep = data$CHD

plot(ind, dep)

# Agregar la curva de la regresión logística
curve(predict_prob(cbind(1, x), beta), add = TRUE, col = "blue")

# Función para predecir la probabilidad para un nuevo valor de edad
predict_probability <- function(age, beta) {
  X_new <- cbind(1, age)  # Agregar 1 para el intercepto
  prob <- 1 / (1 + exp(-X_new %*% beta))
  return(prob)
}

predict_probability2 <- function(age, beta) {
  X_new <- cbind(1, age)  # Agregar 1 para el intercepto
  prob <- exp(X_new %% beta) / (1 + exp(X_new %% beta))
  return(prob)
}
# Ejemplo de predicción para una edad específica
edad_nueva <- 50
probabilidad_predicha <- predict_probability2(edad_nueva, beta)
probabilidad_predicha

