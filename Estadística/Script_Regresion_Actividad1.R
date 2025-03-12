# Cargar librerías necesarias
library(readxl) 
library(dplyr)
library(car)
library(ggplot2)
library(GGally)
library(patchwork)
library(Metrics)
library(glmnet)
library(caret)
library(lmtest)
library(knitr)

# ------------------- Carga y Preparación de Datos ------------------- #

# Cargar el dataset
data <- read_excel("Hotels.xlsx")

# Eliminar los nulos
data <- na.omit(data)

# Ver la estructura (tipo de datos en cada columna)
str(data)

# Resumen estadístico
summary(data)

# Número total de filas y columnas
dim(data)  # [filas, columnas]

# Nombre de las columnas
colnames(data)

# ------------------- Análisis Exploratorio de Datos (EDA) ------------------- #

# ---- HISTOGRAMAS ---- #
# Seleccionar variables numéricas excluyendo "ID"
numeric_vars <- setdiff(names(data)[sapply(data, is.numeric)], "ID")

# Crear lista de histogramas
plots <- lapply(numeric_vars, function(var) {
  ggplot(data, aes_string(x = var)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
    labs(title = paste("Histograma de", var), x = var, y = "Frecuencia") +
    theme_minimal()
})

# Organizar en 2 filas y 4 columnas
final_plot <- wrap_plots(plots) + plot_layout(ncol = 4, nrow = 2)
print(final_plot)

# ---- Gráficos de Dispersión ---- #
# Seleccionar variables numéricas excluyendo "ID" y "Price" (ya que Price es el eje Y)
numeric_vars <- setdiff(names(data)[sapply(data, is.numeric)], c("ID", "Price"))

# Crear lista de gráficos de dispersión
plots <- lapply(numeric_vars, function(var) {
  ggplot(data, aes_string(x = var, y = "Price")) +
    geom_point(color = "blue", alpha = 0.6) +
    geom_smooth(method = "lm", color = "red", se = FALSE) +  # Línea de tendencia
    labs(title = paste("Price vs", var), x = var, y = "Price") +
    theme_minimal()
})

# Organizar en 2 filas y 4 columnas
final_plot <- wrap_plots(plots) + plot_layout(ncol = 4, nrow = 2)
print(final_plot)

# --- Matriz de Correlaciones --- #
# Seleccionar solo variables numéricas (excluyendo "ID")
numeric_vars <- setdiff(names(data)[sapply(data, is.numeric)], "ID")

# Calcular la matriz de correlación
cor_matrix <- cor(data[, numeric_vars], use = "complete.obs")

# Mostrar la matriz en consola
print(cor_matrix)

# Graficar la matriz de correlación
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

# ------------------- Modelado y Evaluación de Modelos ------------------- #

# ------ Regresión Lineal Simple ------- #
# Ajustar el modelo de regresión lineal
modelo <- lm(Price ~ Customer_Rating, data = data)

# Mostrar resumen del modelo
summary(modelo)

# Predicciones del modelo
predicciones <- predict(modelo, newdata = data)

# Calcular métricas de evaluación
rmse_value <- rmse(data$Price, predicciones)  # Raíz del error cuadrático medio
mae_value <- mae(data$Price, predicciones)    # Error absoluto medio
r2_value <- summary(modelo)$r.squared         # R-cuadrado

# Mostrar métricas
cat(sprintf("RMSE: %.2f  |  MAE: %.2f  |  R²: %.4f\n", rmse_value, mae_value, r2_value))

# Gráfico de regresión lineal
ggplot(data, aes(x = Customer_Rating, y = Price)) +
  geom_point(color = "blue", alpha = 0.6) +  # Puntos de dispersión
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Línea de regresión
  labs(title = "Regresión Lineal: Price vs Customer_Rating",
       x = "Customer Rating", y = "Price") +
  theme_minimal()

# Gráfico de residuos vs valores ajustados
ggplot(data, aes(x = predicciones, y = resid(modelo))) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuos vs. Valores Ajustados",
       x = "Valores Ajustados", y = "Residuos") +
  theme_minimal()

# Histograma de residuos
ggplot(data, aes(x = resid(modelo))) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de Residuos", x = "Residuos", y = "Frecuencia") +
  theme_minimal()

# QQ-Plot de residuos
qqnorm(resid(modelo))
qqline(resid(modelo), col = "red", lwd = 2)

# ---- Lineal Polinómico ---- #
# Ajustar modelo de regresión polinómica (grado 2)
modelo_poly2 <- lm(Price ~ poly(Customer_Rating, 2), data = data)

# Mostrar resumen del modelo
summary(modelo_poly2)

# ------------------- Regresión Múltiple ------------------- #
# Ajustar el modelo de regresión múltiple
modelo_multi <- lm(Price ~ Distance + Rooms + Room_Squares + Beach + Parking, data = data)

# Mostrar resumen del modelo
summary(modelo_multi)

# Predicciones del modelo
predicciones_multi <- predict(modelo_multi, newdata = data)

# Calcular métricas de error
rmse_value <- rmse(data$Price, predicciones_multi)  # Error cuadrático medio
mae_value <- mae(data$Price, predicciones_multi)    # Error absoluto medio
r2_value <- summary(modelo_multi)$r.squared         # R²

# Mostrar métricas en una sola fila
cat(sprintf("RMSE: %.2f  |  MAE: %.2f  |  R²: %.4f\n", rmse_value, mae_value, r2_value))

# Gráfico de valores verdaderos vs predicciones
ggplot(data, aes(x = predicciones_multi, y = Price)) +
  geom_point(color = "blue", alpha = 0.6) +  # Puntos de dispersión
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", size = 1.2) +  # Línea ideal
  labs(title = "Valores Verdaderos vs. Predicciones (Regresión Múltiple)",
       x = "Precio Predicho", y = "Precio Real") +
  theme_minimal()

# Gráfico de residuos vs valores ajustados
ggplot(data, aes(x = predicciones_multi, y = resid(modelo_multi))) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuos vs. Valores Ajustados",
       x = "Valores Ajustados", y = "Residuos") +
  theme_minimal()

# Histograma de residuos
ggplot(data, aes(x = resid(modelo_multi))) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de Residuos", x = "Residuos", y = "Frecuencia") +
  theme_minimal()

# QQ-Plot de residuos
qqnorm(resid(modelo_multi))
qqline(resid(modelo_multi), col = "red", lwd = 2)

# ------------------- Regularización Ridge-Lasso ------------------- #
# Definir matriz de predictores (X) y variable objetivo (y)
X <- as.matrix(data[, c("Distance", "Rooms", "Room_Squares")])
y <- data$Price

# Definir una secuencia de valores de lambda (10^ de -4 a 10^4)
lambda_seq <- 10^seq(-4, 4, length = 100)

# Ajustar modelo Ridge
modelo_ridge <- glmnet(X, y, alpha = 0, lambda = lambda_seq)

# Validación cruzada para encontrar el mejor lambda
cv_ridge <- cv.glmnet(X, y, alpha = 0, lambda = lambda_seq)

# Obtener el mejor lambda
best_lambda_ridge <- cv_ridge$lambda.min

# Ajustar modelo final con el mejor lambda
modelo_ridge_final <- glmnet(X, y, alpha = 0, lambda = best_lambda_ridge)

# Mostrar coeficientes del modelo final
coef(modelo_ridge_final)

# Ajustar modelo Lasso
modelo_lasso <- glmnet(X, y, alpha = 1, lambda = lambda_seq)

# Validación cruzada para encontrar el mejor lambda
cv_lasso <- cv.glmnet(X, y, alpha = 1, lambda = lambda_seq)

# Obtener el mejor lambda
best_lambda_lasso <- cv_lasso$lambda.min

# Ajustar modelo final con el mejor lambda
modelo_lasso_final <- glmnet(X, y, alpha = 1, lambda = best_lambda_lasso)

# Mostrar coeficientes del modelo final
coef(modelo_lasso_final)

# Predicciones con los mejores modelos
predicciones_ridge <- predict(modelo_ridge_final, s = best_lambda_ridge, newx = X)
predicciones_lasso <- predict(modelo_lasso_final, s = best_lambda_lasso, newx = X)

# Calcular métricas de error
rmse_ridge <- RMSE(predicciones_ridge, y)
rmse_lasso <- RMSE(predicciones_lasso, y)

mae_ridge <- MAE(predicciones_ridge, y)
mae_lasso <- MAE(predicciones_lasso, y)

# Función para calcular R^2
r2 <- function(y_true, y_pred) {
  ss_res <- sum((y_true - y_pred)^2)
  ss_tot <- sum((y_true - mean(y_true))^2)
  return(1 - ss_res / ss_tot)
}

r2_ridge <- r2(y, predicciones_ridge)
r2_lasso <- r2(y, predicciones_lasso)

cat(sprintf("Ridge -> RMSE: %.2f  |  MAE: %.2f  |  R^2: %.2f\n", rmse_ridge, mae_ridge, r2_ridge))
cat(sprintf("Lasso -> RMSE: %.2f  |  MAE: %.2f  |  R^2: %.2f\n", rmse_lasso, mae_lasso, r2_lasso))

# ------------------- Regresión Logística ------------------- #

# Cargar dataset de cancelaciones
data_cancellation <- read.csv("Hotel_Cancellation.csv", stringsAsFactors = FALSE)

# Seleccionar solo variables numéricas
numeric_vars <- select(data_cancellation, where(is.numeric))

# Calcular la correlación de is_canceled con las demás variables
correlations <- cor(numeric_vars, use = "pairwise.complete.obs")["is_canceled", ]

# Mostrar las correlaciones en orden descendente
correlations_sorted <- sort(correlations, decreasing = TRUE)
print(correlations_sorted)

# Preparar datos para regresión logística
data_logistic <- select(data_cancellation, is_canceled, adults, children, babies, 
                        is_repeated_guest, previous_cancellations, 
                        previous_bookings_not_canceled, booking_changes, 
                        adr,  lead_time)

# Ver estructura del dataset
str(data_logistic)

# Resumen estadístico
summary(data_logistic)

# Balance de cancelaciones
data_balance <- data_logistic %>%
  group_by(is_canceled) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(count / sum(count) * 100, 1))  # Calcular %

# Gráfico de barras con porcentajes
ggplot(data_balance, aes(x = factor(is_canceled), y = count, fill = factor(is_canceled))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5, size = 5) +  # Mostrar %
  scale_fill_manual(values = c("blue", "red"), labels = c("No Cancelado", "Cancelado")) +
  labs(title = "Balance de Cancelaciones", x = "Estado de Reserva", y = "Cantidad") +
  theme_minimal()

# Histogramas de variables
vars_to_plot <- c("adults", "children", "babies", "is_repeated_guest", 
                  "previous_cancellations", "previous_bookings_not_canceled", 
                  "booking_changes", "adr", "lead_time")

# Crear una lista de histogramas
hist_plots <- lapply(vars_to_plot, function(var) {
  ggplot(data_logistic, aes(x = .data[[var]], fill = factor(is_canceled))) +
    geom_histogram(alpha = 0.6, bins = 30, position = "identity") +
    scale_fill_manual(values = c("blue", "red"), labels = c("No Cancelado", "Cancelado")) +
    labs(title = paste("Distribución de", var), x = var, y = "Frecuencia") +
    theme_minimal()
})

# Unir los gráficos en un solo panel
wrap_plots(hist_plots, ncol = 2)  # 2 columnas para organizar mejor

# Ajustar la regresión logística
logistic_model <- glm(is_canceled ~ ., data = data_logistic, family = binomial)

# Resumen del modelo
summary(logistic_model)

# Predicciones en probabilidades
predicted_probs <- predict(logistic_model, type = "response")

# Convertir probabilidades a clases (umbral 0.5)
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

# Crear matriz de confusión
conf_matrix <- table(Predicho = predicted_classes, Real = data_logistic$is_canceled)

# Calcular porcentajes de acierto y error
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix) * 100
error_rate <- 100 - accuracy

# Convertir la matriz de confusión a formato data frame para ggplot2
conf_matrix_df <- as.data.frame(as.table(conf_matrix))
colnames(conf_matrix_df) <- c("Predicho", "Real", "Frecuencia")

# Agregar porcentaje de cada celda
conf_matrix_df$Porcentaje <- round(conf_matrix_df$Frecuencia / sum(conf_matrix_df$Frecuencia) * 100, 2)

# Gráfico de matriz de confusión
ggplot(conf_matrix_df, aes(x = Real, y = Predicho, fill = Tipo)) +
  geom_tile(color = "black") +
  geom_text(aes(label = paste0(Frecuencia, " (", Porcentaje, "%)")), size = 6) +
  scale_fill_manual(values = c("Acierto" = "lightgreen", "Error" = "lightcoral")) + 
  theme_minimal() +
  labs(title = "Matriz de Confusión", x = "Real", y = "Predicho")

# Calcular métricas con confusionMatrix
conf_matrix <- confusionMatrix(predicted_classes, data_logistic$is_canceled, positive = "1")

accuracy <- conf_matrix$overall["Accuracy"]
precision <- conf_matrix$byClass["Pos Pred Value"]  # Precisión
recall <- conf_matrix$byClass["Sensitivity"]  # Recall
f1_score <- 2 * (precision * recall) / (precision + recall)  # F1 Score

# Crear tabla con los resultados
metricas_df <- data.frame(
  Métrica = c("Accuracy", "Precision", "Recall", "F1 Score"),
  Valor = c(accuracy, precision, recall, f1_score)
)

# Mostrar la tabla formateada
kable(metricas_df, digits = 2, col.names = c("Métrica", "Valor"), align = "c")

# ------------------- Regresión Logística con Stepwise Forward ------------------- #

# Modelo base vacío (solo intercepto)
base_model <- glm(is_canceled ~ 1, data = data_logistic, family = binomial)

# Modelo completo con todas las variables
full_model <- glm(is_canceled ~ ., data = data_logistic, family = binomial)

# Stepwise Forward: Selección de variables basada en AIC
stepwise_forward_model <- step(base_model, 
                               scope = list(lower = base_model, upper = full_model), 
                               direction = "forward", 
                               trace = TRUE)

# Resumen del modelo seleccionado
summary(stepwise_forward_model)

# Predicciones en probabilidades
pred_probs <- predict(stepwise_forward_model, type = "response")

# Convertir probabilidades a clases (umbral 0.5)
pred_classes <- ifelse(pred_probs > 0.5, 1, 0)

# Convertir variables a factor para la matriz de confusión
data_logistic$is_canceled <- as.factor(data_logistic$is_canceled)
pred_classes <- as.factor(pred_classes)

# Crear matriz de confusión
conf_matrix <- confusionMatrix(pred_classes, data_logistic$is_canceled, positive = "1")

# Extraer métricas
accuracy <- conf_matrix$overall["Accuracy"]
precision <- conf_matrix$byClass["Pos Pred Value"]  # Precisión
recall <- conf_matrix$byClass["Sensitivity"]  # Recall
f1_score <- 2 * (precision * recall) / (precision + recall)  # F1 Score

# Mostrar métricas en consola
cat(sprintf("Accuracy: %.2f\n", accuracy))
cat(sprintf("Precision: %.2f\n", precision))
cat(sprintf("Recall: %.2f\n", recall))
cat(sprintf("F1 Score: %.2f\n", f1_score))

# Convertir la matriz de confusión a formato data frame para ggplot2
conf_matrix_df <- as.data.frame(conf_matrix$table)
colnames(conf_matrix_df) <- c("Predicho", "Real", "Frecuencia")

# Agregar porcentaje de cada celda
conf_matrix_df$Porcentaje <- round(conf_matrix_df$Frecuencia / sum(conf_matrix_df$Frecuencia) * 100, 2)

# Agregar una columna para identificar aciertos y errores
conf_matrix_df$Tipo <- ifelse(conf_matrix_df$Real == conf_matrix_df$Predicho, "Acierto", "Error")

# Gráfico de matriz de confusión con colores personalizados
ggplot(conf_matrix_df, aes(x = Real, y = Predicho, fill = Tipo)) +
  geom_tile(color = "black") +
  geom_text(aes(label = paste0(Frecuencia, " (", Porcentaje, "%)")), size = 6) +
  scale_fill_manual(values = c("Acierto" = "lightgreen", "Error" = "lightcoral")) + 
  theme_minimal() +
  labs(title = "Matriz de Confusión", x = "Real", y = "Predicho")

# Crear tabla con los resultados de métricas
metricas_df <- data.frame(
  Métrica = c("Accuracy", "Precision", "Recall", "F1 Score"),
  Valor = c(accuracy, precision, recall, f1_score)
)

# Mostrar la tabla formateada
kable(metricas_df, digits = 2, col.names = c("Métrica", "Valor"), align = "c")

# ------------------- Regresión Logística Simple ------------------- #

# Convertir la variable objetivo a factor
data_logistic$is_canceled <- as.factor(data_logistic$is_canceled)

# Lista para almacenar los resultados
resultados <- data.frame(Variable = character(), Accuracy = numeric(), stringsAsFactors = FALSE)

# Seleccionar todas las variables predictoras
variables <- setdiff(names(data_logistic), "is_canceled")  

# Iterar sobre todas las variables para ajustar regresiones logísticas simples
for (var in variables) {
  formula <- as.formula(paste("is_canceled ~", var))
  modelo <- glm(formula, data = data_logistic, family = binomial)
  
  pred_probs <- predict(modelo, type = "response")
  pred_classes <- ifelse(pred_probs > 0.5, 1, 0)
  
  pred_classes <- as.factor(pred_classes)
  
  conf_matrix <- confusionMatrix(pred_classes, data_logistic$is_canceled, positive = "1")
  accuracy <- conf_matrix$overall["Accuracy"]
  
  resultados <- rbind(resultados, data.frame(Variable = var, Accuracy = accuracy))
}

# Seleccionar la mejor variable según la precisión
mejor_variable <- resultados[which.max(resultados$Accuracy), "Variable"]
mejor_precision <- max(resultados$Accuracy)

cat(sprintf("La mejor variable es '%s' con una precisión de %.2f%%\n", mejor_variable, mejor_precision * 100))

# Ajustar el modelo final con la mejor variable
mejor_formula <- as.formula(paste("is_canceled ~", mejor_variable))
mejor_modelo <- glm(mejor_formula, data = data_logistic, family = binomial)

# Predicciones finales
pred_probs_final <- predict(mejor_modelo, type = "response")
pred_classes_final <- ifelse(pred_probs_final > 0.5, 1, 0)

# Matriz de confusión final
conf_matrix_final <- confusionMatrix(as.factor(pred_classes_final), data_logistic$is_canceled, positive = "1")

# Extraer métricas
accuracy_final <- conf_matrix_final$overall["Accuracy"]
precision_final <- conf_matrix_final$byClass["Pos Pred Value"]
recall_final <- conf_matrix_final$byClass["Sensitivity"]
f1_score_final <- 2 * (precision_final * recall_final) / (precision_final + recall_final)

# Convertir matriz de confusión a data frame para ggplot2
conf_matrix_df <- as.data.frame(conf_matrix_final$table)
colnames(conf_matrix_df) <- c("Predicho", "Real", "Frecuencia")

# Agregar porcentaje de cada celda
conf_matrix_df$Porcentaje <- round(conf_matrix_df$Frecuencia / sum(conf_matrix_df$Frecuencia) * 100, 2)

# Agregar columna para diferenciar aciertos y errores
conf_matrix_df$Tipo <- ifelse(conf_matrix_df$Real == conf_matrix_df$Predicho, "Acierto", "Error")

# Gráfico de matriz de confusión con colores
ggplot(conf_matrix_df, aes(x = Real, y = Predicho, fill = Tipo)) +
  geom_tile(color = "black") +
  geom_text(aes(label = paste0(Frecuencia, " (", Porcentaje, "%)")), size = 6) +
  scale_fill_manual(values = c("Acierto" = "lightgreen", "Error" = "lightcoral")) + 
  theme_minimal() +
  labs(title = "Matriz de Confusión - Mejor Modelo", x = "Real", y = "Predicho")

# Tabla con métricas
metricas_df <- data.frame(
  Métrica = c("Accuracy", "Precision", "Recall", "F1 Score"),
  Valor = c(accuracy_final, precision_final, recall_final, f1_score_final)
)

# Mostrar la tabla formateada
kable(metricas_df, digits = 2, col.names = c("Métrica", "Valor"), align = "c")









