library(ggplot2)
library(dplyr)  
library(zoo)    
library(lubridate)  
library(tseries)
library(forecast)
library(tidyr)

#----------------------------------------------------#
# 1º - Lectura de los datos y EDA 
#----------------------------------------------------#

# Leer el dataset
ice_cream <- read.csv("ice_cream.csv", header = TRUE, sep = ",")
ice_cream$DATE <- as.Date(ice_cream$DATE)

# Dividir en train (hasta 2014) y test (2015 en adelante)
train <- subset(ice_cream, DATE < "2015-01-01")
test <- subset(ice_cream, DATE >= "2015-01-01")

# Dibujamos los datos del conjunto de entrenamiento
ggplot(train, aes(x = DATE, y = IPN31152N)) + 
  geom_line(color = "darkblue", size = 1.2) +  
  labs(title = "Ventas de Helado a lo Largo del Tiempo (Train)", 
       x = "Fecha", y = "Ventas") +
  theme_minimal(base_size = 14) +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))  

# Pintamos ventanas ------------
# Ventana crisis (2007-2009) dentro del conjunto de entrenamiento
train_2008 <- subset(train, DATE >= "2007-01-01" & DATE <= "2009-12-31")
ggplot(train_2008, aes(x = DATE, y = IPN31152N)) + 
  geom_line(color = "darkblue", size = 1.2) +  
  labs(title = "Ventas de Helado Durante la Crisis de 2008 (Train)", 
       x = "Fecha", y = "Ventas") +
  theme_minimal(base_size = 14) +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))   

# Ventana entrada del Euro (2001-2003)
ice_cream_euro <- subset(ice_cream, DATE >= "2001-01-01" & DATE <= "2003-12-31")
ggplot(ice_cream_euro, aes(x = DATE, y = IPN31152N)) + 
  geom_line(color = "darkblue", size = 1.2) +  
  labs(title = "Ventas de Helado Durante la Entrada del Euro", x = "Fecha", y = "Ventas") +
  theme_minimal(base_size = 14) +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))  

# Media Móvil -----------
# Calcular la media general solo en el conjunto de entrenamiento
media_general <- mean(train$IPN31152N, na.rm = TRUE)
# Calcular la media móvil en el conjunto de entrenamiento
train <- train %>%
  mutate(media_movil = rollmean(IPN31152N, k = 12, fill = NA, align = "right"))
# Reestructurar los datos para graficar
data_long <- train %>%
  select(DATE, IPN31152N, media_movil) %>%
  pivot_longer(cols = -DATE, names_to = "Serie", values_to = "Ventas")
# Graficar los datos con media móvil y media general
ggplot(data_long, aes(x = DATE, y = Ventas, color = Serie)) + 
  geom_line(size = 1.2) +  
  geom_hline(aes(yintercept = media_general, color = "Media General"), linetype = "dashed", size = 1.2) +  
  scale_color_manual(values = c("IPN31152N" = "lightblue", "media_movil" = "darkgreen", "Media General" = "darkred")) +
  labs(title = "Ventas de Helado con Media General y Media Móvil (Train)", 
       x = "Fecha", y = "Ventas", color = "Serie") +
  theme_minimal(base_size = 14) +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 


# Últimos 36 meses -------------------
# Filtrar los últimos 36 meses dentro del conjunto de entrenamiento
train_last36 <- train %>%
  filter(DATE >= max(DATE) %m-% months(35))  
# Graficar los últimos 36 meses del conjunto de entrenamiento
ggplot(train_last36, aes(x = DATE, y = IPN31152N)) + 
  geom_line(color = "darkblue", size = 1.2) +  
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +  
  labs(title = "Ventas de Helado en los Últimos 36 Meses (Train)", 
       x = "Mes", y = "Ventas") +
  theme_minimal(base_size = 14) +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Descomposiciones ----------------
# Descomposición de la serie temporal
ts_train <- ts(train$IPN31152N, start = c(year(min(train$DATE)), month(min(train$DATE))), frequency = 12)

decomp_add <- decompose(ts_train, type = "additive")
plot(decomp_add, col = "darkgreen")

decomp_mult <- decompose(ts_train, type = "multiplicative")
plot(decomp_mult, col = "darkred")

# ACF y PACF de la serie de entrenamiento
par(mfrow = c(1, 2))  
acf(ts_train, main = "ACF - Serie Original", col = "darkgreen")
pacf(ts_train, main = "PACF - Serie Original", col = "darkred")


#----------------------------------------------------#
# 2º - Modelo AR(p)
#----------------------------------------------------#
# Modelo AR con datos de entrenamiento
modelo_ar <- Arima(ts_train, order = c(13, 0, 0)) # Porque baja el PACF
summary(modelo_ar)
checkresiduals(modelo_ar)

# Predicciones desde 2015 hasta enero de 2020
h <- nrow(test)  
pred_ar <- forecast(modelo_ar, h = h)

# Comparación de predicciones vs test
test$Pred_AR <- as.numeric(pred_ar$mean)

# Unir train y test en un solo dataframe para ggplot
data_plot <- rbind(
  data.frame(DATE = train$DATE, Ventas = train$IPN31152N, Serie = "Entrenamiento"),
  data.frame(DATE = test$DATE, Ventas = test$IPN31152N, Serie = "Test"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_AR, Serie = "Predicción AR(13)")
)

# Graficar con leyenda
ggplot(data_plot, aes(x = DATE, y = Ventas, color = Serie, linetype = Serie)) + 
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Entrenamiento" = "darkblue", 
                                "Test" = "black", 
                                "Predicción AR(13)" = "red")) +
  scale_linetype_manual(values = c("Entrenamiento" = "solid", 
                                   "Test" = "solid", 
                                   "Predicción AR(13)" = "dashed")) +
  labs(title = "Predicciones AR(13) vs Datos Reales", 
       x = "Fecha", y = "Ventas", color = "Serie", linetype = "Serie") +
  theme_minimal(base_size = 14) +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Crear dataframe con solo test y predicción
data_plot_test <- rbind(
  data.frame(DATE = test$DATE, Ventas = test$IPN31152N, Serie = "Test"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_AR, Serie = "Predicción AR(13)")
)

# Graficar test y predicción con leyenda
ggplot(data_plot_test, aes(x = DATE, y = Ventas, color = Serie, linetype = Serie)) + 
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Test" = "black", "Predicción AR(13)" = "red")) +
  scale_linetype_manual(values = c("Test" = "solid", "Predicción AR(13)" = "dashed")) +
  labs(title = "Predicciones AR(13) vs Datos de Test", 
       x = "Fecha", y = "Ventas", color = "Serie", linetype = "Serie") +
  theme_minimal(base_size = 14) +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



#Predicción recursiva
# Crear una función para la evaluación progresiva para AR(p)
rolling_forecast_ar <- function(train_data, test_data, order_ar = 13, h = 1) {
  # Inicializar vector para almacenar predicciones
  n_test <- length(test_data)
  predictions <- numeric(n_test)
  
  # Convertir datos a series de tiempo
  train_ts <- ts(train_data)
  
  # Bucle de predicción progresiva
  for (i in 1:n_test) {
    # Ajustar modelo con los datos disponibles
    current_data <- c(train_data, test_data[1:(i-1)])
    current_ts <- ts(current_data)
    modelo <- Arima(current_ts, order = c(order_ar, 0, 0))
    
    # Hacer predicción para el siguiente paso
    pred <- forecast(modelo, h = 1)
    predictions[i] <- as.numeric(pred$mean)
    
    # Opcional: mostrar progreso
    if (i %% 10 == 0) {
      cat("Completado:", i, "de", n_test, "\n")
    }
  }
  
  return(predictions)
}

# Aplicar la función para tu modelo AR(13)
test$Pred_AR_Rolling <- rolling_forecast_ar(train$IPN31152N, test$IPN31152N, order_ar = 13)

# Gráfica 1: Comparación en el período de test
data_plot_test_only <- rbind(
  data.frame(DATE = test$DATE, Ventas = test$IPN31152N, Serie = "Datos Reales"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_AR, Serie = "Predicción AR(13) Estática"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_AR_Rolling, Serie = "Predicción AR(13) Progresiva")
)

ggplot(data_plot_test_only, aes(x = DATE, y = Ventas, color = Serie, linetype = Serie)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Datos Reales" = "black", 
                                "Predicción AR(13) Estática" = "red",
                                "Predicción AR(13) Progresiva" = "green")) +
  scale_linetype_manual(values = c("Datos Reales" = "solid", 
                                   "Predicción AR(13) Estática" = "dashed",
                                   "Predicción AR(13) Progresiva" = "dotted")) +
  labs(title = "Comparación de Pronósticos AR(13) en Período de Test",
       subtitle = "Predicción Estática vs Predicción Progresiva",
       x = "Fecha", y = "Ventas", color = "Serie", linetype = "Serie") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1))

# Gráfica 2: Errores de predicción
test$Error_Estatico <- test$IPN31152N - test$Pred_AR
test$Error_Progresivo <- test$IPN31152N - test$Pred_AR_Rolling

data_plot_errors <- rbind(
  data.frame(DATE = test$DATE, Error = test$Error_Estatico, Serie = "Error Predicción Estática"),
  data.frame(DATE = test$DATE, Error = test$Error_Progresivo, Serie = "Error Predicción Progresiva")
)

ggplot(data_plot_errors, aes(x = DATE, y = Error, color = Serie)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Error Predicción Estática" = "red",
                                "Error Predicción Progresiva" = "green")) +
  labs(title = "Errores de Predicción AR(13) en Período de Test",
       x = "Fecha", y = "Error (Real - Predicción)", color = "Serie") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")


# Crear dataframe para visualización de la serie completa con predicciones AR(13)
data_plot_rolling_ar <- rbind(
  data.frame(DATE = train$DATE, Ventas = train$IPN31152N, Serie = "Entrenamiento"),
  data.frame(DATE = test$DATE, Ventas = test$IPN31152N, Serie = "Test"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_AR, Serie = "Predicción AR(13) Estática"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_AR_Rolling, Serie = "Predicción AR(13) Progresiva")
)

# Graficar comparación de métodos en la serie completa
ggplot(data_plot_rolling_ar, aes(x = DATE, y = Ventas, color = Serie, linetype = Serie)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Entrenamiento" = "darkblue", 
                                "Test" = "black", 
                                "Predicción AR(13) Estática" = "red",
                                "Predicción AR(13) Progresiva" = "green")) +
  scale_linetype_manual(values = c("Entrenamiento" = "solid", 
                                   "Test" = "solid", 
                                   "Predicción AR(13) Estática" = "dashed",
                                   "Predicción AR(13) Progresiva" = "dotted")) +
  labs(title = "Comparación de Pronósticos AR(13): Estático vs Progresivo",
       subtitle = "Vista de la Serie Temporal Completa",
       x = "Fecha", y = "Ventas", color = "Serie", linetype = "Serie") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# Calcular métricas de error para comparar ambos métodos
library(Metrics)
rmse_ar_static <- rmse(test$IPN31152N, test$Pred_AR)
rmse_ar_rolling <- rmse(test$IPN31152N, test$Pred_AR_Rolling)
mae_ar_static <- mae(test$IPN31152N, test$Pred_AR)
mae_ar_rolling <- mae(test$IPN31152N, test$Pred_AR_Rolling)

# Mostrar resultados
cat("RMSE estático:", rmse_ar_static, "\n")
cat("RMSE progresivo:", rmse_ar_rolling, "\n")
cat("MAE estático:", mae_ar_static, "\n")
cat("MAE progresivo:", mae_ar_rolling, "\n")

library(knitr)

# Crear un dataframe con los resultados
tabla_ar <- data.frame(
  Métrica = c("RMSE estático", "RMSE progresivo", "MAE estático", "MAE progresivo"),
  Valor = c(rmse_ar_static, rmse_ar_rolling, mae_ar_static, mae_ar_rolling)
)

# Imprimir la tabla en formato bonito
kable(tabla_ar, col.names = c("Métrica", "Valor"), caption = "Modelo: AR", digits = 4)









#----------------------------------------------------#
# 3º - Modelo MA(q)
#----------------------------------------------------#
# Ajustar un modelo MA(q) con el q seleccionado 
modelo_ma <- Arima(ts_train, order = c(0, 0, 7))  # Tomamos 7, es cuando baja un poco ACF
# Resumen del modelo
summary(modelo_ma)
# Diagnóstico de residuos
checkresiduals(modelo_ma)


# Predicciones para el horizonte de prueba
h <- nrow(test)
pred_ma <- forecast(modelo_ma, h = h)

# Agregar predicciones al conjunto de test
test$Pred_MA <- as.numeric(pred_ma$mean)


# Unir train, test y predicción MA(q) en un solo dataframe para ggplot
data_plot_ma <- rbind(
  data.frame(DATE = train$DATE, Ventas = train$IPN31152N, Serie = "Entrenamiento"),
  data.frame(DATE = test$DATE, Ventas = test$IPN31152N, Serie = "Test"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_MA, Serie = "Predicción MA(q)")
)

# Graficar con leyenda
ggplot(data_plot_ma, aes(x = DATE, y = Ventas, color = Serie, linetype = Serie)) + 
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Entrenamiento" = "darkblue", 
                                "Test" = "black", 
                                "Predicción MA(q)" = "red")) +
  scale_linetype_manual(values = c("Entrenamiento" = "solid", 
                                   "Test" = "solid", 
                                   "Predicción MA(q)" = "dashed")) +
  labs(title = "Predicciones MA(q) vs Datos Reales", 
       x = "Fecha", y = "Ventas", color = "Serie", linetype = "Serie") +
  theme_minimal(base_size = 14) +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Graficar test vs predicción MA(q)
data_plot_test <- rbind(
  data.frame(DATE = test$DATE, Ventas = test$IPN31152N, Serie = "Test"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_MA, Serie = "Predicción MA(q)")
)

ggplot(data_plot_test, aes(x = DATE, y = Ventas, color = Serie, linetype = Serie)) + 
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Test" = "black", "Predicción MA(q)" = "red")) +
  scale_linetype_manual(values = c("Test" = "solid", "Predicción MA(q)" = "dashed")) +
  labs(title = "Predicciones MA(q) vs Datos de Test", 
       x = "Fecha", y = "Ventas", color = "Serie", linetype = "Serie") +
  theme_minimal(base_size = 14) +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))




# Predicción Recursiva
# Crear una función para la evaluación progresiva
rolling_forecast_ma <- function(train_data, test_data, order_ma = 7, h = 1) {
  # Inicializar vector para almacenar predicciones
  n_test <- length(test_data)
  predictions <- numeric(n_test)
  
  # Convertir datos a series de tiempo
  train_ts <- ts(train_data)
  
  # Bucle de predicción progresiva
  for (i in 1:n_test) {
    # Ajustar modelo con los datos disponibles
    current_data <- c(train_data, test_data[1:(i-1)])
    current_ts <- ts(current_data)
    modelo <- Arima(current_ts, order = c(0, 0, order_ma))
    
    # Hacer predicción para el siguiente paso
    pred <- forecast(modelo, h = 1)
    predictions[i] <- as.numeric(pred$mean)
    
    # Opcional: mostrar progreso
    if (i %% 10 == 0) {
      cat("Completado:", i, "de", n_test, "\n")
    }
  }
  
  return(predictions)
}

# Aplicar la función
test$Pred_MA_Rolling <- rolling_forecast_ma(train$IPN31152N, test$IPN31152N)

# Crear dataframe para visualización
data_plot_rolling <- rbind(
  data.frame(DATE = train$DATE, Ventas = train$IPN31152N, Serie = "Entrenamiento"),
  data.frame(DATE = test$DATE, Ventas = test$IPN31152N, Serie = "Test"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_MA, Serie = "Predicción MA(q) Estática"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_MA_Rolling, Serie = "Predicción MA(q) Progresiva")
)

# Graficar comparación de métodos
ggplot(data_plot_rolling, aes(x = DATE, y = Ventas, color = Serie, linetype = Serie)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Entrenamiento" = "darkblue", 
                                "Test" = "black", 
                                "Predicción MA(q) Estática" = "red",
                                "Predicción MA(q) Progresiva" = "green")) +
  scale_linetype_manual(values = c("Entrenamiento" = "solid", 
                                   "Test" = "solid", 
                                   "Predicción MA(q) Estática" = "dashed",
                                   "Predicción MA(q) Progresiva" = "dotted")) +
  labs(title = "Comparación de Pronósticos: Estático vs Progresivo",
       x = "Fecha", y = "Ventas", color = "Serie", linetype = "Serie") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Calcular métricas de error para ambos métodos
library(Metrics)
rmse_ma_static <- rmse(test$IPN31152N, test$Pred_MA)
rmse_ma_rolling <- rmse(test$IPN31152N, test$Pred_MA_Rolling)
mae_ma_static <- mae(test$IPN31152N, test$Pred_MA)
mae_ma_rolling <- mae(test$IPN31152N, test$Pred_MA_Rolling)

# Mostrar resultados
cat("RMSE estático:", rmse_ma_static, "\n")
cat("RMSE progresivo:", rmse_ma_rolling, "\n")
cat("MAE estático:", mae_ma_static, "\n")
cat("MAE progresivo:", mae_ma_rolling, "\n")

# Crear un dataframe con los resultados
tabla_ma <- data.frame(
  Métrica = c("RMSE estático", "RMSE progresivo", "MAE estático", "MAE progresivo"),
  Valor = c(rmse_ma_static, rmse_ma_rolling, mae_ma_static, mae_ma_rolling)
)

# Imprimir la tabla en formato bonito
kable(tabla_ma, col.names = c("Métrica", "Valor"), caption = "Modelo: MA", digits = 4)












#----------------------------------------------------#
# 3º - Modelo ARIMA
#----------------------------------------------------#
# Comprobamos estacionariedad con el Test ADF
adf_result <- adf.test(ts_train)
print(adf_result)  # Si p-value > 0.05 --> No estacionaria

# Hacemos una diferenciación y analizamos si es estacionaria o no
ts_diff1 <- diff(ts_train)  # Primera diferencia
plot(ts_diff1, main = "Serie Diferenciada (Primera Diferencia)", col = "darkblue")

# Comprobamos con el Test ADF la serie diferenciada
adf_test_diff1 <- adf.test(ts_diff1)
print(adf_test_diff1)  # Si p-value < 0.05 --> Estacionaria

# Pintamos ahora ACF y PACF de la serie diferenciada
par(mfrow = c(1, 2))  # Mismo formato para comparación
acf(ts_diff1, main = "ACF - Serie Diferenciada", col = "darkgreen")
pacf(ts_diff1, main = "PACF - Serie Diferenciada", col = "darkred")



# Ajustar un modelo ARIMA(p, d, q) con los valores seleccionados (por ejemplo, p=3, d=1, q=2)
modelo_arima <- Arima(ts_train, order = c(13,1, 4))  # Ajusta p, d, q según ACF/PACF

# Resumen del modelo
summary(modelo_arima)

# Diagnóstico de residuos
checkresiduals(modelo_arima)

# Predicciones para el horizonte de prueba
h <- nrow(test)
pred_arima <- forecast(modelo_arima, h = h)

# Agregar predicciones al conjunto de test
test$Pred_ARIMA <- as.numeric(pred_arima$mean)


# Unir train, test y predicción ARIMA en un solo dataframe para ggplot
data_plot_arima <- rbind(
  data.frame(DATE = train$DATE, Ventas = train$IPN31152N, Serie = "Entrenamiento"),
  data.frame(DATE = test$DATE, Ventas = test$IPN31152N, Serie = "Test"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_ARIMA, Serie = "Predicción ARIMA")
)

# Graficar con leyenda
ggplot(data_plot_arima, aes(x = DATE, y = Ventas, color = Serie, linetype = Serie)) + 
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Entrenamiento" = "darkblue", 
                                "Test" = "black", 
                                "Predicción ARIMA" = "red")) +
  scale_linetype_manual(values = c("Entrenamiento" = "solid", 
                                   "Test" = "solid", 
                                   "Predicción ARIMA" = "dashed")) +
  labs(title = "Predicciones ARIMA vs Datos Reales", 
       x = "Fecha", y = "Ventas", color = "Serie", linetype = "Serie") +
  theme_minimal(base_size = 14) +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Graficar test vs predicción ARIMA
data_plot_test_arima <- rbind(
  data.frame(DATE = test$DATE, Ventas = test$IPN31152N, Serie = "Test"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_ARIMA, Serie = "Predicción ARIMA")
)

ggplot(data_plot_test_arima, aes(x = DATE, y = Ventas, color = Serie, linetype = Serie)) + 
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Test" = "black", "Predicción ARIMA" = "red")) +
  scale_linetype_manual(values = c("Test" = "solid", "Predicción ARIMA" = "dashed")) +
  labs(title = "Predicciones ARIMA vs Datos de Test", 
       x = "Fecha", y = "Ventas", color = "Serie", linetype = "Serie") +
  theme_minimal(base_size = 14) +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Evaluación progresiva
# Crear función para la evaluación progresiva para ARIMA
# Función de evaluación progresiva robusta para ARIMA
# Función de evaluación progresiva robusta para ARIMA
rolling_forecast_arima_robust <- function(train_data, test_data, order_p = 13, order_d = 1, order_q = 4, h = 1) {
  # Inicializar vector para almacenar predicciones
  n_test <- length(test_data)
  predictions <- numeric(n_test)
  
  # Convertir datos a series de tiempo
  train_ts <- ts(train_data)
  
  # Bucle de predicción progresiva
  for (i in 1:n_test) {
    # Ajustar modelo con los datos disponibles
    current_data <- c(train_data, test_data[1:(i-1)])
    current_ts <- ts(current_data)
    
    # Usar tryCatch para manejar errores
    model_result <- tryCatch({
      # Intentar ajustar el modelo con los parámetros especificados
      modelo <- Arima(current_ts, order = c(order_p, order_d, order_q), method = "ML")
      
      # Hacer predicción para el siguiente paso
      pred <- forecast(modelo, h = 1)
      list(success = TRUE, prediction = as.numeric(pred$mean))
      
    }, error = function(e) {
      # Si falla, intentar con un método diferente o parámetros más conservadores
      cat("Error en iteración", i, ":", e$message, "\n")
      cat("Intentando con método CSS...\n")
      
      tryCatch({
        # Intento alternativo con método CSS en lugar de ML
        modelo_alt <- Arima(current_ts, order = c(order_p, order_d, order_q), method = "CSS")
        pred_alt <- forecast(modelo_alt, h = 1)
        list(success = TRUE, prediction = as.numeric(pred_alt$mean))
        
      }, error = function(e2) {
        cat("Error en segundo intento:", e2$message, "\n")
        cat("Intentando con auto.arima...\n")
        
        # Si ambos fallan, usar auto.arima que es más robusto
        tryCatch({
          modelo_auto <- auto.arima(current_ts, stepwise = TRUE, approximation = TRUE)
          pred_auto <- forecast(modelo_auto, h = 1)
          list(success = TRUE, prediction = as.numeric(pred_auto$mean))
          
        }, error = function(e3) {
          # Si todo falla, usar un método muy simple (media móvil)
          cat("Todos los métodos fallaron. Usando naive forecast.\n")
          if (i > 1) {
            # Si ya tenemos predicciones anteriores, usar la última
            list(success = TRUE, prediction = predictions[i-1])
          } else {
            # Para la primera predicción, usar el último valor de train
            list(success = TRUE, prediction = tail(train_data, 1))
          }
        })
      })
    })
    
    # Guardar la predicción
    predictions[i] <- model_result$prediction
    
    # Mostrar progreso
    if (i %% 10 == 0) {
      cat("Completado:", i, "de", n_test, "\n")
    }
  }
  
  return(predictions)
}

# Aplicar la función para el modelo ARIMA(13,1,4)
test$Pred_ARIMA_Rolling <- rolling_forecast_arima_robust(train$IPN31152N, test$IPN31152N, 
                                                         order_p = 13, order_d = 1, order_q = 4)


# ----- GRÁFICA 1: SERIE COMPLETA CON PREDICCIONES -----
data_plot_rolling_arima <- rbind(
  data.frame(DATE = train$DATE, Ventas = train$IPN31152N, Serie = "Entrenamiento"),
  data.frame(DATE = test$DATE, Ventas = test$IPN31152N, Serie = "Test"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_ARIMA, Serie = "Predicción ARIMA Estática"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_ARIMA_Rolling, Serie = "Predicción ARIMA Progresiva")
)

ggplot(data_plot_rolling_arima, aes(x = DATE, y = Ventas, color = Serie, linetype = Serie)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Entrenamiento" = "darkblue", 
                                "Test" = "black", 
                                "Predicción ARIMA Estática" = "red",
                                "Predicción ARIMA Progresiva" = "green")) +
  scale_linetype_manual(values = c("Entrenamiento" = "solid", 
                                   "Test" = "solid", 
                                   "Predicción ARIMA Estática" = "dashed",
                                   "Predicción ARIMA Progresiva" = "dotted")) +
  labs(title = "Comparación de Pronósticos ARIMA: Estático vs Progresivo",
       subtitle = "Vista de la Serie Temporal Completa",
       x = "Fecha", y = "Ventas", color = "Serie", linetype = "Serie") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# ----- GRÁFICA 2: SOLO PERÍODO DE TEST -----
data_plot_test_only_arima <- rbind(
  data.frame(DATE = test$DATE, Ventas = test$IPN31152N, Serie = "Datos Reales"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_ARIMA, Serie = "Predicción ARIMA Estática"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_ARIMA_Rolling, Serie = "Predicción ARIMA Progresiva")
)

ggplot(data_plot_test_only_arima, aes(x = DATE, y = Ventas, color = Serie, linetype = Serie)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Datos Reales" = "black", 
                                "Predicción ARIMA Estática" = "red",
                                "Predicción ARIMA Progresiva" = "green")) +
  scale_linetype_manual(values = c("Datos Reales" = "solid", 
                                   "Predicción ARIMA Estática" = "dashed",
                                   "Predicción ARIMA Progresiva" = "dotted")) +
  labs(title = "Comparación de Pronósticos ARIMA en Período de Test",
       subtitle = "Predicción Estática vs Predicción Progresiva",
       x = "Fecha", y = "Ventas", color = "Serie", linetype = "Serie") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1))

# ----- GRÁFICA 3: ERRORES DE PREDICCIÓN -----
test$Error_Estatico_ARIMA <- test$IPN31152N - test$Pred_ARIMA
test$Error_Progresivo_ARIMA <- test$IPN31152N - test$Pred_ARIMA_Rolling

data_plot_errors_arima <- rbind(
  data.frame(DATE = test$DATE, Error = test$Error_Estatico_ARIMA, Serie = "Error Predicción Estática"),
  data.frame(DATE = test$DATE, Error = test$Error_Progresivo_ARIMA, Serie = "Error Predicción Progresiva")
)

ggplot(data_plot_errors_arima, aes(x = DATE, y = Error, color = Serie)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Error Predicción Estática" = "red",
                                "Error Predicción Progresiva" = "green")) +
  labs(title = "Errores de Predicción ARIMA en Período de Test",
       x = "Fecha", y = "Error (Real - Predicción)", color = "Serie") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")

# ----- MÉTRICAS DE ERROR -----
library(Metrics)
rmse_static_arima <- rmse(test$IPN31152N, test$Pred_ARIMA)
rmse_rolling_arima <- rmse(test$IPN31152N, test$Pred_ARIMA_Rolling)
mae_static_arima <- mae(test$IPN31152N, test$Pred_ARIMA)
mae_rolling_arima <- mae(test$IPN31152N, test$Pred_ARIMA_Rolling)

# Mostrar resultados
cat("RMSE estático (ARIMA):", rmse_static_arima, "\n")
cat("RMSE progresivo (ARIMA):", rmse_rolling_arima, "\n")
cat("MAE estático (ARIMA):", mae_static_arima, "\n")
cat("MAE progresivo (ARIMA):", mae_rolling_arima, "\n")



# Crear un dataframe con los resultados
tabla_arima <- data.frame(
  Métrica = c("RMSE estático", "RMSE progresivo", "MAE estático", "MAE progresivo"),
  Valor = c(rmse_static_arima, rmse_rolling_arima, mae_static_arima, mae_rolling_arima)
)

# Imprimir la tabla en formato bonito
kable(tabla_arima, col.names = c("Métrica", "Valor"), caption = "Modelo: ARIMA", digits = 4)









#----------------------------------------------------#
# 4º - Modelo SARIMA
#----------------------------------------------------#
# Comprobamos si hay estacionalidad con diferenciación estacional
ts_diff_seasonal <- diff(ts_train, lag = 7)  # Diferenciación estacional
plot(ts_diff_seasonal, main = "Serie Diferenciada Estacionalmente", col = "darkblue")

# Test ADF para la serie diferenciada estacionalmente
adf_test_seasonal <- adf.test(ts_diff_seasonal)
print(adf_test_seasonal) # Sí que es estacionaria

# Pintamos ahora ACF y PACF de la serie diferenciada
par(mfrow = c(2, 1))  # Formato de gráficos


# ACF y PACF de la serie diferenciada estacionalmente
acf(ts_diff_seasonal, main = "ACF - Serie Diferenciada Estacional", col = "darkgreen")
pacf(ts_diff_seasonal, main = "PACF - Serie Diferenciada Estacional", col = "darkred")


# Ajustar un modelo SARIMA(p,d,q)(P,D,Q)[s]
modelo_sarima <- Arima(ts_train, order = c(13,1,4), 
                       seasonal = list(order = c(3,0,1), period = 7))

# Resumen del modelo
summary(modelo_sarima)

# Diagnóstico de residuos
checkresiduals(modelo_sarima)

# Predicciones para el horizonte de prueba
h <- nrow(test)
pred_sarima <- forecast(modelo_sarima, h = h)

# Agregar predicciones al conjunto de test
test$Pred_SARIMA <- as.numeric(pred_sarima$mean)


# Unir train, test y predicción SARIMA en un solo dataframe para ggplot
data_plot_sarima <- rbind(
  data.frame(DATE = train$DATE, Ventas = train$IPN31152N, Serie = "Entrenamiento"),
  data.frame(DATE = test$DATE, Ventas = test$IPN31152N, Serie = "Test"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_SARIMA, Serie = "Predicción SARIMA")
)

# Graficar con leyenda
ggplot(data_plot_sarima, aes(x = DATE, y = Ventas, color = Serie, linetype = Serie)) + 
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Entrenamiento" = "darkblue", 
                                "Test" = "black", 
                                "Predicción SARIMA" = "red")) +
  scale_linetype_manual(values = c("Entrenamiento" = "solid", 
                                   "Test" = "solid", 
                                   "Predicción SARIMA" = "dashed")) +
  labs(title = "Predicciones SARIMA vs Datos Reales", 
       x = "Fecha", y = "Ventas", color = "Serie", linetype = "Serie") +
  theme_minimal(base_size = 14) +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Graficar test vs predicción SARIMA
data_plot_test_sarima <- rbind(
  data.frame(DATE = test$DATE, Ventas = test$IPN31152N, Serie = "Test"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_SARIMA, Serie = "Predicción SARIMA")
)

ggplot(data_plot_test_sarima, aes(x = DATE, y = Ventas, color = Serie, linetype = Serie)) + 
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Test" = "black", "Predicción SARIMA" = "red")) +
  scale_linetype_manual(values = c("Test" = "solid", "Predicción SARIMA" = "dashed")) +
  labs(title = "Predicciones SARIMA vs Datos de Test", 
       x = "Fecha", y = "Ventas", color = "Serie", linetype = "Serie") +
  theme_minimal(base_size = 14) +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Evaluación progresiva
# Crear función para la evaluación progresiva para SARIMA
# Función de evaluación progresiva robusta para SARIMA
rolling_forecast_sarima_robust <- function(train_data, test_data, order_p = 13, order_d = 1, order_q = 4, 
                                           seasonal_P = 3, seasonal_D = 0, seasonal_Q = 1, s = 7, h = 1) {
  # Inicializar vector para almacenar predicciones
  n_test <- length(test_data)
  predictions <- numeric(n_test)
  
  # Convertir datos a series de tiempo
  train_ts <- ts(train_data)
  
  # Bucle de predicción progresiva
  for (i in 1:n_test) {
    # Ajustar modelo con los datos disponibles
    current_data <- c(train_data, test_data[1:(i-1)])
    current_ts <- ts(current_data)
    
    # Usar tryCatch para manejar errores
    model_result <- tryCatch({
      # Intentar ajustar el modelo con los parámetros especificados
      modelo <- Arima(current_ts, 
                      order = c(order_p, order_d, order_q),
                      seasonal = list(order = c(seasonal_P, seasonal_D, seasonal_Q), period = s),
                      method = "ML")
      
      # Hacer predicción para el siguiente paso
      pred <- forecast(modelo, h = 1)
      list(success = TRUE, prediction = as.numeric(pred$mean))
      
    }, error = function(e) {
      # Si falla, intentar con un método diferente o parámetros más conservadores
      cat("Error en iteración", i, ":", e$message, "\n")
      cat("Intentando con método CSS...\n")
      
      tryCatch({
        # Intento alternativo con método CSS
        modelo_alt <- Arima(current_ts, 
                            order = c(order_p, order_d, order_q),
                            seasonal = list(order = c(seasonal_P, seasonal_D, seasonal_Q), period = s),
                            method = "CSS")
        pred_alt <- forecast(modelo_alt, h = 1)
        list(success = TRUE, prediction = as.numeric(pred_alt$mean))
        
      }, error = function(e2) {
        cat("Error en segundo intento:", e2$message, "\n")
        cat("Intentando con auto.arima...\n")
        
        # Si ambos fallan, usar auto.arima que es más robusto
        tryCatch({
          modelo_auto <- auto.arima(current_ts, seasonal = TRUE, stepwise = TRUE, approximation = TRUE)
          pred_auto <- forecast(modelo_auto, h = 1)
          list(success = TRUE, prediction = as.numeric(pred_auto$mean))
          
        }, error = function(e3) {
          # Si todo falla, usar un método muy simple (media móvil o naive)
          cat("Todos los métodos fallaron. Usando naive forecast.\n")
          if (i > 1) {
            # Si ya tenemos predicciones anteriores, usar la última
            list(success = TRUE, prediction = predictions[i-1])
          } else {
            # Para la primera predicción, usar el último valor de train
            list(success = TRUE, prediction = tail(train_data, 1))
          }
        })
      })
    })
    
    # Guardar la predicción
    predictions[i] <- model_result$prediction
    
    # Mostrar progreso
    if (i %% 10 == 0) {
      cat("Completado:", i, "de", n_test, "\n")
    }
  }
  
  return(predictions)
}

# Aplicar la función para el modelo SARIMA(13,1,4)(3,0,1)[7]
test$Pred_SARIMA_Rolling <- rolling_forecast_sarima_robust(train$IPN31152N, test$IPN31152N, 
                                                           order_p = 13, order_d = 1, order_q = 4,
                                                           seasonal_P = 3, seasonal_D = 0, seasonal_Q = 1, s = 7)

# ----- GRÁFICA 1: SERIE COMPLETA CON PREDICCIONES -----
data_plot_rolling_sarima <- rbind(
  data.frame(DATE = train$DATE, Ventas = train$IPN31152N, Serie = "Entrenamiento"),
  data.frame(DATE = test$DATE, Ventas = test$IPN31152N, Serie = "Test"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_SARIMA, Serie = "Predicción SARIMA Estática"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_SARIMA_Rolling, Serie = "Predicción SARIMA Progresiva")
)

ggplot(data_plot_rolling_sarima, aes(x = DATE, y = Ventas, color = Serie, linetype = Serie)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Entrenamiento" = "darkblue", 
                                "Test" = "black", 
                                "Predicción SARIMA Estática" = "red",
                                "Predicción SARIMA Progresiva" = "green")) +
  scale_linetype_manual(values = c("Entrenamiento" = "solid", 
                                   "Test" = "solid", 
                                   "Predicción SARIMA Estática" = "dashed",
                                   "Predicción SARIMA Progresiva" = "dotted")) +
  labs(title = "Predicciones SARIMA Estática vs Progresiva",
       subtitle = "Vista de la Serie Temporal Completa",
       x = "Fecha", y = "Ventas", color = "Serie", linetype = "Serie") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))



# ----- GRÁFICA 2: SOLO PERÍODO DE TEST -----
data_plot_test_only_sarima <- rbind(
  data.frame(DATE = test$DATE, Ventas = test$IPN31152N, Serie = "Datos Reales"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_SARIMA, Serie = "Predicción SARIMA Estática"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_SARIMA_Rolling, Serie = "Predicción SARIMA Progresiva")
)

ggplot(data_plot_test_only_sarima, aes(x = DATE, y = Ventas, color = Serie, linetype = Serie)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Datos Reales" = "black", 
                                "Predicción SARIMA Estática" = "red",
                                "Predicción SARIMA Progresiva" = "green")) +
  scale_linetype_manual(values = c("Datos Reales" = "solid", 
                                   "Predicción SARIMA Estática" = "dashed",
                                   "Predicción SARIMA Progresiva" = "dotted")) +
  labs(title = "Predicciones SARIMA Estática vs Progresiva en Período de Test",
       subtitle = "Comparación de Predicción Estática y Progresiva",
       x = "Fecha", y = "Ventas", color = "Serie", linetype = "Serie") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1))

# ----- GRÁFICA 3: ERRORES DE PREDICCIÓN -----
test$Error_Estatico_SARIMA <- test$IPN31152N - test$Pred_SARIMA
test$Error_Progresivo_SARIMA <- test$IPN31152N - test$Pred_SARIMA_Rolling

data_plot_errors_sarima <- rbind(
  data.frame(DATE = test$DATE, Error = test$Error_Estatico_SARIMA, Serie = "Error Predicción Estática"),
  data.frame(DATE = test$DATE, Error = test$Error_Progresivo_SARIMA, Serie = "Error Predicción Progresiva")
)

ggplot(data_plot_errors_sarima, aes(x = DATE, y = Error, color = Serie)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_color_manual(values = c("Error Predicción Estática" = "red",
                                "Error Predicción Progresiva" = "green")) +
  labs(title = "Errores de Predicción SARIMA en Período de Test",
       x = "Fecha", y = "Error (Real - Predicción)", color = "Serie") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom")


# ----- MÉTRICAS DE ERROR -----
library(Metrics)
rmse_static_sarima <- rmse(test$IPN31152N, test$Pred_SARIMA)
rmse_rolling_sarima <- rmse(test$IPN31152N, test$Pred_SARIMA_Rolling)
mae_static_sarima <- mae(test$IPN31152N, test$Pred_SARIMA)
mae_rolling_sarima <- mae(test$IPN31152N, test$Pred_SARIMA_Rolling)

# Mostrar resultados
cat("RMSE estático (SARIMA):", rmse_static_sarima, "\n")
cat("RMSE progresivo (SARIMA):", rmse_rolling_sarima, "\n")
cat("MAE estático (SARIMA):", mae_static_sarima, "\n")
cat("MAE progresivo (SARIMA):", mae_rolling_sarima, "\n")

# Crear un dataframe con los resultados
tabla_sarima <- data.frame(
  Métrica = c("RMSE estático", "RMSE progresivo", "MAE estático", "MAE progresivo"),
  Valor = c(rmse_static_sarima, rmse_rolling_sarima, mae_static_sarima, mae_rolling_sarima)
)

# Imprimir la tabla en formato bonito
kable(tabla_sarima, col.names = c("Métrica", "Valor"), caption = "Modelo: SARIMA", digits = 4)







#----------------------------------------------------#
# 5º - MODELO AUTOMÁTICO
#----------------------------------------------------#

library(forecast)

modelo_sarima_auto <- auto.arima(ts_train, seasonal = TRUE)  # Ajustar modelo SARIMA
summary(modelo_sarima_auto)

# ---- DIAGNÓSTICO DE RESIDUOS ----
checkresiduals(modelo_sarima_auto)

# ---- PREDICCIONES ESTÁTICAS ----
h <- nrow(test)
pred_sarima_auto <- forecast(modelo_sarima_auto, h = h)

# Agregar predicciones al dataset de test
test$Pred_SARIMA_Auto <- as.numeric(pred_sarima_auto$mean)

# ---- GRÁFICO: SERIE TEMPORAL CON PREDICCIONES ----
data_plot_sarima_auto <- rbind(
  data.frame(DATE = train$DATE, Ventas = train$IPN31152N, Serie = "Entrenamiento"),
  data.frame(DATE = test$DATE, Ventas = test$IPN31152N, Serie = "Test"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_SARIMA_Auto, Serie = "Predicción AUTO SARIMA")
)

ggplot(data_plot_sarima_auto, aes(x = DATE, y = Ventas, color = Serie, linetype = Serie)) + 
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Entrenamiento" = "darkblue", 
                                "Test" = "black", 
                                "Predicción AUTO SARIMA" = "red")) +
  scale_linetype_manual(values = c("Entrenamiento" = "solid", 
                                   "Test" = "solid", 
                                   "Predicción AUTO SARIMA" = "dashed")) +
  labs(title = "Predicción AUTO SARIMA vs Datos Reales", 
       x = "Fecha", y = "Ventas", color = "Serie", linetype = "Serie") +
  theme_minimal(base_size = 14) +  
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# ---- EVALUACIÓN PROGRESIVA ----
rolling_forecast_sarima_auto <- function(train_data, test_data, modelo_auto, h = 1) {
  n_test <- length(test_data)
  predictions <- numeric(n_test)
  
  for (i in 1:n_test) {
    current_data <- c(train_data, test_data[1:(i-1)])
    current_ts <- ts(current_data)
    
    modelo_result <- tryCatch({
      modelo <- auto.arima(current_ts, seasonal = TRUE, stepwise = TRUE, approximation = TRUE)
      pred <- forecast(modelo, h = 1)
      list(success = TRUE, prediction = as.numeric(pred$mean))
    }, error = function(e) {
      cat("Error en iteración", i, ": usando naive forecast.\n")
      list(success = TRUE, prediction = ifelse(i > 1, predictions[i-1], tail(train_data, 1)))
    })
    
    predictions[i] <- modelo_result$prediction
  }
  
  return(predictions)
}

# Aplicar evaluación progresiva
test$Pred_SARIMA_Auto_Rolling <- rolling_forecast_sarima_auto(train$IPN31152N, test$IPN31152N, modelo_sarima_auto)

# ---- GRÁFICO: PREDICCIÓN ESTÁTICA VS PROGRESIVA ----
data_plot_rolling_sarima_auto <- rbind(
  data.frame(DATE = train$DATE, Ventas = train$IPN31152N, Serie = "Entrenamiento"),
  data.frame(DATE = test$DATE, Ventas = test$IPN31152N, Serie = "Test"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_SARIMA_Auto, Serie = "Predicción AUTO SARIMA Estática"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_SARIMA_Auto_Rolling, Serie = "Predicción AUTO SARIMA Progresiva")
)

ggplot(data_plot_rolling_sarima_auto, aes(x = DATE, y = Ventas, color = Serie, linetype = Serie)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Entrenamiento" = "darkblue", 
                                "Test" = "black", 
                                "Predicción AUTO SARIMA Estática" = "red",
                                "Predicción AUTO SARIMA Progresiva" = "green")) +
  scale_linetype_manual(values = c("Entrenamiento" = "solid", 
                                   "Test" = "solid", 
                                   "Predicción AUTO SARIMA Estática" = "dashed",
                                   "Predicción AUTO SARIMA Progresiva" = "dotted")) +
  labs(title = "Predicción AUTO SARIMA Estática vs Progresiva",
       subtitle = "Vista Completa de la Serie Temporal",
       x = "Fecha", y = "Ventas", color = "Serie", linetype = "Serie") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Graficar test vs predicción AUTO SARIMA (Estática y Progresiva) con zoom en el período de test
data_plot_rolling_sarima_auto_test <- rbind(
  data.frame(DATE = test$DATE, Ventas = test$IPN31152N, Serie = "Test"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_SARIMA_Auto, Serie = "Predicción AUTO SARIMA Estática"),
  data.frame(DATE = test$DATE, Ventas = test$Pred_SARIMA_Auto_Rolling, Serie = "Predicción AUTO SARIMA Progresiva")
)

ggplot(data_plot_rolling_sarima_auto_test, aes(x = DATE, y = Ventas, color = Serie, linetype = Serie)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Test" = "black", 
                                "Predicción AUTO SARIMA Estática" = "red",
                                "Predicción AUTO SARIMA Progresiva" = "green")) +
  scale_linetype_manual(values = c("Test" = "solid", 
                                   "Predicción AUTO SARIMA Estática" = "dashed",
                                   "Predicción AUTO SARIMA Progresiva" = "dotted")) +
  labs(title = "Predicción AUTO SARIMA Estática vs Progresiva",
       subtitle = "Zoom en Período de Test",
       x = "Fecha", y = "Ventas", color = "Serie", linetype = "Serie") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# ---- MÉTRICAS DE ERROR ----
rmse_static_sarima_auto <- rmse(test$IPN31152N, test$Pred_SARIMA_Auto)
rmse_rolling_sarima_auto <- rmse(test$IPN31152N, test$Pred_SARIMA_Auto_Rolling)
mae_static_sarima_auto <- mae(test$IPN31152N, test$Pred_SARIMA_Auto)
mae_rolling_sarima_auto <- mae(test$IPN31152N, test$Pred_SARIMA_Auto_Rolling)

tabla_sarima_auto <- data.frame(
  Métrica = c("RMSE Estático", "RMSE Progresivo", "MAE Estático", "MAE Progresivo"),
  Valor = c(rmse_static_sarima_auto, rmse_rolling_sarima_auto, mae_static_sarima_auto, mae_rolling_sarima_auto)
)

kable(tabla_sarima_auto, col.names = c("Métrica", "Valor"), caption = "Modelo: AUTO SARIMA", digits = 4)



#----------------------------------------------------#
# 6º - COMPARACIÓN DE MODELOS
#----------------------------------------------------#
library(dplyr)
library(knitr)

# Crear dataframe con los resultados
resultados <- data.frame(
  Modelo = c("AR", "MA", "ARIMA", "SARIMA", "AUTO SARIMA"),
  AIC = c(AIC(modelo_ar), AIC(modelo_ma), AIC(modelo_arima), AIC(modelo_sarima), AIC(modelo_sarima_auto)),
  RMSE_Train = c(modelo_ar$sigma2^0.5, modelo_ma$sigma2^0.5, modelo_arima$sigma2^0.5, modelo_sarima$sigma2^0.5, modelo_sarima_auto$sigma2^0.5),
  RMSE_Test_Static = c(rmse_ar_static, rmse_ma_static, rmse_static_arima, rmse_static_sarima, rmse_static_sarima_auto),
  RMSE_Test_Rolling = c(rmse_ar_rolling, rmse_ma_rolling, rmse_rolling_arima, rmse_rolling_sarima, rmse_rolling_sarima_auto),
  MAE_Test_Static = c(mae_ar_static, mae_ma_static, mae_static_arima, mae_static_sarima, mae_static_sarima_auto),
  MAE_Test_Rolling = c(mae_ar_rolling, mae_ma_rolling, mae_rolling_arima, mae_rolling_sarima, mae_rolling_sarima_auto)
)

# Mostrar tabla en formato bonito
kable(resultados, digits = 4, caption = "Comparación de Modelos ARIMA")








