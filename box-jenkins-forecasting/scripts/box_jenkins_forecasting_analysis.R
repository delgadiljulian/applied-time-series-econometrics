# ============================================================
# Maestría en Economía Aplicada
# Universidad de Buenos Aires (UBA)
# Materia: Series de Tiempo
# Trabajo Práctico N.º 1
#
# Estudiante: Julián Delgadillo Marín
#
# ============================================================

# ============================================================
# Librerías
# ============================================================

library(readxl)
library(forecast)
library(tseries)
library(seasonal)

# ============================================================
# Función: Eval_Pron
# ============================================================

Eval_Pron=function(Y_P,Y_A,Nombre="Pron_1"){
  RMSE=sqrt(sum((Y_P-Y_A)^2)/length(Y_P))
  MAE=sum(abs(Y_P-Y_A)/length(Y_P))
  MAPE=100*sum(abs((Y_P-Y_A)/Y_A)/length(Y_P))
  D1=sqrt(sum(Y_P^2)/length(Y_P))
  D2=sqrt(sum(Y_A^2)/length(Y_P))
  Theil=RMSE/(D1+D2)
  
  MSE=RMSE^2
  U1=(1/MSE)*(mean(Y_P)- mean(Y_A))^2
  sd_Y_P=sqrt(var(Y_P)*(length(Y_P)-1)/length(Y_P))
  sd_Y_A=sqrt(var(Y_A)*(length(Y_A)-1)/length(Y_A))
  U2=(1/MSE)*((sd_Y_P)-sd_Y_A)^2
  U3=1-U1-U2
  
  Pron_1=matrix(c(RMSE,MAE,MAPE,Theil,U1,U2,U3),ncol=1)
  rownames(Pron_1)=c("RMSE","MAE","MAPE","U_Theil","U_sesgo","U_varianza","U_covarianza")
  colnames(Pron_1)=c(Nombre)
  return(Pron_1)
}

# ============================================================
# Función: corr_res
# ============================================================

corr_res <- function(residuos, lags = 12) {
  Acf(residuos, lag.max = lags, main = "ACF de residuos")
  print(Box.test(residuos, lag = lags, type = "Ljung-Box"))
}


# ============================================================
# Carga de datos
# ============================================================

ruta <- "C:/Users/julla/Downloads/1. Series de Tiempo/TP1/Datos TP1.xlsx"

df <- read_excel(ruta, sheet = "Series TP1")

colnames(df) <- c("fecha", "DL_CPRIV", "DL_EXPO", "DL_IBF", "DL_PIB")

str(df)
summary(df)
colSums(is.na(df))

# =====================================================
# =====================================================
# VARIABLE 1: DL_CPRIV
# =====================================================
# =====================================================

# ============================================================
# a.3. Modelización - DL_CPRIV
# ============================================================

# -------------------------------
# a.3.1 Serie temporal
# -------------------------------

cpriv_ts <- ts(df$DL_CPRIV, start = c(2004, 2), frequency = 4)

# -------------------------------
# a.3.2 Split train / test
# -------------------------------

cpriv_train <- window(cpriv_ts, end = c(2023, 4))
cpriv_test  <- window(cpriv_ts, start = c(2024, 1))

# -------------------------------
# a.3.3 Exploración
# -------------------------------

plot(cpriv_train, main = "DL_CPRIV")

Acf(cpriv_train)
title(main = "ACF - DL_CPRIV")

Pacf(cpriv_train)
title(main = "PACF - DL_CPRIV")

# -------------------------------
# a.3.4 Estimación
# -------------------------------

# A partir del análisis de los correlogramas de la serie DL_CPRIV se identifican
# componentes tanto estacionales como no estacionales.

# En la ACF se observan picos significativos en los rezagos múltiplos de 4 
# (lags 4, 8, 12, ...), lo que indica una clara estacionalidad trimestral.
# Este patrón es consistente con un componente estacional tipo MA(1),
# por lo que se incorpora una estructura (0,1,1)[4].

# Adicionalmente, en los primeros rezagos la ACF presenta un valor negativo
# significativo en el lag 1 seguido de un decaimiento, lo cual sugiere la
# posible presencia de un componente MA(1) no estacional.

# Por su parte, la PACF muestra un pico significativo en el rezago 1 y una
# rápida disipación en los rezagos posteriores, lo que sugiere un posible
# componente AR(1).

# Dado que los correlogramas no permiten identificar de forma concluyente si
# la dinámica de corto plazo es de tipo AR o MA, se plantean dos modelos
# alternativos:

# - Modelo MA: ARIMA(0,0,1)(0,1,1)[4]
# - Modelo AR: ARIMA(1,0,0)(0,1,1)[4]

# Ambos modelos incorporan diferenciación estacional de orden 1 y un componente
# estacional MA(1), consistente con la evidencia observada en la ACF.

# Estas especificaciones se estiman junto con el modelo automático (auto.arima)
# para comparar su desempeño predictivo y evaluar la coherencia entre la
# identificación empírica y la selección automática.


# Modelo automático
modelo_auto <- auto.arima(cpriv_train, seasonal = TRUE)
summary(modelo_auto)

# Modelo manual MA
modelo_m1 <- Arima(cpriv_train,
                   order = c(0,0,1),
                   seasonal = list(order = c(0,1,1), period = 4))
summary(modelo_m1)

# Modelo manual AR
modelo_m2 <- Arima(cpriv_train,
                   order = c(1,0,0),
                   seasonal = list(order = c(0,1,1), period = 4))
summary(modelo_m2)


# -------------------------------
# a.3.5 Diagnóstico
# -------------------------------

checkresiduals(modelo_auto)
corr_res(residuals(modelo_auto))

checkresiduals(modelo_m1)
corr_res(residuals(modelo_m1))

checkresiduals(modelo_m2)
corr_res(residuals(modelo_m2))

# ============================================================
# a.4. Pronósticos y evaluación
# ============================================================

# -------------------------------
# a.4.1 Pronósticos
# -------------------------------

fc_auto <- forecast(modelo_auto, h = 8)
fc_m1   <- forecast(modelo_m1, h = 8)
fc_m2   <- forecast(modelo_m2, h = 8)

# -------------------------------
# a.4.2 Evaluación con Eval_Pron
# -------------------------------

eval_auto <- Eval_Pron(fc_auto$mean, cpriv_test, "Auto")
eval_m1   <- Eval_Pron(fc_m1$mean, cpriv_test, "Manual_MA")
eval_m2   <- Eval_Pron(fc_m2$mean, cpriv_test, "Manual_AR")

# Mostrar resultados
eval_auto
eval_m1
eval_m2


# -------------------------------
# a.4.3 Comparación conjunta
# -------------------------------

cbind(eval_auto, eval_m1, eval_m2)

# ============================================================
# a.5. Desestacionalización con X13 y modelización ARMA
# ============================================================

# -------------------------------
# a.5.1 Desestacionalización (X13)
# -------------------------------

# Usar SOLO la muestra de entrenamiento
cpriv_x13 <- seas(
  x = cpriv_train,
  x11 = "",
  x11.mode = "add",
  transform.function = "none",
  regression.aictest = NULL,
  outlier = NULL
)

# Serie desestacionalizada
cpriv_sa <- final(cpriv_x13)

# Visualización
plot(cpriv_train, main = "Serie original DL_CPRIV")
plot(cpriv_sa, main = "Serie desestacionalizada DL_CPRIV")

# Identificación

Acf(cpriv_sa)
title(main = "ACF - DL_CPRIV desestacionalizada")

Pacf(cpriv_sa)
title(main = "PACF - DL_CPRIV desestacionalizada")

# -------------------------------
# a.5.2 Modelización ARMA (sin estacionalidad)
# -------------------------------

# Modelo automático (benchmark)
modelo_sa_auto <- auto.arima(cpriv_sa, seasonal = FALSE)
summary(modelo_sa_auto)

# Modelo manual (por formalidad): ARMA(1,1)
modelo_sa_m1 <- Arima(cpriv_sa, order = c(1,0,1))
summary(modelo_sa_m1)

# Modelo teórico correcto: ruido blanco
modelo_sa_wn <- Arima(cpriv_sa, order = c(0,0,0))
summary(modelo_sa_wn)

# -------------------------------
# a.5.3 Diagnóstico de residuos
# -------------------------------

# Auto
checkresiduals(modelo_sa_auto)
corr_res(residuals(modelo_sa_auto))

# ARMA(1,1)
checkresiduals(modelo_sa_m1)
corr_res(residuals(modelo_sa_m1))

# Ruido blanco
checkresiduals(modelo_sa_wn)
corr_res(residuals(modelo_sa_wn))


# -------------------------------
# a.5.4 Pronóstico
# -------------------------------

h <- 8  # 2024Q1 - 2025Q4

forecast_sa_auto <- forecast(modelo_sa_auto, h = h)
forecast_sa_m1   <- forecast(modelo_sa_m1, h = h)
forecast_sa_wn   <- forecast(modelo_sa_wn, h = h)


# -------------------------------
# a.5.5 Re-estacionalización (corregido)
# -------------------------------

# Componente estacional
seasonal_component <- cpriv_train - cpriv_sa

# Promedio por trimestre
seasonality <- cycle(cpriv_train)
seasonal_means <- tapply(seasonal_component, seasonality, mean)

# Normalización (clave)
seasonal_means <- seasonal_means - mean(seasonal_means)

# Extender al horizonte
seasonal_future <- rep(seasonal_means, length.out = h)

# Re-estacionalizar pronósticos
forecast_final_auto <- forecast_sa_auto$mean + seasonal_future
forecast_final_m1   <- forecast_sa_m1$mean + seasonal_future
forecast_final_wn   <- forecast_sa_wn$mean + seasonal_future


# -------------------------------
# a.5.6 Evaluación de pronóstico
# -------------------------------

# Serie real (hold-out)
cpriv_test <- window(cpriv_ts, start = c(2024,1))

# Evaluación
eval_sa_auto <- Eval_Pron(
  Y_P = as.numeric(forecast_final_auto),
  Y_A = as.numeric(cpriv_test),
  Nombre = "SA_Auto"
)

eval_sa_m1 <- Eval_Pron(
  Y_P = as.numeric(forecast_final_m1),
  Y_A = as.numeric(cpriv_test),
  Nombre = "SA_ARMA11"
)

eval_sa_wn <- Eval_Pron(
  Y_P = as.numeric(forecast_final_wn),
  Y_A = as.numeric(cpriv_test),
  Nombre = "SA_WN"
)

# Mostrar resultados
eval_sa_auto
eval_sa_m1
eval_sa_wn

# Comparación conjunta
cbind(eval_sa_auto, eval_sa_m1, eval_sa_wn)

# -------------------------------
# a.6. Interpretación ARMA (no se requiere código adicional)
# La modelización ya se realizó en 5.2 y el diagnóstico en 5.3.
# Los resultados muestran que la serie desestacionalizada es ruido blanco.
# -------------------------------

# ============================================================
# a.9. Cuadro comparativo y anexo estadístico (solo visualización)
# ============================================================

# -------------------------------
# a.9.1 Cuadro comparativo global
# -------------------------------

tabla_comp <- cbind(
  eval_auto,
  eval_m1,
  eval_m2,
  eval_sa_auto,
  eval_sa_m1,
  eval_sa_wn
)

# -------------------------------
# a.9.2 Función para marcar el mejor
# -------------------------------

marcar_mejor <- function(tabla) {
  tabla_out <- tabla
  
  for (i in 1:nrow(tabla)) {
    
    fila <- as.numeric(tabla[i, ])
    
    if (rownames(tabla)[i] == "U_covarianza") {
      mejor <- which.max(fila)
    } else {
      mejor <- which.min(fila)
    }
    
    for (j in 1:ncol(tabla)) {
      if (j == mejor) {
        tabla_out[i, j] <- paste0(round(fila[j], 5), " *")
      } else {
        tabla_out[i, j] <- round(fila[j], 5)
      }
    }
  }
  
  return(tabla_out)
}

# -------------------------------
# a.9.3 Mostrar tabla final
# -------------------------------

tabla_final <- marcar_mejor(tabla_comp)

cat("\n==============================\n")
cat("CUADRO COMPARATIVO DE PRONÓSTICOS\n")
cat("==============================\n\n")
print(tabla_final)


# -------------------------------
# a.9.4 Anexo: valores reales vs pronósticos
# -------------------------------

anexo <- data.frame(
  Fecha = time(cpriv_test),
  Real = as.numeric(cpriv_test),
  
  SARIMA_Auto = as.numeric(fc_auto$mean),
  SARIMA_MA   = as.numeric(fc_m1$mean),
  SARIMA_AR   = as.numeric(fc_m2$mean),
  
  SA_Auto = as.numeric(forecast_final_auto),
  SA_ARMA = as.numeric(forecast_final_m1),
  SA_WN   = as.numeric(forecast_final_wn)
)

cat("\n==============================\n")
cat("ANEXO: VALORES REALES VS PRONÓSTICOS\n")
cat("==============================\n\n")
print(anexo)

# -------------------------------
# a.10. Conclusiones del análisis
# -------------------------------

# A partir de los estadísticos de bondad del pronóstico, se observa que los modelos
# estimados directamente sobre la serie original (SARIMA) presentan un mejor desempeño
# predictivo en comparación con los modelos basados en la serie desestacionalizada (SA).
#
# En particular, los errores (RMSE, MAE, MAPE) son sistemáticamente menores en los
# modelos SARIMA, lo que indica una mayor capacidad para capturar la dinámica completa
# de la serie, incluyendo su componente estacional.
#
# Por su parte, los modelos sobre la serie desestacionalizada, aunque metodológicamente
# correctos, pierden información al eliminar la estacionalidad y reintroducirla luego
# mediante promedios, lo que reduce su precisión predictiva.
#
# Adicionalmente, dentro de cada grupo (SARIMA y SA), no se observan diferencias
# significativas entre especificaciones, por lo que el resultado relevante es la
# superioridad del enfoque SARIMA frente al enfoque de desestacionalización.

# =====================================================
# =====================================================
# VARIABLE 2: DL_EXPO
# =====================================================
# =====================================================

# 1. Serie
# 2. Train/Test
# 3. Desestacionalización
# 4. ACF/PACF  ← AQUÍ decides modelo
# 5. Modelos
# 6. Diagnóstico
# 7. Pronóstico
# 8. Re-estacionalización
# 9. Evaluación

# =====================================================
# b. Modelización - DL_EXPO
# =====================================================

# -------------------------------
# b.1.1 Serie temporal
# -------------------------------

expo_ts <- ts(df$DL_EXPO, start = c(2004, 2), frequency = 4)

# -------------------------------
# b.1.2 Split train / test
# -------------------------------

expo_train <- window(expo_ts, end = c(2023, 4))
expo_test  <- window(expo_ts, start = c(2024, 1))

# -------------------------------
# b.1.3 Exploración
# -------------------------------

plot(expo_ts, main = "DL_EXPO", ylab = "Dif. log", xlab = "Tiempo")

dev.off()
acf(expo_train, main = "")
title(main = "ACF - DL_EXPO", line = 2)

pacf(expo_train, main = "")
title(main = "PACF - DL_EXPO", line = 2)

# -------------------------------
# b.2.4 Estimación
# -------------------------------

# Los modelos manuales se determinaron a partir del análisis de los correlogramas (ACF y PACF).
# 
# La ACF muestra picos significativos en los rezagos 4, 8, etc., lo que indica una clara
# estacionalidad trimestral (s = 4), sugiriendo la inclusión de un componente MA estacional (0,1,1)[4].
# 
# En la parte no estacional, la ACF presenta un decaimiento gradual y la PACF muestra significancia
# en los primeros rezagos (principalmente lag 1 y en menor medida lag 2), lo que sugiere la presencia
# de componentes AR y MA de bajo orden.
# 
# Con base en esto, se plantean dos especificaciones:
# - ARIMA(1,0,1)(0,1,1)[4]: captura tanto dinámica AR como MA y la estacionalidad observada.
# - ARIMA(2,0,0)(0,1,1)[4]: alternativa más parsimoniosa basada en la significancia de los primeros
#   rezagos en la PACF.
# 
# Estas especificaciones se contrastan con el modelo sugerido por auto.arima() para seleccionar
# el modelo final en función de criterios de información y diagnóstico de residuos.

# Modelo automático
modelo_expo_auto <- auto.arima(expo_train, seasonal = TRUE)
summary(modelo_expo_auto)

# Modelo manual 1 (ARMA)
modelo_expo_m1 <- Arima(
  expo_train,
  order = c(1,0,1),
  seasonal = list(order = c(0,1,1), period = 4)
)
summary(modelo_expo_m1)

# Modelo manual 2 (AR)
modelo_expo_m2 <- Arima(
  expo_train,
  order = c(2,0,0),
  seasonal = list(order = c(0,1,1), period = 4)
)
summary(modelo_expo_m2)


# -------------------------------
# b.2.5 Diagnóstico
# -------------------------------

checkresiduals(modelo_expo_auto)
corr_res(residuals(modelo_expo_auto), 12)

checkresiduals(modelo_expo_m1)
corr_res(residuals(modelo_expo_m1), 12)

checkresiduals(modelo_expo_m2)
corr_res(residuals(modelo_expo_m2), 12)


# Cerrar dispositivos gráficos (sin romper si no hay)
if (!is.null(dev.list())) dev.off()

h <- length(expo_test)

# -------------------------------
# b.3.1 Pronósticos
# -------------------------------

pron_auto <- forecast(modelo_expo_auto, h = h)
pron_m1   <- forecast(modelo_expo_m1, h = h)
pron_m2   <- forecast(modelo_expo_m2, h = h)


# -------------------------------
# b.3.2 Gráficos comparativos
# -------------------------------

# Abrir nuevo dispositivo (clave)
dev.new(width = 7, height = 10)

par(mfrow = c(3,1), mar = c(2,2,2,2), oma = c(0,0,2,0))

plot(pron_auto, main = "Auto.arima - DL_EXPO")
plot(pron_m1,   main = "MA manual - DL_EXPO")
plot(pron_m2,   main = "AR manual - DL_EXPO")

par(mfrow = c(1,1))


# -------------------------------
# b.4.1 Evaluación individual
# -------------------------------

cat("Modelo Automático\n")
eval_expo_auto <- Eval_Pron(pron_auto$mean, expo_test)

cat("\nModelo MA\n")
eval_expo_m1 <- Eval_Pron(pron_m1$mean, expo_test)

cat("\nModelo AR\n")
eval_expo_m2 <- Eval_Pron(pron_m2$mean, expo_test)


# -------------------------------
# b.4.2 Comparación conjunta
# -------------------------------

cbind(eval_expo_auto, eval_expo_m1, eval_expo_m2)

# -------------------------------
# b.5 Desestacionalización - DL_EXPO
# -------------------------------

# -------------------------------
# b.5.1 Desestacionalización
# -------------------------------

expo_x13 <- seas(
  x = expo_train,
  x11 = "",
  x11.mode = "add",
  transform.function = "none",
  regression.aictest = NULL,
  outlier = NULL
)

# Serie desestacionalizada
expo_sa <- final(expo_x13)


# -------------------------------
# b.5.2 Visualización
# -------------------------------

# Reset gráfico
par(mfrow = c(1,1), mar = c(5,4,4,2))

# Gráfico 1: serie original
plot(expo_train,
     main = "DL_EXPO original",
     ylab = "Dif. log",
     xlab = "Tiempo")

# Gráfico 2: serie desestacionalizada
plot(expo_sa,
     main = "DL_EXPO desestacionalizada",
     ylab = "Dif. log",
     xlab = "Tiempo")


# -------------------------------
# b.5.3 Identificación
# -------------------------------

# ACF
dev.new(width = 7, height = 6)

par(mfrow = c(1,1), mar = c(5,4,4,2))

acf(expo_sa,
    lag.max = 20,
    main = "ACF - DL_EXPO desestacionalizada")


# PACF
dev.new(width = 7, height = 6)

par(mfrow = c(1,1), mar = c(5,4,4,2))

pacf(expo_sa,
     lag.max = 20,
     main = "PACF - DL_EXPO desestacionalizada")


# -------------------------------
# b.6 Modelización - DL_EXPO (SA)
# -------------------------------

# -------------------------------
# b.6.1 Serie temporal
# -------------------------------

expo_sa_ts <- expo_sa


# -------------------------------
# b.6.2 Modelos
# -------------------------------

# Modelo automático
modelo_sa_auto <- auto.arima(expo_sa_ts, seasonal = FALSE)
summary(modelo_sa_auto)

# Modelo manual 1: MA(1) sin media
modelo_sa_m1 <- Arima(expo_sa_ts,
                      order = c(0,0,1),
                      include.mean = FALSE)
summary(modelo_sa_m1)

# Modelo manual 2: MA(1) con media
modelo_sa_m2 <- Arima(expo_sa_ts,
                      order = c(0,0,1),
                      include.mean = TRUE)
summary(modelo_sa_m2)


# -------------------------------
# b.6.3 Diagnóstico
# -------------------------------

checkresiduals(modelo_sa_auto)
corr_res(residuals(modelo_sa_auto))

checkresiduals(modelo_sa_m1)
corr_res(residuals(modelo_sa_m1))

checkresiduals(modelo_sa_m2)
corr_res(residuals(modelo_sa_m2))


# -------------------------------
# b.7 Pronóstico y re-estacionalización - DL_EXPO
# -------------------------------

h <- length(expo_test)

# -------------------------------
# b.7.1 Pronósticos sobre la serie desestacionalizada
# -------------------------------

pron_sa_auto <- forecast(modelo_sa_auto, h = h)
pron_sa_m1   <- forecast(modelo_sa_m1, h = h)
pron_sa_m2   <- forecast(modelo_sa_m2, h = h)


# -------------------------------
# b.7.2 Componente estacional aditiva
# -------------------------------

comp_sa <- expo_train - expo_sa


# -------------------------------
# b.7.3 Promedios estacionales por trimestre
# -------------------------------

coef_est <- tapply(comp_sa, cycle(expo_train), mean)


# -------------------------------
# b.7.4 Normalización (que sumen cero)
# -------------------------------

coef_est <- coef_est - mean(coef_est)


# -------------------------------
# b.7.5 Extensión al horizonte de pronóstico
# -------------------------------

coef_est_fut <- rep(coef_est, length.out = h)


# -------------------------------
# b.7.6 Pronósticos re-estacionalizados
# -------------------------------

pron_final_auto <- pron_sa_auto$mean + coef_est_fut
pron_final_m1   <- pron_sa_m1$mean + coef_est_fut
pron_final_m2   <- pron_sa_m2$mean + coef_est_fut


# -------------------------------
# b.7.7 Visualización (separada)
# -------------------------------

# Convertir a ts
pron_final_auto_ts <- ts(pron_final_auto,
                         start = start(expo_test),
                         frequency = frequency(expo_test))

pron_final_m1_ts <- ts(pron_final_m1,
                       start = start(expo_test),
                       frequency = frequency(expo_test))

pron_final_m2_ts <- ts(pron_final_m2,
                       start = start(expo_test),
                       frequency = frequency(expo_test))

# -------------------------------
# Configuración: 3 gráficos en una sola ventana
# -------------------------------

par(mfrow = c(3,1), mar = c(3,4,3,1))


# -------------------------------
# Configurar layout: 3 filas, 1 columna
# -------------------------------

par(mfrow = c(3,1), mar = c(3,4,3,1))


# -------------------------------
# Modelo automático
# -------------------------------

plot(expo_train,
     xlim = c(time(expo_train)[1], time(expo_test)[length(expo_test)]),
     ylim = range(c(expo_train, expo_test, pron_final_auto_ts)),
     main = "Modelo Auto (MA(1))",
     ylab = "")

lines(expo_test, col = "black", lwd = 2)
lines(pron_final_auto_ts, col = "blue", lwd = 2)

abline(v = time(expo_test)[1], lty = 2)


# -------------------------------
# Modelo MA(1) sin media
# -------------------------------

plot(expo_train,
     xlim = c(time(expo_train)[1], time(expo_test)[length(expo_test)]),
     ylim = range(c(expo_train, expo_test, pron_final_m1_ts)),
     main = "MA(1) sin media",
     ylab = "")

lines(expo_test, col = "black", lwd = 2)
lines(pron_final_m1_ts, col = "red", lwd = 2)

abline(v = time(expo_test)[1], lty = 2)


# -------------------------------
# Modelo MA(1) con media
# -------------------------------

plot(expo_train,
     xlim = c(time(expo_train)[1], time(expo_test)[length(expo_test)]),
     ylim = range(c(expo_train, expo_test, pron_final_m2_ts)),
     main = "MA(1) con media",
     ylab = "")

lines(expo_test, col = "black", lwd = 2)
lines(pron_final_m2_ts, col = "green", lwd = 2)

abline(v = time(expo_test)[1], lty = 2)


# -------------------------------
# Reset gráfico
# -------------------------------

par(mfrow = c(1,1))


# -------------------------------
# b.8.1 Evaluación de pronósticos
# -------------------------------

eval_auto <- Eval_Pron(pron_final_auto_ts, expo_test, "Auto")
eval_m1   <- Eval_Pron(pron_final_m1_ts, expo_test, "MA1")
eval_m2   <- Eval_Pron(pron_final_m2_ts, expo_test, "MA1_media")

# Mostrar resultados individuales
eval_auto
eval_m1
eval_m2

# -------------------------------
# b.8.2 Tabla comparativa
# -------------------------------

cbind(eval_auto, eval_m1, eval_m2)

# -------------------------------
# b.9 Cuadro comparativo global
# -------------------------------

tabla_total <- cbind(
  eval_expo_auto,
  eval_expo_m1,
  eval_expo_m2,
  eval_auto,
  eval_m1,
  eval_m2
)

# Renombrar columnas correctamente
colnames(tabla_total) <- c(
  "SARIMA_auto",
  "SARIMA_MA",
  "SARIMA_AR",
  "SA_Auto",
  "SA_MA1",
  "SA_MA1_media"
)

# Asegurar que sea numérico (sin perder nombres)
tabla_total <- matrix(as.numeric(tabla_total),
                      nrow = nrow(tabla_total),
                      dimnames = list(
                        rownames(eval_auto),
                        colnames(tabla_total)
                      ))

tabla_total


# -------------------------------
# b.9.1 Marcar mejores valores (*)
# -------------------------------

tabla_mark <- matrix("", nrow = nrow(tabla_total), ncol = ncol(tabla_total),
                     dimnames = dimnames(tabla_total))

for(i in 1:nrow(tabla_total)){
  
  fila <- tabla_total[i, ]
  min_val <- min(fila)
  
  for(j in 1:ncol(tabla_total)){
    
    valor <- round(fila[j], 6)
    
    if(fila[j] == min_val){
      tabla_mark[i, j] <- paste0(valor, " *")
    } else {
      tabla_mark[i, j] <- as.character(valor)
    }
    
  }
}

tabla_mark

# -------------------------------
# b.9.2 Anexo: valores reales y pronósticos
# -------------------------------

# Crear etiquetas de tiempo (Año-Trimestre)
tiempo_labels <- paste(
  floor(time(expo_test)),
  paste0("T", cycle(expo_test)),
  sep = "-"
)

# Construir data.frame
anexo <- data.frame(
  Tiempo = tiempo_labels,
  Real = as.numeric(expo_test),
  
  SARIMA_auto = as.numeric(pron_auto$mean),
  SARIMA_MA   = as.numeric(pron_m1$mean),
  SARIMA_AR   = as.numeric(pron_m2$mean),
  
  SA_Auto     = as.numeric(pron_final_auto_ts),
  SA_MA1      = as.numeric(pron_final_m1_ts),
  SA_MA1_media = as.numeric(pron_final_m2_ts)
)

anexo

# -------------------------------
# b.10 Conclusiones
# -------------------------------

# A partir del análisis comparativo de los estadísticos de error (RMSE, MAE, MAPE y U de Theil)
# y de los cuadros de pronósticos, se concluye que los modelos SARIMA presentan el mejor desempeño
# global para la serie DL_EXPO.
#
# En particular, los modelos SARIMA (tanto el automático como el manual) obtienen los menores valores
# de RMSE, MAE y U de Theil, lo que indica una mayor precisión en términos absolutos y una mejor
# capacidad predictiva general. Además, se observa que el modelo automático coincide con uno de los
# modelos manuales, lo que refuerza la consistencia del proceso de identificación basado en los
# correlogramas.
#
# Por otro lado, los modelos construidos sobre la serie desestacionalizada (ARMA + re-estacionalización)
# muestran un desempeño competitivo, destacándose el modelo MA(1) con media en términos del MAPE.
# Sin embargo, esta métrica resulta menos confiable en series en diferencias logarítmicas, por lo que
# su resultado debe interpretarse con cautela.
#
# En consecuencia, se concluye que, para esta serie, la modelización directa mediante SARIMA es más
# adecuada que la estrategia de desestacionalizar previamente y luego aplicar modelos ARMA.
#
# En términos generales, este ejercicio sugiere que no existe un único método universalmente superior
# para el pronóstico de series temporales, sino que la elección óptima depende de las características
# específicas de la serie. No obstante, cuando la estacionalidad es clara y estable, los modelos SARIMA
# tienden a capturar de manera más eficiente la dinámica de la serie, proporcionando mejores resultados
# de pronóstico.

# =====================================================
# =====================================================
# VARIABLE 3: DL_IBF
# =====================================================
# =====================================================

# -------------------------------
# c.1.1 Serie temporal
# -------------------------------

ibf_ts <- ts(df$DL_IBF, start = c(2004, 2), frequency = 4)

# -------------------------------
# c.1.2 Split train / test
# -------------------------------

ibf_train <- window(ibf_ts, end = c(2023, 4))
ibf_test  <- window(ibf_ts, start = c(2024, 1))

# -------------------------------
# c.1.3 Exploración
# -------------------------------

plot(ibf_ts, main = "DL_IBF", ylab = "Dif. log", xlab = "Tiempo")

Acf(ibf_train, main = "ACF - DL_IBF")
Pacf(ibf_train, main = "PACF - DL_IBF")

# -------------------------------
# c.2.4 Estimación
# -------------------------------

# Los modelos manuales se determinaron a partir del análisis de los correlogramas (ACF y PACF).
# 
# La ACF presenta picos significativos en los rezagos 4, 8, 12, etc., lo que evidencia una
# estacionalidad trimestral clara (s = 4). Esto sugiere la inclusión de un componente
# estacional del tipo MA, específicamente (0,1,1)[4].
# 
# En la parte no estacional, la ACF muestra un pico negativo en el rezago 1 seguido de un
# rápido decaimiento, lo que inicialmente sugiere un posible componente MA(1). Sin embargo,
# la evidencia no es contundente y la PACF no muestra una estructura AR clara.
# 
# En conjunto, esto sugiere que la dinámica de corto plazo es débil y que la estacionalidad
# es el componente dominante de la serie.
# 
# Con base en este análisis, se proponen tres especificaciones:
# 
# - ARIMA(0,0,0)(0,1,1)[4]: modelo puramente estacional (sugerido por auto.arima).
# - ARIMA(0,0,1)(0,1,1)[4]: alternativa que incorpora un componente MA de corto plazo.
# - ARIMA(1,0,0)(0,1,1)[4]: alternativa parsimoniosa con componente AR de corto plazo.
# 
# Estas especificaciones se contrastan para evaluar si los componentes no estacionales
# realmente aportan capacidad explicativa o si la dinámica está completamente dominada
# por la estacionalidad.

# Modelo automático (baseline)
modelo_ibf_auto <- auto.arima(ibf_train, seasonal = TRUE)
summary(modelo_ibf_auto)

# Modelo manual 1 (MA corto plazo)
modelo_ibf_m1 <- Arima(
  ibf_train,
  order = c(0,0,1),
  seasonal = list(order = c(0,1,1), period = 4)
)
summary(modelo_ibf_m1)

# Modelo manual 2 (AR corto plazo)
modelo_ibf_m2 <- Arima(
  ibf_train,
  order = c(1,0,0),
  seasonal = list(order = c(0,1,1), period = 4)
)
summary(modelo_ibf_m2)


# -------------------------------
# c.2.5 Diagnóstico
# -------------------------------

checkresiduals(modelo_ibf_auto)
corr_res(residuals(modelo_ibf_auto), 12)

checkresiduals(modelo_ibf_m1)
corr_res(residuals(modelo_ibf_m1), 12)

checkresiduals(modelo_ibf_m2)
corr_res(residuals(modelo_ibf_m2), 12)


# Cerrar dispositivos gráficos (sin romper si no hay)
if (!is.null(dev.list())) dev.off()

h <- length(ibf_test)


# -------------------------------
# c.3.1 Pronósticos
# -------------------------------

pron_ibf_auto <- forecast(modelo_ibf_auto, h = h)
pron_ibf_m1   <- forecast(modelo_ibf_m1,   h = h)
pron_ibf_m2   <- forecast(modelo_ibf_m2,   h = h)


# -------------------------------
# c.3.2 Gráficos comparativos
# -------------------------------

# Abrir nuevo dispositivo
dev.new(width = 7, height = 10)

par(mfrow = c(3,1), mar = c(2,2,2,2), oma = c(0,0,2,0))

plot(pron_ibf_auto, main = "Auto.arima - DL_IBF")
plot(pron_ibf_m1,   main = "MA corto plazo - DL_IBF")
plot(pron_ibf_m2,   main = "AR corto plazo - DL_IBF")

par(mfrow = c(1,1))

# -------------------------------
# c.4.1 Evaluación individual
# -------------------------------

cat("Modelo Automático\n")
eval_ibf_auto <- Eval_Pron(pron_ibf_auto$mean, ibf_test)

cat("\nModelo MA corto plazo\n")
eval_ibf_m1 <- Eval_Pron(pron_ibf_m1$mean, ibf_test)

cat("\nModelo AR corto plazo\n")
eval_ibf_m2 <- Eval_Pron(pron_ibf_m2$mean, ibf_test)


# -------------------------------
# c.4.2 Comparación conjunta
# -------------------------------

cbind(
  Auto = eval_ibf_auto,
  MA    = eval_ibf_m1,
  AR    = eval_ibf_m2
)


# Los resultados de evaluación fuera de muestra muestran que los tres modelos
# presentan un desempeño muy similar en términos de RMSE, MAE y U de Theil.
#
# Si bien el modelo con componente MA de corto plazo presenta valores ligeramente
# menores en algunas métricas, las diferencias son marginales y no representan una
# mejora sustantiva en la capacidad predictiva.
#
# En este contexto, se opta por el modelo ARIMA(0,0,0)(0,1,1)[4], ya que es el más
# parsimonioso y cuenta con el mejor desempeño en términos de criterios de información,
# sin sacrificar precisión en los pronósticos.

# -------------------------------
# c.5 Desestacionalización - DL_IBF
# -------------------------------

# -------------------------------
# c.5.1 Desestacionalización
# -------------------------------

ibf_x13 <- seas(
  x = ibf_train,
  x11 = "",
  x11.mode = "add",
  transform.function = "none",
  regression.aictest = NULL,
  outlier = NULL
)

# Serie desestacionalizada
ibf_sa <- final(ibf_x13)


# -------------------------------
# c.5.2 Visualización
# -------------------------------

plot(ibf_train,
     main = "DL_IBF original",
     ylab = "Dif. log",
     xlab = "Tiempo")

plot(ibf_sa,
     main = "DL_IBF desestacionalizada",
     ylab = "Dif. log",
     xlab = "Tiempo")

# -------------------------------
# c.5.3 Identificación
# -------------------------------

dev.new(width = 7, height = 5)

acf(ibf_sa,
    lag.max = 20,
    main = "ACF - DL_IBF desestacionalizada")


dev.new(width = 7, height = 5)

pacf(ibf_sa,
     lag.max = 20,
     main = "PACF - DL_IBF desestacionalizada")

# -------------------------------
# c.6 Modelización - DL_IBF (SA)
# -------------------------------

# -------------------------------
# c.6.1 Serie temporal
# -------------------------------

ibf_sa_ts <- ibf_sa


# -------------------------------
# c.6.2 Modelos
# -------------------------------

# Modelo automático
modelo_ibf_sa_auto <- auto.arima(ibf_sa_ts, seasonal = FALSE)
summary(modelo_ibf_sa_auto)

# Modelo base correcto: ruido blanco con media
modelo_ibf_sa_wn <- Arima(ibf_sa_ts,
                          order = c(0,0,0),
                          include.mean = TRUE)
summary(modelo_ibf_sa_wn)

# Modelo alternativo (solo para contraste)
modelo_ibf_sa_m1 <- Arima(ibf_sa_ts,
                          order = c(0,0,1),
                          include.mean = TRUE)
summary(modelo_ibf_sa_m1)


# -------------------------------
# c.6.3 Diagnóstico
# -------------------------------

checkresiduals(modelo_ibf_sa_auto)
corr_res(residuals(modelo_ibf_sa_auto), 12)

checkresiduals(modelo_ibf_sa_wn)
corr_res(residuals(modelo_ibf_sa_wn), 12)

checkresiduals(modelo_ibf_sa_m1)
corr_res(residuals(modelo_ibf_sa_m1), 12)

# -------------------------------
# c.7 Pronóstico y re-estacionalización - DL_IBF
# -------------------------------

h <- length(ibf_test)

# -------------------------------
# c.7.1 Pronósticos sobre la serie desestacionalizada
# -------------------------------

# Modelo automático
pron_ibf_sa_auto <- forecast(modelo_ibf_sa_auto, h = h)

# Modelo seleccionado (MA(1))
pron_ibf_sa_m1   <- forecast(modelo_ibf_sa_m1, h = h)


# -------------------------------
# c.7.2 Componente estacional aditiva
# -------------------------------

comp_sa_ibf <- ibf_train - ibf_sa


# -------------------------------
# c.7.3 Promedios estacionales por trimestre
# -------------------------------

coef_est_ibf <- tapply(comp_sa_ibf, cycle(ibf_train), mean)


# -------------------------------
# c.7.4 Normalización (que sumen cero)
# -------------------------------

coef_est_ibf <- coef_est_ibf - mean(coef_est_ibf)


# -------------------------------
# c.7.5 Extensión al horizonte de pronóstico
# -------------------------------

coef_est_fut_ibf <- rep(coef_est_ibf, length.out = h)


# -------------------------------
# c.7.6 Pronósticos re-estacionalizados
# -------------------------------

pron_final_ibf_auto <- pron_ibf_sa_auto$mean + coef_est_fut_ibf
pron_final_ibf_m1   <- pron_ibf_sa_m1$mean   + coef_est_fut_ibf


# -------------------------------
# c.7.7 Conversión a ts
# -------------------------------

pron_final_ibf_auto_ts <- ts(pron_final_ibf_auto,
                             start = start(ibf_test),
                             frequency = frequency(ibf_test))

pron_final_ibf_m1_ts <- ts(pron_final_ibf_m1,
                           start = start(ibf_test),
                           frequency = frequency(ibf_test))

# -------------------------------
# Modelo MA(1) (válido)
# -------------------------------

plot(ibf_train,
     xlim = c(time(ibf_train)[1], time(ibf_test)[length(ibf_test)]),
     ylim = range(c(ibf_train, ibf_test, pron_final_ibf_m1_ts)),
     main = "Pronóstico DL_IBF - Modelo MA(1)",
     ylab = "",
     xlab = "Tiempo")

lines(ibf_test, col = "black", lwd = 2)
lines(pron_final_ibf_m1_ts, col = "blue", lwd = 2)

abline(v = time(ibf_test)[1], lty = 2, col = "gray40")

# -------------------------------
# c.8.1 Evaluación de pronósticos - DL_IBF (SA)
# -------------------------------

eval_ibf_sa_auto <- Eval_Pron(pron_final_ibf_auto_ts, ibf_test, "Auto")
eval_ibf_sa_m1   <- Eval_Pron(pron_final_ibf_m1_ts,   ibf_test, "MA(1)")

# Mostrar resultados individuales
eval_ibf_sa_auto
eval_ibf_sa_m1


# -------------------------------
# c.8.2 Tabla comparativa (SA)
# -------------------------------

cbind(
  Auto = eval_ibf_sa_auto,
  MA1  = eval_ibf_sa_m1
)

# -------------------------------
# c.9 Cuadro comparativo global - DL_IBF
# -------------------------------

tabla_total <- cbind(
  unname(eval_ibf_auto),
  unname(eval_ibf_m1),
  unname(eval_ibf_m2),
  unname(eval_ibf_sa_auto),
  unname(eval_ibf_sa_m1)
)

# Renombrar columnas
colnames(tabla_total) <- c(
  "SARIMA_auto",
  "SARIMA_MA",
  "SARIMA_AR",
  "SA_Auto",
  "SA_MA1"
)

# Asignar nombres de métricas (ANTES de imprimir)
rownames(tabla_total) <- c(
  "RMSE",
  "MAE",
  "MAPE",
  "U_Theil",
  "U_sesgo",
  "U_varianza",
  "U_covarianza"
)

tabla_total


# -------------------------------
# c.9.1 Marcar mejores valores (*)
# -------------------------------

tabla_mark <- matrix("",
                     nrow = nrow(tabla_total),
                     ncol = ncol(tabla_total),
                     dimnames = dimnames(tabla_total))

for(i in 1:nrow(tabla_total)){
  
  fila <- tabla_total[i, ]
  min_val <- min(fila, na.rm = TRUE)
  
  for(j in 1:ncol(tabla_total)){
    
    valor <- round(fila[j], 6)
    
    if(fila[j] == min_val){
      tabla_mark[i, j] <- paste0(valor, " *")
    } else {
      tabla_mark[i, j] <- as.character(valor)
    }
    
  }
}

tabla_mark


# -------------------------------
# c.9.2 Anexo: valores reales y pronósticos - DL_IBF
# -------------------------------

# Validación de consistencia
stopifnot(
  length(ibf_test) == length(pron_ibf_auto$mean),
  length(ibf_test) == length(pron_final_ibf_auto_ts)
)

# Crear etiquetas de tiempo (Año-Trimestre)
tiempo_labels <- paste(
  floor(time(ibf_test)),
  paste0("T", cycle(ibf_test)),
  sep = "-"
)

# Construcción del anexo
anexo <- data.frame(
  Tiempo = tiempo_labels,
  Real = as.numeric(ibf_test),
  
  SARIMA_auto = as.numeric(pron_ibf_auto$mean),
  SARIMA_MA   = as.numeric(pron_ibf_m1$mean),
  SARIMA_AR   = as.numeric(pron_ibf_m2$mean),
  
  SA_Auto     = as.numeric(pron_final_ibf_auto_ts),
  SA_MA1      = as.numeric(pron_final_ibf_m1_ts)
)

anexo

# -------------------------------
# c.10 Conclusiones sobre el mejor método de pronóstico
# -------------------------------

# A partir del análisis de los cuadros comparativos y las métricas de evaluación
# (RMSE, MAE, MAPE y U de Theil), se concluye que los modelos SARIMA estimados
# sobre la serie original superan consistentemente a los modelos basados en la
# serie desestacionalizada (SA).

# En particular, el modelo SARIMA con componente MA(1) presenta el mejor desempeño
# global, al registrar los menores valores en las métricas de error (RMSE y MAE)
# y en el coeficiente U de Theil, lo que indica una mayor capacidad predictiva.

# Por el contrario, los modelos construidos a partir de la serie desestacionalizada
# muestran un deterioro en la calidad del pronóstico, evidenciado en errores más
# elevados y una menor capacidad para capturar la dinámica de la serie, especialmente
# en períodos con cambios abruptos o picos.

# Este resultado sugiere que la estacionalidad contiene información relevante para
# el proceso generador de datos, por lo que su modelación directa mediante componentes
# estacionales (SARIMA) resulta más adecuada que su eliminación previa.

# En síntesis, el mejor método de pronóstico para esta serie es el modelo SARIMA
# (particularmente la especificación con componente MA), ya que logra un mejor
# equilibrio entre ajuste, parsimonia y capacidad predictiva.

# =====================================================
# =====================================================
# VARIABLE 4: DL_PIB
# =====================================================
# =====================================================

# -------------------------------
# d.1.1 Serie temporal
# -------------------------------

pib_ts <- ts(df$DL_PIB, start = c(2004, 2), frequency = 4)

# -------------------------------
# d.1.2 Split train / test
# -------------------------------

pib_train <- window(pib_ts, end = c(2023, 4))
pib_test  <- window(pib_ts, start = c(2024, 1))

# -------------------------------
# d.1.3 Exploración
# -------------------------------

plot(pib_ts, main = "DL_PIB", ylab = "Dif. log", xlab = "Tiempo")

Acf(pib_train, lag.max = 20)
title(main = "ACF - DL_PIB")

Pacf(pib_train, lag.max = 20)
title(main = "PACF - DL_PIB")

# -------------------------------
# d.2.1 Estimación
# -------------------------------

# Los modelos manuales se determinaron a partir del análisis de los correlogramas (ACF y PACF).
#
# La ACF muestra picos significativos en los rezagos 4, 8, 12, etc., lo que evidencia una
# estacionalidad trimestral clara (s = 4). Esto sugiere la inclusión de un componente
# estacional del tipo MA, específicamente (0,1,1)[4].
#
# En la parte no estacional, la ACF presenta un pico significativo (negativo) en el rezago 1
# seguido de un rápido decaimiento, lo que es consistente con un proceso MA(1).
#
# La PACF no muestra un corte claro que indique una estructura AR dominante, aunque se observan
# algunos rezagos iniciales significativos, lo que sugiere que podría existir cierta dinámica
# autoregresiva de bajo orden.
#
# En conjunto, esto sugiere como modelo principal un ARIMA(0,0,1)(0,1,1)[4],
# con una alternativa ARIMA(1,0,1)(0,1,1)[4] para capturar posibles efectos AR adicionales.
#
# Estas especificaciones se contrastan con el modelo sugerido por auto.arima(),
# evaluando su desempeño mediante criterios de información y diagnóstico de residuos.

# Modelo automático (baseline)
modelo_pib_auto <- auto.arima(pib_train, seasonal = TRUE)
summary(modelo_pib_auto)

# Modelo manual 1 (MA corto plazo - principal)
modelo_pib_m1 <- Arima(
  pib_train,
  order = c(0,0,1),
  seasonal = list(order = c(0,1,1), period = 4)
)
summary(modelo_pib_m1)

# Modelo manual 2 (ARMA - alternativa)
modelo_pib_m2 <- Arima(
  pib_train,
  order = c(1,0,1),
  seasonal = list(order = c(0,1,1), period = 4)
)
summary(modelo_pib_m2)


# -------------------------------
# d.2.2 Diagnóstico
# -------------------------------

checkresiduals(modelo_pib_auto)
corr_res(residuals(modelo_pib_auto), 12)

checkresiduals(modelo_pib_m1)
corr_res(residuals(modelo_pib_m1), 12)

checkresiduals(modelo_pib_m2)
corr_res(residuals(modelo_pib_m2), 12)


# Cerrar dispositivos gráficos (sin romper si no hay)
if (!is.null(dev.list())) dev.off()

h <- length(pib_test)

# -------------------------------
# d.3.1 Pronósticos
# -------------------------------

pron_pib_auto <- forecast(modelo_pib_auto, h = h)
pron_pib_m1   <- forecast(modelo_pib_m1,   h = h)
pron_pib_m2   <- forecast(modelo_pib_m2,   h = h)


# -------------------------------
# d.3.2 Gráficos comparativos
# -------------------------------

# Abrir nuevo dispositivo
dev.new(width = 7, height = 10)

par(mfrow = c(3,1), mar = c(2,2,2,2), oma = c(0,0,2,0))

plot(pron_pib_auto, main = "Auto.arima - DL_PIB")
plot(pron_pib_m1,   main = "MA corto plazo - DL_PIB")
plot(pron_pib_m2,   main = "ARMA corto plazo - DL_PIB")

par(mfrow = c(1,1))

# -------------------------------
# d.4.1 Evaluación individual
# -------------------------------

cat("Modelo Automático\n")
eval_pib_auto <- Eval_Pron(pron_pib_auto$mean, pib_test)

cat("\nModelo MA corto plazo\n")
eval_pib_m1 <- Eval_Pron(pron_pib_m1$mean, pib_test)

cat("\nModelo ARMA corto plazo\n")
eval_pib_m2 <- Eval_Pron(pron_pib_m2$mean, pib_test)


# -------------------------------
# d.4.2 Comparación conjunta
# -------------------------------

tabla_pib <- cbind(
  eval_pib_auto,
  eval_pib_m1,
  eval_pib_m2
)

# Renombrar columnas correctamente
colnames(tabla_pib) <- c(
  "SARIMA_auto",
  "SARIMA_MA",
  "SARIMA_ARMA"
)

tabla_pib

# Los resultados de evaluación fuera de muestra muestran que los tres modelos
# presentan un desempeño muy similar en términos de RMSE, MAE, MAPE y U de Theil,
# lo que indica que todos logran capturar adecuadamente la dinámica de la serie.
#
# No obstante, las diferencias entre modelos son marginales. El modelo ARMA presenta
# valores ligeramente menores en algunas métricas (RMSE, MAE y U de Theil), pero
# esta mejora es muy pequeña y no resulta estadísticamente significativa.
#
# Adicionalmente, dicho modelo presenta problemas de estimación en sus parámetros,
# lo que lo hace menos confiable desde el punto de vista econométrico.
#
# Por su parte, el modelo con componente MA tampoco muestra una mejora sustantiva
# respecto al modelo automático, lo que refuerza la idea de que la dinámica de corto
# plazo no es relevante en esta serie.
#
# En consecuencia, se selecciona el modelo ARIMA(0,0,0)(0,1,1)[4], ya que es el más
# parsimonioso, presenta el mejor criterio de información (AIC) y logra un desempeño
# predictivo equivalente sin introducir complejidad innecesaria.

# -------------------------------
# d.5 Desestacionalización - DL_PIB
# -------------------------------

# -------------------------------
# d.5.1 Desestacionalización
# -------------------------------

pib_x13 <- seas(
  x = pib_train,
  x11 = "",
  x11.mode = "add",
  transform.function = "none",
  regression.aictest = NULL,
  outlier = NULL
)

# Serie desestacionalizada
pib_sa <- final(pib_x13)


# -------------------------------
# d.5.2 Visualización
# -------------------------------

# Gráfico 1
dev.new(width = 7, height = 5)

plot(pib_train,
     main = "DL_PIB original",
     ylab = "Dif. log",
     xlab = "Tiempo")


# Gráfico 2
dev.new(width = 7, height = 5)

plot(pib_sa,
     main = "DL_PIB desestacionalizada",
     ylab = "Dif. log",
     xlab = "Tiempo")


# -------------------------------
# d.5.3 Identificación
# -------------------------------

# --- ACF ---
dev.new(width = 7, height = 6)

Acf(pib_sa,
    lag.max = 20,
    main = "")
title("ACF - DL_PIB desestacionalizada")


# --- PACF ---
dev.new(width = 7, height = 6)

Pacf(pib_sa,
     lag.max = 20,
     main = "")
title("PACF - DL_PIB desestacionalizada")

# -------------------------------
# d.6 Modelización - DL_PIB (SA)
# -------------------------------

# -------------------------------
# d.6.1 Serie temporal
# -------------------------------

pib_sa_ts <- pib_sa


# -------------------------------
# d.6.2 Modelos
# -------------------------------

# Modelo automático
modelo_pib_sa_auto <- auto.arima(pib_sa_ts, seasonal = FALSE)
summary(modelo_pib_sa_auto)

# Modelo base: ruido blanco con media
modelo_pib_sa_wn <- Arima(
  pib_sa_ts,
  order = c(0,0,0),
  include.mean = TRUE
)
summary(modelo_pib_sa_wn)

# Modelo alternativo (solo para contraste)
modelo_pib_sa_m1 <- Arima(
  pib_sa_ts,
  order = c(0,0,1),
  include.mean = TRUE
)
summary(modelo_pib_sa_m1)


# -------------------------------
# d.6.3 Diagnóstico
# -------------------------------

checkresiduals(modelo_pib_sa_auto)
corr_res(residuals(modelo_pib_sa_auto), 12)

checkresiduals(modelo_pib_sa_wn)
corr_res(residuals(modelo_pib_sa_wn), 12)

checkresiduals(modelo_pib_sa_m1)
corr_res(residuals(modelo_pib_sa_m1), 12)

# ===============================
# d.7 Pronóstico y re-estacionalización - DL_PIB
# ===============================

h <- length(pib_test)

# Pronósticos sobre la serie desestacionalizada
pron_sa_auto <- forecast(modelo_pib_sa_auto, h = h)
pron_sa_wn   <- forecast(modelo_pib_sa_wn,   h = h)
pron_sa_m1   <- forecast(modelo_pib_sa_m1,   h = h)

# Componente estacional (aditivo)
comp_sa <- pib_train - pib_sa

# Promedios estacionales por trimestre
coef_est <- tapply(comp_sa, cycle(pib_train), mean)

# Normalización (media cero)
coef_est <- coef_est - mean(coef_est)

# Extensión al horizonte de pronóstico
coef_est_fut <- rep(coef_est, length.out = h)

# Re-estacionalización
pron_final_auto <- pron_sa_auto$mean + coef_est_fut
pron_final_wn   <- pron_sa_wn$mean   + coef_est_fut
pron_final_m1   <- pron_sa_m1$mean   + coef_est_fut

# Conversión a objetos ts
pron_auto_ts <- ts(pron_final_auto,
                   start = start(pib_test),
                   frequency = frequency(pib_test))

pron_wn_ts <- ts(pron_final_wn,
                 start = start(pib_test),
                 frequency = frequency(pib_test))

pron_m1_ts <- ts(pron_final_m1,
                 start = start(pib_test),
                 frequency = frequency(pib_test))

# -------------------------------
# d.7.7 Visualización
# -------------------------------

# Rango común (comparabilidad)
ylim_comun <- range(c(pib_train, pib_test,
                      pron_auto_ts, pron_wn_ts, pron_m1_ts))

# Un solo device
dev.new(width = 8, height = 12)

par(mfrow = c(3,1), mar = c(3,4,3,1))

# --- Modelo Automático ---
plot(pib_train,
     xlim = c(time(pib_train)[1], time(pib_test)[length(pib_test)]),
     ylim = ylim_comun,
     main = "DL_PIB - Modelo SA Auto",
     ylab = "")

lines(pib_test, col = "black", lwd = 2)
lines(pron_auto_ts, col = "blue", lwd = 2)
abline(v = time(pib_test)[1], lty = 2)

# --- Modelo Ruido Blanco ---
plot(pib_train,
     xlim = c(time(pib_train)[1], time(pib_test)[length(pib_test)]),
     ylim = ylim_comun,
     main = "DL_PIB - Modelo SA WN",
     ylab = "")

lines(pib_test, col = "black", lwd = 2)
lines(pron_wn_ts, col = "red", lwd = 2)
abline(v = time(pib_test)[1], lty = 2)

# --- Modelo MA(1) ---
plot(pib_train,
     xlim = c(time(pib_train)[1], time(pib_test)[length(pib_test)]),
     ylim = ylim_comun,
     main = "DL_PIB - Modelo SA MA(1)",
     ylab = "",
     xlab = "Tiempo")

lines(pib_test, col = "black", lwd = 2)
lines(pron_m1_ts, col = "darkgreen", lwd = 2)
abline(v = time(pib_test)[1], lty = 2)

# Reset
par(mfrow = c(1,1))


# -------------------------------
# d.8 Evaluación de pronósticos - DL_PIB (SA)
# -------------------------------

# Evaluación individual
eval_pib_sa_auto <- Eval_Pron(
  pron_auto_ts,
  pib_test,
  "Auto_SA"
)

eval_pib_sa_wn <- Eval_Pron(
  pron_wn_ts,
  pib_test,
  "WN_SA"
)

eval_pib_sa_m1 <- Eval_Pron(
  pron_m1_ts,
  pib_test,
  "MA1_SA"
)

# Mostrar resultados individuales
eval_pib_sa_auto
eval_pib_sa_wn
eval_pib_sa_m1


# -------------------------------
# d.8.2 Tabla comparativa (SA)
# -------------------------------

tabla_pib_sa <- cbind(
  Auto = unname(eval_pib_sa_auto),
  WN   = unname(eval_pib_sa_wn),
  MA1  = unname(eval_pib_sa_m1)
)

tabla_pib_sa

# -------------------------------
# d.9 Cuadro comparativo global - DL_PIB
# -------------------------------

tabla_total <- cbind(
  SARIMA_auto = unname(eval_pib_auto),
  SARIMA_MA   = unname(eval_pib_m1),
  SARIMA_ARMA = unname(eval_pib_m2),
  SA_Auto     = unname(eval_pib_sa_auto),
  SA_WN       = unname(eval_pib_sa_wn),
  SA_MA1      = unname(eval_pib_sa_m1)
)

# Asignar nombres de métricas
rownames(tabla_total) <- c(
  "RMSE",
  "MAE",
  "MAPE",
  "U_Theil",
  "U_sesgo",
  "U_varianza",
  "U_covarianza"
)

tabla_total

# -------------------------------
# d.9.1 Marcar mejores valores (*)
# -------------------------------

tabla_mark <- matrix("",
                     nrow = nrow(tabla_total),
                     ncol = ncol(tabla_total),
                     dimnames = dimnames(tabla_total))

for(i in 1:nrow(tabla_total)){
  
  fila <- tabla_total[i, ]
  min_val <- min(fila, na.rm = TRUE)
  
  for(j in 1:ncol(tabla_total)){
    
    valor <- round(fila[j], 6)
    
    if(fila[j] == min_val){
      tabla_mark[i, j] <- paste0(valor, " *")
    } else {
      tabla_mark[i, j] <- as.character(valor)
    }
    
  }
}

tabla_mark

# -------------------------------
# d.9.2 Anexo: valores reales y pronósticos - DL_PIB
# -------------------------------

stopifnot(
  length(pib_test) == length(pron_pib_auto$mean),
  length(pib_test) == length(pron_final_pib_ts)
)

tiempo_labels <- paste(
  floor(time(pib_test)),
  paste0("T", cycle(pib_test)),
  sep = "-"
)

anexo <- data.frame(
  Tiempo = tiempo_labels,
  Real = as.numeric(pib_test),
  
  SARIMA_auto = as.numeric(pron_pib_auto$mean),
  SARIMA_MA   = as.numeric(pron_pib_m1$mean),
  SARIMA_AR   = as.numeric(pron_pib_m2$mean),
  
  SA_WN       = as.numeric(pron_final_pib_ts)
)

anexo


# -------------------------------
# c.10 Conclusiones sobre el mejor método de pronóstico
# -------------------------------

# A partir del análisis comparativo de los modelos estimados para la serie DL_PIB,
# se concluye que los modelos SARIMA presentan un mejor desempeño en términos de
# capacidad predictiva frente al enfoque basado en desestacionalización seguida
# de un modelo de ruido blanco.
#
# Aunque la serie desestacionalizada no evidencia una estructura dinámica clara
# y puede ser representada adecuadamente como un proceso de ruido blanco con media,
# la modelación directa sobre la serie original mediante modelos SARIMA permite
# capturar de manera más eficiente la dinámica conjunta del proceso, incluyendo
# posibles dependencias temporales residuales.
#
# En particular, el modelo SARIMA con componente autorregresivo (SARIMA_AR) muestra
# los menores errores de pronóstico (RMSE, MAE y U-Theil), así como un mejor ajuste
# a la trayectoria observada en el horizonte de evaluación.
#
# Por su parte, el modelo basado en desestacionalización resulta altamente parsimonioso
# y sin sesgo, pero limitado en su capacidad para reproducir la variabilidad de la serie,
# lo que se traduce en un mayor error de pronóstico.
#
# En consecuencia, para la serie DL_PIB se concluye que el mejor método de pronóstico
# es el modelo SARIMA_AR, ya que ofrece un balance óptimo entre precisión y capacidad
# de capturar la dinámica temporal de la serie.

