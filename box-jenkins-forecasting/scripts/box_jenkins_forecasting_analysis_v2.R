#1. Paquetes
paquetes <- c(
  "readxl",
  "forecast",
  "seasonal",
  "tseries",
  "ggplot2",
  "dplyr",
  "writexl"
)

# librerías
library(readxl)
library(forecast)
library(seasonal)
library(tseries)
library(ggplot2)
library(dplyr)
library(writexl)

# 2. Carga archivo

Datos_TP1 <- read_excel("C:/Users/julla/Downloads/1. Series de Tiempo/TP1/Datos TP1.xlsx", 
                        sheet = "Series TP1")
View(Datos_TP1)

# EDA datos
str(Datos_TP1)
head(Datos_TP1)
names(Datos_TP1)

#3. Series trimestrales:

cpriv <- ts(Datos_TP1$DL_CPRIV, start = c(2004, 2), frequency = 4)
expo  <- ts(Datos_TP1$DL_EXPO,  start = c(2004, 2), frequency = 4)
ibf   <- ts(Datos_TP1$DL_IBF,   start = c(2004, 2), frequency = 4)
pib   <- ts(Datos_TP1$DL_PIB,   start = c(2004, 2), frequency = 4)

#cpriv
start(cpriv); end(cpriv); frequency(cpriv)
# expo
start(expo);  end(expo);  frequency(expo)
# ibf
start(ibf);   end(ibf);   frequency(ibf)
# pib
start(pib);   end(pib);   frequency(pib)

#4. Conjunto de train y test

# train
cpriv_train <- window(cpriv, end = c(2023, 4))
expo_train  <- window(expo,  end = c(2023, 4))
ibf_train   <- window(ibf,   end = c(2023, 4))
pib_train   <- window(pib,   end = c(2023, 4))

# test
cpriv_test <- window(cpriv, start = c(2024, 1), end = c(2025, 4))
expo_test  <- window(expo,  start = c(2024, 1), end = c(2025, 4))
ibf_test   <- window(ibf,   start = c(2024, 1), end = c(2025, 4))
pib_test   <- window(pib,   start = c(2024, 1), end = c(2025, 4))

# horizonte 8 trim
length(cpriv_test)
length(expo_test)
length(ibf_test)
length(pib_test)

#5. Serie temporal  de la muestra tomada y autocorrelaciones

autoplot(pib_train) + ggtitle("PIB - muestra de estimación")
Acf(pib_train, main = "ACF PIB")
Pacf(pib_train, main = "PACF PIB")

autoplot(cpriv_train) + ggtitle("Consumo privado - muestra de estimación")
Acf(cpriv_train, main = "ACF cpriv")
Pacf(cpriv_train, main = "PACF cpriv")

autoplot(expo_train) + ggtitle("Exportaciones - muestra de estimación")
Acf(expo_train, main = "ACF expo")
Pacf(expo_train, main = "PACF expo")

autoplot(ibf_train) + ggtitle("Inversión Bruta Fija - muestra de estimación")
Acf(ibf_train, main = "ACF ibf")
Pacf(ibf_train, main = "PACF ibf")

#6. Método 1: modelo ARMA sobre serie cruda

#PIB
modelo_pib_crudo <- auto.arima(
  pib_train,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)
summary(modelo_pib_crudo)

#cpriv
modelo_cpriv_crudo <- auto.arima(
  cpriv_train,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)
summary(modelo_cpriv_crudo)

#expo
modelo_expo_crudo <- auto.arima (
  expo_train,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)
summary(modelo_expo_crudo)

#ibf
modelo_ibf_crudo <- auto.arima(
  ibf_train,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE
)
summary(modelo_ibf_crudo)

# TABLA DE MODELOS ARIMA (SERIES CRUDAS)

# PIB
tabla_pib_crudo <- data.frame(
  Serie = "PIB",
  Modelo = paste0("ARIMA(", paste(arimaorder(modelo_pib_crudo), collapse = ","), ")"),
  AIC = AIC(modelo_pib_crudo),
  BIC = BIC(modelo_pib_crudo),
  LogLik = as.numeric(logLik(modelo_pib_crudo)),
  Sigma2 = modelo_pib_crudo$sigma2
)

# Consumo Privado
tabla_cpriv_crudo <- data.frame(
  Serie = "Consumo Privado",
  Modelo = paste0("ARIMA(", paste(arimaorder(modelo_cpriv_crudo), collapse = ","), ")"),
  AIC = AIC(modelo_cpriv_crudo),
  BIC = BIC(modelo_cpriv_crudo),
  LogLik = as.numeric(logLik(modelo_cpriv_crudo)),
  Sigma2 = modelo_cpriv_crudo$sigma2
)

# Exportaciones
tabla_expo_crudo <- data.frame(
  Serie = "Exportaciones",
  Modelo = paste0("ARIMA(", paste(arimaorder(modelo_expo_crudo), collapse = ","), ")"),
  AIC = AIC(modelo_expo_crudo),
  BIC = BIC(modelo_expo_crudo),
  LogLik = as.numeric(logLik(modelo_expo_crudo)),
  Sigma2 = modelo_expo_crudo$sigma2
)

# Inversión Bruta Fija
tabla_ibf_crudo <- data.frame(
  Serie = "Inversion Bruta Fija",
  Modelo = paste0("ARIMA(", paste(arimaorder(modelo_ibf_crudo), collapse = ","), ")"),
  AIC = AIC(modelo_ibf_crudo),
  BIC = BIC(modelo_ibf_crudo),
  LogLik = as.numeric(logLik(modelo_ibf_crudo)),
  Sigma2 = modelo_ibf_crudo$sigma2
)

# TABLA FINAL UNIFICADA
tabla_modelos_crudo <- rbind(
  tabla_pib_crudo,
  tabla_cpriv_crudo,
  tabla_expo_crudo,
  tabla_ibf_crudo
)
#7. Residuos

checkresiduals(modelo_pib_crudo)
checkresiduals(modelo_cpriv_crudo)
checkresiduals(modelo_expo_crudo)
checkresiduals(modelo_ibf_crudo)

#8. Pronostico

#PIB
pron_pib_crudo <- forecast(modelo_pib_crudo, h = 8)

autoplot(pron_pib_crudo) +
  autolayer(pib_test, series = "Real") +
  ggtitle("PIB - Pronóstico modelo crudo")

pron_pib_crudo$mean

#cpriv
pron_cpriv_crudo <- forecast(modelo_cpriv_crudo, h = 8)

autoplot(pron_cpriv_crudo) +
  autolayer(cpriv_test, series = "Real") +
  ggtitle("cpriv - Pronóstico modelo crudo")

pron_cpriv_crudo$mean

#expo
pron_expo_crudo <- forecast(modelo_expo_crudo, h = 8)

autoplot(pron_expo_crudo) +
  autolayer(expo_test, series = "Real") +
  ggtitle("expo - Pronóstico modelo crudo")

pron_expo_crudo$mean

#ibf
pron_ibf_crudo <- forecast(modelo_ibf_crudo, h = 8)

autoplot(pron_ibf_crudo) +
  autolayer(ibf_test, series = "Real") +
  ggtitle("ibf - Pronóstico modelo crudo")

pron_ibf_crudo$mean

#8. Metricas del modelo

#pib
real <- as.numeric(pib_test)
pred_crudo <- as.numeric(pron_pib_crudo$mean)

err_crudo <- real - pred_crudo

metricas_pib_crudo <- data.frame(
  Serie = "PIB",
  Metodo = "Cruda",
  RMSE = sqrt(mean(err_crudo^2, na.rm = TRUE)),
  MAE  = mean(abs(err_crudo), na.rm = TRUE),
  MAPE = mean(abs(err_crudo / real), na.rm = TRUE) * 100
)

metricas_pib_crudo

#cpriv
real_cpriv <- as.numeric(cpriv_test)
pred_crudo_cpriv <- as.numeric(pron_cpriv_crudo$mean)

err_crudo_cpriv <- real_cpriv - pred_crudo_cpriv

metricas_cpriv_crudo <- data.frame(
  Serie = "cpriv",
  Metodo = "Cruda",
  RMSE = sqrt(mean(err_crudo_cpriv^2, na.rm = TRUE)),
  MAE  = mean(abs(err_crudo_cpriv), na.rm = TRUE),
  MAPE = mean(abs(err_crudo_cpriv / real), na.rm = TRUE) * 100
)

metricas_cpriv_crudo

#expo
real_expo <- as.numeric(expo_test)
pred_crudo_expo <- as.numeric(pron_expo_crudo$mean)

err_crudo_expo <- real_expo - pred_crudo_expo

metricas_expo_crudo <- data.frame(
  Serie = "expo",
  Metodo = "Cruda",
  RMSE = sqrt(mean(err_crudo_expo^2, na.rm = TRUE)),
  MAE  = mean(abs(err_crudo_expo), na.rm = TRUE),
  MAPE = mean(abs(err_crudo_expo / real), na.rm = TRUE) * 100
)

metricas_expo_crudo

#ibf
real_ibf <- as.numeric(ibf_test)
pred_crudo_ibf <- as.numeric(pron_ibf_crudo$mean)

err_crudo_ibf <- real_ibf - pred_crudo_ibf

metricas_ibf_crudo <- data.frame(
  Serie = "ibf",
  Metodo = "Cruda",
  RMSE = sqrt(mean(err_crudo_ibf^2, na.rm = TRUE)),
  MAE  = mean(abs(err_crudo_ibf), na.rm = TRUE),
  MAPE = mean(abs(err_crudo_ibf / real), na.rm = TRUE) * 100
)

metricas_ibf_crudo

#10. Método 2: desestacionalización X13

# X13 SOBRE PIB

serie_x13_pib <- seas(
  x = pib_train,
  x11 = "",
  x11.mode = "add",
  transform.function = "none",
  regression.aictest = NULL,
  outlier = NULL
)

pib_sa <- final(serie_x13_pib)

autoplot(pib_train, series = "Original") +
  autolayer(pib_sa, series = "Desestacionalizada") +
  ggtitle("PIB original vs PIB desestacionalizado")

# X13 SOBRE cpriv

serie_x13_cpriv <- seas(
  x = cpriv_train,
  x11 = "",
  x11.mode = "add",
  transform.function = "none",
  regression.aictest = NULL,
  outlier = NULL
)

cpriv_sa <- final(serie_x13_cpriv)

autoplot(cpriv_train, series = "Original") +
  autolayer(cpriv_sa, series = "Desestacionalizada") +
  ggtitle("cpriv original vs cpriv desestacionalizado")

# X13 SOBRE expo

serie_x13_expo <- seas(
  x = expo_train,
  x11 = "",
  x11.mode = "add",
  transform.function = "none",
  regression.aictest = NULL,
  outlier = NULL
)

expo_sa <- final(serie_x13_expo)

autoplot(expo_train, series = "Original") +
  autolayer(expo_sa, series = "Desestacionalizada") +
  ggtitle("expo original vs expo desestacionalizado")

# x13 SOBRE ibf

serie_x13_ibf <- seas(
  x = ibf_train,
  x11 = "",
  x11.mode = "add",
  transform.function = "none",
  regression.aictest = NULL,
  outlier = NULL
)

ibf_sa <- final(serie_x13_ibf)

autoplot(ibf_train, series = "Original") +
  autolayer(ibf_sa, series = "Desestacionalizada") +
  ggtitle("ibf original vs ibf desestacionalizado")


#11. Componente estacional y coef trimestrales 
# PIB

comp_sa_pib <- pib_train - pib_sa

df_comp_pib <- data.frame(
  comp = as.numeric(comp_sa_pib),
  trim = cycle(comp_sa_pib)
)

prom_est_pib <- df_comp_pib %>%
  group_by(trim) %>%
  summarise(prom = mean(comp, na.rm = TRUE), .groups = "drop")

media_global_pib <- mean(prom_est_pib$prom, na.rm = TRUE)

prom_est_pib <- prom_est_pib %>%
  mutate(prom_norm = prom - media_global_pib)

prom_est_pib
sum(prom_est_pib$prom_norm)

# CONSUMO PRIVADO

comp_sa_cpriv <- cpriv_train - cpriv_sa

df_comp_cpriv <- data.frame(
  comp = as.numeric(comp_sa_cpriv),
  trim = cycle(comp_sa_cpriv)
)

prom_est_cpriv <- df_comp_cpriv %>%
  group_by(trim) %>%
  summarise(prom = mean(comp, na.rm = TRUE), .groups = "drop")

media_global_cpriv <- mean(prom_est_cpriv$prom, na.rm = TRUE)

prom_est_cpriv <- prom_est_cpriv %>%
  mutate(prom_norm = prom - media_global_cpriv)

prom_est_cpriv
sum(prom_est_cpriv$prom_norm)

# EXPORTACIONES

comp_sa_expo <- expo_train - expo_sa

df_comp_expo <- data.frame(
  comp = as.numeric(comp_sa_expo),
  trim = cycle(comp_sa_expo)
)

prom_est_expo <- df_comp_expo %>%
  group_by(trim) %>%
  summarise(prom = mean(comp, na.rm = TRUE), .groups = "drop")

media_global_expo <- mean(prom_est_expo$prom, na.rm = TRUE)

prom_est_expo <- prom_est_expo %>%
  mutate(prom_norm = prom - media_global_expo)

prom_est_expo
sum(prom_est_expo$prom_norm)

# INVERSIÓN BRUTA FIJA

comp_sa_ibf <- ibf_train - ibf_sa

df_comp_ibf <- data.frame(
  comp = as.numeric(comp_sa_ibf),
  trim = cycle(comp_sa_ibf)
)

prom_est_ibf <- df_comp_ibf %>%
  group_by(trim) %>%
  summarise(prom = mean(comp, na.rm = TRUE), .groups = "drop")

media_global_ibf <- mean(prom_est_ibf$prom, na.rm = TRUE)

prom_est_ibf <- prom_est_ibf %>%
  mutate(prom_norm = prom - media_global_ibf)

prom_est_ibf
sum(prom_est_ibf$prom_norm)

# 12: MODELO SOBRE SERIE SA

# PIB
modelo_pib_sa <- auto.arima(
  pib_sa,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE
)

summary(modelo_pib_sa)
checkresiduals(modelo_pib_sa)


# Consumo Privado
modelo_cpriv_sa <- auto.arima(
  cpriv_sa,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE
)

summary(modelo_cpriv_sa)
checkresiduals(modelo_cpriv_sa)


# Exportaciones
modelo_expo_sa <- auto.arima(
  expo_sa,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE
)

summary(modelo_expo_sa)
checkresiduals(modelo_expo_sa)


# Inversión Bruta Fija
modelo_ibf_sa <- auto.arima(
  ibf_sa,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE
)

summary(modelo_ibf_sa)
checkresiduals(modelo_ibf_sa)

# 13: PRONÓSTICO SA + REESTACIONALIZACIÓN
# PIB

pron_pib_sa <- forecast(modelo_pib_sa, h = 8)

ultimo_trim_train_pib <- cycle(pib_train)[length(pib_train)]
futuros_trim_pib <- ((ultimo_trim_train_pib + seq_len(8) - 1) %% 4) + 1

coef_est_pib <- prom_est_pib$prom_norm
estacional_futura_pib <- coef_est_pib[futuros_trim_pib]

pron_pib_final <- as.numeric(pron_pib_sa$mean) + estacional_futura_pib

pron_pib_final

# CONSUMO PRIVADO

pron_cpriv_sa <- forecast(modelo_cpriv_sa, h = 8)

ultimo_trim_train_cpriv <- cycle(cpriv_train)[length(cpriv_train)]
futuros_trim_cpriv <- ((ultimo_trim_train_cpriv + seq_len(8) - 1) %% 4) + 1

coef_est_cpriv <- prom_est_cpriv$prom_norm
estacional_futura_cpriv <- coef_est_cpriv[futuros_trim_cpriv]

pron_cpriv_final <- as.numeric(pron_cpriv_sa$mean) + estacional_futura_cpriv

pron_cpriv_final

# EXPORTACIONES

pron_expo_sa <- forecast(modelo_expo_sa, h = 8)

ultimo_trim_train_expo <- cycle(expo_train)[length(expo_train)]
futuros_trim_expo <- ((ultimo_trim_train_expo + seq_len(8) - 1) %% 4) + 1

coef_est_expo <- prom_est_expo$prom_norm
estacional_futura_expo <- coef_est_expo[futuros_trim_expo]

pron_expo_final <- as.numeric(pron_expo_sa$mean) + estacional_futura_expo

pron_expo_final


# INVERSIÓN BRUTA FIJA

pron_ibf_sa <- forecast(modelo_ibf_sa, h = 8)

ultimo_trim_train_ibf <- cycle(ibf_train)[length(ibf_train)]
futuros_trim_ibf <- ((ultimo_trim_train_ibf + seq_len(8) - 1) %% 4) + 1

coef_est_ibf <- prom_est_ibf$prom_norm
estacional_futura_ibf <- coef_est_ibf[futuros_trim_ibf]

pron_ibf_final <- as.numeric(pron_ibf_sa$mean) + estacional_futura_ibf

pron_ibf_final


#14: MÉTRICAS MODELO DESESTACIONALIZADO

# PIB
real_pib <- as.numeric(pib_test)
pred_pib_sa <- as.numeric(pron_pib_final)

err_pib_sa <- real_pib - pred_pib_sa

metricas_pib_sa <- data.frame(
  Serie = "PIB",
  Metodo = "Desestacionalizada",
  RMSE = sqrt(mean(err_pib_sa^2, na.rm = TRUE)),
  MAE  = mean(abs(err_pib_sa), na.rm = TRUE),
  MAPE = mean(abs(err_pib_sa / real_pib), na.rm = TRUE) * 100
)

metricas_pib_sa

# CONSUMO PRIVADO

real_cpriv <- as.numeric(cpriv_test)
pred_cpriv_sa <- as.numeric(pron_cpriv_final)

err_cpriv_sa <- real_cpriv - pred_cpriv_sa

metricas_cpriv_sa <- data.frame(
  Serie = "Consumo Privado",
  Metodo = "Desestacionalizada",
  RMSE = sqrt(mean(err_cpriv_sa^2, na.rm = TRUE)),
  MAE  = mean(abs(err_cpriv_sa), na.rm = TRUE),
  MAPE = mean(abs(err_cpriv_sa / real_cpriv), na.rm = TRUE) * 100
)

metricas_cpriv_sa

# EXPORTACIONES
real_expo <- as.numeric(expo_test)
pred_expo_sa <- as.numeric(pron_expo_final)

err_expo_sa <- real_expo - pred_expo_sa

metricas_expo_sa <- data.frame(
  Serie = "Exportaciones",
  Metodo = "Desestacionalizada",
  RMSE = sqrt(mean(err_expo_sa^2, na.rm = TRUE)),
  MAE  = mean(abs(err_expo_sa), na.rm = TRUE),
  MAPE = mean(abs(err_expo_sa / real_expo), na.rm = TRUE) * 100
)

metricas_expo_sa

# INVERSIÓN BRUTA FIJA

real_ibf <- as.numeric(ibf_test)
pred_ibf_sa <- as.numeric(pron_ibf_final)

err_ibf_sa <- real_ibf - pred_ibf_sa

metricas_ibf_sa <- data.frame(
  Serie = "Inversion Bruta Fija",
  Metodo = "Desestacionalizada",
  RMSE = sqrt(mean(err_ibf_sa^2, na.rm = TRUE)),
  MAE  = mean(abs(err_ibf_sa), na.rm = TRUE),
  MAPE = mean(abs(err_ibf_sa / real_ibf), na.rm = TRUE) * 100
)

metricas_ibf_sa

#15: COMPARACIÓN PIB

# PIB
comp_pib <- rbind(metricas_pib_crudo, metricas_pib_sa)
comp_pib

# Consumo Privado
comp_cpriv <- rbind(metricas_cpriv_crudo, metricas_cpriv_sa)
comp_cpriv

# Exportaciones
comp_expo <- rbind(metricas_expo_crudo, metricas_expo_sa)
comp_expo

# Inversión Bruta Fija
comp_ibf <- rbind(metricas_ibf_crudo, metricas_ibf_sa)
comp_ibf

# TABLA FINAL COMPARATIVA
tabla_comparacion <- rbind(
  metricas_pib_crudo,   metricas_pib_sa,
  metricas_cpriv_crudo, metricas_cpriv_sa,
  metricas_expo_crudo,  metricas_expo_sa,
  metricas_ibf_crudo,   metricas_ibf_sa
)

tabla_comparacion


##################################################

#FUNCIÓN AUTOMÁTICA

evaluar_serie_tp_guardar <- function(
    serie,
    nombre_serie,
    h = 8,
    mostrar_graficos = TRUE,
    guardar_graficos = TRUE,
    carpeta_salida = "graficos_tp1",
    ancho = 10,
    alto = 6,
    dpi = 300
) {
  

  # 0) Preparar carpeta y nombre limpio

  nombre_limpio <- gsub("[^[:alnum:]_]+", "_", tolower(nombre_serie))
  
  if (guardar_graficos) {
    if (!dir.exists(carpeta_salida)) {
      dir.create(carpeta_salida, recursive = TRUE)
    }
  }
  

  # 1) Train y test

  serie_train <- window(serie, end = c(2023, 4))
  serie_test  <- window(serie, start = c(2024, 1), end = c(2025, 4))
  
  if (length(serie_test) != h) {
    stop(paste("La serie", nombre_serie, "no tiene exactamente", h, "observaciones en test."))
  }
  
  real <- as.numeric(serie_test)
  

  # 2) Gráfico serie original

  g_serie <- autoplot(serie_train) +
    ggtitle(paste("Serie original -", nombre_serie)) +
    xlab("Tiempo") +
    ylab("Valor")
  
  if (mostrar_graficos) print(g_serie)
  
  if (guardar_graficos) {
    ggsave(
      filename = file.path(carpeta_salida, paste0(nombre_limpio, "_01_serie_original.png")),
      plot = g_serie,
      width = ancho,
      height = alto,
      dpi = dpi
    )
  }
  

  # 3) ACF y PACF

  if (guardar_graficos) {
    png(file.path(carpeta_salida, paste0(nombre_limpio, "_02_acf.png")),
        width = ancho, height = alto, units = "in", res = dpi)
    Acf(serie_train, main = paste("ACF -", nombre_serie))
    dev.off()
    
    png(file.path(carpeta_salida, paste0(nombre_limpio, "_03_pacf.png")),
        width = ancho, height = alto, units = "in", res = dpi)
    Pacf(serie_train, main = paste("PACF -", nombre_serie))
    dev.off()
  }
  
  if (mostrar_graficos) {
    Acf(serie_train, main = paste("ACF -", nombre_serie))
    Pacf(serie_train, main = paste("PACF -", nombre_serie))
  }
  

  # 4) Método crudo

  modelo_crudo <- auto.arima(
    serie_train,
    seasonal = TRUE,
    stepwise = FALSE,
    approximation = FALSE
  )
  
  pron_crudo <- forecast(modelo_crudo, h = h)
  pred_crudo <- as.numeric(pron_crudo$mean)
  err_crudo <- real - pred_crudo
  
  metricas_crudo <- data.frame(
    Serie = nombre_serie,
    Metodo = "Cruda",
    RMSE = sqrt(mean(err_crudo^2, na.rm = TRUE)),
    MAE  = mean(abs(err_crudo), na.rm = TRUE),
    MAPE = mean(abs(err_crudo / real), na.rm = TRUE) * 100
  )
  
  g_crudo <- autoplot(pron_crudo) +
    autolayer(serie_test, series = "Real") +
    ggtitle(paste("Pronóstico método crudo -", nombre_serie)) +
    xlab("Tiempo") +
    ylab("Valor")
  
  if (mostrar_graficos) print(g_crudo)
  
  if (guardar_graficos) {
    ggsave(
      filename = file.path(carpeta_salida, paste0(nombre_limpio, "_04_pronostico_crudo.png")),
      plot = g_crudo,
      width = ancho,
      height = alto,
      dpi = dpi
    )
  }
  
  if (guardar_graficos) {
    png(file.path(carpeta_salida, paste0(nombre_limpio, "_05_residuos_crudo.png")),
        width = ancho, height = alto, units = "in", res = dpi)
    checkresiduals(modelo_crudo)
    dev.off()
  }
  
  if (mostrar_graficos) {
    checkresiduals(modelo_crudo)
  }
  

  # 5) Método desestacionalizado

  serie_x13 <- seas(
    x = serie_train,
    x11 = "",
    x11.mode = "add",
    transform.function = "none",
    regression.aictest = NULL,
    outlier = NULL
  )
  
  serie_sa <- final(serie_x13)
  comp_sa <- serie_train - serie_sa
  
  g_original_sa <- autoplot(serie_train, series = "Original") +
    autolayer(serie_sa, series = "Desestacionalizada") +
    ggtitle(paste("Original vs desestacionalizada -", nombre_serie)) +
    xlab("Tiempo") +
    ylab("Valor")
  
  if (mostrar_graficos) print(g_original_sa)
  
  if (guardar_graficos) {
    ggsave(
      filename = file.path(carpeta_salida, paste0(nombre_limpio, "_06_original_vs_sa.png")),
      plot = g_original_sa,
      width = ancho,
      height = alto,
      dpi = dpi
    )
  }
  
  g_comp <- autoplot(comp_sa) +
    ggtitle(paste("Componente estacional -", nombre_serie)) +
    xlab("Tiempo") +
    ylab("Valor")
  
  if (mostrar_graficos) print(g_comp)
  
  if (guardar_graficos) {
    ggsave(
      filename = file.path(carpeta_salida, paste0(nombre_limpio, "_07_componente_estacional.png")),
      plot = g_comp,
      width = ancho,
      height = alto,
      dpi = dpi
    )
  }
  
  df_comp <- data.frame(
    comp = as.numeric(comp_sa),
    trim = cycle(comp_sa)
  )
  
  prom_est <- df_comp %>%
    group_by(trim) %>%
    summarise(prom = mean(comp, na.rm = TRUE), .groups = "drop")
  
  media_global <- mean(prom_est$prom, na.rm = TRUE)
  prom_est <- prom_est %>%
    mutate(prom_norm = prom - media_global)
  
  modelo_sa <- auto.arima(
    serie_sa,
    seasonal = FALSE,
    stepwise = FALSE,
    approximation = FALSE
  )
  
  pron_sa <- forecast(modelo_sa, h = h)
  
  ultimo_trim_train <- cycle(serie_train)[length(serie_train)]
  futuros_trim <- ((ultimo_trim_train + seq_len(h) - 1) %% 4) + 1
  
  coef_est <- prom_est$prom_norm
  estacional_futura <- coef_est[futuros_trim]
  
  pred_sa_final <- as.numeric(pron_sa$mean) + estacional_futura
  err_sa <- real - pred_sa_final
  
  metricas_sa <- data.frame(
    Serie = nombre_serie,
    Metodo = "Desestacionalizada",
    RMSE = sqrt(mean(err_sa^2, na.rm = TRUE)),
    MAE  = mean(abs(err_sa), na.rm = TRUE),
    MAPE = mean(abs(err_sa / real), na.rm = TRUE) * 100
  )
  
  pron_sa_ts <- ts(pred_sa_final, start = c(2024, 1), frequency = 4)
  
  g_sa <- autoplot(pron_sa_ts, series = "Pronóstico SA reestacionalizado") +
    autolayer(serie_test, series = "Real") +
    ggtitle(paste("Pronóstico método desestacionalizado -", nombre_serie)) +
    xlab("Tiempo") +
    ylab("Valor")
  
  if (mostrar_graficos) print(g_sa)
  
  if (guardar_graficos) {
    ggsave(
      filename = file.path(carpeta_salida, paste0(nombre_limpio, "_08_pronostico_desestacionalizado.png")),
      plot = g_sa,
      width = ancho,
      height = alto,
      dpi = dpi
    )
  }
  
  if (guardar_graficos) {
    png(file.path(carpeta_salida, paste0(nombre_limpio, "_09_residuos_desestacionalizado.png")),
        width = ancho, height = alto, units = "in", res = dpi)
    checkresiduals(modelo_sa)
    dev.off()
  }
  
  if (mostrar_graficos) {
    checkresiduals(modelo_sa)
  }

  # 6) Tabla de pronósticos

  tabla_pron <- data.frame(
    Serie = nombre_serie,
    Periodo = as.character(time(serie_test)),
    Real = real,
    Pron_Cruda = pred_crudo,
    Pron_Desest = pred_sa_final
  )
  
  tabla_metricas <- rbind(metricas_crudo, metricas_sa)
  

  # 7) Guardar tablas CSV opcionales

  if (guardar_graficos) {
    write.csv(
      tabla_metricas,
      file = file.path(carpeta_salida, paste0(nombre_limpio, "_metricas.csv")),
      row.names = FALSE
    )
    
    write.csv(
      tabla_pron,
      file = file.path(carpeta_salida, paste0(nombre_limpio, "_pronosticos.csv")),
      row.names = FALSE
    )
    
    write.csv(
      prom_est,
      file = file.path(carpeta_salida, paste0(nombre_limpio, "_coef_estacionales.csv")),
      row.names = FALSE
    )
  }
  
  list(
    metricas = tabla_metricas,
    pronosticos = tabla_pron,
    modelo_crudo = modelo_crudo,
    modelo_sa = modelo_sa,
    coef_estacionales = prom_est,
    serie_sa = serie_sa,
    comp_sa = comp_sa
  )
}


# SECCIÓN 17: APLICAR A LAS 4 SERIES


res_cpriv <- evaluar_serie_tp_guardar(cpriv, "Consumo Privado", carpeta_salida = "graficos_tp1")
res_expo  <- evaluar_serie_tp_guardar(expo,  "Exportaciones", carpeta_salida = "graficos_tp1")
res_ibf   <- evaluar_serie_tp_guardar(ibf,   "Inversion Bruta Fija", carpeta_salida = "graficos_tp1")
res_pib   <- evaluar_serie_tp_guardar(pib,   "PIB", carpeta_salida = "graficos_tp1")


# SECCIÓN 18: TABLA FINAL DE MÉTRICAS


tabla_metricas <- rbind(
  res_cpriv$metricas,
  res_expo$metricas,
  res_ibf$metricas,
  res_pib$metricas
)

tabla_metricas


# SECCIÓN 19: MARCAR GANADOR POR MÉTRICA


tabla_metricas_resumen <- tabla_metricas %>%
  group_by(Serie) %>%
  mutate(
    Mejor_RMSE = ifelse(RMSE == min(RMSE, na.rm = TRUE), "*", ""),
    Mejor_MAE  = ifelse(MAE  == min(MAE,  na.rm = TRUE), "*", ""),
    Mejor_MAPE = ifelse(MAPE == min(MAPE, na.rm = TRUE), "*", "")
  ) %>%
  ungroup()

tabla_metricas_resumen


# SECCIÓN 20: TABLA DE PRONÓSTICOS


tabla_pronosticos <- rbind(
  res_cpriv$pronosticos,
  res_expo$pronosticos,
  res_ibf$pronosticos,
  res_pib$pronosticos
)

tabla_pronosticos


# SECCIÓN 21: MODELOS ELEGIDOS


res_cpriv$modelo_crudo
res_cpriv$modelo_sa

res_expo$modelo_crudo
res_expo$modelo_sa

res_ibf$modelo_crudo
res_ibf$modelo_sa

res_pib$modelo_crudo
res_pib$modelo_sa
