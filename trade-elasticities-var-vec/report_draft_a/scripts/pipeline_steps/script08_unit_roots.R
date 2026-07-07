
# ============================================================
# TP3 - SERIES DE TIEMPO
# Script 08: Raíces unitarias y raíces unitarias estacionales
#
# Herramientas:
#   1. Test.ADF.Ver.3 de la cátedra
#   2. Test OCSB del paquete forecast
#
# Muestra:
#   2004Q1 - 2025Q4
# ============================================================


# ------------------------------------------------------------
# 0. LIMPIAR EL ENTORNO
# ------------------------------------------------------------

rm(list = ls())
gc()

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE
)


# ------------------------------------------------------------
# 1. INSTALAR Y CARGAR PAQUETES
# ------------------------------------------------------------

paquetes <- c(
  "tidyverse",
  "here",
  "dynlm",
  "urca",
  "forecast",
  "writexl"
)

paquetes_faltantes <- paquetes[
  !paquetes %in% rownames(installed.packages())
]

if (length(paquetes_faltantes) > 0) {
  install.packages(paquetes_faltantes)
}

library(tidyverse)
library(here)
library(dynlm)
library(urca)
library(forecast)
library(writexl)


# ------------------------------------------------------------
# 2. CREAR CARPETAS DE SALIDA
# ------------------------------------------------------------

dir.create(
  here::here("outputs", "tables"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here::here("outputs", "models"),
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# 3. LOCALIZAR LA FUNCIÓN Test.ADF.Ver.3
# ------------------------------------------------------------

rutas_posibles_adf <- c(
  here::here(
    "scripts",
    "required_scripts",
    "Test.ADF_Ver.3.R"
  ),
  here::here(
    "scripts",
    "required_scripts",
    "Test.ADF.Ver.3.R"
  ),
  here::here(
    "scripts",
    "Test.ADF_Ver.3.R"
  ),
  here::here(
    "scripts",
    "Test.ADF.Ver.3.R"
  )
)

rutas_adf_existentes <- rutas_posibles_adf[
  file.exists(rutas_posibles_adf)
]

if (length(rutas_adf_existentes) == 0) {
  stop(
    paste0(
      "No se encontró Test.ADF_Ver.3.R.\n\n",
      "Guarda el archivo de la cátedra en:\n",
      "scripts/required_scripts/Test.ADF_Ver.3.R"
    )
  )
}

ruta_adf_curso <- rutas_adf_existentes[1]


# ------------------------------------------------------------
# 4. CARGAR LA FUNCIÓN DE LA CÁTEDRA
# ------------------------------------------------------------

source(
  ruta_adf_curso,
  encoding = "UTF-8"
)

if (!exists("Test.ADF.Ver.3")) {
  stop(
    paste0(
      "El archivo fue cargado, pero no creó la función ",
      "Test.ADF.Ver.3()."
    )
  )
}

cat(
  "\nFunción Test.ADF.Ver.3 cargada desde:\n",
  ruta_adf_curso,
  "\n"
)


# ------------------------------------------------------------
# 5. CARGAR EL PANEL MAESTRO
# ------------------------------------------------------------

ruta_panel <- here::here(
  "data",
  "processed",
  "final_data_panel",
    "panel_maestro_2004_2025.rds"
)

if (!file.exists(ruta_panel)) {
  stop(
    paste0(
      "No se encontró el panel maestro:\n",
      ruta_panel
    )
  )
}

panel_maestro <- readRDS(
  ruta_panel
) |>
  arrange(periodo)


# ------------------------------------------------------------
# 6. MOSTRAR COBERTURA DE LA MUESTRA
# ------------------------------------------------------------

cat(
  "\n============================================================",
  "\nMUESTRA DEL ANÁLISIS",
  "\n============================================================",

  "\nPeríodo inicial:",
  format(
    min(panel_maestro$periodo),
    "%Y Q%q"
  ),

  "\nPeríodo final:",
  format(
    max(panel_maestro$periodo),
    "%Y Q%q"
  ),

  "\nObservaciones:",
  nrow(panel_maestro),

  "\n============================================================",
  "\n"
)


# ------------------------------------------------------------
# 7. CONFIGURAR LAS SERIES
# ------------------------------------------------------------

# Modelos de Test.ADF.Ver.3:
#
# mod = 1:
#   sin intercepto ni tendencia.
#
# mod = 2:
#   con intercepto, sin tendencia.
#
# mod = 3:
#   con intercepto y tendencia.
#
# En niveles:
# - Importaciones, exportaciones, PIB argentino y PIB de socios:
#   modelo 3.
# - ITCRM y commodities:
#   modelo 2.
#
# En primeras diferencias:
# - Todas las series:
#   modelo 2.

config_series <- tibble::tribble(

  ~clave,
  ~etiqueta,
  ~variable_nivel,
  ~variable_diferencia,
  ~modelo_nivel,
  ~modelo_diferencia,
  ~justificacion_modelo_nivel,

  "imports",
  "Importaciones reales",
  "ln_importaciones_reales",
  "d_ln_importaciones_reales",
  3L,
  2L,
  "Cantidad real con tendencia de largo plazo",

  "exports",
  "Exportaciones reales",
  "ln_exportaciones_reales",
  "d_ln_exportaciones_reales",
  3L,
  2L,
  "Cantidad real con tendencia de largo plazo",

  "gdp_arg",
  "PIB real de Argentina",
  "ln_pib_real",
  "d_ln_pib_real",
  3L,
  2L,
  "Producto real con tendencia de largo plazo",

  "itcrm",
  "ITCRM",
  "ln_itcrm",
  "d_ln_itcrm",
  2L,
  2L,
  "Índice relativo sin tendencia determinística impuesta",

  "pib_socios",
  "PIB de socios comerciales",
  "ln_pib_socios",
  "d_ln_pib_socios",
  3L,
  2L,
  "Producto real con tendencia de largo plazo",

  "commodities",
  "Índice de commodities",
  "ln_commodity_price_index",
  "d_ln_commodity_price_index",
  2L,
  2L,
  "Índice de precios sin tendencia determinística impuesta"
)

print(
  config_series,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 8. VERIFICAR VARIABLES REQUERIDAS
# ------------------------------------------------------------

variables_requeridas <- c(
  "periodo",
  "anio",
  "trimestre",
  config_series$variable_nivel,
  config_series$variable_diferencia
)

variables_faltantes <- setdiff(
  variables_requeridas,
  names(panel_maestro)
)

if (length(variables_faltantes) > 0) {
  stop(
    paste0(
      "Faltan variables en el panel maestro:\n",
      paste(
        variables_faltantes,
        collapse = ", "
      )
    )
  )
}


# ------------------------------------------------------------
# 9. CONTROLAR VALORES FALTANTES
# ------------------------------------------------------------

tabla_faltantes <- panel_maestro |>
  summarise(
    across(
      all_of(
        c(
          config_series$variable_nivel,
          config_series$variable_diferencia
        )
      ),
      ~ sum(is.na(.x))
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "faltantes"
  )

print(
  tabla_faltantes,
  n = Inf
)

faltantes_en_niveles <- tabla_faltantes |>
  filter(
    variable %in%
      config_series$variable_nivel,
    faltantes > 0
  )

if (nrow(faltantes_en_niveles) > 0) {
  stop(
    "Existen valores faltantes en las series en niveles."
  )
}

faltantes_en_diferencias <- tabla_faltantes |>
  filter(
    variable %in%
      config_series$variable_diferencia
  )

if (
  any(
    faltantes_en_diferencias$faltantes != 1
  )
) {
  warning(
    paste0(
      "Alguna primera diferencia no tiene exactamente ",
      "un valor faltante inicial. Revisa tabla_faltantes."
    )
  )
}


# ------------------------------------------------------------
# 10. CREAR SERIES DE TIEMPO TRIMESTRALES
# ------------------------------------------------------------

inicio_ts <- c(
  panel_maestro$anio[1],
  panel_maestro$trimestre[1]
)

series_niveles <- purrr::set_names(
  lapply(
    config_series$variable_nivel,
    function(variable) {
      stats::ts(
        panel_maestro[[variable]],
        start = inicio_ts,
        frequency = 4
      )
    }
  ),
  config_series$clave
)

series_diferencias <- purrr::set_names(
  lapply(
    config_series$variable_diferencia,
    function(variable) {

      valores <- panel_maestro[[variable]]

      posicion_inicial <- which(
        !is.na(valores)
      )[1]

      valores_validos <- valores[
        !is.na(valores)
      ]

      stats::ts(
        valores_validos,
        start = c(
          panel_maestro$anio[
            posicion_inicial
          ],
          panel_maestro$trimestre[
            posicion_inicial
          ]
        ),
        frequency = 4
      )
    }
  ),
  config_series$clave
)


# ------------------------------------------------------------
# 11. CONTROLAR LOS OBJETOS ts
# ------------------------------------------------------------

tabla_control_ts <- tibble(
  clave =
    config_series$clave,

  etiqueta =
    config_series$etiqueta,

  observaciones_nivel = as.integer(
    sapply(
      series_niveles,
      length
    )
  ),

  frecuencia_nivel = as.integer(
    sapply(
      series_niveles,
      frequency
    )
  ),

  observaciones_diferencia = as.integer(
    sapply(
      series_diferencias,
      length
    )
  ),

  frecuencia_diferencia = as.integer(
    sapply(
      series_diferencias,
      frequency
    )
  )
)

print(
  tabla_control_ts,
  n = Inf
)

stopifnot(
  nrow(tabla_control_ts) == 6,

  all(
    tabla_control_ts$
      observaciones_nivel == 88
  ),

  all(
    tabla_control_ts$
      observaciones_diferencia == 87
  ),

  all(
    tabla_control_ts$
      frecuencia_nivel == 4
  ),

  all(
    tabla_control_ts$
      frecuencia_diferencia == 4
  )
)


# ------------------------------------------------------------
# 12. CREAR ESPECIFICACIONES ADF
# ------------------------------------------------------------

especificaciones_adf <- bind_rows(

  config_series |>
    transmute(
      clave,
      etiqueta,

      variable =
        variable_nivel,

      transformacion =
        "Logaritmo en nivel",

      modelo_adf =
        modelo_nivel
    ),

  config_series |>
    transmute(
      clave,
      etiqueta,

      variable =
        variable_diferencia,

      transformacion =
        "Primera diferencia logarítmica",

      modelo_adf =
        modelo_diferencia
    )
)

criterios_rezagos <- c(
  "AIC",
  "BIC",
  "SW"
)

print(
  especificaciones_adf,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 13. FUNCIÓN PARA EJECUTAR DIRECTAMENTE
#     Test.ADF.Ver.3
# ------------------------------------------------------------

ejecutar_adf_docente <- function(
    y,
    mod,
    criterio,
    alfa = 0.10
) {

  if (
    !criterio %in%
    c(
      "AIC",
      "BIC",
      "SW"
    )
  ) {
    stop(
      "El criterio debe ser AIC, BIC o SW."
    )
  }

  if (
    !mod %in%
    c(
      1L,
      2L,
      3L
    )
  ) {
    stop(
      "El modelo ADF debe ser 1, 2 o 3."
    )
  }

  x <- as.numeric(
    y
  )

  x <- x[
    !is.na(x)
  ]

  # Se ejecuta directamente la función de la cátedra.
  # La salida se captura para guardarla y para identificar
  # el rezago seleccionado.

  valores_criticos_docente <- NULL

  salida_texto <- capture.output({

    valores_criticos_docente <-
      Test.ADF.Ver.3(
        y = x,
        mod = mod,
        sel.lags = criterio,
        alfa = alfa,
        reporte = FALSE
      )
  })


  # ----------------------------------------------------------
  # Extraer el rezago seleccionado
  # ----------------------------------------------------------

  linea_rezago <- salida_texto[
    stringr::str_detect(
      salida_texto,
      stringr::regex(
        "lags\\s*=",
        ignore_case = TRUE
      )
    )
  ]

  if (length(linea_rezago) == 0) {
    stop(
      paste0(
        "No se pudo extraer el rezago elegido.\n",
        "Criterio: ",
        criterio,
        "\nSalida obtenida:\n",
        paste(
          salida_texto,
          collapse = "\n"
        )
      )
    )
  }

  coincidencia_rezago <- stringr::str_match(
    linea_rezago[1],
    "lags\\s*=\\s*([0-9]+)"
  )

  rezago_elegido <- suppressWarnings(
    as.integer(
      coincidencia_rezago[1, 2]
    )
  )

  if (
    is.na(rezago_elegido) ||
    rezago_elegido < 0
  ) {
    stop(
      paste0(
        "No se pudo interpretar el rezago elegido.\n",
        "Línea encontrada: ",
        linea_rezago[1]
      )
    )
  }


  # ----------------------------------------------------------
  # Extraer el máximo de rezagos
  # ----------------------------------------------------------

  linea_maxlag <- salida_texto[
    stringr::str_detect(
      salida_texto,
      stringr::regex(
        "Cantidad máxima de lags",
        ignore_case = TRUE
      )
    )
  ]

  if (length(linea_maxlag) == 0) {

    maximo_rezagos <- NA_integer_

  } else {

    coincidencia_maxlag <- stringr::str_match(
      linea_maxlag[1],
      "Cantidad máxima de lags:\\s*([0-9]+)"
    )

    maximo_rezagos <- suppressWarnings(
      as.integer(
        coincidencia_maxlag[1, 2]
      )
    )
  }


  # ----------------------------------------------------------
  # Traducir el modelo al argumento type de ur.df
  # ----------------------------------------------------------

  tipo_urdf <- dplyr::case_when(
    mod == 1L ~ "none",
    mod == 2L ~ "drift",
    mod == 3L ~ "trend"
  )


  # ----------------------------------------------------------
  # Recuperar valores exactos con el mismo rezago
  # seleccionado por Test.ADF.Ver.3
  # ----------------------------------------------------------

  modelo_adf <- urca::ur.df(
    y = x,
    type = tipo_urdf,
    lags = rezago_elegido,
    selectlags = "Fixed"
  )

  estadistico_tau <- as.numeric(
    modelo_adf@teststat[1]
  )

  valores_criticos <- modelo_adf@cval[
    1,
  ]

  valor_critico_1pct <- as.numeric(
    valores_criticos[
      "1pct"
    ]
  )

  valor_critico_5pct <- as.numeric(
    valores_criticos[
      "5pct"
    ]
  )

  valor_critico_10pct <- as.numeric(
    valores_criticos[
      "10pct"
    ]
  )


  # ----------------------------------------------------------
  # Crear resultado estructurado
  # ----------------------------------------------------------

  resultado_tabla <- tibble(
    criterio_rezagos =
      criterio,

    modelo_adf =
      mod,

    componente_deterministico =
      tipo_urdf,

    observaciones =
      length(x),

    maximo_rezagos =
      maximo_rezagos,

    rezagos_elegidos =
      rezago_elegido,

    estadistico_tau =
      estadistico_tau,

    valor_critico_1pct =
      valor_critico_1pct,

    valor_critico_5pct =
      valor_critico_5pct,

    valor_critico_10pct =
      valor_critico_10pct,

    rechaza_raiz_unitaria_1pct =
      estadistico_tau <
      valor_critico_1pct,

    rechaza_raiz_unitaria_5pct =
      estadistico_tau <
      valor_critico_5pct,

    rechaza_raiz_unitaria_10pct =
      estadistico_tau <
      valor_critico_10pct
  )

  list(
    tabla =
      resultado_tabla,

    salida_texto =
      salida_texto
  )
}


# ------------------------------------------------------------
# 14. EJECUTAR LAS 36 PRUEBAS ADF
# ------------------------------------------------------------

lista_resultados_adf <- list()

salida_adf_docente <- character()

contador_adf <- 1L

for (
  i in seq_len(
    nrow(especificaciones_adf)
  )
) {

  especificacion_i <-
    especificaciones_adf[
      i,
    ]

  serie_i <- panel_maestro[[
    especificacion_i$variable
  ]]

  serie_i <- serie_i[
    !is.na(serie_i)
  ]

  for (
    criterio_i in criterios_rezagos
  ) {

    cat(
      "\nEstimando:",
      especificacion_i$etiqueta,
      "|",
      especificacion_i$transformacion,
      "|",
      criterio_i,
      "\n"
    )

    resultado_ejecucion <- tryCatch(

      ejecutar_adf_docente(
        y = serie_i,

        mod =
          especificacion_i$modelo_adf,

        criterio =
          criterio_i,

        alfa =
          0.10
      ),

      error = function(e) {
        stop(
          paste0(
            "\nError al ejecutar Test.ADF.Ver.3:\n",
            "Serie: ",
            especificacion_i$etiqueta,
            "\nTransformación: ",
            especificacion_i$transformacion,
            "\nCriterio: ",
            criterio_i,
            "\nMensaje: ",
            conditionMessage(e)
          )
        )
      }
    )

    resultado_i <- resultado_ejecucion$tabla |>
      mutate(
        clave =
          especificacion_i$clave,

        etiqueta =
          especificacion_i$etiqueta,

        variable =
          especificacion_i$variable,

        transformacion =
          especificacion_i$transformacion,

        .before = 1
      )

    lista_resultados_adf[[
      contador_adf
    ]] <- resultado_i

    contador_adf <- contador_adf + 1L

    encabezado_i <- c(
      "",
      paste0(
        "============================================================"
      ),

      paste0(
        "Serie: ",
        especificacion_i$etiqueta
      ),

      paste0(
        "Transformación: ",
        especificacion_i$transformacion
      ),

      paste0(
        "Modelo ADF: ",
        especificacion_i$modelo_adf
      ),

      paste0(
        "Criterio de rezagos: ",
        criterio_i
      ),

      paste0(
        "============================================================"
      )
    )

    salida_adf_docente <- c(
      salida_adf_docente,
      encabezado_i,
      resultado_ejecucion$salida_texto
    )
  }
}

resultados_adf <- bind_rows(
  lista_resultados_adf
)

print(
  resultados_adf,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 15. CONTROLAR Y GUARDAR SALIDAS ADF
# ------------------------------------------------------------

if (nrow(resultados_adf) != 36) {
  stop(
    paste0(
      "Se esperaban 36 resultados ADF, pero se generaron ",
      nrow(resultados_adf),
      "."
    )
  )
}

control_adf <- resultados_adf |>
  summarise(
    resultados =
      n(),

    rezagos_faltantes = sum(
      is.na(
        rezagos_elegidos
      )
    ),

    estadisticos_faltantes = sum(
      is.na(
        estadistico_tau
      )
    ),

    valores_criticos_faltantes = sum(
      is.na(
        valor_critico_10pct
      )
    )
  )

print(
  control_adf
)

stopifnot(
  control_adf$resultados == 36,

  control_adf$rezagos_faltantes == 0,

  control_adf$estadisticos_faltantes == 0,

  control_adf$
    valores_criticos_faltantes == 0
)

writeLines(
  salida_adf_docente,
  con = here::here(
    "outputs",
    "models",
    "salidas_Test_ADF_Ver_3.txt"
  )
)


# ------------------------------------------------------------
# 16. RESULTADOS PRINCIPALES CON AIC
# ------------------------------------------------------------

tabla_adf_principal <- resultados_adf |>
  filter(
    criterio_rezagos == "AIC"
  ) |>
  mutate(
    decision_1pct = if_else(
      rechaza_raiz_unitaria_1pct,
      "Se rechaza H0 de raíz unitaria",
      "No se rechaza H0 de raíz unitaria"
    ),

    decision_5pct = if_else(
      rechaza_raiz_unitaria_5pct,
      "Se rechaza H0 de raíz unitaria",
      "No se rechaza H0 de raíz unitaria"
    ),

    decision_10pct = if_else(
      rechaza_raiz_unitaria_10pct,
      "Se rechaza H0 de raíz unitaria",
      "No se rechaza H0 de raíz unitaria"
    )
  )

print(
  tabla_adf_principal,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 17. ROBUSTEZ ENTRE AIC, BIC Y SW
# ------------------------------------------------------------

robustez_adf <- resultados_adf |>
  group_by(
    clave,
    etiqueta,
    variable,
    transformacion
  ) |>
  summarise(
    criterios_evaluados =
      n(),

    rechazos_10pct = sum(
      rechaza_raiz_unitaria_10pct,
      na.rm = TRUE
    ),

    rechazos_5pct = sum(
      rechaza_raiz_unitaria_5pct,
      na.rm = TRUE
    ),

    decision_robusta_10pct = case_when(
      rechazos_10pct ==
        criterios_evaluados ~
        "Rechazo con AIC, BIC y SW",

      rechazos_10pct == 0 ~
        "No rechazo con AIC, BIC y SW",

      TRUE ~
        "Decisión sensible al criterio"
    ),

    decision_robusta_5pct = case_when(
      rechazos_5pct ==
        criterios_evaluados ~
        "Rechazo con AIC, BIC y SW",

      rechazos_5pct == 0 ~
        "No rechazo con AIC, BIC y SW",

      TRUE ~
        "Decisión sensible al criterio"
    ),

    .groups = "drop"
  )

print(
  robustez_adf,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 18. DETERMINAR EL ORDEN DE INTEGRACIÓN
# ------------------------------------------------------------

tabla_orden_integracion <- tabla_adf_principal |>
  select(
    clave,
    etiqueta,
    transformacion,
    estadistico_tau,
    valor_critico_5pct,
    valor_critico_10pct,
    rezagos_elegidos,
    rechaza_raiz_unitaria_5pct,
    rechaza_raiz_unitaria_10pct
  ) |>
  mutate(
    tipo = if_else(
      transformacion ==
        "Logaritmo en nivel",
      "nivel",
      "diferencia"
    )
  ) |>
  select(
    -transformacion
  ) |>
  pivot_wider(
    names_from =
      tipo,

    values_from = c(
      estadistico_tau,
      valor_critico_5pct,
      valor_critico_10pct,
      rezagos_elegidos,
      rechaza_raiz_unitaria_5pct,
      rechaza_raiz_unitaria_10pct
    )
  ) |>
  mutate(
    orden_integracion_10pct = case_when(
      rechaza_raiz_unitaria_10pct_nivel ~
        "I(0)",

      !rechaza_raiz_unitaria_10pct_nivel &
        rechaza_raiz_unitaria_10pct_diferencia ~
        "I(1)",

      TRUE ~
        "No concluyente"
    ),

    orden_integracion_5pct = case_when(
      rechaza_raiz_unitaria_5pct_nivel ~
        "I(0)",

      !rechaza_raiz_unitaria_5pct_nivel &
        rechaza_raiz_unitaria_5pct_diferencia ~
        "I(1)",

      TRUE ~
        "No concluyente"
    ),

    apta_para_cointegracion = case_when(
      orden_integracion_10pct ==
        "I(1)" ~
        "Sí",

      TRUE ~
        "Revisar"
    )
  )

print(
  tabla_orden_integracion,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 19. FUNCIÓN ROBUSTA PARA OCSB
# ------------------------------------------------------------

# Hipótesis nula:
#   existe una raíz unitaria estacional.
#
# Hipótesis alternativa:
#   estacionariedad estacional.
#
# Regla utilizada por forecast:
#
#   estadístico OCSB > valor crítico
#
# significa que se recomienda una diferencia estacional:
#
#   D = 1
#
# En caso contrario:
#
#   D = 0
#
# El valor crítico de OCSB corresponde al 5%.

ejecutar_ocsb <- function(
    x,
    clave,
    etiqueta
) {

  if (!is.ts(x)) {
    x <- stats::ts(
      as.numeric(x),
      frequency = 4
    )
  }

  if (frequency(x) != 4) {
    return(
      tibble(
        clave = clave,
        etiqueta = etiqueta,
        prueba = "OCSB",
        estado = "ERROR",
        observaciones = length(x),
        frecuencia = frequency(x),
        maxlag_utilizado = NA_integer_,
        lag_seleccionado = NA_integer_,
        estadistico_ocsb = NA_real_,
        valor_critico_5pct = NA_real_,
        requiere_diferencia_estacional_5pct = NA,
        diferencia_estacional_sugerida = NA_integer_,
        conclusion =
          "La serie no tiene frecuencia trimestral",
        mensaje_error =
          "frequency(x) es diferente de 4"
      )
    )
  }

  x <- stats::na.omit(
    x
  )

  # La primera opción reproduce la especificación de clase.
  # Los valores 4 y 0 son alternativas únicamente si
  # maxlag = 12 produce un error numérico.

  maxlags_candidatos <- c(
    12L,
    4L,
    0L
  )

  resultado <- NULL
  maxlag_exitoso <- NA_integer_
  ultimo_error <- NA_character_

  for (
    maxlag_i in maxlags_candidatos
  ) {

    intento <- tryCatch(
      forecast::ocsb.test(
        x = x,
        lag.method = "AIC",
        maxlag = maxlag_i
      ),
      error = function(e) {
        e
      }
    )

    if (
      !inherits(
        intento,
        "error"
      )
    ) {
      resultado <- intento
      maxlag_exitoso <- maxlag_i
      break
    } else {
      ultimo_error <- conditionMessage(
        intento
      )
    }
  }

  if (is.null(resultado)) {
    return(
      tibble(
        clave = clave,
        etiqueta = etiqueta,
        prueba = "OCSB",
        estado = "ERROR",
        observaciones = length(x),
        frecuencia = frequency(x),
        maxlag_utilizado = NA_integer_,
        lag_seleccionado = NA_integer_,
        estadistico_ocsb = NA_real_,
        valor_critico_5pct = NA_real_,
        requiere_diferencia_estacional_5pct = NA,
        diferencia_estacional_sugerida = NA_integer_,
        conclusion =
          "No fue posible ejecutar el test OCSB",
        mensaje_error =
          ultimo_error
      )
    )
  }

  estadistico_ocsb <- as.numeric(
    resultado$statistics[1]
  )

  valor_critico_5pct <- as.numeric(
    resultado$critical[1]
  )

  lag_seleccionado <- if (
    !is.null(
      resultado$lag.order
    )
  ) {
    as.integer(
      resultado$lag.order[1]
    )
  } else {
    NA_integer_
  }


  # ----------------------------------------------------------
  # DECISIÓN OCSB CORREGIDA
  # ----------------------------------------------------------

  requiere_diferencia_estacional <-
    estadistico_ocsb >
    valor_critico_5pct

  diferencia_estacional_sugerida <- if_else(
    requiere_diferencia_estacional,
    1L,
    0L
  )

  conclusion <- if_else(
    requiere_diferencia_estacional,

    paste0(
      "El estadístico OCSB supera el valor crítico; ",
      "no se rechaza la raíz unitaria estacional ",
      "y se recomienda D = 1"
    ),

    paste0(
      "El estadístico OCSB no supera el valor crítico; ",
      "se rechaza la raíz unitaria estacional ",
      "y se establece D = 0"
    )
  )

  tibble(
    clave = clave,
    etiqueta = etiqueta,
    prueba = "OCSB",
    estado = "OK",
    observaciones = length(x),
    frecuencia = frequency(x),
    maxlag_utilizado = maxlag_exitoso,
    lag_seleccionado = lag_seleccionado,
    estadistico_ocsb = estadistico_ocsb,
    valor_critico_5pct = valor_critico_5pct,

    requiere_diferencia_estacional_5pct =
      requiere_diferencia_estacional,

    diferencia_estacional_sugerida =
      diferencia_estacional_sugerida,

    conclusion =
      conclusion,

    mensaje_error =
      NA_character_
  )
}


# ------------------------------------------------------------
# 20. EJECUTAR LOS SEIS TESTS OCSB
# ------------------------------------------------------------

lista_resultados_ocsb <- vector(
  mode = "list",
  length = nrow(config_series)
)

for (
  i in seq_len(
    nrow(config_series)
  )
) {

  clave_i <- config_series$clave[i]

  etiqueta_i <- config_series$etiqueta[i]

  serie_i <- series_niveles[[
    clave_i
  ]]

  cat(
    "\nEjecutando OCSB:",
    etiqueta_i,
    "\n"
  )

  lista_resultados_ocsb[[i]] <-
    ejecutar_ocsb(
      x = serie_i,
      clave = clave_i,
      etiqueta = etiqueta_i
    )
}

resultados_ocsb <- bind_rows(
  lista_resultados_ocsb
)

print(
  resultados_ocsb,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 21. CONTROLAR LOS RESULTADOS OCSB
# ------------------------------------------------------------

if (nrow(resultados_ocsb) != 6) {
  stop(
    paste0(
      "Se esperaban 6 resultados OCSB, pero se generaron ",
      nrow(resultados_ocsb),
      "."
    )
  )
}

ocsb_exitosos <- sum(
  resultados_ocsb$estado ==
    "OK"
)

ocsb_con_error <- sum(
  resultados_ocsb$estado ==
    "ERROR"
)

cat(
  "\nResultados OCSB generados:",
  nrow(resultados_ocsb),

  "\nOCSB ejecutados correctamente:",
  ocsb_exitosos,

  "\nOCSB con error:",
  ocsb_con_error,

  "\nSeries con D = 1 sugerido:",
  sum(
    resultados_ocsb$
      diferencia_estacional_sugerida == 1L,
    na.rm = TRUE
  ),

  "\nSeries con D = 0 sugerido:",
  sum(
    resultados_ocsb$
      diferencia_estacional_sugerida == 0L,
    na.rm = TRUE
  ),

  "\n"
)

if (ocsb_con_error > 0) {
  warning(
    paste0(
      "Alguna prueba OCSB presentó errores. ",
      "Revisa mensaje_error en resultados_ocsb."
    )
  )
}


# ------------------------------------------------------------
# 22. CREAR TABLA FINAL
# ------------------------------------------------------------

tabla_final_raices <- tabla_orden_integracion |>
  left_join(
    resultados_ocsb |>
      select(
        clave,

        estado_ocsb =
          estado,

        maxlag_ocsb =
          maxlag_utilizado,

        lag_ocsb =
          lag_seleccionado,

        estadistico_ocsb,

        valor_critico_ocsb_5pct =
          valor_critico_5pct,

        requiere_diferencia_estacional_5pct,

        diferencia_estacional_sugerida,

        conclusion_ocsb =
          conclusion,

        error_ocsb =
          mensaje_error
      ),
    by = "clave"
  ) |>
  left_join(
    robustez_adf |>
      filter(
        transformacion ==
          "Logaritmo en nivel"
      ) |>
      select(
        clave,

        robustez_nivel_10pct =
          decision_robusta_10pct,

        robustez_nivel_5pct =
          decision_robusta_5pct
      ),
    by = "clave"
  ) |>
  left_join(
    robustez_adf |>
      filter(
        transformacion ==
          "Primera diferencia logarítmica"
      ) |>
      select(
        clave,

        robustez_diferencia_10pct =
          decision_robusta_10pct,

        robustez_diferencia_5pct =
          decision_robusta_5pct
      ),
    by = "clave"
  )

print(
  tabla_final_raices,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 23. TABLA RESUMIDA PARA EL INFORME
# ------------------------------------------------------------

tabla_informe_raices <- tabla_final_raices |>
  transmute(
    serie =
      etiqueta,

    tau_nivel =
      estadistico_tau_nivel,

    valor_critico_10pct_nivel =
      valor_critico_10pct_nivel,

    rezagos_nivel =
      rezagos_elegidos_nivel,

    decision_nivel_10pct = if_else(
      rechaza_raiz_unitaria_10pct_nivel,
      "Se rechaza raíz unitaria",
      "No se rechaza raíz unitaria"
    ),

    tau_primera_diferencia =
      estadistico_tau_diferencia,

    valor_critico_10pct_diferencia =
      valor_critico_10pct_diferencia,

    rezagos_diferencia =
      rezagos_elegidos_diferencia,

    decision_diferencia_10pct = if_else(
      rechaza_raiz_unitaria_10pct_diferencia,
      "Se rechaza raíz unitaria",
      "No se rechaza raíz unitaria"
    ),

    orden_integracion_10pct =
      orden_integracion_10pct,

    orden_integracion_5pct =
      orden_integracion_5pct,

    robustez_nivel_10pct =
      robustez_nivel_10pct,

    robustez_diferencia_10pct =
      robustez_diferencia_10pct,

    estadistico_ocsb =
      estadistico_ocsb,

    valor_critico_ocsb_5pct =
      valor_critico_ocsb_5pct,

    decision_ocsb = case_when(
      estado_ocsb == "ERROR" ~
        "OCSB no ejecutado",

      requiere_diferencia_estacional_5pct ~
        "No se rechaza raíz estacional; D = 1",

      !requiere_diferencia_estacional_5pct ~
        "Se rechaza raíz estacional; D = 0",

      TRUE ~
        "No concluyente"
    ),

    diferencia_estacional_sugerida =
      diferencia_estacional_sugerida
  )

print(
  tabla_informe_raices,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 24. GUARDAR RESULTADOS EN EXCEL
# ------------------------------------------------------------

writexl::write_xlsx(
  list(
    configuracion =
      config_series,

    control_series_ts =
      tabla_control_ts,

    faltantes =
      tabla_faltantes,

    adf_todos_criterios =
      resultados_adf,

    adf_principal_aic =
      tabla_adf_principal,

    robustez_adf =
      robustez_adf,

    orden_integracion =
      tabla_orden_integracion,

    ocsb_estacional =
      resultados_ocsb,

    tabla_final =
      tabla_final_raices,

    tabla_informe =
      tabla_informe_raices
  ),

  path = here::here(
    "outputs",
    "tables",
    "unit_roots_and_seasonality.xlsx"
  )
)


# ------------------------------------------------------------
# 25. GUARDAR OBJETOS RDS
# ------------------------------------------------------------

saveRDS(
  resultados_adf,
  here::here(
    "outputs",
    "models",
    "resultados_adf_curso.rds"
  )
)

saveRDS(
  tabla_adf_principal,
  here::here(
    "outputs",
    "models",
    "tabla_adf_principal_aic.rds"
  )
)

saveRDS(
  robustez_adf,
  here::here(
    "outputs",
    "models",
    "robustez_adf.rds"
  )
)

saveRDS(
  tabla_orden_integracion,
  here::here(
    "outputs",
    "models",
    "tabla_orden_integracion.rds"
  )
)

saveRDS(
  resultados_ocsb,
  here::here(
    "outputs",
    "models",
    "resultados_ocsb.rds"
  )
)

saveRDS(
  tabla_final_raices,
  here::here(
    "outputs",
    "models",
    "final_unit_roots_integration_table.rds"
  )
)

saveRDS(
  tabla_informe_raices,
  here::here(
    "outputs",
    "models",
    "unit_roots_report_table.rds"
  )
)


# ------------------------------------------------------------
# 26. CONTROLES FINALES
# ------------------------------------------------------------

stopifnot(
  nrow(config_series) ==
    6,

  nrow(especificaciones_adf) ==
    12,

  nrow(resultados_adf) ==
    36,

  nrow(tabla_adf_principal) ==
    12,

  nrow(robustez_adf) ==
    12,

  nrow(tabla_orden_integracion) ==
    6,

  nrow(resultados_ocsb) ==
    6,

  nrow(tabla_final_raices) ==
    6,

  nrow(tabla_informe_raices) ==
    6
)


# ------------------------------------------------------------
# 27. MOSTRAR TABLAS CLAVE
# ------------------------------------------------------------

cat(
  "\n\n============================================================",
  "\nTABLA RESUMIDA DE RAÍCES UNITARIAS",
  "\n============================================================\n"
)

tabla_informe_raices |>
  print(
    n = Inf,
    width = Inf
  )

cat(
  "\n\n============================================================",
  "\nRESULTADOS OCSB",
  "\n============================================================\n"
)

resultados_ocsb |>
  select(
    etiqueta,
    estado,
    maxlag_utilizado,
    lag_seleccionado,
    estadistico_ocsb,
    valor_critico_5pct,
    requiere_diferencia_estacional_5pct,
    diferencia_estacional_sugerida,
    conclusion,
    mensaje_error
  ) |>
  print(
    n = Inf,
    width = Inf
  )


# ------------------------------------------------------------
# 28. MENSAJE FINAL
# ------------------------------------------------------------

cat(
  "\n============================================================",
  "\nRAÍCES UNITARIAS Y ESTACIONALIDAD FINALIZADAS",
  "\n============================================================",

  "\nFunción principal utilizada:",
  "Test.ADF.Ver.3",

  "\nSeries evaluadas:",
  nrow(config_series),

  "\nTransformaciones por serie:",
  2,

  "\nCriterios de rezagos:",
  "AIC, BIC y SW",

  "\nADF estimados:",
  nrow(resultados_adf),

  "\nCriterio principal:",
  "AIC",

  "\nNivel principal ADF:",
  "10%",

  "\nTests OCSB generados:",
  nrow(resultados_ocsb),

  "\nTests OCSB exitosos:",
  ocsb_exitosos,

  "\nTests OCSB con error:",
  ocsb_con_error,

  "\nSeries con D = 1:",
  sum(
    resultados_ocsb$
      diferencia_estacional_sugerida == 1L,
    na.rm = TRUE
  ),

  "\nSeries con D = 0:",
  sum(
    resultados_ocsb$
      diferencia_estacional_sugerida == 0L,
    na.rm = TRUE
  ),

  "\nNivel principal OCSB:",
  "5%",

  "\n\nArchivos generados:",

  "\n- outputs/tables/unit_roots_and_seasonality.xlsx",

  "\n- outputs/models/salidas_Test_ADF_Ver_3.txt",

  "\n- outputs/models/resultados_adf_curso.rds",

  "\n- outputs/models/tabla_adf_principal_aic.rds",

  "\n- outputs/models/robustez_adf.rds",

  "\n- outputs/models/tabla_orden_integracion.rds",

  "\n- outputs/models/resultados_ocsb.rds",

  "\n- outputs/models/final_unit_roots_integration_table.rds",

  "\n- outputs/models/unit_roots_report_table.rds",

  "\n============================================================",
  "\n"
)

