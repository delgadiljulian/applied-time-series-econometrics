
# ============================================================
# TP3 - SERIES DE TIEMPO
# SCRIPT 11:
# JOHANSEN, VECM Y VAR EN DIFERENCIAS
#
# Sistemas principales:
#
# 1. Importaciones:
#    ln(importaciones), ln(PIB Argentina), ln(ITCRM)
#
# 2. Exportaciones clásicas:
#    ln(exportaciones), ln(PIB socios), ln(ITCRM)
#
# 3. Exportaciones ampliadas:
#    mismas variables endógenas de exportaciones,
#    con commodities como variable exógena I(0)
#
# Especificación Johansen:
#    ecdet = "const"
#    spec  = "transitory"
#    season = 4
#
# Órdenes:
#    Importaciones: K = 2
#    Exportaciones clásicas: K = 2
#    Exportaciones ampliadas principal: K = 2
#    Exportaciones ampliadas sensibilidad: K = 4
#
# Nivel principal de significancia: 10%
# ============================================================


# ------------------------------------------------------------
# 0. LIMPIAR ENTORNO
# ------------------------------------------------------------

rm(list = ls())
gc()

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE
)


# ------------------------------------------------------------
# 1. PAQUETES
# ------------------------------------------------------------

paquetes <- c(
  "here",
  "dplyr",
  "tidyr",
  "purrr",
  "readr",
  "tibble",
  "ggplot2",
  "zoo",
  "urca",
  "vars",
  "writexl",
  "MASS"
)

paquetes_faltantes <- paquetes[
  !vapply(
    paquetes,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(paquetes_faltantes) > 0) {
  install.packages(paquetes_faltantes)
}

# ------------------------------------------------------------
# FUNCIÓN SEGURA PARA EXTRAER EL REZAGO DE UN TÉRMINO
# ------------------------------------------------------------

# ------------------------------------------------------------
# EXTRAER EL NÚMERO DE REZAGO DE LOS COEFICIENTES
#
# Reconoce:
#   variable.l1
#   variable.l2
#   variable.dl1
#   variable.dl2
#
# Devuelve NA para:
#   const
#   ect1
#   commodity_c
# ------------------------------------------------------------

extraer_lag_termino <- function(
    term
) {
  
  term <- as.character(
    term
  )
  
  
  captura <- stringr::str_match(
    term,
    "\\.d?l([0-9]+)$"
  )[, 2]
  
  
  suppressWarnings(
    as.integer(
      captura
    )
  )
}

# ------------------------------------------------------------
# 2. CARPETAS DE SALIDA
# ------------------------------------------------------------

dir.create(
  here::here(
    "outputs",
    "tables"
  ),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here::here(
    "outputs",
    "models"
  ),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here::here(
    "outputs",
    "figures"
  ),
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# 3. LOCALIZAR vcorr_res.R
# ------------------------------------------------------------

localizar_archivo <- function(
    nombres_posibles
) {
  
  rutas_posibles <- unique(
    unlist(
      lapply(
        nombres_posibles,
        function(nombre_archivo) {
          
          c(
            here::here(
              "scripts",
              "required_scripts",
              nombre_archivo
            ),
            
            here::here(
              "scripts",
              nombre_archivo
            ),
            
            here::here(
              nombre_archivo
            )
          )
        }
      )
    )
  )
  
  rutas_existentes <- rutas_posibles[
    file.exists(rutas_posibles)
  ]
  
  if (length(rutas_existentes) == 0) {
    
    stop(
      paste0(
        "No se encontró el archivo:\n",
        paste(
          nombres_posibles,
          collapse = " o "
        ),
        "\n\nDebe guardarse en scripts/required_scripts/."
      )
    )
  }
  
  rutas_existentes[1]
}


ruta_vcorr <- localizar_archivo(
  c(
    "vcorr_res.R",
    "VCORR_RES.R"
  )
)

source(
  ruta_vcorr,
  encoding = "UTF-8"
)

# La función vcorr_res() llama serial.test() sin namespace.
serial.test <- vars::serial.test

if (!exists(
  "vcorr_res",
  mode = "function"
)) {
  stop(
    "La función vcorr_res() no fue cargada correctamente."
  )
}


# ============================================================
# FUNCIÓN CORREGIDA: CORRELOGRAMA DE RESIDUOS VAR/VECM
#
# Corrección:
# Para los test Portmanteau no se ejecuta serial.test()
# cuando el horizonte h es menor o igual al orden p del VAR,
# porque los grados de libertad no son válidos.
# ============================================================

vcorr_res <- function(
    var_reg,
    lags,
    tipo = "PT.adjusted"
) {
  
  tipos_validos <- c(
    "PT.adjusted",
    "PT.asymptotic",
    "BG",
    "ES"
  )
  
  
  if (!tipo %in% tipos_validos) {
    
    stop(
      paste0(
        "Tipo de prueba no reconocido: ",
        tipo,
        "\nTipos válidos: ",
        paste(
          tipos_validos,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  
  if (
    is.null(var_reg$obs) ||
    !is.numeric(var_reg$obs)
  ) {
    
    stop(
      "El objeto proporcionado no contiene var_reg$obs.",
      call. = FALSE
    )
  }
  
  
  observaciones <- as.integer(
    var_reg$obs
  )
  
  
  orden_var <- if (
    !is.null(var_reg$p)
  ) {
    
    as.integer(
      var_reg$p
    )
    
  } else {
    
    0L
  }
  
  
  lags <- as.integer(
    lags
  )
  
  
  maximo_admisible <- floor(
    observaciones / 3
  )
  
  
  if (
    is.na(lags) ||
    lags < 1
  ) {
    
    stop(
      "El número de rezagos debe ser un entero positivo.",
      call. = FALSE
    )
  }
  
  
  if (
    lags > maximo_admisible
  ) {
    
    stop(
      paste0(
        "La cantidad de rezagos solicitada es demasiado alta.\n",
        "Rezagos solicitados: ",
        lags,
        "\nMáximo recomendado, T/3: ",
        maximo_admisible
      ),
      call. = FALSE
    )
  }
  
  
  lag <- seq_len(
    lags
  )
  
  q_values <- rep(
    NA_real_,
    lags
  )
  
  p_values <- rep(
    NA_real_,
    lags
  )
  
  
  for (
    i in seq_len(
      lags
    )
  ) {
    
    # --------------------------------------------------------
    # Portmanteau:
    # solo es válido para horizontes superiores al orden p.
    # Evita pchisq() con grados de libertad <= 0.
    # --------------------------------------------------------
    
    if (
      tipo %in% c(
        "PT.adjusted",
        "PT.asymptotic"
      ) &&
      i <= orden_var
    ) {
      
      q_values[i] <- NA_real_
      p_values[i] <- NA_real_
      
      next
    }
    
    
    prueba_i <- tryCatch(
      
      {
        
        if (
          tipo %in% c(
            "BG",
            "ES"
          )
        ) {
          
          vars::serial.test(
            x = var_reg,
            lags.bg = i,
            type = tipo
          )
          
        } else {
          
          vars::serial.test(
            x = var_reg,
            lags.pt = i,
            type = tipo
          )
        }
      },
      
      warning = function(w) {
        
        # Se conservan como NA las combinaciones no calculables.
        NULL
      },
      
      error = function(e) {
        
        NULL
      }
    )
    
    
    if (
      is.null(
        prueba_i
      )
    ) {
      
      next
    }
    
    
    estadistico_i <- suppressWarnings(
      as.numeric(
        prueba_i$serial$statistic
      )
    )
    
    
    p_value_i <- suppressWarnings(
      as.numeric(
        prueba_i$serial$p.value
      )
    )
    
    
    if (
      length(estadistico_i) == 1 &&
      is.finite(estadistico_i)
    ) {
      
      q_values[i] <- round(
        estadistico_i,
        digits = 4
      )
    }
    
    
    if (
      length(p_value_i) == 1 &&
      is.finite(p_value_i)
    ) {
      
      p_values[i] <- round(
        p_value_i,
        digits = 4
      )
    }
  }
  
  
  correlograma <- cbind(
    lag,
    q_values,
    p_values
  )
  
  
  rownames(
    correlograma
  ) <- NULL
  
  
  colnames(
    correlograma
  ) <- c(
    "lag",
    "Estadístico Q",
    "p-value"
  )
  
  
  return(
    correlograma
  )
}


# ------------------------------------------------------------
# 4. CARGAR INSUMOS DEL SCRIPT 10
# ------------------------------------------------------------

ruta_datos_sistemas <- here::here(
  "outputs",
  "models",
  "var_system_data.rds"
)

ruta_insumos_johansen <- here::here(
  "outputs",
  "models",
  "insumos_johansen_final.rds"
)

if (!file.exists(ruta_datos_sistemas)) {
  
  stop(
    paste0(
      "No se encontró:\n",
      ruta_datos_sistemas,
      "\nEjecuta primero el script 10."
    )
  )
}

if (!file.exists(ruta_insumos_johansen)) {
  
  stop(
    paste0(
      "No se encontró:\n",
      ruta_insumos_johansen,
      "\nEjecuta primero el bloque final del script 10."
    )
  )
}


datos_sistemas <- readRDS(
  ruta_datos_sistemas
)

insumos_johansen_final <- readRDS(
  ruta_insumos_johansen
)


# ------------------------------------------------------------
# 5. CARGAR RESULTADOS PREVIOS OPCIONALES
# ------------------------------------------------------------

ruta_comparacion_eg <- here::here(
  "outputs",
  "models",
  "comparacion_elasticidades_largo_plazo.rds"
)

ruta_diagnosticos_var10 <- here::here(
  "outputs",
  "models",
  "diagnosticos_var_finales.rds"
)


comparacion_engle_granger <- if (
  file.exists(ruta_comparacion_eg)
) {
  
  readRDS(
    ruta_comparacion_eg
  )
  
} else {
  
  tibble::tibble()
}


diagnosticos_script10 <- if (
  file.exists(ruta_diagnosticos_var10)
) {
  
  readRDS(
    ruta_diagnosticos_var10
  )
  
} else {
  
  tibble::tibble()
}


# ------------------------------------------------------------
# 6. PARÁMETROS GENERALES
# ------------------------------------------------------------

nivel_significancia <- 0.10

lags_autocorrelacion <- 12L

lags_arch <- 4L

tolerancia_raiz_uno <- 0.0001


# ------------------------------------------------------------
# 7. CONFIGURACIÓN DE LAS ESTIMACIONES
# ------------------------------------------------------------

configuracion_principal <- insumos_johansen_final |>
  dplyr::transmute(
    
    model_id =
      paste0(
        model_key,
        "__primary_K",
        K_ca_jo
      ),
    
    model_key,
    
    model_label_run =
      model_label,
    
    model_label,
    
    K =
      K_ca_jo,
    
    ecdet,
    
    spec,
    
    season,
    
    dumvar,
    
    is_primary =
      TRUE,
    
    is_sensitivity =
      FALSE,
    
    formal_status,
    
    johansen_role_final
  )


configuracion_sensibilidad <- insumos_johansen_final |>
  dplyr::filter(
    !is.na(K_sensitivity)
  ) |>
  dplyr::transmute(
    
    model_id =
      paste0(
        model_key,
        "__sensitivity_K",
        K_sensitivity
      ),
    
    model_key,
    
    model_label_run =
      paste0(
        model_label,
        " — sensibilidad K=",
        K_sensitivity
      ),
    
    model_label,
    
    K =
      K_sensitivity,
    
    ecdet,
    
    spec,
    
    season,
    
    dumvar,
    
    is_primary =
      FALSE,
    
    is_sensitivity =
      TRUE,
    
    formal_status =
      paste0(
        formal_status,
        "; sensibilidad con K=",
        K_sensitivity
      ),
    
    johansen_role_final =
      paste0(
        johansen_role_final,
        " Sensibilidad adicional con K=",
        K_sensitivity,
        "."
      )
  )


config_modelos <- dplyr::bind_rows(
  configuracion_principal,
  configuracion_sensibilidad
) |>
  dplyr::mutate(
    
    n_endogenous =
      purrr::map_int(
        model_key,
        ~ ncol(
          datos_sistemas[[.x]]$y
        )
      ),
    
    dependent_variable =
      purrr::map_chr(
        model_key,
        ~ colnames(
          datos_sistemas[[.x]]$y
        )[1]
      ),
    
    endogenous_names =
      purrr::map_chr(
        model_key,
        ~ paste(
          colnames(
            datos_sistemas[[.x]]$y
          ),
          collapse = " + "
        )
      ),
    
    exogenous_names =
      purrr::map_chr(
        model_key,
        function(clave) {
          
          exog_i <-
            datos_sistemas[[clave]]$exogen
          
          if (is.null(exog_i)) {
            
            "Ninguna"
            
          } else {
            
            paste(
              colnames(exog_i),
              collapse = " + "
            )
          }
        }
      )
  )


print(
  config_modelos,
  n = Inf,
  width = Inf
)


stopifnot(
  nrow(config_modelos) == 4,
  all(config_modelos$K >= 2),
  all(config_modelos$n_endogenous == 3)
)


# ------------------------------------------------------------
# 8. FUNCIÓN PARA ESTIMAR ca.jo()
# ------------------------------------------------------------

estimar_ca_jo <- function(
    datos_sistema,
    tipo,
    K,
    ecdet,
    spec,
    season
) {
  
  argumentos <- list(
    
    x =
      datos_sistema$y,
    
    type =
      tipo,
    
    ecdet =
      ecdet,
    
    K =
      as.integer(K),
    
    spec =
      spec,
    
    season =
      as.integer(season)
  )
  
  
  if (!is.null(datos_sistema$exogen)) {
    
    argumentos$dumvar <-
      datos_sistema$exogen
  }
  
  
  do.call(
    urca::ca.jo,
    argumentos
  )
}


# ------------------------------------------------------------
# 9. ESTIMAR TRAZA Y MÁXIMO AUTOVALOR
# ------------------------------------------------------------

modelos_johansen_trace <- list()

modelos_johansen_eigen <- list()


for (
  i in seq_len(
    nrow(config_modelos)
  )
) {
  
  model_id_i <-
    config_modelos$model_id[[i]]
  
  model_key_i <-
    config_modelos$model_key[[i]]
  
  datos_i <-
    datos_sistemas[[model_key_i]]
  
  
  cat(
    "\n============================================================",
    "\nESTIMACIÓN JOHANSEN",
    "\nModelo:",
    config_modelos$model_label_run[[i]],
    "\nK:",
    config_modelos$K[[i]],
    "\n============================================================\n"
  )
  
  
  modelos_johansen_trace[[
    model_id_i
  ]] <- estimar_ca_jo(
    
    datos_sistema =
      datos_i,
    
    tipo =
      "trace",
    
    K =
      config_modelos$K[[i]],
    
    ecdet =
      config_modelos$ecdet[[i]],
    
    spec =
      config_modelos$spec[[i]],
    
    season =
      config_modelos$season[[i]]
  )
  
  
  modelos_johansen_eigen[[
    model_id_i
  ]] <- estimar_ca_jo(
    
    datos_sistema =
      datos_i,
    
    tipo =
      "eigen",
    
    K =
      config_modelos$K[[i]],
    
    ecdet =
      config_modelos$ecdet[[i]],
    
    spec =
      config_modelos$spec[[i]],
    
    season =
      config_modelos$season[[i]]
  )
}


# ------------------------------------------------------------
# 10. EXTRAER TABLAS DE LOS TESTS
# ------------------------------------------------------------

extraer_test_johansen <- function(
    objeto,
    model_id_arg,
    model_key_arg,
    model_label_arg,
    K_arg,
    test_type_arg
) {
  
  valores_criticos <- objeto@cval
  
  if (is.null(valores_criticos)) {
    
    stop(
      paste0(
        "No hay valores críticos para ",
        model_label_arg
      )
    )
  }
  
  
  tibble::tibble(
    
    model_id =
      model_id_arg,
    
    model_key =
      model_key_arg,
    
    model_label_run =
      model_label_arg,
    
    K =
      K_arg,
    
    test_type =
      test_type_arg,
    
    null_hypothesis =
      rownames(valores_criticos),
    
    null_rank =
      as.integer(
        readr::parse_number(
          rownames(valores_criticos)
        )
      ),
    
    statistic =
      as.numeric(
        objeto@teststat
      ),
    
    critical_10pct =
      as.numeric(
        valores_criticos[, "10pct"]
      ),
    
    critical_5pct =
      as.numeric(
        valores_criticos[, "5pct"]
      ),
    
    critical_1pct =
      as.numeric(
        valores_criticos[, "1pct"]
      )
  ) |>
    dplyr::mutate(
      
      reject_10pct =
        statistic >
        critical_10pct,
      
      reject_5pct =
        statistic >
        critical_5pct,
      
      reject_1pct =
        statistic >
        critical_1pct
    )
}


lista_tests_johansen <- list()

contador_tests <- 1L


for (
  i in seq_len(
    nrow(config_modelos)
  )
) {
  
  model_id_i <-
    config_modelos$model_id[[i]]
  
  model_key_i <-
    config_modelos$model_key[[i]]
  
  model_label_i <-
    config_modelos$model_label_run[[i]]
  
  K_i <-
    config_modelos$K[[i]]
  
  
  lista_tests_johansen[[
    contador_tests
  ]] <- extraer_test_johansen(
    
    objeto =
      modelos_johansen_trace[[model_id_i]],
    
    model_id_arg =
      model_id_i,
    
    model_key_arg =
      model_key_i,
    
    model_label_arg =
      model_label_i,
    
    K_arg =
      K_i,
    
    test_type_arg =
      "Trace"
  )
  
  contador_tests <-
    contador_tests + 1L
  
  
  lista_tests_johansen[[
    contador_tests
  ]] <- extraer_test_johansen(
    
    objeto =
      modelos_johansen_eigen[[model_id_i]],
    
    model_id_arg =
      model_id_i,
    
    model_key_arg =
      model_key_i,
    
    model_label_arg =
      model_label_i,
    
    K_arg =
      K_i,
    
    test_type_arg =
      "Maximum eigenvalue"
  )
  
  contador_tests <-
    contador_tests + 1L
}


tests_johansen <- dplyr::bind_rows(
  lista_tests_johansen
) |>
  dplyr::arrange(
    model_id,
    test_type,
    null_rank
  )


print(
  tests_johansen,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 11. FUNCIÓN PARA SELECCIONAR EL RANGO
# ------------------------------------------------------------

seleccionar_rango <- function(
    tabla_test,
    columna_rechazo,
    numero_variables
) {
  
  tabla_ordenada <- tabla_test |>
    dplyr::arrange(
      null_rank
    )
  
  decisiones <- tabla_ordenada[[
    columna_rechazo
  ]]
  
  rangos <- tabla_ordenada$null_rank
  
  
  primera_no_rechazada <- which(
    !decisiones
  )
  
  
  if (length(primera_no_rechazada) > 0) {
    
    rango <- rangos[
      primera_no_rechazada[1]
    ]
    
  } else {
    
    rango <- numero_variables
  }
  
  
  as.integer(rango)
}


# ------------------------------------------------------------
# 12. DETERMINAR RANGOS POR TEST Y NIVEL
# ------------------------------------------------------------

lista_rangos <- list()

contador_rangos <- 1L


for (
  i in seq_len(
    nrow(config_modelos)
  )
) {
  
  model_id_i <-
    config_modelos$model_id[[i]]
  
  p_i <-
    config_modelos$n_endogenous[[i]]
  
  
  for (
    tipo_i in c(
      "Trace",
      "Maximum eigenvalue"
    )
  ) {
    
    tabla_i <- tests_johansen |>
      dplyr::filter(
        model_id == model_id_i,
        test_type == tipo_i
      )
    
    
    lista_rangos[[
      contador_rangos
    ]] <- tibble::tibble(
      
      model_id =
        model_id_i,
      
      model_key =
        config_modelos$model_key[[i]],
      
      model_label_run =
        config_modelos$model_label_run[[i]],
      
      test_type =
        tipo_i,
      
      rank_10pct =
        seleccionar_rango(
          tabla_i,
          "reject_10pct",
          p_i
        ),
      
      rank_5pct =
        seleccionar_rango(
          tabla_i,
          "reject_5pct",
          p_i
        ),
      
      rank_1pct =
        seleccionar_rango(
          tabla_i,
          "reject_1pct",
          p_i
        )
    )
    
    
    contador_rangos <-
      contador_rangos + 1L
  }
}


rangos_por_test <- dplyr::bind_rows(
  lista_rangos
)


rangos_johansen <- rangos_por_test |>
  tidyr::pivot_wider(
    
    id_cols = c(
      model_id,
      model_key,
      model_label_run
    ),
    
    names_from =
      test_type,
    
    values_from = c(
      rank_10pct,
      rank_5pct,
      rank_1pct
    ),
    
    names_glue =
      "{.value}_{test_type}"
  ) |>
  dplyr::rename(
    
    rank_trace_10pct =
      `rank_10pct_Trace`,
    
    rank_trace_5pct =
      `rank_5pct_Trace`,
    
    rank_trace_1pct =
      `rank_1pct_Trace`,
    
    rank_eigen_10pct =
      `rank_10pct_Maximum eigenvalue`,
    
    rank_eigen_5pct =
      `rank_5pct_Maximum eigenvalue`,
    
    rank_eigen_1pct =
      `rank_1pct_Maximum eigenvalue`
  )


# ------------------------------------------------------------
# 13. DECISIÓN DEL RANGO PARA LA ESTIMACIÓN
# ------------------------------------------------------------

decision_rango <- config_modelos |>
  dplyr::left_join(
    rangos_johansen,
    by = c(
      "model_id",
      "model_key",
      "model_label_run"
    )
  ) |>
  dplyr::mutate(
    
    trace_eigen_agree_10pct =
      rank_trace_10pct ==
      rank_eigen_10pct,
    
    cointegration_trace_10pct =
      rank_trace_10pct >= 1 &
      rank_trace_10pct <
      n_endogenous,
    
    cointegration_eigen_10pct =
      rank_eigen_10pct >= 1 &
      rank_eigen_10pct <
      n_endogenous,
    
    # Rango utilizado para el VECM multivariado.
    #
    # Si Johansen devuelve r = 0, se fuerza r = 1 sólo
    # para producir los resultados indicativos exigidos.
    #
    # Si devuelve rango completo, se utiliza P - 1,
    # dejando constancia de la inconsistencia.
    
    rank_system_vecm =
      dplyr::case_when(
        
        rank_trace_10pct == 0 ~
          1L,
        
        rank_trace_10pct >=
          n_endogenous ~
          n_endogenous - 1L,
        
        TRUE ~
          rank_trace_10pct
      ),
    
    rank_system_forced =
      rank_system_vecm !=
      rank_trace_10pct,
    
    # Para comparar las elasticidades con Engle-Granger
    # y Wickens-Breusch se estima también una relación
    # económica única normalizada en la variable comercial.
    
    rank_trade_equation =
      1L,
    
    rank_interpretation =
      dplyr::case_when(
        
        rank_trace_10pct == 0 ~
          paste0(
            "La traza no detecta cointegración al 10%. ",
            "El VECM con r=1 se estima sólo con fines indicativos."
          ),
        
        rank_trace_10pct == 1 &
          rank_eigen_10pct == 1 ~
          paste0(
            "Traza y máximo autovalor coinciden en ",
            "una relación de cointegración al 10%."
          ),
        
        rank_trace_10pct == 1 ~
          paste0(
            "La traza selecciona una relación al 10%, ",
            "pero los tests no coinciden plenamente."
          ),
        
        rank_trace_10pct > 1 &
          rank_trace_10pct <
          n_endogenous ~
          paste0(
            "La traza detecta múltiples relaciones. ",
            "La elasticidad comercial basada en r=1 ",
            "se reporta como normalización económica adicional."
          ),
        
        TRUE ~
          paste0(
            "Johansen sugiere rango completo. ",
            "El resultado no es compatible con un sistema I(1) estándar."
          )
      ),
    
    inference_status =
      paste(
        formal_status,
        rank_interpretation
      )
  )


print(
  decision_rango,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 14. ESTIMAR VECM CON cajorls()
# ------------------------------------------------------------

modelos_cajorls_system <- list()

modelos_cajorls_trade_r1 <- list()

modelos_vec2var_system <- list()

modelos_vec2var_trade_r1 <- list()


for (
  i in seq_len(
    nrow(decision_rango)
  )
) {
  
  model_id_i <-
    decision_rango$model_id[[i]]
  
  objeto_trace_i <-
    modelos_johansen_trace[[model_id_i]]
  
  r_system_i <-
    decision_rango$rank_system_vecm[[i]]
  
  
  cat(
    "\nEstimando VECM:",
    decision_rango$model_label_run[[i]],
    "\nRango multivariado:",
    r_system_i,
    "\nRango económico adicional: 1\n"
  )
  
  
  modelos_cajorls_system[[
    model_id_i
  ]] <- urca::cajorls(
    
    objeto_trace_i,
    
    r =
      r_system_i
  )
  
  
  modelos_cajorls_trade_r1[[
    model_id_i
  ]] <- urca::cajorls(
    
    objeto_trace_i,
    
    r =
      1L
  )
  
  
  modelos_vec2var_system[[
    model_id_i
  ]] <- vars::vec2var(
    
    objeto_trace_i,
    
    r =
      r_system_i
  )
  
  
  modelos_vec2var_trade_r1[[
    model_id_i
  ]] <- vars::vec2var(
    
    objeto_trace_i,
    
    r =
      1L
  )
}


# ------------------------------------------------------------
# 15. EXTRAER MATRICES BETA
# ------------------------------------------------------------

extraer_beta <- function(
    cajorls_object,
    model_id_arg,
    model_key_arg,
    model_label_arg,
    rank_type_arg
) {
  
  beta <- cajorls_object$beta
  
  beta_df <- as.data.frame(
    beta,
    check.names = FALSE
  ) |>
    tibble::rownames_to_column(
      var = "term"
    ) |>
    tidyr::pivot_longer(
      cols = -term,
      names_to = "cointegrating_vector",
      values_to = "beta_coefficient"
    ) |>
    dplyr::mutate(
      
      model_id =
        model_id_arg,
      
      model_key =
        model_key_arg,
      
      model_label_run =
        model_label_arg,
      
      rank_type =
        rank_type_arg,
      
      .before = 1
    )
  
  beta_df
}


lista_beta_system <- list()

lista_beta_trade <- list()


for (
  i in seq_len(
    nrow(decision_rango)
  )
) {
  
  model_id_i <-
    decision_rango$model_id[[i]]
  
  lista_beta_system[[i]] <- extraer_beta(
    
    cajorls_object =
      modelos_cajorls_system[[model_id_i]],
    
    model_id_arg =
      model_id_i,
    
    model_key_arg =
      decision_rango$model_key[[i]],
    
    model_label_arg =
      decision_rango$model_label_run[[i]],
    
    rank_type_arg =
      "Data-driven system rank"
  )
  
  
  lista_beta_trade[[i]] <- extraer_beta(
    
    cajorls_object =
      modelos_cajorls_trade_r1[[model_id_i]],
    
    model_id_arg =
      model_id_i,
    
    model_key_arg =
      decision_rango$model_key[[i]],
    
    model_label_arg =
      decision_rango$model_label_run[[i]],
    
    rank_type_arg =
      "Economic rank r=1"
  )
}


beta_system <- dplyr::bind_rows(
  lista_beta_system
)

beta_trade_r1 <- dplyr::bind_rows(
  lista_beta_trade
)


# ------------------------------------------------------------
# 16. ELASTICIDADES DE LARGO PLAZO JOHANSEN
# ------------------------------------------------------------

extraer_elasticidades_r1 <- function(
    cajorls_object,
    dependent_variable,
    model_id_arg,
    model_key_arg,
    model_label_arg,
    rank_trace_10pct_arg,
    formal_status_arg
) {
  
  beta <- cajorls_object$beta[, 1]
  
  beta_names <- rownames(
    cajorls_object$beta
  )
  
  names(beta) <- beta_names
  
  
  dependent_term <- paste0(
    make.names(dependent_variable),
    ".l1"
  )
  
  
  if (!dependent_term %in% names(beta)) {
    
    stop(
      paste0(
        "No se encontró el término dependiente ",
        dependent_term,
        " en beta."
      )
    )
  }
  
  
  beta_dependiente <- as.numeric(
    beta[dependent_term]
  )
  
  
  terminos_nivel <- names(beta)[
    grepl(
      "\\.l1$",
      names(beta)
    )
  ]
  
  
  tabla_variables <- tibble::tibble(
    
    model_id =
      model_id_arg,
    
    model_key =
      model_key_arg,
    
    model_label_run =
      model_label_arg,
    
    dependent_variable =
      dependent_variable,
    
    term =
      terminos_nivel,
    
    variable =
      sub(
        "\\.l1$",
        "",
        terminos_nivel
      ),
    
    beta_coefficient =
      as.numeric(
        beta[
          terminos_nivel
        ]
      )
  ) |>
    dplyr::mutate(
      
      is_dependent =
        variable ==
        make.names(
          dependent_variable
        ),
      
      long_run_elasticity =
        dplyr::if_else(
          
          is_dependent,
          
          NA_real_,
          
          -beta_coefficient /
            beta_dependiente
        ),
      
      rank_trace_10pct =
        rank_trace_10pct_arg,
      
      validity =
        dplyr::case_when(
          
          rank_trace_10pct_arg == 0 ~
            paste0(
              "Elasticidad indicativa: la traza ",
              "no confirmó cointegración al 10%."
            ),
          
          rank_trace_10pct_arg == 1 ~
            formal_status_arg,
          
          rank_trace_10pct_arg > 1 ~
            paste0(
              "Elasticidad de la relación r=1 normalizada ",
              "económicamente; existen múltiples vectores."
            ),
          
          TRUE ~
            formal_status_arg
        )
    )
  
  
  constante_nombre <- names(beta)[
    names(beta) == "constant"
  ]
  
  
  constante_largo_plazo <- if (
    length(constante_nombre) == 1
  ) {
    
    -as.numeric(
      beta["constant"]
    ) /
      beta_dependiente
    
  } else {
    
    NA_real_
  }
  
  
  list(
    
    elasticities =
      tabla_variables |>
      dplyr::filter(
        !is_dependent
      ),
    
    constant =
      tibble::tibble(
        
        model_id =
          model_id_arg,
        
        model_key =
          model_key_arg,
        
        model_label_run =
          model_label_arg,
        
        long_run_constant =
          constante_largo_plazo
      )
  )
}


lista_elasticidades <- list()

lista_constantes_lr <- list()


for (
  i in seq_len(
    nrow(decision_rango)
  )
) {
  
  model_id_i <-
    decision_rango$model_id[[i]]
  
  resultado_i <- extraer_elasticidades_r1(
    
    cajorls_object =
      modelos_cajorls_trade_r1[[model_id_i]],
    
    dependent_variable =
      decision_rango$dependent_variable[[i]],
    
    model_id_arg =
      model_id_i,
    
    model_key_arg =
      decision_rango$model_key[[i]],
    
    model_label_arg =
      decision_rango$model_label_run[[i]],
    
    rank_trace_10pct_arg =
      decision_rango$rank_trace_10pct[[i]],
    
    formal_status_arg =
      decision_rango$formal_status[[i]]
  )
  
  
  lista_elasticidades[[i]] <-
    resultado_i$elasticities
  
  lista_constantes_lr[[i]] <-
    resultado_i$constant
}


elasticidades_johansen <- dplyr::bind_rows(
  lista_elasticidades
)

constantes_largo_plazo <- dplyr::bind_rows(
  lista_constantes_lr
)


print(
  elasticidades_johansen,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 17. EXTRAER COEFICIENTES DE OBJETOS mlm
# ------------------------------------------------------------

extraer_coeficientes_mlm <- function(
    cajorls_object,
    model_id_arg,
    model_key_arg,
    model_label_arg,
    rank_type_arg
) {
  
  fit <- cajorls_object$rlm
  
  X <- stats::model.matrix(
    fit
  )
  
  B <- stats::coef(
    fit
  )
  
  U <- stats::residuals(
    fit
  )
  
  
  if (is.null(dim(B))) {
    
    B <- matrix(
      B,
      ncol = 1
    )
  }
  
  if (is.null(dim(U))) {
    
    U <- matrix(
      U,
      ncol = 1
    )
  }
  
  
  if (is.null(colnames(B))) {
    
    colnames(B) <- colnames(U)
  }
  
  if (is.null(colnames(B))) {
    
    colnames(B) <- paste0(
      "Equation_",
      seq_len(
        ncol(B)
      )
    )
  }
  
  
  XtX <- crossprod(
    X
  )
  
  XtX_inverse <- tryCatch(
    
    solve(
      XtX
    ),
    
    error = function(e) {
      
      MASS::ginv(
        XtX
      )
    }
  )
  
  
  residual_df <- nrow(X) -
    qr(X)$rank
  
  
  resultados <- vector(
    mode = "list",
    length = ncol(B)
  )
  
  
  for (
    j in seq_len(
      ncol(B)
    )
  ) {
    
    sigma2_j <- sum(
      U[, j]^2
    ) /
      residual_df
    
    std_error_j <- sqrt(
      pmax(
        diag(
          XtX_inverse
        ) *
          sigma2_j,
        0
      )
    )
    
    estimate_j <- as.numeric(
      B[, j]
    )
    
    statistic_j <- estimate_j /
      std_error_j
    
    p_value_j <- 2 *
      stats::pt(
        
        abs(
          statistic_j
        ),
        
        df =
          residual_df,
        
        lower.tail =
          FALSE
      )
    
    
    resultados[[j]] <- tibble::tibble(
      
      model_id =
        model_id_arg,
      
      model_key =
        model_key_arg,
      
      model_label_run =
        model_label_arg,
      
      rank_type =
        rank_type_arg,
      
      equation =
        colnames(B)[j],
      
      term =
        rownames(B),
      
      estimate =
        estimate_j,
      
      std_error =
        std_error_j,
      
      statistic =
        statistic_j,
      
      p_value =
        p_value_j,
      
      residual_df =
        residual_df
    )
  }
  
  
  dplyr::bind_rows(
    resultados
  )
}


lista_coef_system <- list()

lista_coef_trade <- list()


for (
  i in seq_len(
    nrow(decision_rango)
  )
) {
  
  model_id_i <-
    decision_rango$model_id[[i]]
  
  
  lista_coef_system[[i]] <- extraer_coeficientes_mlm(
    
    cajorls_object =
      modelos_cajorls_system[[model_id_i]],
    
    model_id_arg =
      model_id_i,
    
    model_key_arg =
      decision_rango$model_key[[i]],
    
    model_label_arg =
      decision_rango$model_label_run[[i]],
    
    rank_type_arg =
      "Data-driven system rank"
  )
  
  
  lista_coef_trade[[i]] <- extraer_coeficientes_mlm(
    
    cajorls_object =
      modelos_cajorls_trade_r1[[model_id_i]],
    
    model_id_arg =
      model_id_i,
    
    model_key_arg =
      decision_rango$model_key[[i]],
    
    model_label_arg =
      decision_rango$model_label_run[[i]],
    
    rank_type_arg =
      "Economic rank r=1"
  )
}


coeficientes_vecm_system <- dplyr::bind_rows(
  lista_coef_system
)

coeficientes_vecm_trade <- dplyr::bind_rows(
  lista_coef_trade
)


# ------------------------------------------------------------
# 18. COEFICIENTES DE AJUSTE ALFA
# ------------------------------------------------------------

ajuste_alpha <- coeficientes_vecm_trade |>
  dplyr::filter(
    grepl(
      "^ect[0-9]+$",
      term
    )
  ) |>
  dplyr::left_join(
    
    decision_rango |>
      dplyr::select(
        model_id,
        dependent_variable,
        rank_trace_10pct,
        formal_status
      ),
    
    by =
      "model_id"
  ) |>
  dplyr::mutate(
    
    target_equation =
      paste0(
        make.names(
          dependent_variable
        ),
        ".d"
      ),
    
    is_trade_equation =
      equation ==
      target_equation,
    
    significant_10pct =
      p_value <
      nivel_significancia,
    
    adjustment_interpretation =
      dplyr::case_when(
        
        is_trade_equation &
          estimate < 0 &
          significant_10pct ~
          paste0(
            "Ajuste negativo y significativo: ",
            round(
              -100 * estimate,
              1
            ),
            "% del desequilibrio se corrige por trimestre."
          ),
        
        is_trade_equation &
          estimate < 0 ~
          paste0(
            "Ajuste negativo, pero no significativo al 10%."
          ),
        
        is_trade_equation &
          estimate >= 0 ~
          paste0(
            "El signo del ajuste no es compatible ",
            "con convergencia de la ecuación comercial."
          ),
        
        TRUE ~
          paste0(
            "Coeficiente de ajuste de otra ecuación del sistema."
          )
      )
  )


ajuste_alpha_trade <- ajuste_alpha |>
  dplyr::filter(
    is_trade_equation,
    term == "ect1"
  )


print(
  ajuste_alpha_trade,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 19. ELASTICIDADES DE CORTO PLAZO DEL VECM
# ------------------------------------------------------------

corto_plazo_vecm <- coeficientes_vecm_trade |>
  dplyr::left_join(
    
    decision_rango |>
      dplyr::select(
        model_id,
        dependent_variable,
        is_primary,
        is_sensitivity
      ),
    
    by =
      "model_id"
  ) |>
  dplyr::mutate(
    
    target_equation =
      paste0(
        make.names(
          dependent_variable
        ),
        ".d"
      ),
    
    is_trade_equation =
      equation ==
      target_equation,
    
    is_lagged_difference =
      grepl(
        "\\.dl[0-9]+$",
        term
      ),
    
    is_stationary_exogenous =
      term == "commodity_c",
    
    variable =
      dplyr::case_when(
        
        is_lagged_difference ~
          sub(
            "\\.dl[0-9]+$",
            "",
            term
          ),
        
        is_stationary_exogenous ~
          term,
        
        TRUE ~
          NA_character_
      ),
    
    lag = dplyr::case_when(
      
      is_stationary_exogenous ~
        0L,
      
      is_lagged_difference ~
        extraer_lag_termino(
          term
        ),
      
      TRUE ~
        NA_integer_
    ),
    
    coefficient_type =
      dplyr::case_when(
        
        is_lagged_difference ~
          "Lagged first difference",
        
        is_stationary_exogenous ~
          "Stationary exogenous control",
        
        TRUE ~
          "Other"
      )
  ) |>
  dplyr::filter(
    
    is_trade_equation,
    
    is_lagged_difference |
      is_stationary_exogenous
  )


corto_plazo_acumulado <- corto_plazo_vecm |>
  dplyr::filter(
    is_lagged_difference
  ) |>
  dplyr::group_by(
    model_id,
    model_key,
    model_label_run,
    equation,
    variable
  ) |>
  dplyr::summarise(
    
    number_of_lags =
      dplyr::n(),
    
    cumulative_short_run_coefficient =
      sum(
        estimate
      ),
    
    .groups =
      "drop"
  )


print(
  corto_plazo_vecm,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 20. DIAGNÓSTICOS NATIVOS DE vec2var
# ------------------------------------------------------------

diagnosticar_modelo_var <- function(
    modelo,
    model_id_arg,
    model_key_arg,
    model_label_arg,
    model_type_arg
) {
  
  pt <- tryCatch(
    
    vars::serial.test(
      modelo,
      lags.pt =
        lags_autocorrelacion,
      type =
        "PT.adjusted"
    ),
    
    error = function(e) {
      e
    }
  )
  
  
  bg <- tryCatch(
    
    vars::serial.test(
      modelo,
      lags.bg =
        lags_autocorrelacion,
      type =
        "BG"
    ),
    
    error = function(e) {
      e
    }
  )
  
  
  normalidad <- tryCatch(
    
    vars::normality.test(
      modelo,
      multivariate.only =
        FALSE
    ),
    
    error = function(e) {
      e
    }
  )
  
  
  arch <- tryCatch(
    
    vars::arch.test(
      modelo,
      lags.multi =
        lags_arch,
      multivariate.only =
        FALSE
    ),
    
    error = function(e) {
      e
    }
  )
  
  
  pt_p <- if (
    inherits(pt, "error")
  ) {
    NA_real_
  } else {
    as.numeric(
      pt$serial$p.value
    )
  }
  
  
  bg_p <- if (
    inherits(bg, "error")
  ) {
    NA_real_
  } else {
    as.numeric(
      bg$serial$p.value
    )
  }
  
  
  normalidad_p <- if (
    inherits(normalidad, "error")
  ) {
    NA_real_
  } else {
    as.numeric(
      normalidad$jb.mul$JB$p.value
    )
  }
  
  
  arch_p <- if (
    inherits(arch, "error")
  ) {
    NA_real_
  } else {
    as.numeric(
      arch$arch.mul$p.value
    )
  }
  
  
  tibble::tibble(
    
    model_id =
      model_id_arg,
    
    model_key =
      model_key_arg,
    
    model_label_run =
      model_label_arg,
    
    model_type =
      model_type_arg,
    
    p_model =
      modelo$p,
    
    observations =
      modelo$obs,
    
    pt_adjusted_p_value =
      pt_p,
    
    bg_p_value =
      bg_p,
    
    normality_p_value =
      normalidad_p,
    
    arch_p_value =
      arch_p,
    
    serial_clean_10pct =
      !is.na(pt_p) &
      !is.na(bg_p) &
      pt_p >
      nivel_significancia &
      bg_p >
      nivel_significancia,
    
    normality_valid_10pct =
      !is.na(normalidad_p) &
      normalidad_p >
      nivel_significancia,
    
    arch_valid_10pct =
      !is.na(arch_p) &
      arch_p >
      nivel_significancia,
    
    diagnostic_interpretation =
      dplyr::case_when(
        
        !is.na(pt_p) &
          !is.na(bg_p) &
          pt_p >
          nivel_significancia &
          bg_p >
          nivel_significancia &
          !is.na(normalidad_p) &
          normalidad_p >
          nivel_significancia ~
          paste0(
            "Sin autocorrelación y sin rechazo ",
            "de normalidad al 10%."
          ),
        
        !is.na(pt_p) &
          !is.na(bg_p) &
          pt_p >
          nivel_significancia &
          bg_p >
          nivel_significancia ~
          paste0(
            "Sin autocorrelación, pero se rechaza ",
            "normalidad; inferencia indicativa."
          ),
        
        TRUE ~
          paste0(
            "Persisten problemas de autocorrelación ",
            "o diagnósticos no concluyentes."
          )
      )
  )
}


lista_diagnosticos_vec2var <- list()


for (
  i in seq_len(
    nrow(decision_rango)
  )
) {
  
  model_id_i <-
    decision_rango$model_id[[i]]
  
  
  lista_diagnosticos_vec2var[[i]] <-
    diagnosticar_modelo_var(
      
      modelo =
        modelos_vec2var_system[[model_id_i]],
      
      model_id_arg =
        model_id_i,
      
      model_key_arg =
        decision_rango$model_key[[i]],
      
      model_label_arg =
        decision_rango$model_label_run[[i]],
      
      model_type_arg =
        paste0(
          "VECM transformed to VAR, r=",
          decision_rango$rank_system_vecm[[i]]
        )
    )
}


diagnosticos_vec2var <- dplyr::bind_rows(
  lista_diagnosticos_vec2var
)


print(
  diagnosticos_vec2var,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 21. CORRELOGRAMAS CON vcorr_res()
# ------------------------------------------------------------

ordenar_vcorr <- function(
    salida,
    model_id_arg,
    model_key_arg,
    model_label_arg,
    test_type_arg
) {
  
  if (
    is.null(salida) ||
    !(
      is.matrix(salida) ||
      is.data.frame(salida)
    )
  ) {
    
    return(
      tibble::tibble(
        
        model_id =
          model_id_arg,
        
        model_key =
          model_key_arg,
        
        model_label_run =
          model_label_arg,
        
        test_type =
          test_type_arg,
        
        lag =
          NA_integer_,
        
        statistic =
          NA_real_,
        
        p_value =
          NA_real_,
        
        status =
          "ERROR"
      )
    )
  }
  
  
  salida_df <- as.data.frame(
    salida,
    stringsAsFactors = FALSE
  )
  
  
  tibble::tibble(
    
    model_id =
      model_id_arg,
    
    model_key =
      model_key_arg,
    
    model_label_run =
      model_label_arg,
    
    test_type =
      test_type_arg,
    
    lag =
      suppressWarnings(
        as.integer(
          salida_df[[1]]
        )
      ),
    
    statistic =
      suppressWarnings(
        as.numeric(
          salida_df[[2]]
        )
      ),
    
    p_value =
      suppressWarnings(
        as.numeric(
          as.character(
            salida_df[[3]]
          )
        )
      ),
    
    status =
      "OK"
  ) |>
    dplyr::mutate(
      
      decision_10pct =
        dplyr::case_when(
          
          is.na(p_value) ~
            "No disponible",
          
          p_value <
            nivel_significancia ~
            "Se rechaza ausencia de autocorrelación",
          
          TRUE ~
            "No se rechaza ausencia de autocorrelación"
        )
    )
}


lista_vcorr_vecm <- list()

contador_vcorr <- 1L


for (
  i in seq_len(
    nrow(decision_rango)
  )
) {
  
  model_id_i <-
    decision_rango$model_id[[i]]
  
  modelo_i <-
    modelos_vec2var_system[[model_id_i]]
  
  max_lag_i <- min(
    lags_autocorrelacion,
    floor(
      modelo_i$obs /
        3
    )
  )
  
  
  for (
    tipo_i in c(
      "PT.adjusted",
      "BG",
      "ES"
    )
  ) {
    
    salida_i <- tryCatch(
      
      vcorr_res(
        
        var_reg =
          modelo_i,
        
        lags =
          max_lag_i,
        
        tipo =
          tipo_i
      ),
      
      error = function(e) {
        NULL
      }
    )
    
    
    lista_vcorr_vecm[[
      contador_vcorr
    ]] <- ordenar_vcorr(
      
      salida =
        salida_i,
      
      model_id_arg =
        model_id_i,
      
      model_key_arg =
        decision_rango$model_key[[i]],
      
      model_label_arg =
        decision_rango$model_label_run[[i]],
      
      test_type_arg =
        tipo_i
    )
    
    
    contador_vcorr <-
      contador_vcorr + 1L
  }
}


vcorr_vecm <- dplyr::bind_rows(
  lista_vcorr_vecm
)


# ------------------------------------------------------------
# 22. RAÍCES DEL VAR EQUIVALENTE
# ------------------------------------------------------------

extraer_raices_vec2var <- function(
    modelo,
    rank_arg,
    model_id_arg,
    model_key_arg,
    model_label_arg
) {
  
  A_list <- modelo$A
  
  K_model <- modelo$K
  
  p_model <- modelo$p
  
  
  bloque_superior <- do.call(
    cbind,
    A_list
  )
  
  
  if (p_model == 1) {
    
    companion <- bloque_superior
    
  } else {
    
    bloque_inferior <- cbind(
      
      diag(
        K_model *
          (p_model - 1L)
      ),
      
      matrix(
        0,
        nrow =
          K_model *
          (p_model - 1L),
        ncol =
          K_model
      )
    )
    
    
    companion <- rbind(
      bloque_superior,
      bloque_inferior
    )
  }
  
  
  eigenvalues <- eigen(
    companion,
    only.values = TRUE
  )$values
  
  
  modulus <- Mod(
    eigenvalues
  )
  
  
  tibble::tibble(
    
    model_id =
      model_id_arg,
    
    model_key =
      model_key_arg,
    
    model_label_run =
      model_label_arg,
    
    rank =
      rank_arg,
    
    expected_unit_roots =
      K_model -
      rank_arg,
    
    root_number =
      seq_along(
        eigenvalues
      ),
    
    real_part =
      Re(
        eigenvalues
      ),
    
    imaginary_part =
      Im(
        eigenvalues
      ),
    
    modulus =
      modulus,
    
    unit_root_numeric =
      abs(
        modulus - 1
      ) <
      tolerancia_raiz_uno,
    
    near_unit_root =
      abs(
        modulus - 1
      ) <
      0.02
  )
}


lista_raices_vecm <- list()


for (
  i in seq_len(
    nrow(decision_rango)
  )
) {
  
  model_id_i <-
    decision_rango$model_id[[i]]
  
  
  lista_raices_vecm[[i]] <-
    extraer_raices_vec2var(
      
      modelo =
        modelos_vec2var_system[[model_id_i]],
      
      rank_arg =
        decision_rango$rank_system_vecm[[i]],
      
      model_id_arg =
        model_id_i,
      
      model_key_arg =
        decision_rango$model_key[[i]],
      
      model_label_arg =
        decision_rango$model_label_run[[i]]
    )
}


raices_vecm <- dplyr::bind_rows(
  lista_raices_vecm
)


resumen_raices_vecm <- raices_vecm |>
  dplyr::group_by(
    model_id,
    model_key,
    model_label_run,
    rank,
    expected_unit_roots
  ) |>
  dplyr::summarise(
    
    detected_unit_roots =
      sum(
        unit_root_numeric
      ),
    
    near_unit_roots =
      sum(
        near_unit_root
      ),
    
    maximum_modulus =
      max(
        modulus
      ),
    
    maximum_nonunit_modulus = {
      
      no_unit <- modulus[
        !unit_root_numeric
      ]
      
      if (length(no_unit) == 0) {
        NA_real_
      } else {
        max(no_unit)
      }
    },
    
    nonunit_roots_inside_circle =
      is.na(
        maximum_nonunit_modulus
      ) |
      maximum_nonunit_modulus <
      1,
    
    .groups =
      "drop"
  )


# ------------------------------------------------------------
# 23. SERIES DEL TÉRMINO DE CORRECCIÓN DEL ERROR
# ------------------------------------------------------------

lista_ect <- list()


for (
  i in seq_len(
    nrow(decision_rango)
  )
) {
  
  model_id_i <-
    decision_rango$model_id[[i]]
  
  model_key_i <-
    decision_rango$model_key[[i]]
  
  K_i <-
    decision_rango$K[[i]]
  
  y_i <-
    datos_sistemas[[model_key_i]]$y
  
  johansen_i <-
    modelos_johansen_trace[[model_id_i]]
  
  beta_i <-
    modelos_cajorls_trade_r1[[
      model_id_i
    ]]$beta
  
  
  ect_i <- as.numeric(
    johansen_i@ZK %*%
      beta_i
  )
  
  
  tiempo_completo <- stats::time(
    y_i
  )
  
  indice_ect <- seq.int(
    from =
      K_i + 1L,
    
    to =
      length(
        tiempo_completo
      )
  )
  
  
  fecha_ect <- as.Date(
    zoo::as.yearqtr(
      tiempo_completo[
        indice_ect
      ]
    ),
    frac = 1
  )
  
  
  lista_ect[[i]] <- tibble::tibble(
    
    model_id =
      model_id_i,
    
    model_key =
      model_key_i,
    
    model_label_run =
      decision_rango$model_label_run[[i]],
    
    date =
      fecha_ect,
    
    ect1 =
      ect_i
  )
}


series_ect <- dplyr::bind_rows(
  lista_ect
)


# ------------------------------------------------------------
# 24. ESTIMAR VAR EN DIFERENCIAS
# ------------------------------------------------------------

estimar_var_diferencias <- function(
    datos_sistema,
    K_levels
) {
  
  y_diff <- diff(
    datos_sistema$y
  )
  
  
  p_diff <- max(
    1L,
    as.integer(
      K_levels - 1L
    )
  )
  
  
  argumentos <- list(
    
    y =
      y_diff,
    
    p =
      p_diff,
    
    type =
      "const",
    
    season =
      4L
  )
  
  
  if (!is.null(datos_sistema$exogen)) {
    
    exogen_aligned <- stats::window(
      
      datos_sistema$exogen,
      
      start =
        stats::start(
          y_diff
        ),
      
      end =
        stats::end(
          y_diff
        )
    )
    
    
    argumentos$exogen <-
      exogen_aligned
  }
  
  
  do.call(
    vars::VAR,
    argumentos
  )
}


modelos_var_diferencias <- list()


for (
  i in seq_len(
    nrow(decision_rango)
  )
) {
  
  model_id_i <-
    decision_rango$model_id[[i]]
  
  model_key_i <-
    decision_rango$model_key[[i]]
  
  
  modelos_var_diferencias[[
    model_id_i
  ]] <- estimar_var_diferencias(
    
    datos_sistema =
      datos_sistemas[[model_key_i]],
    
    K_levels =
      decision_rango$K[[i]]
  )
}


# ------------------------------------------------------------
# 25. DIAGNÓSTICOS DEL VAR EN DIFERENCIAS
# ------------------------------------------------------------

lista_diagnosticos_diferencias <- list()


for (
  i in seq_len(
    nrow(decision_rango)
  )
) {
  
  model_id_i <-
    decision_rango$model_id[[i]]
  
  
  lista_diagnosticos_diferencias[[i]] <-
    diagnosticar_modelo_var(
      
      modelo =
        modelos_var_diferencias[[model_id_i]],
      
      model_id_arg =
        model_id_i,
      
      model_key_arg =
        decision_rango$model_key[[i]],
      
      model_label_arg =
        decision_rango$model_label_run[[i]],
      
      model_type_arg =
        "VAR in first differences"
    )
}


diagnosticos_var_diferencias <- dplyr::bind_rows(
  lista_diagnosticos_diferencias
)


# ------------------------------------------------------------
# 26. COEFICIENTES DEL VAR EN DIFERENCIAS
# ------------------------------------------------------------

extraer_coeficientes_varest <- function(
    modelo,
    model_id_arg,
    model_key_arg,
    model_label_arg
) {
  
  purrr::imap_dfr(
    
    modelo$varresult,
    
    function(fit, equation_name) {
      
      tabla_coef <- summary(
        fit
      )$coefficients
      
      
      tibble::tibble(
        
        model_id =
          model_id_arg,
        
        model_key =
          model_key_arg,
        
        model_label_run =
          model_label_arg,
        
        equation =
          equation_name,
        
        term =
          rownames(
            tabla_coef
          ),
        
        estimate =
          as.numeric(
            tabla_coef[, 1]
          ),
        
        std_error =
          as.numeric(
            tabla_coef[, 2]
          ),
        
        statistic =
          as.numeric(
            tabla_coef[, 3]
          ),
        
        p_value =
          as.numeric(
            tabla_coef[, 4]
          )
      )
    }
  )
}


lista_coef_diferencias <- list()


for (
  i in seq_len(
    nrow(decision_rango)
  )
) {
  
  model_id_i <-
    decision_rango$model_id[[i]]
  
  
  lista_coef_diferencias[[i]] <-
    extraer_coeficientes_varest(
      
      modelo =
        modelos_var_diferencias[[model_id_i]],
      
      model_id_arg =
        model_id_i,
      
      model_key_arg =
        decision_rango$model_key[[i]],
      
      model_label_arg =
        decision_rango$model_label_run[[i]]
    )
}


coeficientes_var_diferencias <- dplyr::bind_rows(
  lista_coef_diferencias
)


corto_plazo_var_diferencias <- coeficientes_var_diferencias |>
  dplyr::left_join(
    
    decision_rango |>
      dplyr::select(
        model_id,
        dependent_variable
      ),
    
    by =
      "model_id"
  ) |>
  dplyr::filter(
    equation ==
      make.names(
        dependent_variable
      )
  ) |>
  dplyr::mutate(
    
    lag = extraer_lag_termino(
      term
    ),
    
    variable =
      sub(
        "\\.l[0-9]+$",
        "",
        term
      ),
    
    is_dynamic_term =
      grepl(
        "\\.l[0-9]+$",
        term
      )
  ) |>
  dplyr::filter(
    is_dynamic_term
  )


# ------------------------------------------------------------
# 27. COMPARAR ELASTICIDADES CON TP2
# ------------------------------------------------------------

elasticidades_johansen_principales <- elasticidades_johansen |>
  dplyr::left_join(
    
    decision_rango |>
      dplyr::select(
        model_id,
        is_primary
      ),
    
    by =
      "model_id"
  ) |>
  dplyr::filter(
    is_primary
  ) |>
  dplyr::select(
    
    model_key,
    
    variable,
    
    estimate_johansen =
      long_run_elasticity,
    
    johansen_validity =
      validity,
    
    rank_trace_10pct
  )


if (nrow(comparacion_engle_granger) > 0) {
  
  comparacion_metodos <- comparacion_engle_granger |>
    dplyr::left_join(
      
      elasticidades_johansen_principales,
      
      by = c(
        "model_key",
        "variable"
      )
    ) |>
    dplyr::mutate(
      
      difference_johansen_eg =
        estimate_johansen -
        estimate_eg,
      
      difference_johansen_wb =
        estimate_johansen -
        estimate_wb,
      
      same_sign_all =
        dplyr::case_when(
          
          is.na(estimate_johansen) ~
            NA,
          
          sign(
            estimate_eg
          ) ==
            sign(
              estimate_wb
            ) &
            sign(
              estimate_wb
            ) ==
            sign(
              estimate_johansen
            ) ~
            TRUE,
          
          TRUE ~
            FALSE
        )
    )
  
} else {
  
  comparacion_metodos <- tibble::tibble()
}


# ------------------------------------------------------------
# 28. RESUMEN INTEGRAL DEL SCRIPT 11
# ------------------------------------------------------------

resumen_johansen <- decision_rango |>
  dplyr::left_join(
    
    ajuste_alpha_trade |>
      dplyr::select(
        
        model_id,
        
        adjustment_estimate =
          estimate,
        
        adjustment_p_value =
          p_value,
        
        adjustment_interpretation
      ),
    
    by =
      "model_id"
  ) |>
  dplyr::left_join(
    
    diagnosticos_vec2var |>
      dplyr::select(
        
        model_id,
        
        vecm_pt_p_value =
          pt_adjusted_p_value,
        
        vecm_bg_p_value =
          bg_p_value,
        
        vecm_normality_p_value =
          normality_p_value,
        
        vecm_arch_p_value =
          arch_p_value,
        
        vecm_diagnostic_interpretation =
          diagnostic_interpretation
      ),
    
    by =
      "model_id"
  ) |>
  dplyr::left_join(
    
    resumen_raices_vecm,
    
    by = c(
      "model_id",
      "model_key",
      "model_label_run",
      "rank_system_vecm" =
        "rank"
    )
  ) |>
  dplyr::left_join(
    
    diagnosticos_var_diferencias |>
      dplyr::select(
        
        model_id,
        
        diff_var_pt_p_value =
          pt_adjusted_p_value,
        
        diff_var_bg_p_value =
          bg_p_value,
        
        diff_var_normality_p_value =
          normality_p_value,
        
        diff_var_arch_p_value =
          arch_p_value
      ),
    
    by =
      "model_id"
  ) |>
  dplyr::mutate(
    
    recommended_multivariate_model =
      dplyr::case_when(
        
        rank_trace_10pct >= 1 &
          rank_trace_10pct <
          n_endogenous ~
          paste0(
            "VECM con rango ",
            rank_system_vecm
          ),
        
        TRUE ~
          paste0(
            "VAR en diferencias; VECM r=1 ",
            "sólo indicativo"
          )
      )
  )


print(
  resumen_johansen,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 29. GUARDAR RESÚMENES TEXTUALES
# ------------------------------------------------------------

salida_texto <- character()


for (
  i in seq_len(
    nrow(decision_rango)
  )
) {
  
  model_id_i <-
    decision_rango$model_id[[i]]
  
  
  encabezado <- c(
    
    "",
    
    "============================================================",
    
    paste0(
      "Modelo: ",
      decision_rango$model_label_run[[i]]
    ),
    
    paste0(
      "K: ",
      decision_rango$K[[i]]
    ),
    
    paste0(
      "Rango traza al 10%: ",
      decision_rango$rank_trace_10pct[[i]]
    ),
    
    paste0(
      "Rango máximo autovalor al 10%: ",
      decision_rango$rank_eigen_10pct[[i]]
    ),
    
    paste0(
      "Rango utilizado en el VECM: ",
      decision_rango$rank_system_vecm[[i]]
    ),
    
    "============================================================"
  )
  
  
  resumen_trace <- capture.output(
    summary(
      modelos_johansen_trace[[model_id_i]]
    )
  )
  
  
  resumen_eigen <- capture.output(
    summary(
      modelos_johansen_eigen[[model_id_i]]
    )
  )
  
  
  resumen_vecm_system <- capture.output(
    summary(
      modelos_cajorls_system[[
        model_id_i
      ]]$rlm
    )
  )
  
  
  resumen_vecm_r1 <- capture.output(
    summary(
      modelos_cajorls_trade_r1[[
        model_id_i
      ]]$rlm
    )
  )
  
  
  salida_texto <- c(
    
    salida_texto,
    
    encabezado,
    
    "",
    
    "TEST DE TRAZA:",
    
    resumen_trace,
    
    "",
    
    "TEST DE MÁXIMO AUTOVALOR:",
    
    resumen_eigen,
    
    "",
    
    "VECM CON RANGO DEL SISTEMA:",
    
    resumen_vecm_system,
    
    "",
    
    "VECM ECONÓMICO CON r = 1:",
    
    resumen_vecm_r1
  )
}


writeLines(
  
  salida_texto,
  
  con = here::here(
    "outputs",
    "models",
    "resumen_johansen_vecm.txt"
  )
)


# ------------------------------------------------------------
# 30. GRÁFICO DE LOS TESTS DE JOHANSEN
# ------------------------------------------------------------

datos_grafico_johansen <- tests_johansen |>
  dplyr::mutate(
    
    null_rank_factor =
      factor(
        null_rank,
        levels = sort(
          unique(
            null_rank
          )
        )
      )
  )


grafico_johansen <- ggplot2::ggplot(
  
  datos_grafico_johansen,
  
  ggplot2::aes(
    x =
      null_rank_factor
  )
) +
  ggplot2::geom_point(
    
    ggplot2::aes(
      y =
        statistic,
      shape =
        "Estadístico"
    ),
    
    size =
      2.5
  ) +
  ggplot2::geom_point(
    
    ggplot2::aes(
      y =
        critical_10pct,
      shape =
        "Valor crítico 10%"
    ),
    
    size =
      2.5
  ) +
  ggplot2::facet_grid(
    test_type ~ model_label_run,
    scales = "free_y"
  ) +
  ggplot2::labs(
    
    title =
      "Pruebas de cointegración de Johansen",
    
    subtitle =
      paste0(
        "Comparación del estadístico con ",
        "el valor crítico al 10%"
      ),
    
    x =
      "Rango bajo la hipótesis nula",
    
    y =
      "Valor",
    
    shape =
      NULL
  ) +
  ggplot2::theme_minimal(
    base_size = 10
  ) +
  ggplot2::theme(
    
    axis.text.x =
      ggplot2::element_text(
        angle = 45,
        hjust = 1
      ),
    
    legend.position =
      "bottom"
  )


ggplot2::ggsave(
  
  filename = here::here(
    "outputs",
    "figures",
    "11_tests_johansen.png"
  ),
  
  plot =
    grafico_johansen,
  
  width =
    16,
  
  height =
    8,
  
  dpi =
    300
)


# ------------------------------------------------------------
# 31. GRÁFICO DE LOS TÉRMINOS DE CORRECCIÓN
# ------------------------------------------------------------

grafico_ect <- ggplot2::ggplot(
  
  series_ect,
  
  ggplot2::aes(
    x =
      date,
    y =
      ect1
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(
    ~ model_label_run,
    scales = "free_y",
    ncol = 1
  ) +
  ggplot2::labs(
    
    title =
      "Término de corrección del error del VECM",
    
    subtitle =
      "Relación económica normalizada con rango r = 1",
    
    x =
      NULL,
    
    y =
      "ECT"
  ) +
  ggplot2::theme_minimal(
    base_size = 10
  )


ggplot2::ggsave(
  
  filename = here::here(
    "outputs",
    "figures",
    "11_terminos_correccion_error.png"
  ),
  
  plot =
    grafico_ect,
  
  width =
    12,
  
  height =
    10,
  
  dpi =
    300
)


# ------------------------------------------------------------
# 32. GRÁFICO DE COMPARACIÓN DE ELASTICIDADES
# ------------------------------------------------------------

if (nrow(comparacion_metodos) > 0) {
  
  datos_grafico_elasticidades <- comparacion_metodos |>
    dplyr::select(
      
      model_label,
      
      variable,
      
      Engle_Granger =
        estimate_eg,
      
      Wickens_Breusch =
        estimate_wb,
      
      Johansen =
        estimate_johansen
    ) |>
    tidyr::pivot_longer(
      
      cols = c(
        Engle_Granger,
        Wickens_Breusch,
        Johansen
      ),
      
      names_to =
        "method",
      
      values_to =
        "elasticity"
    ) |>
    dplyr::mutate(
      
      method =
        dplyr::recode(
          
          method,
          
          Engle_Granger =
            "Engle-Granger",
          
          Wickens_Breusch =
            "Wickens-Breusch",
          
          Johansen =
            "Johansen"
        )
    )
  
  
  grafico_elasticidades <- ggplot2::ggplot(
    
    datos_grafico_elasticidades,
    
    ggplot2::aes(
      
      x =
        variable,
      
      y =
        elasticity,
      
      shape =
        method,
      
      group =
        method
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    ggplot2::geom_point(
      size = 2.5,
      position =
        ggplot2::position_dodge(
          width = 0.4
        )
    ) +
    ggplot2::facet_wrap(
      ~ model_label,
      scales = "free_x"
    ) +
    ggplot2::labs(
      
      title =
        "Comparación de elasticidades de largo plazo",
      
      subtitle =
        paste0(
          "Engle-Granger, Wickens-Breusch ",
          "y Johansen"
        ),
      
      x =
        NULL,
      
      y =
        "Elasticidad",
      
      shape =
        "Método"
    ) +
    ggplot2::theme_minimal(
      base_size = 10
    ) +
    ggplot2::theme(
      
      axis.text.x =
        ggplot2::element_text(
          angle = 35,
          hjust = 1
        ),
      
      legend.position =
        "bottom"
    )
  
  
  ggplot2::ggsave(
    
    filename = here::here(
      "outputs",
      "figures",
      "11_comparacion_elasticidades.png"
    ),
    
    plot =
      grafico_elasticidades,
    
    width =
      13,
    
    height =
      7,
    
    dpi =
      300
  )
}


# ------------------------------------------------------------
# 33. GUARDAR RESULTADOS EN EXCEL
# ------------------------------------------------------------

hojas_excel <- list(
  
  config =
    config_modelos,
  
  johansen_tests =
    tests_johansen,
  
  rank_decision =
    decision_rango,
  
  beta_system =
    beta_system,
  
  beta_trade_r1 =
    beta_trade_r1,
  
  lr_elasticities =
    elasticidades_johansen,
  
  lr_constants =
    constantes_largo_plazo,
  
  alpha =
    ajuste_alpha,
  
  alpha_trade =
    ajuste_alpha_trade,
  
  vecm_coeff_system =
    coeficientes_vecm_system,
  
  vecm_coeff_r1 =
    coeficientes_vecm_trade,
  
  short_run_vecm =
    corto_plazo_vecm,
  
  short_run_sum =
    corto_plazo_acumulado,
  
  vecm_diagnostics =
    diagnosticos_vec2var,
  
  vcorr_vecm =
    vcorr_vecm,
  
  vecm_roots =
    raices_vecm,
  
  vecm_roots_summary =
    resumen_raices_vecm,
  
  diff_var_diagnostics =
    diagnosticos_var_diferencias,
  
  diff_var_coefficients =
    coeficientes_var_diferencias,
  
  diff_var_short_run =
    corto_plazo_var_diferencias,
  
  final_summary =
    resumen_johansen
)


if (nrow(comparacion_metodos) > 0) {
  
  hojas_excel$methods_comparison <-
    comparacion_metodos
}


if (nrow(diagnosticos_script10) > 0) {
  
  hojas_excel$script10_diagnostics <-
    diagnosticos_script10
}


writexl::write_xlsx(
  
  hojas_excel,
  
  path = here::here(
    "outputs",
    "tables",
    "johansen_vecm_results.xlsx"
  )
)


# ------------------------------------------------------------
# 34. GUARDAR OBJETOS RDS
# ------------------------------------------------------------

saveRDS(
  
  modelos_johansen_trace,
  
  here::here(
    "outputs",
    "models",
    "modelos_johansen_trace.rds"
  )
)


saveRDS(
  
  modelos_johansen_eigen,
  
  here::here(
    "outputs",
    "models",
    "modelos_johansen_eigen.rds"
  )
)


saveRDS(
  
  modelos_cajorls_system,
  
  here::here(
    "outputs",
    "models",
    "modelos_vecm_system.rds"
  )
)


saveRDS(
  
  modelos_cajorls_trade_r1,
  
  here::here(
    "outputs",
    "models",
    "modelos_vecm_trade_r1.rds"
  )
)


saveRDS(
  
  modelos_vec2var_system,
  
  here::here(
    "outputs",
    "models",
    "modelos_vec2var_system.rds"
  )
)


saveRDS(
  
  modelos_vec2var_trade_r1,
  
  here::here(
    "outputs",
    "models",
    "modelos_vec2var_trade_r1.rds"
  )
)


saveRDS(
  
  modelos_var_diferencias,
  
  here::here(
    "outputs",
    "models",
    "modelos_var_diferencias.rds"
  )
)


saveRDS(
  
  decision_rango,
  
  here::here(
    "outputs",
    "models",
    "decision_rango_johansen.rds"
  )
)


saveRDS(
  
  elasticidades_johansen,
  
  here::here(
    "outputs",
    "models",
    "elasticidades_largo_plazo_johansen.rds"
  )
)


saveRDS(
  
  corto_plazo_vecm,
  
  here::here(
    "outputs",
    "models",
    "elasticidades_corto_plazo_vecm.rds"
  )
)


saveRDS(
  
  resumen_johansen,
  
  here::here(
    "outputs",
    "models",
    "resumen_johansen_vecm.rds"
  )
)


# ------------------------------------------------------------
# 35. CONTROLES FINALES
# ------------------------------------------------------------

stopifnot(
  
  nrow(config_modelos) ==
    4,
  
  length(
    modelos_johansen_trace
  ) ==
    4,
  
  length(
    modelos_johansen_eigen
  ) ==
    4,
  
  length(
    modelos_cajorls_system
  ) ==
    4,
  
  length(
    modelos_vec2var_system
  ) ==
    4,
  
  nrow(
    decision_rango
  ) ==
    4,
  
  nrow(
    diagnosticos_vec2var
  ) ==
    4,
  
  nrow(
    diagnosticos_var_diferencias
  ) ==
    4
)


# ------------------------------------------------------------
# 36. RESULTADOS PRINCIPALES EN CONSOLA
# ------------------------------------------------------------

cat(
  "\n\n============================================================",
  "\nRANGOS DE COINTEGRACIÓN",
  "\n============================================================\n"
)


decision_rango |>
  dplyr::select(
    
    model_label_run,
    
    K,
    
    rank_trace_10pct,
    
    rank_trace_5pct,
    
    rank_eigen_10pct,
    
    rank_eigen_5pct,
    
    trace_eigen_agree_10pct,
    
    rank_system_vecm,
    
    rank_system_forced,
    
    rank_interpretation
  ) |>
  print(
    n = Inf,
    width = Inf
  )


cat(
  "\n\n============================================================",
  "\nELASTICIDADES DE LARGO PLAZO JOHANSEN",
  "\n============================================================\n"
)


elasticidades_johansen |>
  dplyr::select(
    
    model_label_run,
    
    variable,
    
    beta_coefficient,
    
    long_run_elasticity,
    
    rank_trace_10pct,
    
    validity
  ) |>
  print(
    n = Inf,
    width = Inf
  )


cat(
  "\n\n============================================================",
  "\nCOEFICIENTES DE AJUSTE DE LA ECUACIÓN COMERCIAL",
  "\n============================================================\n"
)


ajuste_alpha_trade |>
  dplyr::select(
    
    model_label_run,
    
    equation,
    
    estimate,
    
    std_error,
    
    statistic,
    
    p_value,
    
    adjustment_interpretation
  ) |>
  print(
    n = Inf,
    width = Inf
  )


cat(
  "\n\n============================================================",
  "\nDIAGNÓSTICOS DEL VECM",
  "\n============================================================\n"
)


diagnosticos_vec2var |>
  dplyr::select(
    
    model_label_run,
    
    model_type,
    
    pt_adjusted_p_value,
    
    bg_p_value,
    
    normality_p_value,
    
    arch_p_value,
    
    diagnostic_interpretation
  ) |>
  print(
    n = Inf,
    width = Inf
  )


cat(
  "\n\n============================================================",
  "\nMODELO MULTIVARIADO RECOMENDADO",
  "\n============================================================\n"
)


resumen_johansen |>
  dplyr::select(
    
    model_label_run,
    
    rank_trace_10pct,
    
    rank_eigen_10pct,
    
    rank_system_vecm,
    
    adjustment_estimate,
    
    adjustment_p_value,
    
    recommended_multivariate_model,
    
    inference_status
  ) |>
  print(
    n = Inf,
    width = Inf
  )


# ------------------------------------------------------------
# 37. MENSAJE FINAL
# ------------------------------------------------------------

cat(
  "\n============================================================",
  "\nSCRIPT 11 FINALIZADO",
  "\n============================================================",
  
  "\nEstimaciones de traza:",
  length(
    modelos_johansen_trace
  ),
  
  "\nEstimaciones de máximo autovalor:",
  length(
    modelos_johansen_eigen
  ),
  
  "\nVECM estimados:",
  length(
    modelos_cajorls_system
  ),
  
  "\nVAR en diferencias estimados:",
  length(
    modelos_var_diferencias
  ),
  
  "\nNivel principal de significancia:",
  "10%",
  
  "\nEspecificación determinística:",
  "ecdet = 'const'",
  
  "\nEspecificación VECM:",
  "spec = 'transitory'",
  
  "\nControl estacional:",
  "season = 4",
  
  "\n\nArchivos generados:",
  
  "\n- outputs/tables/johansen_vecm_results.xlsx",
  
  "\n- outputs/models/modelos_johansen_trace.rds",
  
  "\n- outputs/models/modelos_johansen_eigen.rds",
  
  "\n- outputs/models/modelos_vecm_system.rds",
  
  "\n- outputs/models/modelos_vecm_trade_r1.rds",
  
  "\n- outputs/models/modelos_vec2var_system.rds",
  
  "\n- outputs/models/modelos_vec2var_trade_r1.rds",
  
  "\n- outputs/models/modelos_var_diferencias.rds",
  
  "\n- outputs/models/decision_rango_johansen.rds",
  
  "\n- outputs/models/elasticidades_largo_plazo_johansen.rds",
  
  "\n- outputs/models/elasticidades_corto_plazo_vecm.rds",
  
  "\n- outputs/models/resumen_johansen_vecm.rds",
  
  "\n- outputs/models/resumen_johansen_vecm.txt",
  
  "\n- figures/11_tests_johansen.png",
  
  "\n- figures/11_terminos_correccion_error.png",
  
  "\n- figures/11_comparacion_elasticidades.png",
  
  "\n============================================================",
  "\n"
)

decision_rango |>
  dplyr::select(
    model_label_run,
    K,
    rank_trace_10pct,
    rank_trace_5pct,
    rank_eigen_10pct,
    rank_eigen_5pct,
    rank_system_vecm,
    rank_interpretation
  ) |>
  print(n = Inf, width = Inf)

elasticidades_johansen |>
  print(n = Inf, width = Inf)

ajuste_alpha_trade |>
  print(n = Inf, width = Inf)

diagnosticos_vec2var |>
  print(n = Inf, width = Inf)


# ------------------------------------------------------------
# 38. DECISIÓN FINAL DE LOS MODELOS MULTIVARIADOS
# ------------------------------------------------------------

decision_modelos_finales_tp3 <- decision_rango |>
  dplyr::left_join(
    ajuste_alpha_trade |>
      dplyr::select(
        model_id,
        adjustment_estimate = estimate,
        adjustment_p_value = p_value
      ),
    by = "model_id"
  ) |>
  dplyr::left_join(
    diagnosticos_vec2var |>
      dplyr::select(
        model_id,
        pt_adjusted_p_value,
        bg_p_value,
        normality_p_value,
        arch_p_value
      ),
    by = "model_id"
  ) |>
  dplyr::mutate(
    
    recommended_status = dplyr::case_when(
      
      model_key == "imports" &
        is_primary ~
        paste0(
          "VECM multivariado con r=2. Resultado indicativo; ",
          "la relación r=1 se usa sólo como comparación económica."
        ),
      
      model_key == "exports_classic" &
        is_primary ~
        paste0(
          "VECM principal de exportaciones con r=1 y K=2."
        ),
      
      model_key == "exports_expanded" &
        is_primary ~
        paste0(
          "VECM de robustez con r=1, K=2 y commodities exógena."
        ),
      
      model_key == "exports_expanded" &
        is_sensitivity ~
        paste0(
          "Sensibilidad con K=4. No recomendado como modelo ",
          "principal porque el ajuste no es significativo."
        ),
      
      TRUE ~
        "Resultado complementario"
    ),
    
    final_use = dplyr::case_when(
      
      model_key == "imports" &
        is_primary ~
        "Robustez multivariada indicativa",
      
      model_key == "exports_classic" &
        is_primary ~
        "Modelo multivariado principal",
      
      model_key == "exports_expanded" &
        is_primary ~
        "Robustez ampliada principal",
      
      is_sensitivity ~
        "Análisis de sensibilidad",
      
      TRUE ~
        "Complementario"
    )
  ) |>
  dplyr::select(
    model_id,
    model_key,
    model_label_run,
    K,
    rank_trace_10pct,
    rank_trace_5pct,
    rank_eigen_10pct,
    rank_eigen_5pct,
    rank_system_vecm,
    adjustment_estimate,
    adjustment_p_value,
    pt_adjusted_p_value,
    bg_p_value,
    normality_p_value,
    arch_p_value,
    recommended_status,
    final_use
  )


print(
  decision_modelos_finales_tp3,
  n = Inf,
  width = Inf
)


readr::write_csv(
  decision_modelos_finales_tp3,
  here::here(
    "outputs",
    "tables",
    "decision_modelos_finales_tp3.csv"
  )
)


saveRDS(
  decision_modelos_finales_tp3,
  here::here(
    "outputs",
    "models",
    "decision_modelos_finales_tp3.rds"
  )
)




