
# ============================================================
# TP3 - SERIES DE TIEMPO
# SCRIPT 13 FINAL
#
# CONSOLIDACIÓN DE RESULTADOS,
# COMPARACIÓN DE METODOLOGÍAS
# Y TABLAS FINALES PARA EL INFORME
#
# Integra:
#   1. Engle-Granger
#   2. Wickens-Breusch
#   3. Johansen
#   4. ECM y VECM
#   5. IRF estructurales
#   6. Proyecciones locales
#   7. FEVD
#   8. Decisiones econométricas finales
#
# Este script no reestima los modelos.
# Consolida los resultados generados previamente.
# ============================================================


# ------------------------------------------------------------
# 0. LIMPIAR EL ENTORNO
# ------------------------------------------------------------

rm(list = ls())

gc()

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE,
  repos = c(
    CRAN = "https://cloud.r-project.org"
  )
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
  "writexl"
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
  
  install.packages(
    paquetes_faltantes,
    dependencies = TRUE
  )
}


paquetes_no_disponibles <- paquetes[
  !vapply(
    paquetes,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]


if (length(paquetes_no_disponibles) > 0) {
  
  stop(
    paste0(
      "No fue posible instalar o cargar:\n",
      paste(
        paquetes_no_disponibles,
        collapse = ", "
      )
    )
  )
}


cat(
  "\n============================================================",
  "\nPAQUETES DEL SCRIPT 13 DISPONIBLES",
  "\n============================================================\n"
)


# ------------------------------------------------------------
# 2. CARPETAS
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
# 3. FUNCIONES DE LECTURA
# ------------------------------------------------------------

leer_rds_requerido <- function(
    ruta
) {
  
  if (!file.exists(ruta)) {
    
    stop(
      paste0(
        "No se encontró el archivo requerido:\n",
        ruta,
        "\n\nEjecuta primero el script que genera este archivo."
      )
    )
  }
  
  
  readRDS(
    ruta
  )
}


leer_rds_opcional <- function(
    rutas
) {
  
  rutas <- as.character(
    rutas
  )
  
  
  rutas_existentes <- rutas[
    file.exists(
      rutas
    )
  ]
  
  
  if (length(rutas_existentes) == 0) {
    
    return(
      NULL
    )
  }
  
  
  readRDS(
    rutas_existentes[1]
  )
}


primera_ruta_existente <- function(
    rutas
) {
  
  rutas <- as.character(
    rutas
  )
  
  
  rutas_existentes <- rutas[
    file.exists(
      rutas
    )
  ]
  
  
  if (length(rutas_existentes) == 0) {
    
    return(
      NA_character_
    )
  }
  
  
  rutas_existentes[1]
}


# ------------------------------------------------------------
# 4. FUNCIONES PARA BUSCAR COLUMNAS
# ------------------------------------------------------------

buscar_columna <- function(
    tabla,
    candidatas,
    requerida = FALSE,
    etiqueta = "columna"
) {
  
  coincidencias <- candidatas[
    candidatas %in%
      names(tabla)
  ]
  
  
  if (length(coincidencias) == 0) {
    
    if (requerida) {
      
      stop(
        paste0(
          "No se encontró ",
          etiqueta,
          ".\n\n",
          "Columnas buscadas:\n",
          paste(
            candidatas,
            collapse = ", "
          ),
          "\n\nColumnas disponibles:\n",
          paste(
            names(tabla),
            collapse = ", "
          )
        )
      )
    }
    
    
    return(
      NA_character_
    )
  }
  
  
  coincidencias[1]
}


# ------------------------------------------------------------
# 5. FUNCIONES DE NORMALIZACIÓN
# ------------------------------------------------------------

normalizar_model_key <- function(
    x
) {
  
  x_original <- as.character(
    x
  )
  
  
  x_minuscula <- tolower(
    x_original
  )
  
  
  resultado <- x_original
  
  
  indice_importaciones <- grepl(
    pattern = "import",
    x = x_minuscula
  )
  
  
  indice_exportaciones_ampliadas <-
    grepl(
      pattern = "export",
      x = x_minuscula
    ) &
    grepl(
      pattern = "expand|commod|ampli",
      x = x_minuscula
    )
  
  
  indice_exportaciones_clasicas <-
    grepl(
      pattern = "export",
      x = x_minuscula
    ) &
    !indice_exportaciones_ampliadas
  
  
  resultado[
    indice_importaciones
  ] <- "imports"
  
  
  resultado[
    indice_exportaciones_ampliadas
  ] <- "exports_expanded"
  
  
  resultado[
    indice_exportaciones_clasicas
  ] <- "exports_classic"
  
  
  resultado
}


normalizar_variable <- function(
    x
) {
  
  x_original <- as.character(
    x
  )
  
  
  x_minuscula <- tolower(
    x_original
  )
  
  
  resultado <- x_original
  
  
  resultado[
    grepl(
      "ln_pib_real|pib_real|pib argentino|income.*arg",
      x_minuscula
    )
  ] <- "ln_pib_real"
  
  
  resultado[
    grepl(
      "ln_pib_socios|pib_socios|pib socios|income.*partner",
      x_minuscula
    )
  ] <- "ln_pib_socios"
  
  
  resultado[
    grepl(
      "ln_itcrm|itcrm|tipo.*cambio|exchange",
      x_minuscula
    )
  ] <- "ln_itcrm"
  
  
  resultado[
    grepl(
      "commodity",
      x_minuscula
    )
  ] <- "commodity_c"
  
  
  resultado
}


etiqueta_modelo <- function(
    model_key
) {
  
  dplyr::case_when(
    
    model_key ==
      "imports" ~
      "Importaciones",
    
    model_key ==
      "exports_classic" ~
      "Exportaciones clásicas",
    
    model_key ==
      "exports_expanded" ~
      "Exportaciones ampliadas",
    
    TRUE ~
      as.character(
        model_key
      )
  )
}


etiqueta_variable <- function(
    variable
) {
  
  dplyr::case_when(
    
    variable ==
      "ln_pib_real" ~
      "PIB argentino",
    
    variable ==
      "ln_pib_socios" ~
      "PIB de socios",
    
    variable ==
      "ln_itcrm" ~
      "ITCRM",
    
    variable ==
      "commodity_c" ~
      "Commodities",
    
    variable ==
      "ln_importaciones_reales" ~
      "Importaciones reales",
    
    variable ==
      "ln_exportaciones_reales" ~
      "Exportaciones reales",
    
    TRUE ~
      as.character(
        variable
      )
  )
}


signo_esperado <- function(
    model_key,
    variable
) {
  
  dplyr::case_when(
    
    variable %in%
      c(
        "ln_pib_real",
        "ln_pib_socios"
      ) ~
      "Positivo",
    
    model_key ==
      "imports" &
      variable ==
      "ln_itcrm" ~
      "Negativo",
    
    model_key %in%
      c(
        "exports_classic",
        "exports_expanded"
      ) &
      variable ==
      "ln_itcrm" ~
      "Positivo",
    
    TRUE ~
      "No definido"
  )
}


# ------------------------------------------------------------
# 6. FUNCIÓN PARA APLANAR TABLAS PARA EXCEL
# ------------------------------------------------------------

aplanar_para_excel <- function(
    tabla
) {
  
  tabla <- as.data.frame(
    tabla,
    stringsAsFactors = FALSE
  )
  
  
  for (
    nombre_columna in names(
      tabla
    )
  ) {
    
    if (
      is.list(
        tabla[[
          nombre_columna
        ]]
      )
    ) {
      
      tabla[[
        nombre_columna
      ]] <- vapply(
        
        tabla[[
          nombre_columna
        ]],
        
        function(x) {
          
          paste(
            x,
            collapse = " | "
          )
        },
        
        FUN.VALUE =
          character(1)
      )
    }
  }
  
  
  tabla
}


# ------------------------------------------------------------
# 7. RUTAS DE ARCHIVOS REQUERIDOS
# ------------------------------------------------------------

ruta_comparacion_lr <- here::here(
  "outputs",
  "models",
  "comparacion_elasticidades_largo_plazo.rds"
)


ruta_elasticidades_johansen <- here::here(
  "outputs",
  "models",
  "elasticidades_largo_plazo_johansen.rds"
)


ruta_resumen_johansen <- here::here(
  "outputs",
  "models",
  "resumen_johansen_vecm.rds"
)


ruta_resumen_irf <- here::here(
  "outputs",
  "models",
  "resumen_respuestas_irf.rds"
)


ruta_fevd <- here::here(
  "outputs",
  "models",
  "fevd_svar.rds"
)


ruta_decision_irf <- here::here(
  "outputs",
  "models",
  "decision_irf_final.rds"
)


# ------------------------------------------------------------
# 8. RUTAS DE ARCHIVOS OPCIONALES
# ------------------------------------------------------------

ruta_corto_plazo_vecm <- here::here(
  "outputs",
  "models",
  "elasticidades_corto_plazo_vecm.rds"
)


ruta_comparacion_svar_lp <- here::here(
  "outputs",
  "models",
  "comparacion_svar_lpirfs.rds"
)


ruta_sensibilidad_orden <- here::here(
  "outputs",
  "models",
  "sensibilidad_orden_irf.rds"
)


rutas_decision_eg <- c(
  
  here::here(
    "outputs",
    "models",
    "decision_engle_granger_integral.rds"
  ),
  
  here::here(
    "outputs",
    "models",
    "decision_cointegracion_final.rds"
  )
)


# ------------------------------------------------------------
# 9. CARGAR ARCHIVOS REQUERIDOS
# ------------------------------------------------------------

comparacion_lr_raw <- leer_rds_requerido(
  ruta_comparacion_lr
)


elasticidades_johansen_raw <- leer_rds_requerido(
  ruta_elasticidades_johansen
)


resumen_johansen_raw <- leer_rds_requerido(
  ruta_resumen_johansen
)


resumen_irf_raw <- leer_rds_requerido(
  ruta_resumen_irf
)


fevd_raw <- leer_rds_requerido(
  ruta_fevd
)


decision_irf_raw <- leer_rds_requerido(
  ruta_decision_irf
)


# ------------------------------------------------------------
# 10. CARGAR ARCHIVOS OPCIONALES
# ------------------------------------------------------------

corto_plazo_vecm_raw <- leer_rds_opcional(
  ruta_corto_plazo_vecm
)


comparacion_svar_lp_raw <- leer_rds_opcional(
  ruta_comparacion_svar_lp
)


sensibilidad_orden_raw <- leer_rds_opcional(
  ruta_sensibilidad_orden
)


decision_eg_raw <- leer_rds_opcional(
  rutas_decision_eg
)


# ------------------------------------------------------------
# 11. CREAR TABLA ECM CORRECTA
# ------------------------------------------------------------

# Los tres coeficientes corresponden a los ECM previamente
# estimados. Se incluyen con fines indicativos porque las
# especificaciones principales Engle-Granger no rechazaron
# ausencia de cointegración al 10%.

ajuste_ecm_correcto <- tibble::tibble(
  
  model_id = c(
    "imports__ecm",
    "exports_classic__ecm",
    "exports_expanded__ecm"
  ),
  
  model_key = c(
    "imports",
    "exports_classic",
    "exports_expanded"
  ),
  
  model_label = c(
    "Importaciones: PIB argentino e ITCRM",
    "Exportaciones: PIB socios e ITCRM",
    "Exportaciones ampliadas: PIB socios, ITCRM y commodities"
  ),
  
  method_code =
    "ecm",
  
  method =
    "ECM",
  
  adjustment_estimate = c(
    -0.305,
    -0.395,
    -0.427
  ),
  
  adjustment_std_error = c(
    0.0781,
    0.0934,
    0.0980
  ),
  
  adjustment_statistic = c(
    -3.90,
    -4.22,
    -4.35
  ),
  
  adjustment_p_value = c(
    0.000207,
    0.0000658,
    0.0000418
  ),
  
  adjustment_percent = c(
    30.5,
    39.5,
    42.7
  ),
  
  cointegration_10pct = c(
    FALSE,
    FALSE,
    FALSE
  ),
  
  significant_10pct = c(
    TRUE,
    TRUE,
    TRUE
  ),
  
  is_primary = c(
    TRUE,
    TRUE,
    TRUE
  ),
  
  validity = c(
    "Coeficiente ECM estimado con fines indicativos",
    "Coeficiente ECM estimado con fines indicativos",
    "Coeficiente ECM estimado con fines indicativos"
  ),
  
  interpretation = c(
    paste0(
      "Ajuste negativo y significativo; se corrige 30.5% ",
      "del desequilibrio por trimestre. Interpretación indicativa."
    ),
    paste0(
      "Ajuste negativo y significativo; se corrige 39.5% ",
      "del desequilibrio por trimestre. Interpretación indicativa."
    ),
    paste0(
      "Ajuste negativo y significativo; se corrige 42.7% ",
      "del desequilibrio por trimestre. Interpretación indicativa."
    )
  )
)


ruta_ajuste_ecm_correcto <- here::here(
  "outputs",
  "models",
  "ajuste_ecm_correcto.rds"
)


saveRDS(
  ajuste_ecm_correcto,
  ruta_ajuste_ecm_correcto
)


# ------------------------------------------------------------
# 12. CATÁLOGO DE FUENTES
# ------------------------------------------------------------

catalogo_fuentes <- tibble::tibble(
  
  fuente = c(
    "Comparación de largo plazo",
    "Elasticidades Johansen",
    "Resumen Johansen",
    "Resumen IRF",
    "FEVD",
    "Decisión IRF",
    "Coeficientes ECM correctos",
    "Corto plazo VECM",
    "Comparación SVAR-LP",
    "Sensibilidad al orden",
    "Decisión Engle-Granger"
  ),
  
  ruta = c(
    ruta_comparacion_lr,
    ruta_elasticidades_johansen,
    ruta_resumen_johansen,
    ruta_resumen_irf,
    ruta_fevd,
    ruta_decision_irf,
    ruta_ajuste_ecm_correcto,
    ruta_corto_plazo_vecm,
    ruta_comparacion_svar_lp,
    ruta_sensibilidad_orden,
    primera_ruta_existente(
      rutas_decision_eg
    )
  )
) |>
  dplyr::mutate(
    
    disponible =
      !is.na(ruta) &
      file.exists(ruta),
    
    estado =
      dplyr::if_else(
        disponible,
        "Disponible",
        "No disponible"
      )
  )


print(
  catalogo_fuentes,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 13. NORMALIZAR ENGLE-GRANGER Y WICKENS-BREUSCH
# ------------------------------------------------------------

normalizar_comparacion_lr <- function(
    tabla
) {
  
  tabla <- tibble::as_tibble(
    tabla
  )
  
  
  columna_modelo <- buscar_columna(
    
    tabla,
    
    c(
      "model_key",
      "modelo",
      "system",
      "trade_flow",
      "model_label"
    ),
    
    requerida = TRUE,
    
    etiqueta =
      "la identificación del modelo"
  )
  
  
  columna_variable <- buscar_columna(
    
    tabla,
    
    c(
      "variable",
      "regressor",
      "term",
      "explanatory_variable"
    ),
    
    requerida = TRUE,
    
    etiqueta =
      "la variable explicativa"
  )
  
  
  columna_eg <- buscar_columna(
    
    tabla,
    
    c(
      "estimate_eg",
      "eg_estimate",
      "engle_granger_estimate",
      "elasticity_eg"
    ),
    
    requerida = TRUE,
    
    etiqueta =
      "la estimación Engle-Granger"
  )
  
  
  columna_wb <- buscar_columna(
    
    tabla,
    
    c(
      "estimate_wb",
      "wb_estimate",
      "wickens_breusch_estimate",
      "elasticity_wb"
    ),
    
    requerida = TRUE,
    
    etiqueta =
      "la estimación Wickens-Breusch"
  )
  
  
  columna_p_eg <- buscar_columna(
    
    tabla,
    
    c(
      "p_value_eg",
      "eg_p_value",
      "pvalue_eg"
    ),
    
    requerida = FALSE
  )
  
  
  columna_p_wb <- buscar_columna(
    
    tabla,
    
    c(
      "p_value_wb",
      "wb_p_value",
      "pvalue_wb"
    ),
    
    requerida = FALSE
  )
  
  
  columna_cointegracion <- buscar_columna(
    
    tabla,
    
    c(
      "cointegration_10pct",
      "cointegrated_10pct"
    ),
    
    requerida = FALSE
  )
  
  
  columna_ecm_status <- buscar_columna(
    
    tabla,
    
    c(
      "ecm_status",
      "adjustment_validity"
    ),
    
    requerida = FALSE
  )
  
  
  p_eg <- if (
    is.na(
      columna_p_eg
    )
  ) {
    
    rep(
      NA_real_,
      nrow(tabla)
    )
    
  } else {
    
    as.numeric(
      tabla[[
        columna_p_eg
      ]]
    )
  }
  
  
  p_wb <- if (
    is.na(
      columna_p_wb
    )
  ) {
    
    rep(
      NA_real_,
      nrow(tabla)
    )
    
  } else {
    
    as.numeric(
      tabla[[
        columna_p_wb
      ]]
    )
  }
  
  
  cointegracion <- if (
    is.na(
      columna_cointegracion
    )
  ) {
    
    rep(
      NA,
      nrow(tabla)
    )
    
  } else {
    
    as.logical(
      tabla[[
        columna_cointegracion
      ]]
    )
  }
  
  
  ecm_status <- if (
    is.na(
      columna_ecm_status
    )
  ) {
    
    rep(
      "Resultado uniecuacional.",
      nrow(tabla)
    )
    
  } else {
    
    as.character(
      tabla[[
        columna_ecm_status
      ]]
    )
  }
  
  
  base <- tibble::tibble(
    
    model_key =
      normalizar_model_key(
        tabla[[
          columna_modelo
        ]]
      ),
    
    variable =
      normalizar_variable(
        tabla[[
          columna_variable
        ]]
      ),
    
    cointegration_10pct =
      cointegracion,
    
    ecm_status =
      ecm_status
  )
  
  
  tabla_eg <- base |>
    dplyr::mutate(
      
      model_id =
        paste0(
          model_key,
          "__engle_granger"
        ),
      
      method_code =
        "engle_granger",
      
      method =
        "Engle-Granger",
      
      estimate =
        as.numeric(
          tabla[[
            columna_eg
          ]]
        ),
      
      p_value =
        p_eg,
      
      specification =
        "Principal",
      
      is_primary =
        TRUE,
      
      rank_trace_10pct =
        NA_integer_,
      
      validity =
        dplyr::if_else(
          cointegration_10pct %in% TRUE,
          "Resultado Engle-Granger con evidencia de cointegración.",
          paste0(
            "Resultado Engle-Granger; elasticidad reportada ",
            "con interpretación indicativa."
          )
        )
    )
  
  
  tabla_wb <- base |>
    dplyr::mutate(
      
      model_id =
        paste0(
          model_key,
          "__wickens_breusch"
        ),
      
      method_code =
        "wickens_breusch",
      
      method =
        "Wickens-Breusch",
      
      estimate =
        as.numeric(
          tabla[[
            columna_wb
          ]]
        ),
      
      p_value =
        p_wb,
      
      specification =
        "Principal",
      
      is_primary =
        TRUE,
      
      rank_trace_10pct =
        NA_integer_,
      
      validity =
        paste0(
          "Resultado de la reparametrización ",
          "Wickens-Breusch."
        )
    )
  
  
  dplyr::bind_rows(
    tabla_eg,
    tabla_wb
  )
}


elasticidades_lr_previas <- normalizar_comparacion_lr(
  comparacion_lr_raw
)


# ------------------------------------------------------------
# 14. NORMALIZAR ELASTICIDADES JOHANSEN
# ------------------------------------------------------------

elasticidades_johansen <- tibble::as_tibble(
  elasticidades_johansen_raw
)


columnas_johansen_requeridas <- c(
  "model_id",
  "model_key",
  "variable",
  "long_run_elasticity"
)


faltantes_johansen <- setdiff(
  columnas_johansen_requeridas,
  names(
    elasticidades_johansen
  )
)


if (length(faltantes_johansen) > 0) {
  
  stop(
    paste0(
      "Faltan columnas en elasticidades Johansen:\n",
      paste(
        faltantes_johansen,
        collapse = ", "
      )
    )
  )
}


rank_trace_vector <- if (
  "rank_trace_10pct" %in%
  names(
    elasticidades_johansen
  )
) {
  
  as.integer(
    elasticidades_johansen[[
      "rank_trace_10pct"
    ]]
  )
  
} else {
  
  rep(
    NA_integer_,
    nrow(
      elasticidades_johansen
    )
  )
}


validity_vector <- if (
  "validity" %in%
  names(
    elasticidades_johansen
  )
) {
  
  as.character(
    elasticidades_johansen[[
      "validity"
    ]]
  )
  
} else {
  
  rep(
    "Resultado Johansen.",
    nrow(
      elasticidades_johansen
    )
  )
}


elasticidades_lr_johansen <- elasticidades_johansen |>
  dplyr::transmute(
    
    model_id,
    
    model_key =
      normalizar_model_key(
        model_key
      ),
    
    variable =
      normalizar_variable(
        variable
      ),
    
    cointegration_10pct =
      rank_trace_vector >=
      1L,
    
    ecm_status =
      paste0(
        "VECM estimado con rango ",
        rank_trace_vector,
        "."
      ),
    
    method_code =
      "johansen",
    
    method =
      "Johansen",
    
    estimate =
      as.numeric(
        long_run_elasticity
      ),
    
    p_value =
      NA_real_,
    
    specification =
      dplyr::if_else(
        grepl(
          "sensitivity",
          model_id
        ),
        "Sensibilidad K=4",
        "Principal"
      ),
    
    is_primary =
      grepl(
        "__primary_",
        model_id
      ),
    
    rank_trace_10pct =
      rank_trace_vector,
    
    validity =
      validity_vector
  )


# ------------------------------------------------------------
# 15. TABLA INTEGRAL DE ELASTICIDADES
# ------------------------------------------------------------

elasticidades_largo_plazo <- dplyr::bind_rows(
  
  elasticidades_lr_previas,
  
  elasticidades_lr_johansen
) |>
  dplyr::mutate(
    
    model_label =
      etiqueta_modelo(
        model_key
      ),
    
    variable_label =
      etiqueta_variable(
        variable
      ),
    
    expected_sign =
      signo_esperado(
        model_key,
        variable
      ),
    
    observed_sign =
      dplyr::case_when(
        
        is.na(estimate) ~
          "No disponible",
        
        estimate > 0 ~
          "Positivo",
        
        estimate < 0 ~
          "Negativo",
        
        TRUE ~
          "Cero"
      ),
    
    sign_consistent =
      dplyr::case_when(
        
        expected_sign ==
          "No definido" ~
          NA,
        
        observed_sign ==
          "No disponible" ~
          NA,
        
        TRUE ~
          expected_sign ==
          observed_sign
      )
  ) |>
  dplyr::arrange(
    model_key,
    variable,
    method_code,
    specification
  )


# ------------------------------------------------------------
# 16. COMPARACIÓN ANCHA ENTRE MÉTODOS
# ------------------------------------------------------------

comparacion_elasticidades_metodos <- elasticidades_largo_plazo |>
  dplyr::filter(
    is_primary
  ) |>
  dplyr::group_by(
    model_key,
    model_label,
    variable,
    variable_label,
    method_code
  ) |>
  dplyr::summarise(
    
    estimate =
      dplyr::first(
        estimate
      ),
    
    .groups =
      "drop"
  ) |>
  tidyr::pivot_wider(
    
    names_from =
      method_code,
    
    values_from =
      estimate
  )


if (
  !"engle_granger" %in%
  names(
    comparacion_elasticidades_metodos
  )
) {
  
  comparacion_elasticidades_metodos$engle_granger <-
    NA_real_
}


if (
  !"wickens_breusch" %in%
  names(
    comparacion_elasticidades_metodos
  )
) {
  
  comparacion_elasticidades_metodos$wickens_breusch <-
    NA_real_
}


if (
  !"johansen" %in%
  names(
    comparacion_elasticidades_metodos
  )
) {
  
  comparacion_elasticidades_metodos$johansen <-
    NA_real_
}


comparacion_elasticidades_metodos <-
  comparacion_elasticidades_metodos |>
  dplyr::mutate(
    
    difference_johansen_eg =
      johansen -
      engle_granger,
    
    difference_johansen_wb =
      johansen -
      wickens_breusch,
    
    difference_wb_eg =
      wickens_breusch -
      engle_granger
  )


# ------------------------------------------------------------
# 17. CONSISTENCIA ENTRE MÉTODOS
# ------------------------------------------------------------

resumen_consistencia_metodos <- elasticidades_largo_plazo |>
  dplyr::filter(
    is_primary,
    !is.na(
      estimate
    )
  ) |>
  dplyr::group_by(
    model_key,
    model_label,
    variable,
    variable_label,
    expected_sign
  ) |>
  dplyr::summarise(
    
    number_of_methods =
      dplyr::n_distinct(
        method_code
      ),
    
    minimum_estimate =
      min(
        estimate,
        na.rm = TRUE
      ),
    
    maximum_estimate =
      max(
        estimate,
        na.rm = TRUE
      ),
    
    mean_estimate =
      mean(
        estimate,
        na.rm = TRUE
      ),
    
    estimate_range =
      maximum_estimate -
      minimum_estimate,
    
    all_same_sign =
      dplyr::n_distinct(
        sign(
          estimate
        )
      ) ==
      1,
    
    all_expected_sign =
      all(
        sign_consistent,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  )


# ------------------------------------------------------------
# 18. ESTIMACIONES PREFERIDAS
# ------------------------------------------------------------

estimaciones_preferidas <- elasticidades_largo_plazo |>
  dplyr::filter(
    
    is_primary,
    
    (
      model_key ==
        "imports" &
        method_code %in%
        c(
          "engle_granger",
          "wickens_breusch"
        )
    ) |
      
      (
        model_key ==
          "exports_classic" &
          method_code ==
          "johansen"
      ) |
      
      (
        model_key ==
          "exports_expanded" &
          method_code ==
          "johansen"
      )
  ) |>
  dplyr::mutate(
    
    final_role =
      dplyr::case_when(
        
        model_key ==
          "imports" ~
          paste0(
            "Estimación principal uniecuacional de importaciones; ",
            "Johansen se utiliza como evidencia complementaria."
          ),
        
        model_key ==
          "exports_classic" ~
          "Estimación multivariada principal de exportaciones.",
        
        model_key ==
          "exports_expanded" ~
          paste0(
            "Robustez con commodities como variable ",
            "exógena estacionaria."
          ),
        
        TRUE ~
          "Resultado complementario."
      )
  )


# ------------------------------------------------------------
# 19. EXTRAER COEFICIENTES VECM
# ------------------------------------------------------------

resumen_johansen <- tibble::as_tibble(
  resumen_johansen_raw
)


columnas_resumen_requeridas <- c(
  "model_id",
  "model_key",
  "adjustment_estimate",
  "adjustment_p_value"
)


faltantes_resumen <- setdiff(
  columnas_resumen_requeridas,
  names(
    resumen_johansen
  )
)


if (length(faltantes_resumen) > 0) {
  
  stop(
    paste0(
      "Faltan columnas en resumen_johansen_vecm.rds:\n",
      paste(
        faltantes_resumen,
        collapse = ", "
      )
    )
  )
}


model_label_vecm <- if (
  "model_label_run" %in%
  names(
    resumen_johansen
  )
) {
  
  as.character(
    resumen_johansen[[
      "model_label_run"
    ]]
  )
  
} else {
  
  etiqueta_modelo(
    normalizar_model_key(
      resumen_johansen[[
        "model_key"
      ]]
    )
  )
}


validity_vecm <- if (
  "inference_status" %in%
  names(
    resumen_johansen
  )
) {
  
  as.character(
    resumen_johansen[[
      "inference_status"
    ]]
  )
  
} else {
  
  rep(
    "Resultado VECM.",
    nrow(
      resumen_johansen
    )
  )
}


cointegracion_vecm <- if (
  "rank_system_vecm" %in%
  names(
    resumen_johansen
  )
) {
  
  as.integer(
    resumen_johansen[[
      "rank_system_vecm"
    ]]
  ) >=
    1L
  
} else {
  
  rep(
    TRUE,
    nrow(
      resumen_johansen
    )
  )
}


ajuste_vecm <- resumen_johansen |>
  dplyr::transmute(
    
    model_id,
    
    model_key =
      normalizar_model_key(
        model_key
      ),
    
    model_label =
      model_label_vecm,
    
    method_code =
      "vecm",
    
    method =
      "VECM",
    
    adjustment_estimate =
      as.numeric(
        adjustment_estimate
      ),
    
    adjustment_std_error =
      NA_real_,
    
    adjustment_statistic =
      NA_real_,
    
    adjustment_p_value =
      as.numeric(
        adjustment_p_value
      ),
    
    adjustment_percent =
      -100 *
      as.numeric(
        adjustment_estimate
      ),
    
    cointegration_10pct =
      cointegracion_vecm,
    
    significant_10pct =
      !is.na(
        adjustment_p_value
      ) &
      adjustment_p_value <
      0.10,
    
    is_primary =
      grepl(
        "__primary_",
        model_id
      ),
    
    validity =
      validity_vecm,
    
    interpretation =
      dplyr::case_when(
        
        adjustment_estimate < 0 &
          !is.na(
            adjustment_p_value
          ) &
          adjustment_p_value <
          0.10 ~
          paste0(
            "Ajuste negativo y significativo; se corrige ",
            round(
              -100 *
                adjustment_estimate,
              1
            ),
            "% del desequilibrio por trimestre."
          ),
        
        adjustment_estimate < 0 ~
          "Ajuste negativo, pero no significativo al 10%.",
        
        TRUE ~
          paste0(
            "El signo del ajuste no es compatible ",
            "con convergencia."
          )
      )
  )


# ------------------------------------------------------------
# 20. CONSOLIDAR ECM Y VECM
# ------------------------------------------------------------

ajustes_ecm_vecm <- dplyr::bind_rows(
  
  ajuste_ecm_correcto,
  
  ajuste_vecm
) |>
  dplyr::mutate(
    
    model_label_short =
      etiqueta_modelo(
        model_key
      )
  ) |>
  dplyr::arrange(
    model_key,
    method_code,
    model_id
  )


# ------------------------------------------------------------
# 21. RESUMEN ENGLE-GRANGER
# ------------------------------------------------------------

if (
  !is.null(
    decision_eg_raw
  ) &&
  is.data.frame(
    decision_eg_raw
  )
) {
  
  cointegracion_eg_resumen <- tibble::as_tibble(
    decision_eg_raw
  )
  
  
  columna_modelo_eg <- buscar_columna(
    
    cointegracion_eg_resumen,
    
    c(
      "model_key",
      "model_label",
      "modelo"
    ),
    
    requerida = FALSE
  )
  
  
  if (!is.na(
    columna_modelo_eg
  )) {
    
    cointegracion_eg_resumen <-
      cointegracion_eg_resumen |>
      dplyr::mutate(
        
        model_key =
          normalizar_model_key(
            .data[[
              columna_modelo_eg
            ]]
          ),
        
        model_label_short =
          etiqueta_modelo(
            model_key
          )
      )
  }
  
} else {
  
  columna_modelo_lr <- buscar_columna(
    
    comparacion_lr_raw,
    
    c(
      "model_key",
      "model_label",
      "modelo"
    ),
    
    requerida = TRUE,
    
    etiqueta =
      "el modelo en la comparación de largo plazo"
  )
  
  
  columna_cointegracion_lr <- buscar_columna(
    
    comparacion_lr_raw,
    
    c(
      "cointegration_10pct",
      "cointegrated_10pct"
    ),
    
    requerida = FALSE
  )
  
  
  columna_ecm_status_lr <- buscar_columna(
    
    comparacion_lr_raw,
    
    c(
      "ecm_status"
    ),
    
    requerida = FALSE
  )
  
  
  cointegracion_lr_vector <- if (
    is.na(
      columna_cointegracion_lr
    )
  ) {
    
    rep(
      NA,
      nrow(
        comparacion_lr_raw
      )
    )
    
  } else {
    
    as.logical(
      comparacion_lr_raw[[
        columna_cointegracion_lr
      ]]
    )
  }
  
  
  ecm_status_lr_vector <- if (
    is.na(
      columna_ecm_status_lr
    )
  ) {
    
    rep(
      "No disponible",
      nrow(
        comparacion_lr_raw
      )
    )
    
  } else {
    
    as.character(
      comparacion_lr_raw[[
        columna_ecm_status_lr
      ]]
    )
  }
  
  
  cointegracion_eg_resumen <- tibble::tibble(
    
    model_key =
      normalizar_model_key(
        comparacion_lr_raw[[
          columna_modelo_lr
        ]]
      ),
    
    model_label =
      as.character(
        comparacion_lr_raw[[
          columna_modelo_lr
        ]]
      ),
    
    cointegration_10pct =
      cointegracion_lr_vector,
    
    ecm_status =
      ecm_status_lr_vector
  ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      
      model_label_short =
        etiqueta_modelo(
          model_key
        )
    )
}


# ------------------------------------------------------------
# 22. RESUMEN JOHANSEN
# ------------------------------------------------------------

columnas_decision_johansen <- intersect(
  
  c(
    "model_id",
    "model_key",
    "model_label_run",
    "K",
    "rank_trace_10pct",
    "rank_trace_5pct",
    "rank_eigen_10pct",
    "rank_eigen_5pct",
    "rank_system_vecm",
    "adjustment_estimate",
    "adjustment_p_value",
    "vecm_pt_p_value",
    "vecm_bg_p_value",
    "vecm_normality_p_value",
    "vecm_arch_p_value",
    "recommended_multivariate_model",
    "inference_status"
  ),
  
  names(
    resumen_johansen
  )
)


cointegracion_johansen_resumen <- resumen_johansen |>
  dplyr::select(
    dplyr::all_of(
      columnas_decision_johansen
    )
  ) |>
  dplyr::mutate(
    
    model_key =
      normalizar_model_key(
        model_key
      ),
    
    is_primary =
      grepl(
        "__primary_",
        model_id
      )
  )


# ------------------------------------------------------------
# 23. PREPARAR DECISIÓN IRF
# ------------------------------------------------------------

decision_irf <- tibble::as_tibble(
  decision_irf_raw
)


columnas_irf_necesarias <- c(
  "model_id",
  "primary_order",
  "alternative_order",
  "report_use",
  "recommended_model"
)


for (
  nombre_columna in columnas_irf_necesarias
) {
  
  if (
    !nombre_columna %in%
    names(
      decision_irf
    )
  ) {
    
    if (
      nombre_columna ==
      "recommended_model"
    ) {
      
      decision_irf[[
        nombre_columna
      ]] <- TRUE
      
    } else {
      
      decision_irf[[
        nombre_columna
      ]] <- NA_character_
    }
  }
}


decision_irf_slim <- decision_irf |>
  dplyr::select(
    model_id,
    primary_order,
    alternative_order,
    report_use,
    recommended_model
  )


# ------------------------------------------------------------
# 24. DECISIÓN ECONOMÉTRICA FINAL
# ------------------------------------------------------------

decision_final_modelos <- cointegracion_johansen_resumen |>
  dplyr::left_join(
    
    decision_irf_slim,
    
    by =
      "model_id"
  ) |>
  dplyr::mutate(
    
    model_label =
      etiqueta_modelo(
        model_key
      ),
    
    final_role =
      dplyr::case_when(
        
        model_key ==
          "imports" &
          is_primary ~
          paste0(
            "Robustez multivariada indicativa. ",
            "Las elasticidades principales de importaciones ",
            "se apoyan en Engle-Granger y Wickens-Breusch."
          ),
        
        model_key ==
          "exports_classic" &
          is_primary ~
          "Modelo multivariado principal de exportaciones.",
        
        model_key ==
          "exports_expanded" &
          is_primary ~
          paste0(
            "Robustez principal con commodities ",
            "como control exógeno I(0)."
          ),
        
        grepl(
          "sensitivity",
          model_id
        ) ~
          paste0(
            "Análisis de sensibilidad. No utilizar ",
            "como especificación central."
          ),
        
        TRUE ~
          "Resultado complementario."
      ),
    
    recommended_final =
      is_primary &
      !grepl(
        "sensitivity",
        model_id
      )
  ) |>
  dplyr::arrange(
    model_key,
    dplyr::desc(
      is_primary
    )
  )


# ------------------------------------------------------------
# 25. IRF PRINCIPALES
# ------------------------------------------------------------

resumen_irf <- tibble::as_tibble(
  resumen_irf_raw
)


columnas_irf_requeridas <- c(
  "model_id",
  "model_key",
  "model_short",
  "method_code",
  "impulse",
  "impulse_label",
  "response_h4",
  "response_h8",
  "response_h12",
  "is_primary"
)


faltantes_irf <- setdiff(
  columnas_irf_requeridas,
  names(
    resumen_irf
  )
)


if (length(faltantes_irf) > 0) {
  
  stop(
    paste0(
      "Faltan columnas en resumen_respuestas_irf.rds:\n",
      paste(
        faltantes_irf,
        collapse = ", "
      )
    )
  )
}


irf_principales <- resumen_irf |>
  dplyr::filter(
    method_code ==
      "chol_primary",
    is_primary
  ) |>
  dplyr::select(
    
    dplyr::any_of(
      c(
        "model_id",
        "model_key",
        "model_short",
        "impulse",
        "impulse_label",
        "response",
        "response_label",
        "impact_response",
        "response_h1",
        "response_h4",
        "response_h8",
        "response_h12",
        "maximum_absolute_response",
        "peak_horizon",
        "expected_sign",
        "expected_sign_h4"
      )
    )
  ) |>
  dplyr::arrange(
    model_key,
    impulse
  )


columnas_horizonte_disponibles <- intersect(
  
  c(
    "impact_response",
    "response_h4",
    "response_h8",
    "response_h12"
  ),
  
  names(
    irf_principales
  )
)


irf_horizontes <- irf_principales |>
  tidyr::pivot_longer(
    
    cols =
      dplyr::all_of(
        columnas_horizonte_disponibles
      ),
    
    names_to =
      "horizon_name",
    
    values_to =
      "response_percent"
  ) |>
  dplyr::mutate(
    
    horizon =
      dplyr::case_when(
        
        horizon_name ==
          "impact_response" ~
          0L,
        
        horizon_name ==
          "response_h4" ~
          4L,
        
        horizon_name ==
          "response_h8" ~
          8L,
        
        horizon_name ==
          "response_h12" ~
          12L,
        
        TRUE ~
          NA_integer_
      )
  )


# ------------------------------------------------------------
# 26. FEVD A 12 TRIMESTRES
# ------------------------------------------------------------

fevd <- tibble::as_tibble(
  fevd_raw
)


columnas_fevd_requeridas <- c(
  "model_id",
  "model_key",
  "method_code",
  "response",
  "impulse",
  "horizon",
  "fevd_percent"
)


faltantes_fevd <- setdiff(
  columnas_fevd_requeridas,
  names(
    fevd
  )
)


if (length(faltantes_fevd) > 0) {
  
  stop(
    paste0(
      "Faltan columnas en fevd_svar.rds:\n",
      paste(
        faltantes_fevd,
        collapse = ", "
      )
    )
  )
}


impulse_label_fevd <- if (
  "impulse_label" %in%
  names(
    fevd
  )
) {
  
  as.character(
    fevd[[
      "impulse_label"
    ]]
  )
  
} else {
  
  etiqueta_variable(
    fevd[[
      "impulse"
    ]]
  )
}


response_label_fevd <- if (
  "response_label" %in%
  names(
    fevd
  )
) {
  
  as.character(
    fevd[[
      "response_label"
    ]]
  )
  
} else {
  
  etiqueta_variable(
    fevd[[
      "response"
    ]]
  )
}


model_short_fevd <- if (
  "model_short" %in%
  names(
    fevd
  )
) {
  
  as.character(
    fevd[[
      "model_short"
    ]]
  )
  
} else {
  
  etiqueta_modelo(
    fevd[[
      "model_key"
    ]]
  )
}


fevd_preparada <- fevd |>
  dplyr::mutate(
    
    impulse_label =
      impulse_label_fevd,
    
    response_label =
      response_label_fevd,
    
    model_short =
      model_short_fevd,
    
    trade_variable =
      dplyr::case_when(
        
        model_key ==
          "imports" ~
          "ln_importaciones_reales",
        
        model_key %in%
          c(
            "exports_classic",
            "exports_expanded"
          ) ~
          "ln_exportaciones_reales",
        
        TRUE ~
          NA_character_
      )
  )


fevd_h12 <- fevd_preparada |>
  dplyr::left_join(
    
    decision_irf |>
      dplyr::select(
        model_id,
        recommended_model
      ),
    
    by =
      "model_id"
  ) |>
  dplyr::filter(
    
    method_code ==
      "chol_primary",
    
    horizon ==
      12L,
    
    response ==
      trade_variable,
    
    recommended_model
  ) |>
  dplyr::select(
    model_id,
    model_key,
    model_short,
    response,
    response_label,
    impulse,
    impulse_label,
    horizon,
    fevd_percent
  ) |>
  dplyr::arrange(
    model_key,
    dplyr::desc(
      fevd_percent
    )
  )


control_fevd <- fevd_h12 |>
  dplyr::group_by(
    model_id,
    model_short
  ) |>
  dplyr::summarise(
    
    total_fevd =
      sum(
        fevd_percent,
        na.rm = TRUE
      ),
    
    difference_from_100 =
      total_fevd -
      100,
    
    valid_sum =
      abs(
        difference_from_100
      ) <
      0.10,
    
    .groups =
      "drop"
  )


# ------------------------------------------------------------
# 27. RESULTADOS OPCIONALES
# ------------------------------------------------------------

comparacion_svar_lp <- if (
  !is.null(
    comparacion_svar_lp_raw
  ) &&
  is.data.frame(
    comparacion_svar_lp_raw
  )
) {
  
  tibble::as_tibble(
    comparacion_svar_lp_raw
  )
  
} else {
  
  tibble::tibble()
}


sensibilidad_orden <- if (
  !is.null(
    sensibilidad_orden_raw
  ) &&
  is.data.frame(
    sensibilidad_orden_raw
  )
) {
  
  tibble::as_tibble(
    sensibilidad_orden_raw
  )
  
} else {
  
  tibble::tibble()
}


corto_plazo_vecm <- if (
  !is.null(
    corto_plazo_vecm_raw
  ) &&
  is.data.frame(
    corto_plazo_vecm_raw
  )
) {
  
  tibble::as_tibble(
    corto_plazo_vecm_raw
  )
  
} else {
  
  tibble::tibble()
}


# ------------------------------------------------------------
# 28. PLANTILLA DE LITERATURA
# ------------------------------------------------------------

literatura_referencias <- tibble::tibble(
  
  reference_id = c(
    "berrettoni_castresana",
    "bus_nicolini_llosa",
    "zack_sotelsek",
    "song_witt_li"
  ),
  
  reference_short = c(
    "Berrettoni y Castresana",
    "Bus y Nicolini-Llosa",
    "Zack y Sotelsek",
    "Song, Witt y Li"
  ),
  
  full_reference =
    rep(
      NA_character_,
      4
    ),
  
  sample_period =
    rep(
      NA_character_,
      4
    ),
  
  frequency =
    rep(
      NA_character_,
      4
    ),
  
  country_or_region =
    rep(
      NA_character_,
      4
    ),
  
  trade_flow =
    rep(
      NA_character_,
      4
    ),
  
  methodology =
    rep(
      NA_character_,
      4
    ),
  
  income_elasticity =
    rep(
      NA_real_,
      4
    ),
  
  relative_price_elasticity =
    rep(
      NA_real_,
      4
    ),
  
  adjustment_coefficient =
    rep(
      NA_real_,
      4
    ),
  
  notes =
    rep(
      paste0(
        "Completar con los valores verificados directamente ",
        "en el paper correspondiente."
      ),
      4
    )
)


literatura_estimaciones_propias <- estimaciones_preferidas |>
  dplyr::select(
    model_key,
    model_label,
    variable,
    variable_label,
    method,
    estimate,
    expected_sign,
    observed_sign,
    validity,
    final_role
  ) |>
  dplyr::mutate(
    
    literature_reference =
      NA_character_,
    
    literature_estimate =
      NA_real_,
    
    difference_with_literature =
      NA_real_,
    
    sign_match_literature =
      NA
  )


# ------------------------------------------------------------
# 29. FIGURA DE ELASTICIDADES DE LARGO PLAZO
# ------------------------------------------------------------

datos_grafico_lr <- elasticidades_largo_plazo |>
  dplyr::filter(
    is_primary,
    !is.na(
      estimate
    )
  )


if (nrow(datos_grafico_lr) > 0) {
  
  grafico_elasticidades_lr <- ggplot2::ggplot(
    
    datos_grafico_lr,
    
    ggplot2::aes(
      x =
        method,
      y =
        estimate,
      shape =
        method
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    ggplot2::geom_point(
      size = 2.8
    ) +
    ggplot2::facet_grid(
      model_label ~ variable_label,
      scales = "free_y"
    ) +
    ggplot2::labs(
      
      title =
        "Elasticidades de largo plazo por metodología",
      
      subtitle =
        "Engle-Granger, Wickens-Breusch y Johansen",
      
      x =
        NULL,
      
      y =
        "Elasticidad estimada",
      
      shape =
        "Metodología"
    ) +
    ggplot2::theme_minimal(
      base_size = 11
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
      "13_elasticidades_largo_plazo.png"
    ),
    
    plot =
      grafico_elasticidades_lr,
    
    width =
      13,
    
    height =
      9,
    
    dpi =
      300
  )
}


# ------------------------------------------------------------
# 30. FIGURA DE COEFICIENTES ECM Y VECM
# ------------------------------------------------------------

datos_grafico_ajuste <- ajustes_ecm_vecm |>
  dplyr::filter(
    is_primary,
    !is.na(
      adjustment_estimate
    )
  )


if (nrow(datos_grafico_ajuste) > 0) {
  
  grafico_ajuste <- ggplot2::ggplot(
    
    datos_grafico_ajuste,
    
    ggplot2::aes(
      x =
        method,
      y =
        adjustment_estimate,
      shape =
        significant_10pct
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    ggplot2::geom_point(
      size = 3
    ) +
    ggplot2::facet_wrap(
      ~ model_label_short
    ) +
    ggplot2::labs(
      
      title =
        "Coeficientes de corrección del desequilibrio",
      
      subtitle =
        paste0(
          "Comparación ECM y VECM; los ECM uniecuacionales ",
          "se presentan con fines indicativos"
        ),
      
      x =
        NULL,
      
      y =
        "Coeficiente de ajuste",
      
      shape =
        "Significativo al 10%"
    ) +
    ggplot2::theme_minimal(
      base_size = 11
    ) +
    ggplot2::theme(
      legend.position =
        "bottom"
    )
  
  
  ggplot2::ggsave(
    
    filename = here::here(
      "outputs",
      "figures",
      "13_coeficientes_ajuste.png"
    ),
    
    plot =
      grafico_ajuste,
    
    width =
      11,
    
    height =
      6,
    
    dpi =
      300
  )
}


# ------------------------------------------------------------
# 31. FIGURA DE IRF
# ------------------------------------------------------------

if (nrow(irf_horizontes) > 0) {
  
  grafico_irf_horizontes <- ggplot2::ggplot(
    
    irf_horizontes,
    
    ggplot2::aes(
      x =
        horizon,
      y =
        response_percent,
      linetype =
        impulse_label,
      shape =
        impulse_label,
      group =
        impulse_label
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    ggplot2::geom_line(
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      size = 2.2
    ) +
    ggplot2::facet_wrap(
      ~ model_short,
      scales = "free_y"
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(
        0,
        4,
        8,
        12
      )
    ) +
    ggplot2::labs(
      
      title =
        "Respuestas de las variables comerciales",
      
      subtitle =
        "SVAR Cholesky principal; shock positivo de 1%",
      
      x =
        "Horizonte, trimestres",
      
      y =
        "Respuesta porcentual",
      
      linetype =
        "Shock",
      
      shape =
        "Shock"
    ) +
    ggplot2::theme_minimal(
      base_size = 11
    ) +
    ggplot2::theme(
      legend.position =
        "bottom"
    )
  
  
  ggplot2::ggsave(
    
    filename = here::here(
      "outputs",
      "figures",
      "13_irf_horizontes_seleccionados.png"
    ),
    
    plot =
      grafico_irf_horizontes,
    
    width =
      12,
    
    height =
      7,
    
    dpi =
      300
  )
}


# ------------------------------------------------------------
# 32. FIGURA DE FEVD
# ------------------------------------------------------------

if (nrow(fevd_h12) > 0) {
  
  grafico_fevd_h12 <- ggplot2::ggplot(
    
    fevd_h12,
    
    ggplot2::aes(
      x =
        model_short,
      y =
        fevd_percent,
      fill =
        impulse_label
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::labs(
      
      title =
        "Descomposición de la varianza a 12 trimestres",
      
      subtitle =
        "Variable comercial de cada sistema",
      
      x =
        NULL,
      
      y =
        "Participación porcentual",
      
      fill =
        "Shock estructural"
    ) +
    ggplot2::theme_minimal(
      base_size = 11
    ) +
    ggplot2::theme(
      
      axis.text.x =
        ggplot2::element_text(
          angle = 20,
          hjust = 1
        ),
      
      legend.position =
        "bottom"
    )
  
  
  ggplot2::ggsave(
    
    filename = here::here(
      "outputs",
      "figures",
      "13_fevd_h12.png"
    ),
    
    plot =
      grafico_fevd_h12,
    
    width =
      11,
    
    height =
      7,
    
    dpi =
      300
  )
}


# ------------------------------------------------------------
# 33. GUARDAR TABLAS CSV
# ------------------------------------------------------------

readr::write_csv(
  
  decision_final_modelos,
  
  here::here(
    "outputs",
    "tables",
    "13_decision_final_modelos.csv"
  )
)


readr::write_csv(
  
  elasticidades_largo_plazo,
  
  here::here(
    "outputs",
    "tables",
    "13_elasticidades_largo_plazo.csv"
  )
)


readr::write_csv(
  
  comparacion_elasticidades_metodos,
  
  here::here(
    "outputs",
    "tables",
    "13_comparacion_elasticidades_metodos.csv"
  )
)


readr::write_csv(
  
  ajustes_ecm_vecm,
  
  here::here(
    "outputs",
    "tables",
    "13_ajustes_ecm_vecm.csv"
  )
)


readr::write_csv(
  
  irf_principales,
  
  here::here(
    "outputs",
    "tables",
    "13_irf_principales.csv"
  )
)


readr::write_csv(
  
  fevd_h12,
  
  here::here(
    "outputs",
    "tables",
    "13_fevd_h12.csv"
  )
)


readr::write_csv(
  
  estimaciones_preferidas,
  
  here::here(
    "outputs",
    "tables",
    "13_estimaciones_preferidas.csv"
  )
)


# ------------------------------------------------------------
# 34. PREPARAR LIBRO EXCEL
# ------------------------------------------------------------

hojas_excel <- list(
  
  fuentes =
    catalogo_fuentes,
  
  decision_final =
    decision_final_modelos,
  
  cointegracion_eg =
    cointegracion_eg_resumen,
  
  johansen =
    cointegracion_johansen_resumen,
  
  elasticidades_lr =
    elasticidades_largo_plazo,
  
  comparacion_lr =
    comparacion_elasticidades_metodos,
  
  consistencia_lr =
    resumen_consistencia_metodos,
  
  estimaciones_finales =
    estimaciones_preferidas,
  
  ajustes_ecm_vecm =
    ajustes_ecm_vecm,
  
  irf_principal =
    irf_principales,
  
  irf_horizontes =
    irf_horizontes,
  
  fevd_h12 =
    fevd_h12,
  
  control_fevd =
    control_fevd,
  
  literatura =
    literatura_referencias,
  
  literatura_propias =
    literatura_estimaciones_propias
)


if (ncol(corto_plazo_vecm) > 0) {
  
  hojas_excel$corto_plazo_vecm <-
    corto_plazo_vecm
}


if (ncol(comparacion_svar_lp) > 0) {
  
  hojas_excel$svar_vs_lp <-
    comparacion_svar_lp
}


if (ncol(sensibilidad_orden) > 0) {
  
  hojas_excel$orden_sensibilidad <-
    sensibilidad_orden
}


hojas_excel <- hojas_excel[
  vapply(
    hojas_excel,
    ncol,
    FUN.VALUE = integer(1)
  ) >
    0
]


hojas_excel <- lapply(
  hojas_excel,
  aplanar_para_excel
)


writexl::write_xlsx(
  
  hojas_excel,
  
  path = here::here(
    "outputs",
    "tables",
    "13_resultados_finales_tp3.xlsx"
  )
)


# ------------------------------------------------------------
# 35. GUARDAR OBJETOS RDS
# ------------------------------------------------------------

resultados_finales_tp3 <- list(
  
  catalogo_fuentes =
    catalogo_fuentes,
  
  decision_final_modelos =
    decision_final_modelos,
  
  cointegracion_engle_granger =
    cointegracion_eg_resumen,
  
  cointegracion_johansen =
    cointegracion_johansen_resumen,
  
  elasticidades_largo_plazo =
    elasticidades_largo_plazo,
  
  comparacion_elasticidades =
    comparacion_elasticidades_metodos,
  
  consistencia_metodos =
    resumen_consistencia_metodos,
  
  estimaciones_preferidas =
    estimaciones_preferidas,
  
  ajustes_ecm_vecm =
    ajustes_ecm_vecm,
  
  corto_plazo_vecm =
    corto_plazo_vecm,
  
  irf_principales =
    irf_principales,
  
  comparacion_svar_lp =
    comparacion_svar_lp,
  
  sensibilidad_orden =
    sensibilidad_orden,
  
  fevd_h12 =
    fevd_h12,
  
  literatura_referencias =
    literatura_referencias,
  
  literatura_estimaciones_propias =
    literatura_estimaciones_propias
)


saveRDS(
  
  resultados_finales_tp3,
  
  here::here(
    "outputs",
    "models",
    "resultados_finales_tp3.rds"
  )
)


saveRDS(
  
  estimaciones_preferidas,
  
  here::here(
    "outputs",
    "models",
    "estimaciones_preferidas_tp3.rds"
  )
)


saveRDS(
  
  decision_final_modelos,
  
  here::here(
    "outputs",
    "models",
    "decision_final_modelos_tp3.rds"
  )
)


saveRDS(
  
  ajustes_ecm_vecm,
  
  here::here(
    "outputs",
    "models",
    "ajustes_ecm_vecm_final.rds"
  )
)


# ------------------------------------------------------------
# 36. RESUMEN TEXTUAL
# ------------------------------------------------------------

texto_resumen <- c(
  
  "============================================================",
  
  "SCRIPT 13: RESULTADOS FINALES DEL TP3",
  
  "============================================================",
  
  "",
  
  "DECISIÓN FINAL DE MODELOS:",
  
  capture.output(
    
    decision_final_modelos |>
      print(
        n = Inf,
        width = Inf
      )
  ),
  
  "",
  
  "ELASTICIDADES DE LARGO PLAZO:",
  
  capture.output(
    
    elasticidades_largo_plazo |>
      dplyr::filter(
        is_primary
      ) |>
      dplyr::select(
        model_label,
        variable_label,
        method,
        estimate,
        p_value,
        expected_sign,
        observed_sign,
        sign_consistent,
        validity
      ) |>
      print(
        n = Inf,
        width = Inf
      )
  ),
  
  "",
  
  "ESTIMACIONES PREFERIDAS:",
  
  capture.output(
    
    estimaciones_preferidas |>
      print(
        n = Inf,
        width = Inf
      )
  ),
  
  "",
  
  "COEFICIENTES ECM Y VECM:",
  
  capture.output(
    
    ajustes_ecm_vecm |>
      dplyr::filter(
        is_primary
      ) |>
      print(
        n = Inf,
        width = Inf
      )
  ),
  
  "",
  
  "RESPUESTAS DINÁMICAS PRINCIPALES:",
  
  capture.output(
    
    irf_principales |>
      print(
        n = Inf,
        width = Inf
      )
  ),
  
  "",
  
  "FEVD A DOCE TRIMESTRES:",
  
  capture.output(
    
    fevd_h12 |>
      print(
        n = Inf,
        width = Inf
      )
  )
)


writeLines(
  
  texto_resumen,
  
  con = here::here(
    "outputs",
    "models",
    "13_resumen_resultados_finales.txt"
  )
)


capture.output(
  
  sessionInfo(),
  
  file = here::here(
    "outputs",
    "models",
    "13_session_info.txt"
  )
)


# ------------------------------------------------------------
# 37. CATÁLOGO DE SALIDAS
# ------------------------------------------------------------

rutas_salidas <- c(
  
  here::here(
    "outputs",
    "tables",
    "13_resultados_finales_tp3.xlsx"
  ),
  
  here::here(
    "outputs",
    "tables",
    "13_decision_final_modelos.csv"
  ),
  
  here::here(
    "outputs",
    "tables",
    "13_elasticidades_largo_plazo.csv"
  ),
  
  here::here(
    "outputs",
    "tables",
    "13_comparacion_elasticidades_metodos.csv"
  ),
  
  here::here(
    "outputs",
    "tables",
    "13_ajustes_ecm_vecm.csv"
  ),
  
  here::here(
    "outputs",
    "tables",
    "13_irf_principales.csv"
  ),
  
  here::here(
    "outputs",
    "tables",
    "13_fevd_h12.csv"
  ),
  
  here::here(
    "outputs",
    "tables",
    "13_estimaciones_preferidas.csv"
  ),
  
  here::here(
    "outputs",
    "models",
    "ajuste_ecm_correcto.rds"
  ),
  
  here::here(
    "outputs",
    "models",
    "resultados_finales_tp3.rds"
  ),
  
  here::here(
    "outputs",
    "models",
    "estimaciones_preferidas_tp3.rds"
  ),
  
  here::here(
    "outputs",
    "models",
    "decision_final_modelos_tp3.rds"
  ),
  
  here::here(
    "outputs",
    "models",
    "ajustes_ecm_vecm_final.rds"
  ),
  
  here::here(
    "outputs",
    "models",
    "13_resumen_resultados_finales.txt"
  ),
  
  here::here(
    "outputs",
    "figures",
    "13_elasticidades_largo_plazo.png"
  ),
  
  here::here(
    "outputs",
    "figures",
    "13_coeficientes_ajuste.png"
  ),
  
  here::here(
    "outputs",
    "figures",
    "13_irf_horizontes_seleccionados.png"
  ),
  
  here::here(
    "outputs",
    "figures",
    "13_fevd_h12.png"
  )
)


catalogo_salidas <- tibble::tibble(
  
  file_name =
    basename(
      rutas_salidas
    ),
  
  path =
    rutas_salidas,
  
  exists =
    file.exists(
      rutas_salidas
    ),
  
  size_kb =
    dplyr::if_else(
      
      file.exists(
        rutas_salidas
      ),
      
      as.numeric(
        file.info(
          rutas_salidas
        )$size
      ) /
        1024,
      
      NA_real_
    )
)


readr::write_csv(
  
  catalogo_salidas,
  
  here::here(
    "outputs",
    "tables",
    "13_catalogo_salidas.csv"
  )
)


# ------------------------------------------------------------
# 38. CONTROLES FINALES
# ------------------------------------------------------------

if (
  nrow(
    elasticidades_largo_plazo
  ) !=
  20
) {
  
  warning(
    paste0(
      "Se esperaban 20 elasticidades consolidadas, ",
      "pero se encontraron ",
      nrow(
        elasticidades_largo_plazo
      ),
      "."
    )
  )
}


if (
  nrow(
    decision_final_modelos
  ) !=
  4
) {
  
  warning(
    paste0(
      "Se esperaban 4 modelos en la decisión final, ",
      "pero se encontraron ",
      nrow(
        decision_final_modelos
      ),
      "."
    )
  )
}


if (
  nrow(
    ajuste_ecm_correcto
  ) !=
  3
) {
  
  stop(
    "La tabla ECM correcta debe contener exactamente 3 filas."
  )
}


ajustes_principales <- ajustes_ecm_vecm |>
  dplyr::filter(
    is_primary
  )


if (
  nrow(
    ajustes_principales
  ) !=
  6
) {
  
  stop(
    paste0(
      "Se esperaban 6 coeficientes principales de ajuste ",
      "(3 ECM y 3 VECM), pero se encontraron ",
      nrow(
        ajustes_principales
      ),
      "."
    )
  )
}


if (
  any(
    abs(
      ajuste_ecm_correcto$adjustment_estimate -
      c(
        -0.305,
        -0.395,
        -0.427
      )
    ) >
    0.000001
  )
) {
  
  stop(
    "Los coeficientes ECM no coinciden con los valores validados."
  )
}


if (
  nrow(
    irf_principales
  ) !=
  6
) {
  
  warning(
    paste0(
      "Se esperaban 6 respuestas IRF principales, ",
      "pero se encontraron ",
      nrow(
        irf_principales
      ),
      "."
    )
  )
}


if (
  nrow(
    fevd_h12
  ) !=
  9
) {
  
  warning(
    paste0(
      "Se esperaban 9 componentes FEVD, ",
      "pero se encontraron ",
      nrow(
        fevd_h12
      ),
      "."
    )
  )
}


if (
  nrow(control_fevd) > 0 &&
  any(
    !control_fevd$valid_sum
  )
) {
  
  warning(
    paste0(
      "Alguna FEVD no suma aproximadamente 100%. ",
      "Revisa control_fevd."
    )
  )
}


if (
  !file.exists(
    here::here(
      "outputs",
      "tables",
      "13_resultados_finales_tp3.xlsx"
    )
  )
) {
  
  stop(
    "No se generó el libro Excel final."
  )
}


# ------------------------------------------------------------
# 39. RESULTADOS PRINCIPALES EN CONSOLA
# ------------------------------------------------------------

cat(
  "\n\n============================================================",
  "\nDECISIÓN FINAL DE MODELOS",
  "\n============================================================\n"
)


decision_final_modelos |>
  print(
    n = Inf,
    width = Inf
  )


cat(
  "\n\n============================================================",
  "\nELASTICIDADES DE LARGO PLAZO",
  "\n============================================================\n"
)


elasticidades_largo_plazo |>
  dplyr::filter(
    is_primary
  ) |>
  dplyr::select(
    model_label,
    variable_label,
    method,
    estimate,
    p_value,
    expected_sign,
    observed_sign,
    sign_consistent,
    validity
  ) |>
  print(
    n = Inf,
    width = Inf
  )


cat(
  "\n\n============================================================",
  "\nESTIMACIONES PREFERIDAS PARA EL INFORME",
  "\n============================================================\n"
)


estimaciones_preferidas |>
  dplyr::select(
    model_label,
    variable_label,
    method,
    estimate,
    expected_sign,
    final_role
  ) |>
  print(
    n = Inf,
    width = Inf
  )


cat(
  "\n\n============================================================",
  "\nCOEFICIENTES ECM Y VECM",
  "\n============================================================\n"
)


ajustes_principales |>
  dplyr::select(
    model_label_short,
    method,
    adjustment_estimate,
    adjustment_std_error,
    adjustment_statistic,
    adjustment_p_value,
    adjustment_percent,
    cointegration_10pct,
    significant_10pct,
    validity,
    interpretation
  ) |>
  dplyr::arrange(
    model_label_short,
    method
  ) |>
  print(
    n = Inf,
    width = Inf
  )


cat(
  "\n\n============================================================",
  "\nRESPUESTAS DINÁMICAS PRINCIPALES",
  "\n============================================================\n"
)


irf_principales |>
  print(
    n = Inf,
    width = Inf
  )


cat(
  "\n\n============================================================",
  "\nFEVD A DOCE TRIMESTRES",
  "\n============================================================\n"
)


fevd_h12 |>
  print(
    n = Inf,
    width = Inf
  )


cat(
  "\n\n============================================================",
  "\nCATÁLOGO DE SALIDAS",
  "\n============================================================\n"
)


catalogo_salidas |>
  print(
    n = Inf,
    width = Inf
  )


# ------------------------------------------------------------
# 40. MENSAJE FINAL
# ------------------------------------------------------------

cat(
  "\n============================================================",
  "\nSCRIPT 13 FINALIZADO",
  "\n============================================================",
  
  "\nElasticidades consolidadas:",
  nrow(
    elasticidades_largo_plazo
  ),
  
  "\nModelos en la decisión final:",
  nrow(
    decision_final_modelos
  ),
  
  "\nCoeficientes ECM correctos:",
  nrow(
    ajuste_ecm_correcto
  ),
  
  "\nCoeficientes VECM principales:",
  nrow(
    ajuste_vecm |>
      dplyr::filter(
        is_primary
      )
  ),
  
  "\nCoeficientes de ajuste principales:",
  nrow(
    ajustes_principales
  ),
  
  "\nRespuestas dinámicas principales:",
  nrow(
    irf_principales
  ),
  
  "\nComponentes FEVD a 12 trimestres:",
  nrow(
    fevd_h12
  ),
  
  "\n\nArchivos principales generados:",
  
  "\n- outputs/tables/13_resultados_finales_tp3.xlsx",
  
  "\n- outputs/tables/13_decision_final_modelos.csv",
  
  "\n- outputs/tables/13_elasticidades_largo_plazo.csv",
  
  "\n- outputs/tables/13_comparacion_elasticidades_metodos.csv",
  
  "\n- outputs/tables/13_ajustes_ecm_vecm.csv",
  
  "\n- outputs/tables/13_irf_principales.csv",
  
  "\n- outputs/tables/13_fevd_h12.csv",
  
  "\n- outputs/tables/13_estimaciones_preferidas.csv",
  
  "\n- outputs/tables/13_catalogo_salidas.csv",
  
  "\n- outputs/models/ajuste_ecm_correcto.rds",
  
  "\n- outputs/models/resultados_finales_tp3.rds",
  
  "\n- outputs/models/estimaciones_preferidas_tp3.rds",
  
  "\n- outputs/models/decision_final_modelos_tp3.rds",
  
  "\n- outputs/models/ajustes_ecm_vecm_final.rds",
  
  "\n- outputs/models/13_resumen_resultados_finales.txt",
  
  "\n- figures/13_elasticidades_largo_plazo.png",
  
  "\n- figures/13_coeficientes_ajuste.png",
  
  "\n- figures/13_irf_horizontes_seleccionados.png",
  
  "\n- figures/13_fevd_h12.png",
  
  "\n============================================================",
  "\n"
)

