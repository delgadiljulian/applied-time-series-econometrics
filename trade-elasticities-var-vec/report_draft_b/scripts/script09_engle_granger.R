# ============================================================
# TP3 - SERIES DE TIEMPO
# Script 09:
# Engle-Granger, modelos en diferencias, ECM y
# verificación Wickens-Breusch
#
# Sistemas:
#
# 1. Importaciones:
#    ln(M) = a0 + a1*ln(PIB Argentina) + a2*ln(ITCRM)
#
# 2. Exportaciones clásicas:
#    ln(X) = b0 + b1*ln(PIB socios) + b2*ln(ITCRM)
#
# 3. Exportaciones ampliadas:
#    agrega el índice de commodities como control I(0).
#
# Nivel principal de significancia del TP3: 10%
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
  "tidyverse",
  "here",
  "zoo",
  "urca",
  "lmtest",
  "sandwich",
  "tseries",
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
library(zoo)
library(urca)
library(lmtest)
library(sandwich)
library(tseries)
library(writexl)


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
# 3. CARGAR PANEL MAESTRO
# ------------------------------------------------------------

ruta_panel <- here::here(
  "data",
  "processed",
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
  arrange(periodo) |>
  mutate(
    row_id = row_number(),
    
    fecha = as.Date(
      periodo,
      frac = 1
    ),
    
    q2 = as.integer(
      trimestre == 2
    ),
    
    q3 = as.integer(
      trimestre == 3
    ),
    
    q4 = as.integer(
      trimestre == 4
    )
  )


# ------------------------------------------------------------
# 4. VERIFICAR COBERTURA
# ------------------------------------------------------------

cat(
  "\n============================================================",
  "\nMUESTRA DEL SCRIPT 09",
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
# 5. CONFIGURACIÓN DE LOS MODELOS
# ------------------------------------------------------------

especificaciones <- tibble(
  
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
  
  dependent = c(
    "ln_importaciones_reales",
    "ln_exportaciones_reales",
    "ln_exportaciones_reales"
  ),
  
  x1 = c(
    "ln_pib_real",
    "ln_pib_socios",
    "ln_pib_socios"
  ),
  
  x2 = c(
    "ln_itcrm",
    "ln_itcrm",
    "ln_itcrm"
  ),
  
  stationary_controls = list(
    character(0),
    character(0),
    "ln_commodity_price_index"
  ),
  
  q_i1 = c(
    3L,
    3L,
    3L
  ),
  
  formal_engle_granger = c(
    TRUE,
    TRUE,
    FALSE
  ),
  
  interpretation = c(
    "Modelo clásico de importaciones",
    "Modelo clásico de exportaciones",
    paste0(
      "Robustez ampliada: commodities es I(0) ",
      "y no integra el vector I(1)"
    )
  )
)

print(
  especificaciones |>
    mutate(
      stationary_controls =
        map_chr(
          stationary_controls,
          ~ ifelse(
            length(.x) == 0,
            "Ninguno",
            paste(.x, collapse = " + ")
          )
        )
    ),
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 6. VERIFICAR VARIABLES
# ------------------------------------------------------------

variables_nivel <- unique(
  c(
    especificaciones$dependent,
    especificaciones$x1,
    especificaciones$x2,
    unlist(
      especificaciones$stationary_controls
    )
  )
)

variables_requeridas <- c(
  "periodo",
  "anio",
  "trimestre",
  variables_nivel
)

variables_faltantes <- setdiff(
  variables_requeridas,
  names(panel_maestro)
)

if (length(variables_faltantes) > 0) {
  stop(
    paste0(
      "Faltan las siguientes variables:\n",
      paste(
        variables_faltantes,
        collapse = ", "
      )
    )
  )
}


# ------------------------------------------------------------
# 7. RECALCULAR PRIMERAS DIFERENCIAS
# ------------------------------------------------------------

# Se recalculan para asegurar que todas las diferencias
# correspondan exactamente a las series en niveles utilizadas.

for (variable_i in variables_nivel) {
  
  nombre_diferencia <- paste0(
    "d_",
    variable_i
  )
  
  panel_maestro[[
    nombre_diferencia
  ]] <- c(
    NA_real_,
    diff(
      panel_maestro[[
        variable_i
      ]]
    )
  )
}


# ------------------------------------------------------------
# 8. CREAR REZAGOS
# ------------------------------------------------------------

max_lag_dinamico <- 4L

variables_diferencia <- paste0(
  "d_",
  variables_nivel
)

# Rezagos de niveles.

for (variable_i in variables_nivel) {
  
  panel_maestro[[
    paste0(
      "l1_",
      variable_i
    )
  ]] <- dplyr::lag(
    panel_maestro[[
      variable_i
    ]],
    1
  )
}

# Rezagos de primeras diferencias.

for (variable_i in variables_diferencia) {
  
  for (
    lag_i in seq_len(
      max_lag_dinamico
    )
  ) {
    
    panel_maestro[[
      paste0(
        variable_i,
        "_l",
        lag_i
      )
    ]] <- dplyr::lag(
      panel_maestro[[
        variable_i
      ]],
      lag_i
    )
  }
}


# ------------------------------------------------------------
# 9. FUNCIONES GENERALES
# ------------------------------------------------------------

crear_formula <- function(
    response,
    terms,
    intercept = TRUE
) {
  
  terms <- unique(
    terms
  )
  
  stats::reformulate(
    termlabels = terms,
    response = response,
    intercept = intercept
  )
}


extraer_coeftest <- function(
    fit,
    vcov_matrix,
    model_key,
    model_type
) {
  
  resultado <- lmtest::coeftest(
    fit,
    vcov. = vcov_matrix
  )
  
  tibble(
    model_key = model_key,
    model_type = model_type,
    term = rownames(resultado),
    estimate = as.numeric(
      resultado[, 1]
    ),
    std_error = as.numeric(
      resultado[, 2]
    ),
    statistic = as.numeric(
      resultado[, 3]
    ),
    p_value = as.numeric(
      resultado[, 4]
    )
  )
}


resumen_lm <- function(
    fit,
    model_key,
    model_type,
    formula_text = NULL
) {
  
  resumen <- summary(
    fit
  )
  
  if (is.null(formula_text)) {
    formula_text <- paste(
      deparse(
        formula(fit)
      ),
      collapse = " "
    )
  }
  
  tibble(
    model_key = model_key,
    model_type = model_type,
    observations = stats::nobs(fit),
    r_squared = resumen$r.squared,
    adjusted_r_squared =
      resumen$adj.r.squared,
    residual_std_error =
      resumen$sigma,
    AIC = AIC(fit),
    BIC = BIC(fit),
    formula = formula_text
  )
}


obtener_p_value <- function(
    expresion
) {
  
  tryCatch(
    as.numeric(
      expresion$p.value
    ),
    error = function(e) {
      NA_real_
    }
  )
}


diagnosticos_lm <- function(
    fit,
    model_key,
    model_type
) {
  
  residuos <- residuals(
    fit
  )
  
  orden_bg <- min(
    4L,
    max(
      1L,
      floor(
        length(residuos) / 5
      )
    )
  )
  
  p_bg <- tryCatch(
    obtener_p_value(
      lmtest::bgtest(
        fit,
        order = orden_bg
      )
    ),
    error = function(e) {
      NA_real_
    }
  )
  
  p_bp <- tryCatch(
    obtener_p_value(
      lmtest::bptest(
        fit
      )
    ),
    error = function(e) {
      NA_real_
    }
  )
  
  p_jb <- tryCatch(
    obtener_p_value(
      tseries::jarque.bera.test(
        residuos
      )
    ),
    error = function(e) {
      NA_real_
    }
  )
  
  p_ljung <- tryCatch(
    obtener_p_value(
      Box.test(
        residuos,
        lag = min(
          8L,
          length(residuos) - 1L
        ),
        type = "Ljung-Box"
      )
    ),
    error = function(e) {
      NA_real_
    }
  )
  
  p_reset <- tryCatch(
    obtener_p_value(
      lmtest::resettest(
        fit,
        power = 2:3,
        type = "fitted"
      )
    ),
    error = function(e) {
      NA_real_
    }
  )
  
  tibble(
    model_key = model_key,
    model_type = model_type,
    
    test = c(
      "Breusch-Godfrey",
      "Breusch-Pagan",
      "Jarque-Bera",
      "Ljung-Box",
      "RESET"
    ),
    
    null_hypothesis = c(
      "No autocorrelación serial",
      "Homoscedasticidad",
      "Normalidad",
      "No autocorrelación serial",
      "Especificación funcional correcta"
    ),
    
    p_value = c(
      p_bg,
      p_bp,
      p_jb,
      p_ljung,
      p_reset
    ),
    
    decision_5pct = if_else(
      p_value < 0.05,
      "Se rechaza H0",
      "No se rechaza H0"
    ),
    
    decision_10pct = if_else(
      p_value < 0.10,
      "Se rechaza H0",
      "No se rechaza H0"
    )
  )
}


# ------------------------------------------------------------
# 10. ADF SOBRE RESIDUOS DE ENGLE-GRANGER
# ------------------------------------------------------------

# La regresión auxiliar es:
#
# Δu_t = rho*u_(t-1) +
#        sum(gamma_i*Δu_(t-i)) + error_t
#
# No se incluye constante ni tendencia porque los residuos
# de la primera etapa tienen media aproximadamente cero.

adf_residual_engle_granger <- function(
    residuos
) {
  
  residuos <- as.numeric(
    residuos
  )
  
  residuos <- residuos[
    !is.na(residuos)
  ]
  
  n <- length(
    residuos
  )
  
  max_lag <- floor(
    12 *
      (n / 100)^0.25
  )
  
  max_lag <- min(
    max_lag,
    floor(
      (n - 1) / 3
    )
  )
  
  max_lag <- max(
    max_lag,
    0L
  )
  
  base_adf <- tibble(
    du = diff(
      residuos
    ),
    
    u_l1 = head(
      residuos,
      -1
    )
  )
  
  if (max_lag > 0) {
    
    for (
      lag_i in seq_len(
        max_lag
      )
    ) {
      
      base_adf[[
        paste0(
          "du_l",
          lag_i
        )
      ]] <- dplyr::lag(
        base_adf$du,
        lag_i
      )
    }
  }
  
  # Muestra común para comparar AIC y BIC.
  
  base_comun <- base_adf |>
    drop_na()
  
  tabla_seleccion <- map_dfr(
    0:max_lag,
    function(p) {
      
      terminos_p <- c(
        "u_l1",
        
        if (p > 0) {
          paste0(
            "du_l",
            seq_len(p)
          )
        } else {
          character(0)
        }
      )
      
      formula_p <- crear_formula(
        response = "du",
        terms = terminos_p,
        intercept = FALSE
      )
      
      fit_p <- lm(
        formula_p,
        data = base_comun
      )
      
      tibble(
        lag = p,
        AIC = AIC(fit_p),
        BIC = BIC(fit_p)
      )
    }
  )
  
  lag_aic <- tabla_seleccion$lag[
    which.min(
      tabla_seleccion$AIC
    )
  ]
  
  lag_bic <- tabla_seleccion$lag[
    which.min(
      tabla_seleccion$BIC
    )
  ]
  
  estimar_adf_final <- function(
    lag_elegido,
    criterio
  ) {
    
    terminos_finales <- c(
      "u_l1",
      
      if (lag_elegido > 0) {
        paste0(
          "du_l",
          seq_len(
            lag_elegido
          )
        )
      } else {
        character(0)
      }
    )
    
    datos_finales <- base_adf |>
      select(
        all_of(
          c(
            "du",
            terminos_finales
          )
        )
      ) |>
      drop_na()
    
    formula_final <- crear_formula(
      response = "du",
      terms = terminos_finales,
      intercept = FALSE
    )
    
    fit_final <- lm(
      formula_final,
      data = datos_finales
    )
    
    coeficientes <- summary(
      fit_final
    )$coefficients
    
    tibble(
      criterio = criterio,
      observations = nrow(
        datos_finales
      ),
      max_lag = max_lag,
      selected_lag =
        as.integer(
          lag_elegido
        ),
      statistic = as.numeric(
        coeficientes[
          "u_l1",
          "t value"
        ]
      ),
      rho_estimate = as.numeric(
        coeficientes[
          "u_l1",
          "Estimate"
        ]
      )
    )
  }
  
  resultados <- bind_rows(
    estimar_adf_final(
      lag_aic,
      "AIC"
    ),
    
    estimar_adf_final(
      lag_bic,
      "BIC"
    )
  )
  
  list(
    results = resultados,
    lag_selection = tabla_seleccion
  )
}


# ------------------------------------------------------------
# 11. VALORES CRÍTICOS ENGLE-GRANGER
# ------------------------------------------------------------

# Tabla utilizada en el TP2.
#
# q_i1 cuenta:
# variable dependiente + regresores I(1).
#
# En nuestros sistemas clásicos:
# q_i1 = 3.

tabla_criticos_eg <- tribble(
  
  ~q_i1,
  ~reference_n,
  ~critical_1pct,
  ~critical_5pct,
  ~critical_10pct,
  
  2L, 50L,  -4.32, -3.67, -3.28,
  2L, 100L, -4.07, -3.37, -3.03,
  2L, 200L, -4.00, -3.37, -3.02,
  
  3L, 50L,  -4.84, -4.11, -3.73,
  3L, 100L, -4.45, -3.93, -3.59,
  3L, 200L, -4.35, -3.78, -3.47
)


buscar_criticos_eg <- function(
    q_i1,
    n_obs
) {
  
  candidatos <- tabla_criticos_eg |>
    filter(
      .data$q_i1 ==
        !!q_i1
    )
  
  if (nrow(candidatos) == 0) {
    return(
      tibble(
        q_i1 = q_i1,
        reference_n = NA_integer_,
        critical_1pct = NA_real_,
        critical_5pct = NA_real_,
        critical_10pct = NA_real_
      )
    )
  }
  
  candidatos |>
    mutate(
      distancia = abs(
        reference_n - n_obs
      )
    ) |>
    slice_min(
      distancia,
      n = 1,
      with_ties = FALSE
    ) |>
    select(
      -distancia
    )
}


# ------------------------------------------------------------
# 12. ESTIMAR UNA VARIANTE ENGLE-GRANGER
# ------------------------------------------------------------

estimar_engle_granger <- function(
    datos,
    spec,
    seasonal_dummies = FALSE
) {
  
  model_key_i <-
    spec$model_key[[1]]
  
  model_label_i <-
    spec$model_label[[1]]
  
  dependent_i <-
    spec$dependent[[1]]
  
  x1_i <-
    spec$x1[[1]]
  
  x2_i <-
    spec$x2[[1]]
  
  controls_i <-
    spec$stationary_controls[[1]]
  
  q_i1_i <-
    spec$q_i1[[1]]
  
  formal_i <-
    spec$formal_engle_granger[[1]]
  
  variant_i <- ifelse(
    seasonal_dummies,
    "seasonal_dummies",
    "baseline"
  )
  
  regresores <- c(
    x1_i,
    x2_i,
    controls_i,
    
    if (seasonal_dummies) {
      c(
        "q2",
        "q3",
        "q4"
      )
    } else {
      character(0)
    }
  )
  
  variables_modelo <- c(
    "row_id",
    "periodo",
    "fecha",
    dependent_i,
    regresores
  )
  
  datos_modelo <- datos |>
    select(
      all_of(
        variables_modelo
      )
    ) |>
    drop_na()
  
  formula_largo_plazo <- crear_formula(
    response = dependent_i,
    terms = regresores
  )
  
  fit <- lm(
    formula_largo_plazo,
    data = datos_modelo
  )
  
  vcov_hac <- sandwich::NeweyWest(
    fit,
    lag = 4,
    prewhite = FALSE,
    adjust = TRUE
  )
  
  coeficientes <- extraer_coeftest(
    fit = fit,
    vcov_matrix = vcov_hac,
    model_key = model_key_i,
    model_type = paste0(
      "engle_granger_",
      variant_i
    )
  ) |>
    mutate(
      model_label = model_label_i,
      variant = variant_i,
      .after = model_key
    )
  
  resumen <- resumen_lm(
    fit = fit,
    model_key = model_key_i,
    model_type = paste0(
      "engle_granger_",
      variant_i
    )
  ) |>
    mutate(
      model_label = model_label_i,
      variant = variant_i,
      formal_engle_granger = formal_i,
      .after = model_key
    )
  
  prueba_adf <- adf_residual_engle_granger(
    residuals(
      fit
    )
  )
  
  criticos <- buscar_criticos_eg(
    q_i1 = q_i1_i,
    n_obs = nrow(
      datos_modelo
    )
  )
  
  prueba_residual <- prueba_adf$results |>
    mutate(
      model_key = model_key_i,
      model_label = model_label_i,
      variant = variant_i,
      q_i1 = q_i1_i,
      formal_engle_granger = formal_i,
      reference_n =
        criticos$reference_n,
      critical_1pct =
        criticos$critical_1pct,
      critical_5pct =
        criticos$critical_5pct,
      critical_10pct =
        criticos$critical_10pct,
      
      cointegration_1pct =
        statistic <
        critical_1pct,
      
      cointegration_5pct =
        statistic <
        critical_5pct,
      
      cointegration_10pct =
        statistic <
        critical_10pct,
      
      validity = case_when(
        formal_i &
          !seasonal_dummies ~
          "Formal",
        
        formal_i &
          seasonal_dummies ~
          "Robustez con dummies estacionales",
        
        TRUE ~
          paste0(
            "Indicativo: incluye control I(0) ",
            "en la relación"
          )
      ),
      
      .before = 1
    )
  
  residuos <- datos_modelo |>
    transmute(
      row_id,
      periodo,
      fecha,
      model_key = model_key_i,
      model_label = model_label_i,
      variant = variant_i,
      residual = as.numeric(
        residuals(
          fit
        )
      )
    )
  
  list(
    fit = fit,
    coefficients = coeficientes,
    summary = resumen,
    residual_test = prueba_residual,
    lag_selection =
      prueba_adf$lag_selection |>
      mutate(
        model_key = model_key_i,
        model_label = model_label_i,
        variant = variant_i,
        .before = 1
      ),
    residuals = residuos
  )
}


# ------------------------------------------------------------
# 13. EJECUTAR ENGLE-GRANGER
# ------------------------------------------------------------

resultados_eg <- list()
contador_eg <- 1L

for (
  i in seq_len(
    nrow(especificaciones)
  )
) {
  
  spec_i <- especificaciones[
    i,
  ]
  
  for (
    seasonal_i in c(
      FALSE,
      TRUE
    )
  ) {
    
    cat(
      "\nEstimando Engle-Granger:",
      spec_i$model_label,
      "| Dummies estacionales:",
      seasonal_i,
      "\n"
    )
    
    resultados_eg[[
      contador_eg
    ]] <- estimar_engle_granger(
      datos = panel_maestro,
      spec = spec_i,
      seasonal_dummies =
        seasonal_i
    )
    
    contador_eg <-
      contador_eg + 1L
  }
}


modelos_eg <- list()

contador_modelos <- 1L

for (
  i in seq_along(
    resultados_eg
  )
) {
  
  clave_modelo <- paste0(
    resultados_eg[[i]]$
      summary$model_key,
    "__",
    resultados_eg[[i]]$
      summary$variant
  )
  
  modelos_eg[[
    clave_modelo
  ]] <- resultados_eg[[i]]$fit
  
  contador_modelos <-
    contador_modelos + 1L
}


resumen_eg <- map_dfr(
  resultados_eg,
  "summary"
)

coeficientes_eg <- map_dfr(
  resultados_eg,
  "coefficients"
)

tests_eg <- map_dfr(
  resultados_eg,
  "residual_test"
)

seleccion_lags_eg <- map_dfr(
  resultados_eg,
  "lag_selection"
)

residuos_eg <- map_dfr(
  resultados_eg,
  "residuals"
)


# ------------------------------------------------------------
# 14. DECISIÓN PRINCIPAL DE COINTEGRACIÓN
# ------------------------------------------------------------

decision_cointegracion <- tests_eg |>
  filter(
    variant == "baseline",
    criterio == "AIC"
  ) |>
  transmute(
    model_key,
    model_label,
    formal_engle_granger,
    observations,
    selected_lag,
    statistic,
    critical_10pct,
    critical_5pct,
    cointegration_10pct,
    cointegration_5pct,
    validity,
    
    decision_10pct = if_else(
      cointegration_10pct,
      "Existe evidencia de cointegración al 10%",
      "No existe evidencia de cointegración al 10%"
    ),
    
    ecm_status = case_when(
      formal_engle_granger &
        cointegration_10pct ~
        "ECM válido según Engle-Granger",
      
      formal_engle_granger &
        !cointegration_10pct ~
        paste0(
          "ECM se estima sólo con fines ",
          "indicativos"
        ),
      
      TRUE ~
        paste0(
          "Especificación ampliada de ",
          "robustez; interpretación indicativa"
        )
    )
  )

print(
  decision_cointegracion,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 15. AGREGAR RESIDUOS BASE AL PANEL
# ------------------------------------------------------------

residuos_eg_baseline <- residuos_eg |>
  filter(
    variant == "baseline"
  ) |>
  select(
    row_id,
    model_key,
    residual
  ) |>
  mutate(
    residual_name = paste0(
      "resid_",
      model_key,
      "_eg"
    )
  ) |>
  select(
    -model_key
  ) |>
  pivot_wider(
    names_from =
      residual_name,
    values_from =
      residual
  )

panel_modelos <- panel_maestro |>
  left_join(
    residuos_eg_baseline,
    by = "row_id"
  )

for (
  model_key_i in
  especificaciones$model_key
) {
  
  nombre_residuo <- paste0(
    "resid_",
    model_key_i,
    "_eg"
  )
  
  nombre_ecm <- paste0(
    "ecm_",
    model_key_i,
    "_l1"
  )
  
  panel_modelos[[
    nombre_ecm
  ]] <- dplyr::lag(
    panel_modelos[[
      nombre_residuo
    ]],
    1
  )
}


# ------------------------------------------------------------
# 16. FUNCIÓN PARA MODELOS DINÁMICOS
# ------------------------------------------------------------

ajustar_modelo_dinamico <- function(
    datos,
    spec,
    model_type = c(
      "ecm",
      "differences",
      "wickens_breusch"
    ),
    max_lag = 4L
) {
  
  model_type <- match.arg(
    model_type
  )
  
  model_key_i <-
    spec$model_key[[1]]
  
  model_label_i <-
    spec$model_label[[1]]
  
  dependent_i <-
    spec$dependent[[1]]
  
  x1_i <-
    spec$x1[[1]]
  
  x2_i <-
    spec$x2[[1]]
  
  controls_i <-
    spec$stationary_controls[[1]]
  
  dy <- paste0(
    "d_",
    dependent_i
  )
  
  dx1 <- paste0(
    "d_",
    x1_i
  )
  
  dx2 <- paste0(
    "d_",
    x2_i
  )
  
  dcontrols <- if (
    length(controls_i) > 0
  ) {
    paste0(
      "d_",
      controls_i
    )
  } else {
    character(0)
  }
  
  variables_dinamicas <- c(
    dy,
    dx1,
    dx2,
    dcontrols
  )
  
  terminos_estacionales <- c(
    "q2",
    "q3",
    "q4"
  )
  
  if (model_type == "ecm") {
    
    termino_ecm <- paste0(
      "ecm_",
      model_key_i,
      "_l1"
    )
    
    terminos_core <- c(
      dx1,
      dx2,
      dcontrols,
      termino_ecm,
      terminos_estacionales
    )
  }
  
  if (model_type == "differences") {
    
    terminos_core <- c(
      dx1,
      dx2,
      dcontrols,
      terminos_estacionales
    )
  }
  
  if (
    model_type ==
    "wickens_breusch"
  ) {
    
    terminos_niveles <- c(
      paste0(
        "l1_",
        dependent_i
      ),
      paste0(
        "l1_",
        x1_i
      ),
      paste0(
        "l1_",
        x2_i
      )
    )
    
    # Los controles I(0) se incluyen directamente.
    terminos_core <- c(
      terminos_niveles,
      dx1,
      dx2,
      dcontrols,
      controls_i,
      terminos_estacionales
    )
  }
  
  terminos_rezagados_max <- unlist(
    lapply(
      seq_len(
        max_lag
      ),
      function(lag_i) {
        
        paste0(
          variables_dinamicas,
          "_l",
          lag_i
        )
      }
    )
  )
  
  variables_muestra <- unique(
    c(
      dy,
      terminos_core,
      terminos_rezagados_max
    )
  )
  
  datos_muestra <- datos |>
    select(
      all_of(
        variables_muestra
      )
    ) |>
    drop_na()
  
  if (nrow(datos_muestra) < 30) {
    stop(
      paste0(
        "La muestra quedó demasiado pequeña para ",
        model_key_i,
        " - ",
        model_type
      )
    )
  }
  
  obtener_terminos_lag <- function(
    p
  ) {
    
    if (p == 0) {
      return(
        character(0)
      )
    }
    
    unlist(
      lapply(
        seq_len(p),
        function(lag_i) {
          
          paste0(
            variables_dinamicas,
            "_l",
            lag_i
          )
        }
      )
    )
  }
  
  tabla_lags <- map_dfr(
    0:max_lag,
    function(p) {
      
      terminos_p <- c(
        terminos_core,
        obtener_terminos_lag(
          p
        )
      )
      
      formula_p <- crear_formula(
        response = dy,
        terms = terminos_p
      )
      
      fit_p <- lm(
        formula_p,
        data = datos_muestra
      )
      
      tibble(
        selected_lag_candidate = p,
        observations =
          nobs(fit_p),
        AIC = AIC(fit_p),
        BIC = BIC(fit_p)
      )
    }
  )
  
  lag_seleccionado <- tabla_lags$
    selected_lag_candidate[
      which.min(
        tabla_lags$BIC
      )
    ]
  
  terminos_seleccionados <- c(
    terminos_core,
    obtener_terminos_lag(
      lag_seleccionado
    )
  )
  
  formula_general <- crear_formula(
    response = dy,
    terms =
      terminos_seleccionados
  )
  
  fit_general <- lm(
    formula_general,
    data = datos_muestra
  )
  
  formula_core <- crear_formula(
    response = dy,
    terms = terminos_core
  )
  
  # Reducción BIC general-to-specific.
  #
  # Los componentes económicos principales permanecen
  # protegidos en la fórmula inferior.
  
  fit_reducido <- tryCatch(
    
    suppressWarnings(
      stats::step(
        fit_general,
        scope = list(
          lower =
            formula_core,
          upper =
            formula_general
        ),
        direction = "backward",
        k = log(
          nrow(
            datos_muestra
          )
        ),
        trace = 0
      )
    ),
    
    error = function(e) {
      fit_general
    }
  )
  
  vcov_hac <- sandwich::NeweyWest(
    fit_reducido,
    lag = 4,
    prewhite = FALSE,
    adjust = TRUE
  )
  
  coeficientes <- extraer_coeftest(
    fit = fit_reducido,
    vcov_matrix = vcov_hac,
    model_key = model_key_i,
    model_type = model_type
  ) |>
    mutate(
      model_label =
        model_label_i,
      selected_lag =
        lag_seleccionado,
      .after = model_key
    )
  
  resumen <- resumen_lm(
    fit = fit_reducido,
    model_key = model_key_i,
    model_type = model_type
  ) |>
    mutate(
      model_label =
        model_label_i,
      selected_lag =
        lag_seleccionado,
      general_formula =
        paste(
          deparse(
            formula_general
          ),
          collapse = " "
        ),
      .after = model_key
    )
  
  diagnosticos <- diagnosticos_lm(
    fit = fit_reducido,
    model_key = model_key_i,
    model_type = model_type
  ) |>
    mutate(
      model_label =
        model_label_i,
      selected_lag =
        lag_seleccionado,
      .after = model_key
    )
  
  tabla_lags <- tabla_lags |>
    mutate(
      model_key =
        model_key_i,
      model_label =
        model_label_i,
      model_type =
        model_type,
      selected_by_BIC =
        selected_lag_candidate ==
        lag_seleccionado,
      .before = 1
    )
  
  list(
    fit = fit_reducido,
    fit_general = fit_general,
    vcov_hac = vcov_hac,
    coefficients = coeficientes,
    summary = resumen,
    diagnostics = diagnosticos,
    lag_selection = tabla_lags,
    selected_lag = lag_seleccionado,
    response = dy
  )
}


# ------------------------------------------------------------
# 17. ESTIMAR ECM Y MODELOS EN DIFERENCIAS
# ------------------------------------------------------------

resultados_ecm <- list()
resultados_diferencias <- list()

for (
  i in seq_len(
    nrow(especificaciones)
  )
) {
  
  spec_i <- especificaciones[
    i,
  ]
  
  cat(
    "\nEstimando ECM:",
    spec_i$model_label,
    "\n"
  )
  
  resultados_ecm[[
    spec_i$model_key
  ]] <- ajustar_modelo_dinamico(
    datos = panel_modelos,
    spec = spec_i,
    model_type = "ecm",
    max_lag =
      max_lag_dinamico
  )
  
  cat(
    "\nEstimando modelo en diferencias:",
    spec_i$model_label,
    "\n"
  )
  
  resultados_diferencias[[
    spec_i$model_key
  ]] <- ajustar_modelo_dinamico(
    datos = panel_modelos,
    spec = spec_i,
    model_type =
      "differences",
    max_lag =
      max_lag_dinamico
  )
}


coeficientes_ecm <- map_dfr(
  resultados_ecm,
  "coefficients"
)

resumen_ecm <- map_dfr(
  resultados_ecm,
  "summary"
)

diagnosticos_ecm <- map_dfr(
  resultados_ecm,
  "diagnostics"
)

seleccion_lags_ecm <- map_dfr(
  resultados_ecm,
  "lag_selection"
)


coeficientes_diferencias <- map_dfr(
  resultados_diferencias,
  "coefficients"
)

resumen_diferencias <- map_dfr(
  resultados_diferencias,
  "summary"
)

diagnosticos_diferencias <- map_dfr(
  resultados_diferencias,
  "diagnostics"
)

seleccion_lags_diferencias <- map_dfr(
  resultados_diferencias,
  "lag_selection"
)


# ------------------------------------------------------------
# 18. TÉRMINO DE CORRECCIÓN DEL ERROR
# ------------------------------------------------------------

tabla_terminos_ecm <- especificaciones |>
  transmute(
    model_key,
    ecm_term = paste0(
      "ecm_",
      model_key,
      "_l1"
    )
  )

ajuste_ecm <- coeficientes_ecm |>
  left_join(
    tabla_terminos_ecm,
    by = "model_key"
  ) |>
  filter(
    term == ecm_term
  ) |>
  select(
    -ecm_term
  ) |>
  left_join(
    decision_cointegracion |>
      select(
        model_key,
        formal_engle_granger,
        cointegration_10pct,
        ecm_status
      ),
    by = "model_key"
  ) |>
  mutate(
    adjustment_validity = case_when(
      
      formal_engle_granger &
        cointegration_10pct &
        estimate < 0 &
        p_value < 0.10 ~
        paste0(
          "Ajuste válido y significativo ",
          "al 10%"
        ),
      
      formal_engle_granger &
        cointegration_10pct &
        (
          estimate >= 0 |
            p_value >= 0.10
        ) ~
        paste0(
          "Existe cointegración, pero el ",
          "ajuste ECM no es concluyente"
        ),
      
      TRUE ~
        paste0(
          "Coeficiente estimado con fines ",
          "indicativos"
        )
    ),
    
    porcentaje_ajuste_trimestral =
      -100 * estimate
  )

print(
  ajuste_ecm,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 19. WICKENS-BREUSCH DE UNA ETAPA
# ------------------------------------------------------------

resultados_wb <- list()

for (
  i in seq_len(
    nrow(especificaciones)
  )
) {
  
  spec_i <- especificaciones[
    i,
  ]
  
  cat(
    "\nEstimando Wickens-Breusch:",
    spec_i$model_label,
    "\n"
  )
  
  resultados_wb[[
    spec_i$model_key
  ]] <- ajustar_modelo_dinamico(
    datos = panel_modelos,
    spec = spec_i,
    model_type =
      "wickens_breusch",
    max_lag =
      max_lag_dinamico
  )
}


coeficientes_wb <- map_dfr(
  resultados_wb,
  "coefficients"
)

resumen_wb <- map_dfr(
  resultados_wb,
  "summary"
)

diagnosticos_wb <- map_dfr(
  resultados_wb,
  "diagnostics"
)

seleccion_lags_wb <- map_dfr(
  resultados_wb,
  "lag_selection"
)


# ------------------------------------------------------------
# 20. DELTA METHOD PARA EL LARGO PLAZO
# ------------------------------------------------------------

calcular_ratio_largo_plazo <- function(
    fit,
    vcov_matrix,
    numerator_term,
    denominator_term,
    model_key,
    variable_label
) {
  
  beta <- coef(
    fit
  )
  
  nombres <- names(
    beta
  )
  
  if (
    !numerator_term %in%
    nombres ||
    !denominator_term %in%
    nombres
  ) {
    
    return(
      tibble(
        model_key = model_key,
        variable = variable_label,
        numerator_term = numerator_term,
        denominator_term =
          denominator_term,
        estimate = NA_real_,
        std_error = NA_real_,
        statistic = NA_real_,
        p_value = NA_real_,
        lower_90 = NA_real_,
        upper_90 = NA_real_,
        lower_95 = NA_real_,
        upper_95 = NA_real_
      )
    )
  }
  
  a <- as.numeric(
    beta[
      denominator_term
    ]
  )
  
  b <- as.numeric(
    beta[
      numerator_term
    ]
  )
  
  estimacion <- -b / a
  
  gradiente <- setNames(
    rep(
      0,
      length(beta)
    ),
    nombres
  )
  
  # Derivada de -b/a respecto de a.
  gradiente[
    denominator_term
  ] <- b / a^2
  
  # Derivada respecto de b.
  gradiente[
    numerator_term
  ] <- -1 / a
  
  varianza <- as.numeric(
    t(gradiente) %*%
      vcov_matrix %*%
      gradiente
  )
  
  error_estandar <- ifelse(
    varianza >= 0,
    sqrt(varianza),
    NA_real_
  )
  
  estadistico <- estimacion /
    error_estandar
  
  p_value <- 2 * pnorm(
    abs(estadistico),
    lower.tail = FALSE
  )
  
  tibble(
    model_key = model_key,
    variable = variable_label,
    numerator_term = numerator_term,
    denominator_term =
      denominator_term,
    estimate = estimacion,
    std_error = error_estandar,
    statistic = estadistico,
    p_value = p_value,
    
    lower_90 =
      estimacion -
      qnorm(0.95) *
      error_estandar,
    
    upper_90 =
      estimacion +
      qnorm(0.95) *
      error_estandar,
    
    lower_95 =
      estimacion -
      qnorm(0.975) *
      error_estandar,
    
    upper_95 =
      estimacion +
      qnorm(0.975) *
      error_estandar
  )
}


elasticidades_wb <- list()
contador_wb <- 1L

for (
  i in seq_len(
    nrow(especificaciones)
  )
) {
  
  spec_i <- especificaciones[
    i,
  ]
  
  model_key_i <-
    spec_i$model_key[[1]]
  
  dependent_i <-
    spec_i$dependent[[1]]
  
  x1_i <-
    spec_i$x1[[1]]
  
  x2_i <-
    spec_i$x2[[1]]
  
  fit_i <-
    resultados_wb[[
      model_key_i
    ]]$fit
  
  vcov_i <-
    resultados_wb[[
      model_key_i
    ]]$vcov_hac
  
  denominador <- paste0(
    "l1_",
    dependent_i
  )
  
  elasticidades_wb[[
    contador_wb
  ]] <- calcular_ratio_largo_plazo(
    fit = fit_i,
    vcov_matrix = vcov_i,
    numerator_term = paste0(
      "l1_",
      x1_i
    ),
    denominator_term =
      denominador,
    model_key =
      model_key_i,
    variable_label =
      x1_i
  )
  
  contador_wb <-
    contador_wb + 1L
  
  elasticidades_wb[[
    contador_wb
  ]] <- calcular_ratio_largo_plazo(
    fit = fit_i,
    vcov_matrix = vcov_i,
    numerator_term = paste0(
      "l1_",
      x2_i
    ),
    denominator_term =
      denominador,
    model_key =
      model_key_i,
    variable_label =
      x2_i
  )
  
  contador_wb <-
    contador_wb + 1L
}

elasticidades_largo_plazo_wb <- bind_rows(
  elasticidades_wb
) |>
  left_join(
    especificaciones |>
      select(
        model_key,
        model_label,
        formal_engle_granger
      ),
    by = "model_key"
  ) |>
  left_join(
    decision_cointegracion |>
      select(
        model_key,
        cointegration_10pct,
        ecm_status
      ),
    by = "model_key"
  ) |>
  mutate(
    interpretation = case_when(
      
      formal_engle_granger &
        cointegration_10pct ~
        paste0(
          "Elasticidad de largo plazo ",
          "con respaldo de cointegración"
        ),
      
      formal_engle_granger &
        !cointegration_10pct ~
        paste0(
          "Elasticidad sólo indicativa: ",
          "Engle-Granger no confirma ",
          "cointegración"
        ),
      
      TRUE ~
        paste0(
          "Resultado ampliado indicativo: ",
          "incluye un control I(0)"
        )
    )
  )

print(
  elasticidades_largo_plazo_wb,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 21. COEFICIENTE DE AJUSTE WICKENS-BREUSCH
# ------------------------------------------------------------

tabla_lag_dependiente <- especificaciones |>
  transmute(
    model_key,
    lag_dependent = paste0(
      "l1_",
      dependent
    )
  )

ajuste_wb <- coeficientes_wb |>
  left_join(
    tabla_lag_dependiente,
    by = "model_key"
  ) |>
  filter(
    term == lag_dependent
  ) |>
  select(
    -lag_dependent
  ) |>
  left_join(
    decision_cointegracion |>
      select(
        model_key,
        formal_engle_granger,
        cointegration_10pct,
        ecm_status
      ),
    by = "model_key"
  ) |>
  mutate(
    adjustment_interpretation = case_when(
      
      estimate < 0 &
        p_value < 0.10 &
        formal_engle_granger &
        cointegration_10pct ~
        paste0(
          "Ajuste negativo y significativo; ",
          "verificación favorable"
        ),
      
      estimate < 0 &
        p_value < 0.10 ~
        paste0(
          "Ajuste negativo significativo, ",
          "pero interpretación indicativa"
        ),
      
      TRUE ~
        paste0(
          "No se verifica un mecanismo de ",
          "ajuste concluyente"
        )
    )
  )


# ------------------------------------------------------------
# 22. ELASTICIDADES ENGLE-GRANGER
# ------------------------------------------------------------

variables_largo_plazo <- especificaciones |>
  select(
    model_key,
    x1,
    x2
  ) |>
  pivot_longer(
    cols = c(
      x1,
      x2
    ),
    names_to = "variable_role",
    values_to = "variable"
  )

elasticidades_largo_plazo_eg <-
  coeficientes_eg |>
  filter(
    variant == "baseline"
  ) |>
  inner_join(
    variables_largo_plazo,
    by = c(
      "model_key",
      "term" = "variable"
    )
  ) |>
  transmute(
    model_key,
    model_label,
    variable = term,
    variable_role,
    estimate_eg = estimate,
    std_error_eg = std_error,
    statistic_eg = statistic,
    p_value_eg = p_value
  ) |>
  left_join(
    decision_cointegracion |>
      select(
        model_key,
        formal_engle_granger,
        cointegration_10pct,
        ecm_status
      ),
    by = "model_key"
  )


# ------------------------------------------------------------
# 23. COMPARAR ENGLE-GRANGER Y WICKENS-BREUSCH
# ------------------------------------------------------------

comparacion_largo_plazo <-
  elasticidades_largo_plazo_eg |>
  left_join(
    elasticidades_largo_plazo_wb |>
      select(
        model_key,
        variable,
        estimate_wb = estimate,
        std_error_wb = std_error,
        p_value_wb = p_value,
        lower_90_wb = lower_90,
        upper_90_wb = upper_90,
        interpretation_wb =
          interpretation
      ),
    by = c(
      "model_key",
      "variable"
    )
  ) |>
  mutate(
    difference_wb_minus_eg =
      estimate_wb -
      estimate_eg,
    
    same_sign = case_when(
      is.na(estimate_eg) |
        is.na(estimate_wb) ~
        NA,
      
      sign(estimate_eg) ==
        sign(estimate_wb) ~
        TRUE,
      
      TRUE ~
        FALSE
    )
  )

print(
  comparacion_largo_plazo,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 24. ELASTICIDADES DE CORTO PLAZO
# ------------------------------------------------------------

variables_corto_plazo <- especificaciones |>
  transmute(
    model_key,
    
    d_x1 = paste0(
      "d_",
      x1
    ),
    
    d_x2 = paste0(
      "d_",
      x2
    )
  ) |>
  pivot_longer(
    cols = c(
      d_x1,
      d_x2
    ),
    names_to = "variable_role",
    values_to = "term"
  )

elasticidades_corto_plazo_ecm <-
  coeficientes_ecm |>
  inner_join(
    variables_corto_plazo,
    by = c(
      "model_key",
      "term"
    )
  ) |>
  mutate(
    method =
      "ECM de dos etapas"
  )

elasticidades_corto_plazo_diferencias <-
  coeficientes_diferencias |>
  inner_join(
    variables_corto_plazo,
    by = c(
      "model_key",
      "term"
    )
  ) |>
  mutate(
    method =
      "Modelo en diferencias"
  )

elasticidades_corto_plazo <- bind_rows(
  elasticidades_corto_plazo_ecm,
  elasticidades_corto_plazo_diferencias
) |>
  left_join(
    decision_cointegracion |>
      select(
        model_key,
        cointegration_10pct,
        ecm_status
      ),
    by = "model_key"
  )


# ------------------------------------------------------------
# 25. COMPARAR AJUSTE ECM Y MODELO EN DIFERENCIAS
# ------------------------------------------------------------

comparacion_modelos_corto_plazo <-
  bind_rows(
    resumen_ecm,
    resumen_diferencias
  ) |>
  left_join(
    decision_cointegracion |>
      select(
        model_key,
        formal_engle_granger,
        cointegration_10pct,
        ecm_status
      ),
    by = "model_key"
  ) |>
  mutate(
    recommended_model = case_when(
      
      model_type == "ecm" &
        formal_engle_granger &
        cointegration_10pct ~
        "Candidato válido",
      
      model_type == "ecm" ~
        "Sólo indicativo",
      
      model_type ==
        "differences" ~
        "Disponible aun sin cointegración",
      
      TRUE ~
        NA_character_
    )
  )


# ------------------------------------------------------------
# 26. UNIR DIAGNÓSTICOS
# ------------------------------------------------------------

diagnosticos_todos <- bind_rows(
  diagnosticos_ecm,
  diagnosticos_diferencias,
  diagnosticos_wb
)


# ------------------------------------------------------------
# 27. GRÁFICO DE RESIDUOS ENGLE-GRANGER
# ------------------------------------------------------------

grafico_residuos_eg <- residuos_eg |>
  filter(
    variant == "baseline"
  ) |>
  ggplot(
    aes(
      x = fecha,
      y = residual
    )
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_line(
    linewidth = 0.7
  ) +
  facet_wrap(
    ~ model_label,
    scales = "free_y",
    ncol = 1
  ) +
  labs(
    title =
      "Residuos de las ecuaciones de largo plazo",
    subtitle =
      "Primera etapa de Engle-Granger",
    x = NULL,
    y = "Residuo"
  ) +
  theme_minimal(
    base_size = 11
  )

ggsave(
  filename = here::here(
    "outputs",
    "figures",
    "09_residuos_engle_granger.png"
  ),
  plot = grafico_residuos_eg,
  width = 11,
  height = 8,
  dpi = 300
)


# ------------------------------------------------------------
# 28. GRÁFICO COMPARATIVO DE ELASTICIDADES
# ------------------------------------------------------------

grafico_elasticidades <- comparacion_largo_plazo |>
  select(
    model_key,
    model_label,
    variable,
    estimate_eg,
    estimate_wb
  ) |>
  pivot_longer(
    cols = c(
      estimate_eg,
      estimate_wb
    ),
    names_to = "method",
    values_to = "elasticity"
  ) |>
  mutate(
    method = recode(
      method,
      estimate_eg =
        "Engle-Granger",
      estimate_wb =
        "Wickens-Breusch"
    )
  ) |>
  ggplot(
    aes(
      x = variable,
      y = elasticity,
      group = method
    )
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  geom_point(
    size = 2.5,
    position =
      position_dodge(
        width = 0.4
      )
  ) +
  facet_wrap(
    ~ model_label,
    scales = "free_x"
  ) +
  labs(
    title =
      "Elasticidades de largo plazo",
    subtitle =
      paste0(
        "Comparación Engle-Granger y ",
        "Wickens-Breusch"
      ),
    x = NULL,
    y = "Elasticidad"
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    axis.text.x =
      element_text(
        angle = 35,
        hjust = 1
      )
  )

ggsave(
  filename = here::here(
    "outputs",
    "figures",
    "09_comparacion_elasticidades_largo_plazo.png"
  ),
  plot = grafico_elasticidades,
  width = 12,
  height = 7,
  dpi = 300
)


# ------------------------------------------------------------
# 29. PREPARAR CONFIGURACIÓN PARA EXCEL
# ------------------------------------------------------------

configuracion_excel <- especificaciones |>
  mutate(
    stationary_controls =
      map_chr(
        stationary_controls,
        ~ ifelse(
          length(.x) == 0,
          "Ninguno",
          paste(
            .x,
            collapse = " + "
          )
        )
      )
  )


# ------------------------------------------------------------
# 30. GUARDAR EXCEL
# ------------------------------------------------------------

writexl::write_xlsx(
  list(
    
    configuracion =
      configuracion_excel,
    
    criticos_eg =
      tabla_criticos_eg,
    
    eg_resumen =
      resumen_eg,
    
    eg_coeficientes =
      coeficientes_eg,
    
    eg_test_residuos =
      tests_eg,
    
    eg_decision =
      decision_cointegracion,
    
    eg_lags =
      seleccion_lags_eg,
    
    ecm_resumen =
      resumen_ecm,
    
    ecm_coeficientes =
      coeficientes_ecm,
    
    ecm_ajuste =
      ajuste_ecm,
    
    ecm_lags =
      seleccion_lags_ecm,
    
    diferencias_resumen =
      resumen_diferencias,
    
    diferencias_coef =
      coeficientes_diferencias,
    
    diferencias_lags =
      seleccion_lags_diferencias,
    
    wb_resumen =
      resumen_wb,
    
    wb_coeficientes =
      coeficientes_wb,
    
    wb_ajuste =
      ajuste_wb,
    
    wb_elasticidades =
      elasticidades_largo_plazo_wb,
    
    wb_lags =
      seleccion_lags_wb,
    
    largo_plazo_compara =
      comparacion_largo_plazo,
    
    corto_plazo =
      elasticidades_corto_plazo,
    
    modelos_compara =
      comparacion_modelos_corto_plazo,
    
    diagnosticos =
      diagnosticos_todos
  ),
  
  path = here::here(
    "outputs",
    "tables",
    "engle_granger_ecm_wickens_breusch.xlsx"
  )
)


# ------------------------------------------------------------
# 31. GUARDAR OBJETOS RDS
# ------------------------------------------------------------

saveRDS(
  resultados_eg,
  here::here(
    "outputs",
    "models",
    "resultados_engle_granger.rds"
  )
)

saveRDS(
  decision_cointegracion,
  here::here(
    "outputs",
    "models",
    "decision_cointegracion_engle_granger.rds"
  )
)

saveRDS(
  resultados_ecm,
  here::here(
    "outputs",
    "models",
    "modelos_ecm_dos_etapas.rds"
  )
)

saveRDS(
  resultados_diferencias,
  here::here(
    "outputs",
    "models",
    "modelos_en_diferencias.rds"
  )
)

saveRDS(
  resultados_wb,
  here::here(
    "outputs",
    "models",
    "modelos_wickens_breusch.rds"
  )
)

saveRDS(
  comparacion_largo_plazo,
  here::here(
    "outputs",
    "models",
    "comparacion_elasticidades_largo_plazo.rds"
  )
)

saveRDS(
  elasticidades_corto_plazo,
  here::here(
    "outputs",
    "models",
    "elasticidades_corto_plazo.rds"
  )
)

saveRDS(
  panel_modelos,
  here::here(
    "data",
    "processed",
    "panel_engle_granger_ecm_2004_2025.rds"
  )
)


# ------------------------------------------------------------
# 32. CONTROLES FINALES
# ------------------------------------------------------------

stopifnot(
  
  nrow(especificaciones) ==
    3,
  
  nrow(resumen_eg) ==
    6,
  
  nrow(
    decision_cointegracion
  ) ==
    3,
  
  length(
    resultados_ecm
  ) ==
    3,
  
  length(
    resultados_diferencias
  ) ==
    3,
  
  length(
    resultados_wb
  ) ==
    3,
  
  nrow(
    elasticidades_largo_plazo_wb
  ) ==
    6,
  
  nrow(
    comparacion_largo_plazo
  ) ==
    6
)


# ------------------------------------------------------------
# 33. MOSTRAR RESULTADOS PRINCIPALES
# ------------------------------------------------------------

cat(
  "\n\n============================================================",
  "\nDECISIÓN ENGLE-GRANGER",
  "\n============================================================\n"
)

decision_cointegracion |>
  select(
    model_label,
    selected_lag,
    statistic,
    critical_10pct,
    cointegration_10pct,
    validity,
    ecm_status
  ) |>
  print(
    n = Inf,
    width = Inf
  )


cat(
  "\n\n============================================================",
  "\nAJUSTE DE LOS ECM",
  "\n============================================================\n"
)

ajuste_ecm |>
  select(
    model_label,
    estimate,
    std_error,
    p_value,
    porcentaje_ajuste_trimestral,
    adjustment_validity
  ) |>
  print(
    n = Inf,
    width = Inf
  )


cat(
  "\n\n============================================================",
  "\nELASTICIDADES WICKENS-BREUSCH",
  "\n============================================================\n"
)

elasticidades_largo_plazo_wb |>
  select(
    model_label,
    variable,
    estimate,
    std_error,
    p_value,
    lower_90,
    upper_90,
    interpretation
  ) |>
  print(
    n = Inf,
    width = Inf
  )


cat(
  "\n\n============================================================",
  "\nCOMPARACIÓN DE ELASTICIDADES DE LARGO PLAZO",
  "\n============================================================\n"
)

comparacion_largo_plazo |>
  select(
    model_label,
    variable,
    estimate_eg,
    estimate_wb,
    difference_wb_minus_eg,
    same_sign,
    cointegration_10pct,
    ecm_status
  ) |>
  print(
    n = Inf,
    width = Inf
  )


# ------------------------------------------------------------
# 34. MENSAJE FINAL
# ------------------------------------------------------------

cat(
  "\n============================================================",
  "\nSCRIPT 09 FINALIZADO",
  "\n============================================================",
  
  "\nEcuaciones Engle-Granger estimadas:",
  nrow(resumen_eg),
  
  "\nDecisiones principales de cointegración:",
  nrow(decision_cointegracion),
  
  "\nModelos ECM estimados:",
  length(resultados_ecm),
  
  "\nModelos en diferencias estimados:",
  length(resultados_diferencias),
  
  "\nModelos Wickens-Breusch estimados:",
  length(resultados_wb),
  
  "\nNivel principal de significancia:",
  "10%",
  
  "\nMáximo de rezagos dinámicos:",
  max_lag_dinamico,
  
  "\nSelección dinámica:",
  "BIC y reducción general-to-specific",
  
  "\nErrores estándar:",
  "HAC/Newey-West, rezago 4",
  
  "\nControl estacional:",
  "Dummies Q2, Q3 y Q4",
  
  "\n\nArchivos generados:",
  
  "\n- outputs/tables/engle_granger_ecm_wickens_breusch.xlsx",
  
  "\n- outputs/models/resultados_engle_granger.rds",
  
  "\n- outputs/models/decision_cointegracion_engle_granger.rds",
  
  "\n- outputs/models/modelos_ecm_dos_etapas.rds",
  
  "\n- outputs/models/modelos_en_diferencias.rds",
  
  "\n- outputs/models/modelos_wickens_breusch.rds",
  
  "\n- outputs/models/comparacion_elasticidades_largo_plazo.rds",
  
  "\n- outputs/models/elasticidades_corto_plazo.rds",
  
  "\n- data/processed/panel_engle_granger_ecm_2004_2025.rds",
  
  "\n- figures/09_residuos_engle_granger.png",
  
  "\n- figures/09_comparacion_elasticidades_largo_plazo.png",
  
  "\n============================================================",
  "\n"
)


# ------------------------------------------------------------
# 35. DECISIÓN INTEGRAL DE ENGLE-GRANGER
# ------------------------------------------------------------

decision_eg_integral <- tests_eg |>
  filter(
    criterio %in% c("AIC", "BIC")
  ) |>
  select(
    model_key,
    model_label,
    formal_engle_granger,
    variant,
    criterio,
    statistic,
    cointegration_10pct,
    cointegration_5pct
  ) |>
  pivot_wider(
    names_from = c(
      variant,
      criterio
    ),
    values_from = c(
      statistic,
      cointegration_10pct,
      cointegration_5pct
    ),
    names_sep = "_"
  ) |>
  mutate(
    evidencia_eg = case_when(

      !formal_engle_granger &
        cointegration_10pct_seasonal_dummies_AIC &
        cointegration_10pct_seasonal_dummies_BIC ~
        paste0(
          "Evidencia indicativa de cointegración ",
          "al controlar estacionalidad; incluye control I(0)"
        ),

      formal_engle_granger &
        cointegration_10pct_baseline_AIC &
        cointegration_10pct_baseline_BIC ~
        "Evidencia robusta de cointegración sin dummies",

      formal_engle_granger &
        xor(
          cointegration_10pct_baseline_AIC,
          cointegration_10pct_baseline_BIC
        ) ~
        paste0(
          "Evidencia mixta de cointegración; ",
          "sensible al criterio de rezagos"
        ),

      formal_engle_granger &
        !cointegration_10pct_baseline_AIC &
        !cointegration_10pct_baseline_BIC &
        cointegration_10pct_seasonal_dummies_AIC &
        cointegration_10pct_seasonal_dummies_BIC ~
        paste0(
          "Cointegración sólo al controlar ",
          "la estacionalidad"
        ),

      TRUE ~
        "No se obtiene evidencia concluyente de cointegración"
    ),

    clasificacion_para_tp3 = case_when(

      model_key == "imports" ~
        paste0(
          "Evidencia mixta: mantener ECM como resultado ",
          "indicativo y contrastar con Johansen"
        ),

      model_key == "exports_classic" ~
        paste0(
          "Evidencia favorable con dummies estacionales; ",
          "contrastar con Johansen"
        ),

      model_key == "exports_expanded" ~
        paste0(
          "Robustez indicativa; commodities es I(0)"
        ),

      TRUE ~
        evidencia_eg
    )
  )

print(
  decision_eg_integral,
  n = Inf,
  width = Inf
)

readr::write_csv(
  decision_eg_integral,
  here::here(
    "outputs",
    "tables",
    "decision_engle_granger_integral.csv"
  )
)

saveRDS(
  decision_eg_integral,
  here::here(
    "outputs",
    "models",
    "decision_engle_granger_integral.rds"
  )
)