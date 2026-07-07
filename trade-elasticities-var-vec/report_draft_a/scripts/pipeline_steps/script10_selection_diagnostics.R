
# ============================================================
# TP3 - SERIES DE TIEMPO
# SCRIPT 10:
# SELECCIÓN DE REZAGOS Y DIAGNÓSTICOS DE MODELOS VAR
#
# Sistemas:
#
# 1. Importaciones:
#    ln(importaciones), ln(PIB Argentina), ln(ITCRM)
#
# 2. Exportaciones clásicas:
#    ln(exportaciones), ln(PIB socios), ln(ITCRM)
#
# 3. Exportaciones ampliadas:
#    mismas variables endógenas del sistema clásico,
#    con commodities como control exógeno I(0)
#
# Variantes:
#    - Seasonal: constante y dummies trimestrales
#    - No seasonal: constante sin dummies trimestrales
#
# Funciones de la cátedra:
#    - vcorr_res()
#    - VAR_white_no_cross()
#    - VAR_lag_exclusion_wald()
#
# Nivel principal de significancia: 10%
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
# 1. INSTALAR PAQUETES NECESARIOS
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
  "vars",
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
  install.packages(paquetes_faltantes)
}


# ------------------------------------------------------------
# 2. CREAR CARPETAS DE SALIDA
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
# 3. LOCALIZAR LAS FUNCIONES DE LA CÁTEDRA
# ------------------------------------------------------------

localizar_archivo_funcion <- function(
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
        "No se encontró alguno de estos archivos:\n",
        paste(
          nombres_posibles,
          collapse = ", "
        ),
        "\n\nDebe guardarse en:\n",
        "scripts/required_scripts/"
      )
    )
  }

  rutas_existentes[1]
}


ruta_vcorr <- localizar_archivo_funcion(
  c(
    "vcorr_res.R",
    "VCORR_RES.R"
  )
)

ruta_white <- localizar_archivo_funcion(
  c(
    "VAR_white_no_cross.R",
    "var_white_no_cross.R"
  )
)

ruta_exclusion <- localizar_archivo_funcion(
  c(
    "VAR_lag_exclusion_wald.R",
    "var_lag_exclusion_wald.R"
  )
)


# ------------------------------------------------------------
# 4. CARGAR LAS FUNCIONES DE LA CÁTEDRA
# ------------------------------------------------------------

source(
  ruta_vcorr,
  encoding = "UTF-8"
)

source(
  ruta_white,
  encoding = "UTF-8"
)

source(
  ruta_exclusion,
  encoding = "UTF-8"
)


funciones_requeridas <- c(
  "vcorr_res",
  "VAR_white_no_cross",
  "VAR_lag_exclusion_wald"
)

funciones_no_cargadas <- funciones_requeridas[
  !vapply(
    funciones_requeridas,
    exists,
    mode = "function",
    FUN.VALUE = logical(1)
  )
]

if (length(funciones_no_cargadas) > 0) {

  stop(
    paste0(
      "No se cargaron correctamente estas funciones:\n",
      paste(
        funciones_no_cargadas,
        collapse = ", "
      )
    )
  )
}


cat(
  "\n============================================================",
  "\nFUNCIONES DE LA CÁTEDRA CARGADAS",
  "\n============================================================",

  "\nvcorr_res:",
  ruta_vcorr,

  "\nVAR_white_no_cross:",
  ruta_white,

  "\nVAR_lag_exclusion_wald:",
  ruta_exclusion,

  "\n============================================================",
  "\n"
)


# ------------------------------------------------------------
# 5. CARGAR PANEL MAESTRO
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
  dplyr::arrange(
    .data$periodo
  ) |>
  dplyr::mutate(

    commodity_c =
      .data$ln_commodity_price_index -
      mean(
        .data$ln_commodity_price_index,
        na.rm = TRUE
      )
  )


# ------------------------------------------------------------
# 6. MOSTRAR COBERTURA
# ------------------------------------------------------------

cat(
  "\n============================================================",
  "\nMUESTRA DEL SCRIPT 10",
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
# 7. PARÁMETROS GENERALES
# ------------------------------------------------------------

nivel_significancia <- 0.10

max_lag_var <- 8L

lag_correlacion_objetivo <- 12L

lags_arch <- 4L


# ------------------------------------------------------------
# 8. CONFIGURACIÓN DE LOS SISTEMAS
# ------------------------------------------------------------

config_sistemas <- tibble::tibble(

  model_key = c(
    "imports",
    "exports_classic",
    "exports_expanded"
  ),

  model_label = c(
    "Importaciones: PIB argentino e ITCRM",

    "Exportaciones: PIB socios e ITCRM",

    paste0(
      "Exportaciones ampliadas: ",
      "PIB socios, ITCRM y commodities"
    )
  ),

  endogenous = list(

    c(
      "ln_importaciones_reales",
      "ln_pib_real",
      "ln_itcrm"
    ),

    c(
      "ln_exportaciones_reales",
      "ln_pib_socios",
      "ln_itcrm"
    ),

    c(
      "ln_exportaciones_reales",
      "ln_pib_socios",
      "ln_itcrm"
    )
  ),

  exogenous = list(
    character(0),
    character(0),
    "commodity_c"
  ),

  model_role = c(
    "Sistema principal",
    "Sistema principal",
    "Robustez con control exógeno I(0)"
  ),

  johansen_role = c(

    paste0(
      "Sistema principal; formal o indicativo ",
      "según diagnósticos"
    ),

    paste0(
      "Sistema principal; formal o indicativo ",
      "según diagnósticos"
    ),

    paste0(
      "Robustez con commodities como ",
      "variable exógena I(0)"
    )
  )
)


# ------------------------------------------------------------
# 9. VARIANTES DETERMINÍSTICAS
# ------------------------------------------------------------

config_variantes <- tibble::tibble(

  variant = c(
    "seasonal",
    "no_season"
  ),

  variant_label = c(
    "Constante y dummies estacionales",
    "Constante sin dummies estacionales"
  ),

  season = c(
    4L,
    NA_integer_
  ),

  main_variant = c(
    TRUE,
    FALSE
  )
)


# ------------------------------------------------------------
# 10. VERIFICAR VARIABLES
# ------------------------------------------------------------

variables_requeridas <- unique(
  c(
    "periodo",
    "anio",
    "trimestre",
    unlist(config_sistemas$endogenous),
    unlist(config_sistemas$exogenous)
  )
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


variables_numericas <- setdiff(
  variables_requeridas,
  c(
    "periodo",
    "anio",
    "trimestre"
  )
)


tabla_faltantes <- panel_maestro |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(
        variables_numericas
      ),
      ~ sum(is.na(.x))
    )
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "variable",
    values_to = "faltantes"
  )


print(
  tabla_faltantes,
  n = Inf,
  width = Inf
)


if (any(tabla_faltantes$faltantes > 0)) {

  stop(
    paste0(
      "Existen datos faltantes en las variables del VAR.\n",
      "Revisa tabla_faltantes."
    )
  )
}


# ------------------------------------------------------------
# 11. CREAR OBJETOS ts
# ------------------------------------------------------------

inicio_ts <- c(
  panel_maestro$anio[1],
  panel_maestro$trimestre[1]
)


crear_matriz_ts <- function(
    variables
) {

  matriz <- as.matrix(
    panel_maestro[
      ,
      variables,
      drop = FALSE
    ]
  )

  colnames(matriz) <- variables

  stats::ts(
    matriz,
    start = inicio_ts,
    frequency = 4
  )
}


datos_sistemas <- stats::setNames(
  vector(
    mode = "list",
    length = nrow(config_sistemas)
  ),
  config_sistemas$model_key
)


for (
  i in seq_len(
    nrow(config_sistemas)
  )
) {

  model_key_i <-
    config_sistemas$model_key[[i]]

  endogenous_i <-
    config_sistemas$endogenous[[i]]

  exogenous_i <-
    config_sistemas$exogenous[[i]]

  y_i <- crear_matriz_ts(
    endogenous_i
  )

  if (length(exogenous_i) > 0) {

    x_i <- crear_matriz_ts(
      exogenous_i
    )

  } else {

    x_i <- NULL
  }

  datos_sistemas[[model_key_i]] <- list(

    y =
      y_i,

    exogen =
      x_i,

    endogenous_names =
      endogenous_i,

    exogenous_names =
      exogenous_i
  )
}


# ------------------------------------------------------------
# 12. CONTROLAR LOS SISTEMAS
# ------------------------------------------------------------

tabla_control_sistemas <- purrr::map_dfr(

  seq_len(
    nrow(config_sistemas)
  ),

  function(i) {

    model_key_i <-
      config_sistemas$model_key[[i]]

    datos_i <-
      datos_sistemas[[model_key_i]]

    tibble::tibble(

      model_key =
        model_key_i,

      model_label =
        config_sistemas$model_label[[i]],

      observations =
        nrow(datos_i$y),

      endogenous_variables =
        ncol(datos_i$y),

      endogenous_names =
        paste(
          colnames(datos_i$y),
          collapse = " + "
        ),

      exogenous_names = if (
        is.null(datos_i$exogen)
      ) {

        "Ninguna"

      } else {

        paste(
          colnames(datos_i$exogen),
          collapse = " + "
        )
      },

      frequency =
        stats::frequency(datos_i$y)
    )
  }
)


print(
  tabla_control_sistemas,
  n = Inf,
  width = Inf
)


stopifnot(
  all(
    tabla_control_sistemas$observations == 88
  ),

  all(
    tabla_control_sistemas$endogenous_variables == 3
  ),

  all(
    tabla_control_sistemas$frequency == 4
  )
)


# ------------------------------------------------------------
# 13. FUNCIONES PARA ESTIMAR VAR Y VARselect
# ------------------------------------------------------------

crear_clave_modelo <- function(
    model_key,
    variant,
    p
) {

  paste0(
    model_key,
    "__",
    variant,
    "__p",
    p
  )
}


estimar_var <- function(
    datos_sistema,
    p,
    season_value = NA_integer_
) {

  argumentos <- list(

    y =
      datos_sistema$y,

    p =
      as.integer(p),

    type =
      "const"
  )

  if (!is.na(season_value)) {

    argumentos$season <-
      as.integer(season_value)
  }

  if (!is.null(datos_sistema$exogen)) {

    argumentos$exogen <-
      datos_sistema$exogen
  }

  do.call(
    vars::VAR,
    argumentos
  )
}


ejecutar_varselect <- function(
    datos_sistema,
    lag_max,
    season_value = NA_integer_
) {

  argumentos <- list(

    y =
      datos_sistema$y,

    lag.max =
      as.integer(lag_max),

    type =
      "const"
  )

  if (!is.na(season_value)) {

    argumentos$season <-
      as.integer(season_value)
  }

  if (!is.null(datos_sistema$exogen)) {

    argumentos$exogen <-
      datos_sistema$exogen
  }

  do.call(
    vars::VARselect,
    argumentos
  )
}


# ------------------------------------------------------------
# 14. ORDENAR LOS RESULTADOS DE VARselect
# ------------------------------------------------------------

ordenar_varselect <- function(
    objeto_select,
    model_key_arg,
    model_label_arg,
    variant_arg
) {

  criterios_df <- as.data.frame(
    objeto_select$criteria,
    check.names = FALSE
  )

  criterios <- criterios_df |>
    tibble::rownames_to_column(
      var = "criterion"
    ) |>
    tidyr::pivot_longer(
      cols = -1,
      names_to = "lag_label",
      values_to = "criterion_value"
    ) |>
    dplyr::mutate(

      model_key =
        .env$model_key_arg,

      model_label =
        .env$model_label_arg,

      variant =
        .env$variant_arg,

      p = as.integer(
        readr::parse_number(
          .data$lag_label
        )
      ),

      .before = 1
    ) |>
    dplyr::select(
      -.data$lag_label
    )


  seleccion <- objeto_select$selection


  seleccion_tabla <- tibble::tibble(

    model_key =
      model_key_arg,

    model_label =
      model_label_arg,

    variant =
      variant_arg,

    p_AIC = as.integer(
      unname(
        seleccion["AIC(n)"]
      )
    ),

    p_HQ = as.integer(
      unname(
        seleccion["HQ(n)"]
      )
    ),

    p_SC = as.integer(
      unname(
        seleccion["SC(n)"]
      )
    ),

    p_FPE = as.integer(
      unname(
        seleccion["FPE(n)"]
      )
    )
  )


  list(
    criteria =
      criterios,

    selection =
      seleccion_tabla
  )
}


# ------------------------------------------------------------
# 15. TEST SERIAL MULTIVARIADO SEGURO
# ------------------------------------------------------------

test_serial_seguro <- function(
    modelo_var,
    tipo = "PT.adjusted",
    lag_objetivo = 12L
) {

  p_modelo <- as.integer(
    modelo_var$p
  )

  observaciones <- as.integer(
    modelo_var$obs
  )

  maximo_factible <- max(
    p_modelo + 1L,
    floor(
      observaciones / 3
    )
  )

  lag_inicial <- min(
    as.integer(lag_objetivo),
    maximo_factible
  )

  lag_minimo <- max(
    p_modelo + 1L,
    2L
  )

  if (lag_inicial < lag_minimo) {
    lag_inicial <- lag_minimo
  }

  lags_candidatos <- seq(
    from = lag_inicial,
    to = lag_minimo,
    by = -1L
  )

  ultimo_error <- NA_character_


  for (lag_i in lags_candidatos) {

    resultado_i <- tryCatch(

      {
        if (
          tipo %in%
          c(
            "BG",
            "ES"
          )
        ) {

          vars::serial.test(
            modelo_var,
            lags.bg = lag_i,
            type = tipo
          )

        } else {

          vars::serial.test(
            modelo_var,
            lags.pt = lag_i,
            type = tipo
          )
        }
      },

      error = function(e) {
        e
      }
    )


    if (!inherits(resultado_i, "error")) {

      return(
        tibble::tibble(

          test_type =
            tipo,

          lag_used =
            lag_i,

          statistic =
            as.numeric(
              resultado_i$serial$statistic
            ),

          p_value =
            as.numeric(
              resultado_i$serial$p.value
            ),

          status =
            "OK",

          error =
            NA_character_
        )
      )

    } else {

      ultimo_error <- conditionMessage(
        resultado_i
      )
    }
  }


  tibble::tibble(

    test_type =
      tipo,

    lag_used =
      NA_integer_,

    statistic =
      NA_real_,

    p_value =
      NA_real_,

    status =
      "ERROR",

    error =
      ultimo_error
  )
}


# ------------------------------------------------------------
# 16. ESTIMAR VARselect Y VAR CANDIDATOS
# ------------------------------------------------------------

lista_criterios_ic <- list()

lista_selecciones_ic <- list()

lista_diagnosticos_candidatos <- list()

modelos_var_candidatos <- list()

contador_ic <- 1L

contador_diagnosticos <- 1L


for (
  i in seq_len(
    nrow(config_sistemas)
  )
) {

  model_key_i <-
    config_sistemas$model_key[[i]]

  model_label_i <-
    config_sistemas$model_label[[i]]

  datos_i <-
    datos_sistemas[[model_key_i]]


  for (
    j in seq_len(
      nrow(config_variantes)
    )
  ) {

    variant_i <-
      config_variantes$variant[[j]]

    season_i <-
      config_variantes$season[[j]]


    cat(
      "\n============================================================",
      "\nVARselect:",
      model_label_i,
      "\nVariante:",
      variant_i,
      "\n============================================================\n"
    )


    seleccion_i <- ejecutar_varselect(

      datos_sistema =
        datos_i,

      lag_max =
        max_lag_var,

      season_value =
        season_i
    )


    seleccion_ordenada <- ordenar_varselect(

      objeto_select =
        seleccion_i,

      model_key_arg =
        model_key_i,

      model_label_arg =
        model_label_i,

      variant_arg =
        variant_i
    )


    lista_criterios_ic[[contador_ic]] <-
      seleccion_ordenada$criteria

    lista_selecciones_ic[[contador_ic]] <-
      seleccion_ordenada$selection

    contador_ic <- contador_ic + 1L


    # --------------------------------------------------------
    # Estimar todos los VAR candidatos: p = 1,...,8
    # --------------------------------------------------------

    for (
      p_i in seq_len(max_lag_var)
    ) {

      cat(
        "Estimando p =",
        p_i,
        "\n"
      )


      clave_i <- crear_clave_modelo(

        model_key =
          model_key_i,

        variant =
          variant_i,

        p =
          p_i
      )


      ajuste_i <- tryCatch(

        estimar_var(

          datos_sistema =
            datos_i,

          p =
            p_i,

          season_value =
            season_i
        ),

        error = function(e) {
          e
        }
      )


      if (inherits(ajuste_i, "error")) {

        lista_diagnosticos_candidatos[[
          contador_diagnosticos
        ]] <- tibble::tibble(

          model_key =
            model_key_i,

          model_label =
            model_label_i,

          variant =
            variant_i,

          p =
            p_i,

          observations =
            NA_integer_,

          pt_lag =
            NA_integer_,

          pt_statistic =
            NA_real_,

          pt_p_value =
            NA_real_,

          bg_lag =
            NA_integer_,

          bg_statistic =
            NA_real_,

          bg_p_value =
            NA_real_,

          maximum_root_modulus =
            NA_real_,

          roots_inside_unit_circle =
            NA,

          status =
            "ERROR",

          error =
            conditionMessage(ajuste_i)
        )

        contador_diagnosticos <-
          contador_diagnosticos + 1L

        next
      }


      modelos_var_candidatos[[clave_i]] <-
        ajuste_i


      # ------------------------------------------------------
      # Test Portmanteau ajustado
      # ------------------------------------------------------

      pt_i <- test_serial_seguro(

        modelo_var =
          ajuste_i,

        tipo =
          "PT.adjusted",

        lag_objetivo =
          lag_correlacion_objetivo
      )


      # ------------------------------------------------------
      # Test LM de Breusch-Godfrey
      # ------------------------------------------------------

      bg_i <- test_serial_seguro(

        modelo_var =
          ajuste_i,

        tipo =
          "BG",

        lag_objetivo =
          lag_correlacion_objetivo
      )


      # ------------------------------------------------------
      # Raíces del VAR
      # ------------------------------------------------------

      raices_i <- tryCatch(

        vars::roots(
          ajuste_i,
          modulus = TRUE
        ),

        error = function(e) {
          NA_real_
        }
      )


      if (all(is.na(raices_i))) {

        max_raiz_i <- NA_real_

      } else {

        max_raiz_i <- max(
          raices_i,
          na.rm = TRUE
        )
      }


      lista_diagnosticos_candidatos[[
        contador_diagnosticos
      ]] <- tibble::tibble(

        model_key =
          model_key_i,

        model_label =
          model_label_i,

        variant =
          variant_i,

        p =
          p_i,

        observations =
          ajuste_i$obs,

        pt_lag =
          pt_i$lag_used,

        pt_statistic =
          pt_i$statistic,

        pt_p_value =
          pt_i$p_value,

        bg_lag =
          bg_i$lag_used,

        bg_statistic =
          bg_i$statistic,

        bg_p_value =
          bg_i$p_value,

        maximum_root_modulus =
          max_raiz_i,

        roots_inside_unit_circle =
          !is.na(max_raiz_i) &&
          max_raiz_i < 1,

        status =
          "OK",

        error =
          NA_character_
      )


      contador_diagnosticos <-
        contador_diagnosticos + 1L
    }
  }
}


criterios_ic <- dplyr::bind_rows(
  lista_criterios_ic
)

selecciones_ic <- dplyr::bind_rows(
  lista_selecciones_ic
)

diagnosticos_candidatos <- dplyr::bind_rows(
  lista_diagnosticos_candidatos
)


# ------------------------------------------------------------
# 17. INCORPORAR SELECCIONES IC A LOS DIAGNÓSTICOS
# ------------------------------------------------------------

diagnosticos_candidatos <- diagnosticos_candidatos |>
  dplyr::left_join(

    selecciones_ic |>
      dplyr::select(
        .data$model_key,
        .data$variant,
        .data$p_AIC,
        .data$p_HQ,
        .data$p_SC,
        .data$p_FPE
      ),

    by = c(
      "model_key",
      "variant"
    )
  ) |>
  dplyr::mutate(

    pt_clean_10pct =
      !is.na(.data$pt_p_value) &
      .data$pt_p_value >
      nivel_significancia,

    bg_clean_10pct =
      !is.na(.data$bg_p_value) &
      .data$bg_p_value >
      nivel_significancia,

    serial_clean_10pct =
      .data$pt_clean_10pct &
      .data$bg_clean_10pct
  )


print(
  diagnosticos_candidatos,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 18. FUNCIÓN PARA SELECCIONAR EL ORDEN DEL VAR
# ------------------------------------------------------------

# Regla:
#
# 1. Se considera el mayor rezago sugerido por SC y HQ.
# 2. Se impone un mínimo p = 2 para ca.jo().
# 3. Se elige el menor p desde ese punto que no presente
#    autocorrelación PT ni BG al 10%.
# 4. Si ninguno cumple, se selecciona el modelo con el mayor
#    mínimo entre los p-valores de PT y BG.
#
# AIC y FPE se conservan como información complementaria.

seleccionar_orden_var <- function(
    diagnosticos,
    seleccion_ic,
    max_lag
) {

  p_AIC_i <- as.integer(
    seleccion_ic$p_AIC[[1]]
  )

  p_HQ_i <- as.integer(
    seleccion_ic$p_HQ[[1]]
  )

  p_SC_i <- as.integer(
    seleccion_ic$p_SC[[1]]
  )

  p_FPE_i <- as.integer(
    seleccion_ic$p_FPE[[1]]
  )


  p_inicio <- max(
    2L,
    p_SC_i,
    p_HQ_i,
    na.rm = TRUE
  )

  p_inicio <- min(
    p_inicio,
    max_lag
  )


  candidatos <- diagnosticos |>
    dplyr::filter(
      .data$status == "OK",
      .data$p >= .env$p_inicio
    ) |>
    dplyr::arrange(
      .data$p
    )


  if (nrow(candidatos) == 0) {

    candidatos <- diagnosticos |>
      dplyr::filter(
        .data$status == "OK"
      ) |>
      dplyr::arrange(
        .data$p
      )
  }


  if (nrow(candidatos) == 0) {

    stop(
      paste0(
        "No existen VAR candidatos válidos para ",
        seleccion_ic$model_key[[1]],
        " - ",
        seleccion_ic$variant[[1]]
      )
    )
  }


  candidatos_validos <- candidatos |>
    dplyr::filter(
      .data$serial_clean_10pct
    )


  if (nrow(candidatos_validos) > 0) {

    seleccionado <- candidatos_validos |>
      dplyr::slice(1)

    razon <- paste0(
      "Menor p >= max(2, SC, HQ) sin ",
      "autocorrelación PT ni BG al 10%"
    )

    seleccion_formal <- TRUE

  } else {

    seleccionado <- candidatos |>
      dplyr::mutate(

        minimum_serial_p = pmin(

          tidyr::replace_na(
            .data$pt_p_value,
            0
          ),

          tidyr::replace_na(
            .data$bg_p_value,
            0
          )
        )
      ) |>
      dplyr::arrange(
        dplyr::desc(
          .data$minimum_serial_p
        ),
        .data$p
      ) |>
      dplyr::slice(1)

    razon <- paste0(
      "Ningún p eliminó completamente la autocorrelación; ",
      "se eligió el mayor mínimo entre los p-valores"
    )

    seleccion_formal <- FALSE
  }


  seleccionado |>
    dplyr::transmute(

      model_key =
        .data$model_key,

      model_label =
        .data$model_label,

      variant =
        .data$variant,

      p_selected =
        .data$p,

      observations =
        .data$observations,

      p_AIC =
        .env$p_AIC_i,

      p_HQ =
        .env$p_HQ_i,

      p_SC =
        .env$p_SC_i,

      p_FPE =
        .env$p_FPE_i,

      p_start =
        .env$p_inicio,

      pt_lag =
        .data$pt_lag,

      pt_p_value =
        .data$pt_p_value,

      bg_lag =
        .data$bg_lag,

      bg_p_value =
        .data$bg_p_value,

      serial_clean_10pct =
        .data$serial_clean_10pct,

      maximum_root_modulus =
        .data$maximum_root_modulus,

      roots_inside_unit_circle =
        .data$roots_inside_unit_circle,

      selection_formal =
        .env$seleccion_formal,

      selection_reason =
        .env$razon
    )
}


# ------------------------------------------------------------
# 19. APLICAR LA REGLA DE SELECCIÓN
# ------------------------------------------------------------

lista_seleccion_final <- list()

contador_seleccion <- 1L


for (
  i in seq_len(
    nrow(selecciones_ic)
  )
) {

  seleccion_ic_i <-
    selecciones_ic[i, ]

  model_key_i <-
    seleccion_ic_i$model_key[[1]]

  variant_i <-
    seleccion_ic_i$variant[[1]]


  diagnosticos_i <- diagnosticos_candidatos |>
    dplyr::filter(
      .data$model_key ==
        .env$model_key_i,

      .data$variant ==
        .env$variant_i
    )


  lista_seleccion_final[[
    contador_seleccion
  ]] <- seleccionar_orden_var(

    diagnosticos =
      diagnosticos_i,

    seleccion_ic =
      seleccion_ic_i,

    max_lag =
      max_lag_var
  )


  contador_seleccion <-
    contador_seleccion + 1L
}


seleccion_final <- dplyr::bind_rows(
  lista_seleccion_final
) |>
  dplyr::left_join(

    config_variantes |>
      dplyr::select(
        .data$variant,
        .data$variant_label,
        .data$season,
        .data$main_variant
      ),

    by = "variant"
  ) |>
  dplyr::left_join(

    config_sistemas |>
      dplyr::select(
        .data$model_key,
        .data$model_role,
        .data$johansen_role
      ),

    by = "model_key"
  ) |>
  dplyr::mutate(

    johansen_K =
      .data$p_selected,

    vecm_lags_in_differences =
      .data$p_selected - 1L
  )


print(
  seleccion_final,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 20. RECUPERAR LOS MODELOS VAR FINALES
# ------------------------------------------------------------

modelos_var_finales <- list()


for (
  i in seq_len(
    nrow(seleccion_final)
  )
) {

  model_key_i <-
    seleccion_final$model_key[[i]]

  variant_i <-
    seleccion_final$variant[[i]]

  p_i <-
    seleccion_final$p_selected[[i]]


  clave_candidata <- crear_clave_modelo(

    model_key =
      model_key_i,

    variant =
      variant_i,

    p =
      p_i
  )


  clave_final <- paste0(
    model_key_i,
    "__",
    variant_i
  )


  modelo_i <- modelos_var_candidatos[[
    clave_candidata
  ]]


  if (is.null(modelo_i)) {

    season_i <-
      seleccion_final$season[[i]]

    modelo_i <- estimar_var(

      datos_sistema =
        datos_sistemas[[model_key_i]],

      p =
        p_i,

      season_value =
        season_i
    )
  }


  modelos_var_finales[[
    clave_final
  ]] <- modelo_i
}


stopifnot(
  length(modelos_var_finales) ==
    nrow(seleccion_final)
)


# ------------------------------------------------------------
# 21. ORDENAR LA SALIDA DE vcorr_res()
# ------------------------------------------------------------

ejecutar_vcorr_ordenado <- function(
    modelo_var,
    model_key_arg,
    model_label_arg,
    variant_arg,
    tipo_arg,
    lag_objetivo = 12L
) {

  observaciones <- as.integer(
    modelo_var$obs
  )

  p_modelo <- as.integer(
    modelo_var$p
  )

  lag_maximo <- min(
    as.integer(lag_objetivo),
    floor(
      observaciones / 3
    )
  )

  lag_minimo <- max(
    p_modelo + 1L,
    2L
  )

  if (lag_maximo < lag_minimo) {
    lag_maximo <- lag_minimo
  }


  lags_candidatos <- seq(
    from = lag_maximo,
    to = lag_minimo,
    by = -1L
  )


  ultimo_error <- NA_character_


  for (lag_i in lags_candidatos) {

    resultado_i <- tryCatch(

      suppressWarnings(
        vcorr_res(
          var_reg = modelo_var,
          lags = lag_i,
          tipo = tipo_arg
        )
      ),

      error = function(e) {
        e
      }
    )


    if (
      !inherits(resultado_i, "error") &&
      !is.null(resultado_i) &&
      (
        is.matrix(resultado_i) ||
        is.data.frame(resultado_i)
      )
    ) {

      resultado_df <- as.data.frame(
        resultado_i,
        stringsAsFactors = FALSE
      )


      nombres_resultado <- names(
        resultado_df
      )


      lag_vector <- suppressWarnings(
        as.integer(
          resultado_df[[1]]
        )
      )

      statistic_vector <- suppressWarnings(
        as.numeric(
          resultado_df[[2]]
        )
      )

      p_vector_original <- as.character(
        resultado_df[[3]]
      )

      p_vector_original[
        p_vector_original %in%
          c(
            "NA",
            "",
            "NaN"
          )
      ] <- NA_character_

      p_vector <- suppressWarnings(
        as.numeric(
          p_vector_original
        )
      )


      return(
        tibble::tibble(

          model_key =
            model_key_arg,

          model_label =
            model_label_arg,

          variant =
            variant_arg,

          test_type =
            tipo_arg,

          maximum_lag_requested =
            lag_i,

          lag =
            lag_vector,

          statistic =
            statistic_vector,

          p_value =
            p_vector,

          decision_10pct =
            dplyr::case_when(

              is.na(p_vector) ~
                "No disponible",

              p_vector <
                nivel_significancia ~
                paste0(
                  "Se rechaza ausencia ",
                  "de autocorrelación"
                ),

              TRUE ~
                paste0(
                  "No se rechaza ausencia ",
                  "de autocorrelación"
                )
            ),

          status =
            "OK",

          error =
            NA_character_
        )
      )

    } else {

      ultimo_error <- if (
        inherits(resultado_i, "error")
      ) {

        conditionMessage(resultado_i)

      } else {

        "vcorr_res no devolvió una matriz o data frame"
      }
    }
  }


  tibble::tibble(

    model_key =
      model_key_arg,

    model_label =
      model_label_arg,

    variant =
      variant_arg,

    test_type =
      tipo_arg,

    maximum_lag_requested =
      NA_integer_,

    lag =
      NA_integer_,

    statistic =
      NA_real_,

    p_value =
      NA_real_,

    decision_10pct =
      "No disponible",

    status =
      "ERROR",

    error =
      ultimo_error
  )
}


# ------------------------------------------------------------
# 22. EJECUTAR vcorr_res()
# ------------------------------------------------------------

lista_vcorr <- list()

contador_vcorr <- 1L


for (
  i in seq_len(
    nrow(seleccion_final)
  )
) {

  model_key_i <-
    seleccion_final$model_key[[i]]

  model_label_i <-
    seleccion_final$model_label[[i]]

  variant_i <-
    seleccion_final$variant[[i]]


  clave_final <- paste0(
    model_key_i,
    "__",
    variant_i
  )


  modelo_i <- modelos_var_finales[[
    clave_final
  ]]


  for (
    tipo_i in c(
      "PT.adjusted",
      "BG",
      "ES"
    )
  ) {

    cat(
      "\nEjecutando vcorr_res:",
      model_label_i,
      "|",
      variant_i,
      "|",
      tipo_i,
      "\n"
    )


    lista_vcorr[[
      contador_vcorr
    ]] <- ejecutar_vcorr_ordenado(

      modelo_var =
        modelo_i,

      model_key_arg =
        model_key_i,

      model_label_arg =
        model_label_i,

      variant_arg =
        variant_i,

      tipo_arg =
        tipo_i,

      lag_objetivo =
        lag_correlacion_objetivo
    )


    contador_vcorr <-
      contador_vcorr + 1L
  }
}


vcorr_final <- dplyr::bind_rows(
  lista_vcorr
)


print(
  vcorr_final,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 23. ORDENAR EXCLUSIÓN DE REZAGOS
# ------------------------------------------------------------

ordenar_exclusion_lags <- function(
    modelo_var,
    model_key_arg,
    model_label_arg,
    variant_arg
) {

  resultado <- tryCatch(

    VAR_lag_exclusion_wald(
      modelo_var
    ),

    error = function(e) {
      e
    }
  )


  if (inherits(resultado, "error")) {

    return(
      tibble::tibble(

        model_key =
          model_key_arg,

        model_label =
          model_label_arg,

        variant =
          variant_arg,

        lag =
          NA_character_,

        lag_number =
          NA_integer_,

        equation =
          NA_character_,

        statistic =
          NA_real_,

        p_value =
          NA_real_,

        significant_10pct =
          NA,

        status =
          "ERROR",

        error =
          conditionMessage(resultado)
      )
    )
  }


  matriz_stat <- resultado$statistic

  matriz_p <- resultado$p.value


  tabla <- expand.grid(

    lag =
      rownames(matriz_stat),

    equation =
      colnames(matriz_stat),

    KEEP.OUT.ATTRS = FALSE,

    stringsAsFactors = FALSE
  )


  tabla$statistic <- as.numeric(
    as.vector(matriz_stat)
  )

  tabla$p_value <- as.numeric(
    as.vector(matriz_p)
  )


  tibble::as_tibble(
    tabla
  ) |>
    dplyr::mutate(

      model_key =
        .env$model_key_arg,

      model_label =
        .env$model_label_arg,

      variant =
        .env$variant_arg,

      lag_number = as.integer(
        readr::parse_number(
          .data$lag
        )
      ),

      significant_10pct =
        .data$p_value <
        nivel_significancia,

      status =
        "OK",

      error =
        NA_character_,

      .before = 1
    )
}


# ------------------------------------------------------------
# 24. EJECUTAR EXCLUSIÓN DE REZAGOS
# ------------------------------------------------------------

lista_exclusion <- list()

contador_exclusion <- 1L


for (
  i in seq_len(
    nrow(seleccion_final)
  )
) {

  model_key_i <-
    seleccion_final$model_key[[i]]

  model_label_i <-
    seleccion_final$model_label[[i]]

  variant_i <-
    seleccion_final$variant[[i]]


  clave_final <- paste0(
    model_key_i,
    "__",
    variant_i
  )


  modelo_i <- modelos_var_finales[[
    clave_final
  ]]


  cat(
    "\nExclusión de rezagos:",
    model_label_i,
    "|",
    variant_i,
    "\n"
  )


  lista_exclusion[[
    contador_exclusion
  ]] <- ordenar_exclusion_lags(

    modelo_var =
      modelo_i,

    model_key_arg =
      model_key_i,

    model_label_arg =
      model_label_i,

    variant_arg =
      variant_i
  )


  contador_exclusion <-
    contador_exclusion + 1L
}


exclusion_lags <- dplyr::bind_rows(
  lista_exclusion
)


print(
  exclusion_lags,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 25. TEST DE WHITE AUXILIAR
# ------------------------------------------------------------

# VAR_white_no_cross() no acepta:
#
# - variables exógenas;
# - dummies estacionales;
# - restricciones.
#
# Por eso se estima un VAR auxiliar:
#
# - mismas variables endógenas;
# - mismo número de rezagos;
# - constante;
# - sin season;
# - sin commodities exógena.
#
# El resultado debe reportarse como diagnóstico auxiliar.

ejecutar_white_auxiliar <- function(
    datos_sistema,
    p,
    model_key_arg,
    model_label_arg,
    variant_arg
) {

  modelo_auxiliar <- tryCatch(

    vars::VAR(

      y =
        datos_sistema$y,

      p =
        as.integer(p),

      type =
        "const"
    ),

    error = function(e) {
      e
    }
  )


  if (inherits(modelo_auxiliar, "error")) {

    return(
      tibble::tibble(

        model_key =
          model_key_arg,

        model_label =
          model_label_arg,

        variant =
          variant_arg,

        auxiliary_p =
          p,

        statistic =
          NA_real_,

        degrees_freedom =
          NA_real_,

        p_value =
          NA_real_,

        decision_10pct =
          "No disponible",

        status =
          "ERROR",

        note =
          "VAR auxiliar sin season ni exógenas",

        error =
          conditionMessage(modelo_auxiliar)
      )
    )
  }


  resultado <- tryCatch(

    VAR_white_no_cross(
      modelo_auxiliar
    ),

    error = function(e) {
      e
    }
  )


  if (inherits(resultado, "error")) {

    return(
      tibble::tibble(

        model_key =
          model_key_arg,

        model_label =
          model_label_arg,

        variant =
          variant_arg,

        auxiliary_p =
          p,

        statistic =
          NA_real_,

        degrees_freedom =
          NA_real_,

        p_value =
          NA_real_,

        decision_10pct =
          "No disponible",

        status =
          "ERROR",

        note =
          "VAR auxiliar sin season ni exógenas",

        error =
          conditionMessage(resultado)
      )
    )
  }


  tibble::tibble(

    model_key =
      model_key_arg,

    model_label =
      model_label_arg,

    variant =
      variant_arg,

    auxiliary_p =
      p,

    statistic =
      as.numeric(
        resultado$statistic
      ),

    degrees_freedom =
      as.numeric(
        resultado$parameter
      ),

    p_value =
      as.numeric(
        resultado$p.value
      ),

    decision_10pct = ifelse(

      resultado$p.value <
        nivel_significancia,

      "Se rechaza homocedasticidad",

      "No se rechaza homocedasticidad"
    ),

    status =
      "OK",

    note =
      paste0(
        "Diagnóstico auxiliar sin dummies ",
        "estacionales ni variables exógenas"
      ),

    error =
      NA_character_
  )
}


# ------------------------------------------------------------
# 26. EJECUTAR TEST DE WHITE
# ------------------------------------------------------------

lista_white <- list()

contador_white <- 1L


for (
  i in seq_len(
    nrow(seleccion_final)
  )
) {

  model_key_i <-
    seleccion_final$model_key[[i]]

  model_label_i <-
    seleccion_final$model_label[[i]]

  variant_i <-
    seleccion_final$variant[[i]]

  p_i <-
    seleccion_final$p_selected[[i]]


  lista_white[[
    contador_white
  ]] <- ejecutar_white_auxiliar(

    datos_sistema =
      datos_sistemas[[model_key_i]],

    p =
      p_i,

    model_key_arg =
      model_key_i,

    model_label_arg =
      model_label_i,

    variant_arg =
      variant_i
  )


  contador_white <-
    contador_white + 1L
}


white_final <- dplyr::bind_rows(
  lista_white
)


print(
  white_final,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 27. NORMALIDAD Y ARCH MULTIVARIADOS
# ------------------------------------------------------------

ejecutar_normalidad_arch <- function(
    modelo_var,
    model_key_arg,
    model_label_arg,
    variant_arg
) {

  normalidad <- tryCatch(

    vars::normality.test(
      modelo_var,
      multivariate.only = FALSE
    ),

    error = function(e) {
      e
    }
  )


  arch <- tryCatch(

    vars::arch.test(
      modelo_var,
      lags.multi = lags_arch,
      multivariate.only = FALSE
    ),

    error = function(e) {
      e
    }
  )


  # ----------------------------------------------------------
  # Normalidad
  # ----------------------------------------------------------

  if (inherits(normalidad, "error")) {

    normalidad_tabla <- tibble::tibble(

      model_key =
        model_key_arg,

      model_label =
        model_label_arg,

      variant =
        variant_arg,

      diagnostic_group =
        "Normalidad",

      test = c(
        "Jarque-Bera multivariado",
        "Asimetría multivariada",
        "Curtosis multivariada"
      ),

      statistic =
        NA_real_,

      p_value =
        NA_real_,

      decision_10pct =
        "No disponible",

      status =
        "ERROR",

      error =
        conditionMessage(normalidad)
    )

  } else {

    jb_obj <-
      normalidad$jb.mul$JB

    skew_obj <-
      normalidad$jb.mul$Skewness

    kurt_obj <-
      normalidad$jb.mul$Kurtosis


    p_normalidad <- c(

      as.numeric(
        jb_obj$p.value
      ),

      as.numeric(
        skew_obj$p.value
      ),

      as.numeric(
        kurt_obj$p.value
      )
    )


    normalidad_tabla <- tibble::tibble(

      model_key =
        model_key_arg,

      model_label =
        model_label_arg,

      variant =
        variant_arg,

      diagnostic_group =
        "Normalidad",

      test = c(
        "Jarque-Bera multivariado",
        "Asimetría multivariada",
        "Curtosis multivariada"
      ),

      statistic = c(

        as.numeric(
          jb_obj$statistic
        ),

        as.numeric(
          skew_obj$statistic
        ),

        as.numeric(
          kurt_obj$statistic
        )
      ),

      p_value =
        p_normalidad,

      decision_10pct = ifelse(

        p_normalidad <
          nivel_significancia,

        "Se rechaza normalidad",

        "No se rechaza normalidad"
      ),

      status =
        "OK",

      error =
        NA_character_
    )
  }


  # ----------------------------------------------------------
  # ARCH multivariado
  # ----------------------------------------------------------

  if (inherits(arch, "error")) {

    arch_tabla <- tibble::tibble(

      model_key =
        model_key_arg,

      model_label =
        model_label_arg,

      variant =
        variant_arg,

      diagnostic_group =
        "Heterocedasticidad ARCH",

      test =
        paste0(
          "ARCH multivariado, ",
          lags_arch,
          " rezagos"
        ),

      statistic =
        NA_real_,

      p_value =
        NA_real_,

      decision_10pct =
        "No disponible",

      status =
        "ERROR",

      error =
        conditionMessage(arch)
    )

  } else {

    arch_obj <- arch$arch.mul

    arch_p <- as.numeric(
      arch_obj$p.value
    )


    arch_tabla <- tibble::tibble(

      model_key =
        model_key_arg,

      model_label =
        model_label_arg,

      variant =
        variant_arg,

      diagnostic_group =
        "Heterocedasticidad ARCH",

      test =
        paste0(
          "ARCH multivariado, ",
          lags_arch,
          " rezagos"
        ),

      statistic =
        as.numeric(
          arch_obj$statistic
        ),

      p_value =
        arch_p,

      decision_10pct = ifelse(

        arch_p <
          nivel_significancia,

        "Se rechaza ausencia de ARCH",

        "No se rechaza ausencia de ARCH"
      ),

      status =
        "OK",

      error =
        NA_character_
    )
  }


  dplyr::bind_rows(
    normalidad_tabla,
    arch_tabla
  )
}


# ------------------------------------------------------------
# 28. EJECUTAR NORMALIDAD Y ARCH
# ------------------------------------------------------------

lista_normalidad_arch <- list()

contador_normalidad <- 1L


for (
  i in seq_len(
    nrow(seleccion_final)
  )
) {

  model_key_i <-
    seleccion_final$model_key[[i]]

  model_label_i <-
    seleccion_final$model_label[[i]]

  variant_i <-
    seleccion_final$variant[[i]]


  clave_final <- paste0(
    model_key_i,
    "__",
    variant_i
  )


  modelo_i <- modelos_var_finales[[
    clave_final
  ]]


  lista_normalidad_arch[[
    contador_normalidad
  ]] <- ejecutar_normalidad_arch(

    modelo_var =
      modelo_i,

    model_key_arg =
      model_key_i,

    model_label_arg =
      model_label_i,

    variant_arg =
      variant_i
  )


  contador_normalidad <-
    contador_normalidad + 1L
}


normalidad_arch_final <- dplyr::bind_rows(
  lista_normalidad_arch
)


print(
  normalidad_arch_final,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 29. RAÍCES DE LOS VAR FINALES
# ------------------------------------------------------------

lista_raices <- list()

contador_raices <- 1L


for (
  i in seq_len(
    nrow(seleccion_final)
  )
) {

  model_key_i <-
    seleccion_final$model_key[[i]]

  model_label_i <-
    seleccion_final$model_label[[i]]

  variant_i <-
    seleccion_final$variant[[i]]

  p_i <-
    seleccion_final$p_selected[[i]]


  clave_final <- paste0(
    model_key_i,
    "__",
    variant_i
  )


  modelo_i <- modelos_var_finales[[
    clave_final
  ]]


  raices_i <- tryCatch(

    vars::roots(
      modelo_i,
      modulus = TRUE
    ),

    error = function(e) {

      rep(
        NA_real_,
        ncol(
          datos_sistemas[[model_key_i]]$y
        ) * p_i
      )
    }
  )


  lista_raices[[
    contador_raices
  ]] <- tibble::tibble(

    model_key =
      model_key_i,

    model_label =
      model_label_i,

    variant =
      variant_i,

    p_selected =
      p_i,

    root_number =
      seq_along(raices_i),

    modulus =
      as.numeric(raices_i),

    inside_unit_circle =
      as.numeric(raices_i) < 1,

    near_unit_root =
      as.numeric(raices_i) >= 0.95
  )


  contador_raices <-
    contador_raices + 1L
}


raices_finales <- dplyr::bind_rows(
  lista_raices
)


print(
  raices_finales,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 30. RESUMIR EXCLUSIÓN DEL ÚLTIMO REZAGO
# ------------------------------------------------------------

exclusion_joint <- exclusion_lags |>
  dplyr::filter(
    .data$equation == "Joint",
    .data$status == "OK"
  ) |>
  dplyr::select(

    .data$model_key,

    .data$variant,

    .data$lag_number,

    highest_lag_joint_statistic =
      .data$statistic,

    highest_lag_joint_p_value =
      .data$p_value
  )


exclusion_lag_mayor <- seleccion_final |>
  dplyr::select(

    .data$model_key,

    .data$model_label,

    .data$variant,

    .data$p_selected
  ) |>
  dplyr::left_join(

    exclusion_joint,

    by = c(
      "model_key",
      "variant",
      "p_selected" =
        "lag_number"
    )
  ) |>
  dplyr::mutate(

    highest_lag_significant_10pct =
      !is.na(
        .data$highest_lag_joint_p_value
      ) &
      .data$highest_lag_joint_p_value <
      nivel_significancia
  )


# ------------------------------------------------------------
# 31. CREAR RESUMEN FINAL DE DIAGNÓSTICOS
# ------------------------------------------------------------

normalidad_jb <- normalidad_arch_final |>
  dplyr::filter(
    .data$test ==
      "Jarque-Bera multivariado"
  ) |>
  dplyr::select(

    .data$model_key,

    .data$variant,

    normality_statistic =
      .data$statistic,

    normality_p_value =
      .data$p_value
  )


arch_resumen <- normalidad_arch_final |>
  dplyr::filter(
    .data$diagnostic_group ==
      "Heterocedasticidad ARCH"
  ) |>
  dplyr::select(

    .data$model_key,

    .data$variant,

    arch_statistic =
      .data$statistic,

    arch_p_value =
      .data$p_value
  )


white_resumen <- white_final |>
  dplyr::select(

    .data$model_key,

    .data$variant,

    white_statistic =
      .data$statistic,

    white_p_value =
      .data$p_value,

    white_note =
      .data$note
  )


max_raices <- raices_finales |>
  dplyr::group_by(
    .data$model_key,
    .data$variant
  ) |>
  dplyr::summarise(

    maximum_root_modulus_final = if (
      all(is.na(.data$modulus))
    ) {

      NA_real_

    } else {

      max(
        .data$modulus,
        na.rm = TRUE
      )
    },

    roots_near_one = sum(
      .data$near_unit_root,
      na.rm = TRUE
    ),

    .groups =
      "drop"
  )


resumen_final_var <- seleccion_final |>
  dplyr::left_join(

    exclusion_lag_mayor |>
      dplyr::select(

        .data$model_key,

        .data$variant,

        .data$highest_lag_joint_statistic,

        .data$highest_lag_joint_p_value,

        .data$highest_lag_significant_10pct
      ),

    by = c(
      "model_key",
      "variant"
    )
  ) |>
  dplyr::left_join(

    normalidad_jb,

    by = c(
      "model_key",
      "variant"
    )
  ) |>
  dplyr::left_join(

    arch_resumen,

    by = c(
      "model_key",
      "variant"
    )
  ) |>
  dplyr::left_join(

    white_resumen,

    by = c(
      "model_key",
      "variant"
    )
  ) |>
  dplyr::left_join(

    max_raices,

    by = c(
      "model_key",
      "variant"
    )
  ) |>
  dplyr::mutate(

    serial_valid_10pct =
      !is.na(.data$pt_p_value) &
      !is.na(.data$bg_p_value) &
      .data$pt_p_value >
      nivel_significancia &
      .data$bg_p_value >
      nivel_significancia,

    normality_valid_10pct =
      !is.na(.data$normality_p_value) &
      .data$normality_p_value >
      nivel_significancia,

    arch_valid_10pct =
      !is.na(.data$arch_p_value) &
      .data$arch_p_value >
      nivel_significancia,

    white_valid_10pct =
      !is.na(.data$white_p_value) &
      .data$white_p_value >
      nivel_significancia,

    johansen_validity =
      dplyr::case_when(

        .data$serial_valid_10pct &
          .data$normality_valid_10pct ~
          paste0(
            "Diagnósticos compatibles con ",
            "inferencia formal de Johansen"
          ),

        .data$serial_valid_10pct &
          !.data$normality_valid_10pct ~
          paste0(
            "Sin autocorrelación, pero no normalidad; ",
            "Johansen será indicativo"
          ),

        !.data$serial_valid_10pct ~
          paste0(
            "Persiste autocorrelación; ",
            "revisar el orden antes de Johansen"
          ),

        TRUE ~
          "Diagnóstico no concluyente"
      ),

    roots_interpretation =
      dplyr::case_when(

        is.na(
          .data$maximum_root_modulus_final
        ) ~
          "Raíces no disponibles",

        .data$maximum_root_modulus_final <
          0.95 ~
          paste0(
            "Raíces claramente dentro ",
            "del círculo unitario"
          ),

        .data$maximum_root_modulus_final <
          1 ~
          paste0(
            "Raíces cercanas a uno, compatibles ",
            "con un VAR en niveles de series I(1)"
          ),

        TRUE ~
          paste0(
            "Alguna raíz es igual o superior a uno; ",
            "evaluar mediante cointegración"
          )
      )
  )


print(
  resumen_final_var,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 32. PREPARAR INSUMOS PARA JOHANSEN
# ------------------------------------------------------------

insumos_johansen <- resumen_final_var |>
  dplyr::filter(
    .data$main_variant
  ) |>
  dplyr::mutate(

    deterministic_johansen =
      "ecdet = 'const'",

    seasonal_johansen =
      4L,

    exogenous_johansen =
      dplyr::case_when(

        .data$model_key ==
          "exports_expanded" ~
          "commodity_c como dumvar",

        TRUE ~
          "Ninguna"
      ),

    K_ca_jo =
      .data$p_selected,

    vecm_short_run_lags =
      .data$p_selected - 1L,

    interpretation_role =
      dplyr::case_when(

        .data$model_key ==
          "imports" ~
          paste0(
            "Sistema principal; Engle-Granger ",
            "presentó evidencia mixta"
          ),

        .data$model_key ==
          "exports_classic" ~
          paste0(
            "Sistema principal; Engle-Granger ",
            "favoreció cointegración con estacionalidad"
          ),

        .data$model_key ==
          "exports_expanded" ~
          paste0(
            "Robustez con commodities ",
            "exógena I(0)"
          ),

        TRUE ~
          .data$model_role
      )
  ) |>
  dplyr::select(

    .data$model_key,

    .data$model_label,

    .data$interpretation_role,

    .data$p_AIC,

    .data$p_HQ,

    .data$p_SC,

    .data$p_FPE,

    .data$p_selected,

    .data$K_ca_jo,

    .data$vecm_short_run_lags,

    .data$pt_p_value,

    .data$bg_p_value,

    .data$normality_p_value,

    .data$arch_p_value,

    .data$white_p_value,

    .data$highest_lag_joint_p_value,

    .data$deterministic_johansen,

    .data$seasonal_johansen,

    .data$exogenous_johansen,

    .data$johansen_validity
  )


print(
  insumos_johansen,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 33. GUARDAR RESÚMENES TEXTUALES DE LOS VAR
# ------------------------------------------------------------

salida_texto_var <- character()


for (
  i in seq_len(
    nrow(seleccion_final)
  )
) {

  model_key_i <-
    seleccion_final$model_key[[i]]

  model_label_i <-
    seleccion_final$model_label[[i]]

  variant_i <-
    seleccion_final$variant[[i]]

  p_i <-
    seleccion_final$p_selected[[i]]


  clave_final <- paste0(
    model_key_i,
    "__",
    variant_i
  )


  modelo_i <- modelos_var_finales[[
    clave_final
  ]]


  encabezado <- c(

    "",

    "============================================================",

    paste0(
      "Sistema: ",
      model_label_i
    ),

    paste0(
      "Variante: ",
      variant_i
    ),

    paste0(
      "Orden p: ",
      p_i
    ),

    "============================================================"
  )


  resumen_modelo <- capture.output(
    summary(modelo_i)
  )


  exclusion_impresa <- capture.output(
    VAR_lag_exclusion_wald(
      modelo_i
    )
  )


  salida_texto_var <- c(

    salida_texto_var,

    encabezado,

    "",

    "RESUMEN DEL VAR:",

    resumen_modelo,

    "",

    "EXCLUSIÓN DE REZAGOS:",

    exclusion_impresa
  )
}


writeLines(

  salida_texto_var,

  con = here::here(
    "outputs",
    "models",
    "resumen_modelos_var_finales.txt"
  )
)


# ------------------------------------------------------------
# 34. GRÁFICO DE RAÍCES
# ------------------------------------------------------------

grafico_raices <- ggplot2::ggplot(

  raices_finales,

  ggplot2::aes(
    x = .data$root_number,
    y = .data$modulus
  )
) +
  ggplot2::geom_hline(
    yintercept = 1,
    linetype = "dashed"
  ) +
  ggplot2::geom_hline(
    yintercept = 0.95,
    linetype = "dotted"
  ) +
  ggplot2::geom_point(
    size = 2
  ) +
  ggplot2::geom_line() +
  ggplot2::facet_grid(
    variant ~ model_label,
    scales = "free_x"
  ) +
  ggplot2::labs(

    title =
      "Módulo de las raíces de los VAR",

    subtitle =
      paste0(
        "En VAR en niveles con series I(1), ",
        "pueden aparecer raíces cercanas a uno"
      ),

    x =
      "Número de raíz",

    y =
      "Módulo"
  ) +
  ggplot2::theme_minimal(
    base_size = 10
  ) +
  ggplot2::theme(

    axis.text.x =
      ggplot2::element_text(
        angle = 45,
        hjust = 1
      )
  )


ggplot2::ggsave(

  filename = here::here(
    "outputs",
    "figures",
    "10_var_roots.png"
  ),

  plot =
    grafico_raices,

  width =
    15,

  height =
    8,

  dpi =
    300
)


# ------------------------------------------------------------
# 35. GRÁFICO DE AUTOCORRELACIÓN vcorr_res
# ------------------------------------------------------------

datos_grafico_vcorr <- vcorr_final |>
  dplyr::filter(

    .data$status == "OK",

    .data$test_type %in%
      c(
        "PT.adjusted",
        "BG"
      ),

    !is.na(.data$p_value)
  )


grafico_vcorr <- ggplot2::ggplot(

  datos_grafico_vcorr,

  ggplot2::aes(

    x =
      .data$lag,

    y =
      .data$p_value,

    linetype =
      .data$test_type,

    group =
      .data$test_type
  )
) +
  ggplot2::geom_hline(

    yintercept =
      nivel_significancia,

    linetype =
      "dashed"
  ) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::facet_grid(
    variant ~ model_label
  ) +
  ggplot2::labs(

    title =
      "Tests multivariados de autocorrelación",

    subtitle =
      paste0(
        "Línea horizontal: nivel ",
        "de significancia del 10%"
      ),

    x =
      "Rezago",

    y =
      "Valor p",

    linetype =
      "Prueba"
  ) +
  ggplot2::theme_minimal(
    base_size = 10
  ) +
  ggplot2::theme(

    axis.text.x =
      ggplot2::element_text(
        angle = 45,
        hjust = 1
      )
  )


ggplot2::ggsave(

  filename = here::here(
    "outputs",
    "figures",
    "10_autocorrelacion_var.png"
  ),

  plot =
    grafico_vcorr,

  width =
    15,

  height =
    8,

  dpi =
    300
)


# ------------------------------------------------------------
# 36. PREPARAR CONFIGURACIÓN PLANA PARA EXCEL
# ------------------------------------------------------------

config_sistemas_excel <- config_sistemas |>
  dplyr::mutate(

    endogenous =
      purrr::map_chr(
        .data$endogenous,
        ~ paste(
          .x,
          collapse = " + "
        )
      ),

    exogenous =
      purrr::map_chr(
        .data$exogenous,
        ~ if (
          length(.x) == 0
        ) {

          "Ninguna"

        } else {

          paste(
            .x,
            collapse = " + "
          )
        }
      )
  )


# ------------------------------------------------------------
# 37. GUARDAR RESULTADOS EN EXCEL
# ------------------------------------------------------------

writexl::write_xlsx(

  list(

    configuracion =
      config_sistemas_excel,

    variantes =
      config_variantes,

    control_sistemas =
      tabla_control_sistemas,

    faltantes =
      tabla_faltantes,

    seleccion_ic =
      selecciones_ic,

    criterios_ic =
      criterios_ic,

    diagnosticos_p =
      diagnosticos_candidatos,

    seleccion_final =
      seleccion_final,

    vcorr =
      vcorr_final,

    exclusion_lags =
      exclusion_lags,

    white_auxiliar =
      white_final,

    normalidad_arch =
      normalidad_arch_final,

    raices =
      raices_finales,

    resumen_final =
      resumen_final_var,

    johansen_input =
      insumos_johansen
  ),

  path = here::here(
    "outputs",
    "tables",
    "seleccion_y_diagnosticos_var.xlsx"
  )
)


# ------------------------------------------------------------
# 38. GUARDAR OBJETOS RDS
# ------------------------------------------------------------

saveRDS(

  datos_sistemas,

  here::here(
    "outputs",
    "models",
    "var_system_data.rds"
  )
)


saveRDS(

  modelos_var_finales,

  here::here(
    "outputs",
    "models",
    "modelos_var_finales.rds"
  )
)


saveRDS(

  seleccion_final,

  here::here(
    "outputs",
    "models",
    "seleccion_final_var.rds"
  )
)


saveRDS(

  diagnosticos_candidatos,

  here::here(
    "outputs",
    "models",
    "var_candidate_diagnostics.rds"
  )
)


saveRDS(

  resumen_final_var,

  here::here(
    "outputs",
    "models",
    "diagnosticos_var_finales.rds"
  )
)


saveRDS(

  insumos_johansen,

  here::here(
    "outputs",
    "models",
    "insumos_para_johansen.rds"
  )
)


saveRDS(

  exclusion_lags,

  here::here(
    "outputs",
    "models",
    "exclusion_lags_var.rds"
  )
)


saveRDS(

  vcorr_final,

  here::here(
    "outputs",
    "models",
    "vcorr_var_final.rds"
  )
)


saveRDS(

  white_final,

  here::here(
    "outputs",
    "models",
    "white_var_auxiliar.rds"
  )
)


saveRDS(

  normalidad_arch_final,

  here::here(
    "outputs",
    "models",
    "normalidad_arch_var.rds"
  )
)


# ------------------------------------------------------------
# 39. CONTROLES FINALES
# ------------------------------------------------------------

stopifnot(

  nrow(config_sistemas) ==
    3,

  nrow(config_variantes) ==
    2,

  nrow(selecciones_ic) ==
    6,

  nrow(seleccion_final) ==
    6,

  length(modelos_var_finales) ==
    6,

  nrow(insumos_johansen) ==
    3
)


# ------------------------------------------------------------
# 40. MOSTRAR RESULTADOS CLAVE
# ------------------------------------------------------------

cat(
  "\n\n============================================================",
  "\nSELECCIÓN FINAL DE REZAGOS",
  "\n============================================================\n"
)


seleccion_final |>
  dplyr::select(

    .data$model_label,

    .data$variant,

    .data$p_AIC,

    .data$p_HQ,

    .data$p_SC,

    .data$p_FPE,

    .data$p_start,

    .data$p_selected,

    .data$pt_p_value,

    .data$bg_p_value,

    .data$serial_clean_10pct,

    .data$selection_formal,

    .data$selection_reason
  ) |>
  print(
    n = Inf,
    width = Inf
  )


cat(
  "\n\n============================================================",
  "\nDIAGNÓSTICOS DE LOS VAR FINALES",
  "\n============================================================\n"
)


resumen_final_var |>
  dplyr::select(

    .data$model_label,

    .data$variant,

    .data$p_selected,

    .data$pt_p_value,

    .data$bg_p_value,

    .data$highest_lag_joint_p_value,

    .data$normality_p_value,

    .data$arch_p_value,

    .data$white_p_value,

    .data$maximum_root_modulus_final,

    .data$johansen_validity
  ) |>
  print(
    n = Inf,
    width = Inf
  )


cat(
  "\n\n============================================================",
  "\nINSUMOS PARA JOHANSEN",
  "\n============================================================\n"
)


insumos_johansen |>
  print(
    n = Inf,
    width = Inf
  )


# ------------------------------------------------------------
# 41. MENSAJE FINAL
# ------------------------------------------------------------

cat(
  "\n============================================================",
  "\nSCRIPT 10 FINALIZADO",
  "\n============================================================",

  "\nSistemas analizados:",
  nrow(config_sistemas),

  "\nVariantes por sistema:",
  nrow(config_variantes),

  "\nVAR candidatos estimados:",
  length(modelos_var_candidatos),

  "\nVAR finales:",
  length(modelos_var_finales),

  "\nMáximo de rezagos evaluado:",
  max_lag_var,

  "\nNivel de significancia:",
  "10%",

  "\nVariante principal:",
  "Constante y season = 4",

  "\nVariante de robustez:",
  "Constante sin season",

  "\nAutocorrelación:",
  "vcorr_res: PT-adjusted, BG y ES",

  "\nHeterocedasticidad:",
  "White auxiliar y ARCH multivariado",

  "\nExclusión de rezagos:",
  "VAR_lag_exclusion_wald",

  "\nNormalidad:",
  "Jarque-Bera multivariado",

  "\n\nArchivos generados:",

  "\n- outputs/tables/seleccion_y_diagnosticos_var.xlsx",

  "\n- outputs/models/var_system_data.rds",

  "\n- outputs/models/modelos_var_finales.rds",

  "\n- outputs/models/seleccion_final_var.rds",

  "\n- outputs/models/var_candidate_diagnostics.rds",

  "\n- outputs/models/diagnosticos_var_finales.rds",

  "\n- outputs/models/insumos_para_johansen.rds",

  "\n- outputs/models/exclusion_lags_var.rds",

  "\n- outputs/models/vcorr_var_final.rds",

  "\n- outputs/models/white_var_auxiliar.rds",

  "\n- outputs/models/normalidad_arch_var.rds",

  "\n- outputs/models/resumen_modelos_var_finales.txt",

  "\n- figures/10_var_roots.png",

  "\n- figures/10_autocorrelacion_var.png",

  "\n============================================================",
  "\n"
)

insumos_johansen |>
  print(
    n = Inf,
    width = Inf
  )

diagnosticos_rezagos_johansen <- diagnosticos_candidatos |>
  dplyr::filter(
    .data$variant == "seasonal"
  ) |>
  dplyr::select(
    .data$model_key,
    .data$model_label,
    .data$p,
    .data$observations,
    .data$pt_lag,
    .data$pt_p_value,
    .data$bg_lag,
    .data$bg_p_value,
    .data$serial_clean_10pct,
    .data$maximum_root_modulus,
    .data$p_AIC,
    .data$p_HQ,
    .data$p_SC,
    .data$p_FPE
  ) |>
  dplyr::arrange(
    .data$model_key,
    .data$p
  )

diagnosticos_rezagos_johansen |>
  print(
    n = Inf,
    width = Inf
  )

candidatos_limpios_johansen <- diagnosticos_candidatos |>
  dplyr::filter(
    .data$variant == "seasonal",
    .data$status == "OK",
    !is.na(.data$pt_p_value),
    !is.na(.data$bg_p_value)
  ) |>
  dplyr::mutate(
    limpio_10pct =
      .data$pt_p_value > 0.10 &
      .data$bg_p_value > 0.10,

    minimo_p_serial =
      pmin(
        .data$pt_p_value,
        .data$bg_p_value
      )
  ) |>
  dplyr::group_by(
    .data$model_key,
    .data$model_label
  ) |>
  dplyr::arrange(
    dplyr::desc(.data$limpio_10pct),
    .data$p
  ) |>
  dplyr::mutate(
    primer_limpio =
      .data$limpio_10pct &
      dplyr::row_number() ==
      which.max(.data$limpio_10pct)
  ) |>
  dplyr::ungroup()

candidatos_limpios_johansen |>
  dplyr::select(
    .data$model_label,
    .data$p,
    .data$pt_p_value,
    .data$bg_p_value,
    .data$limpio_10pct,
    .data$minimo_p_serial
  ) |>
  print(
    n = Inf,
    width = Inf
  )

diagnosticos_importaciones_variantes <- diagnosticos_candidatos |>
  dplyr::filter(
    model_key == "imports",
    status == "OK"
  ) |>
  dplyr::select(
    model_key,
    model_label,
    variant,
    p,
    observations,
    pt_p_value,
    bg_p_value,
    serial_clean_10pct,
    maximum_root_modulus,
    p_AIC,
    p_HQ,
    p_SC,
    p_FPE
  ) |>
  dplyr::arrange(
    variant,
    p
  )

diagnosticos_importaciones_variantes |>
  print(
    n = Inf,
    width = Inf
  )

vcorr_revision <- vcorr_final |>
  dplyr::filter(
    model_key %in% c(
      "imports",
      "exports_expanded"
    ),
    test_type %in% c(
      "PT.adjusted",
      "BG",
      "ES"
    )
  ) |>
  dplyr::select(
    model_key,
    model_label,
    variant,
    test_type,
    maximum_lag_requested,
    lag,
    statistic,
    p_value,
    decision_10pct,
    status,
    error
  ) |>
  dplyr::arrange(
    model_key,
    variant,
    test_type,
    lag
  )

vcorr_revision |>
  print(
    n = Inf,
    width = Inf
  )

# ------------------------------------------------------------
# 42. CONFIGURACIÓN DEFINITIVA PARA JOHANSEN
# ------------------------------------------------------------

ordenes_johansen_definitivos <- tibble::tibble(

  model_key = c(
    "imports",
    "exports_classic",
    "exports_expanded"
  ),

  K_primary = c(
    2L,
    2L,
    2L
  ),

  K_sensitivity = c(
    NA_integer_,
    NA_integer_,
    4L
  ),

  variant_johansen = c(
    "seasonal",
    "seasonal",
    "seasonal"
  ),

  johansen_role_final = c(

    paste0(
      "Sistema principal. Johansen indicativo por ",
      "autocorrelación, no normalidad y heterocedasticidad."
    ),

    paste0(
      "Sistema principal. Sin autocorrelación; ",
      "Johansen indicativo por no normalidad y ARCH."
    ),

    paste0(
      "Robustez con commodities exógena I(0). ",
      "Johansen indicativo por autocorrelación y no normalidad."
    )
  )
)


# Recuperar los diagnósticos correspondientes al K definitivo.

diagnosticos_k_definitivo <- diagnosticos_candidatos |>
  dplyr::filter(
    variant == "seasonal"
  ) |>
  dplyr::inner_join(
    ordenes_johansen_definitivos |>
      dplyr::select(
        model_key,
        K_primary
      ),
    by = "model_key"
  ) |>
  dplyr::filter(
    p == K_primary
  ) |>
  dplyr::select(
    model_key,
    model_label,
    K_primary,
    observations,
    pt_p_value,
    bg_p_value,
    serial_clean_10pct,
    maximum_root_modulus,
    p_AIC,
    p_HQ,
    p_SC,
    p_FPE
  )


# Crear la tabla final para ca.jo().

insumos_johansen_final <- ordenes_johansen_definitivos |>
  dplyr::left_join(
    diagnosticos_k_definitivo,
    by = c(
      "model_key",
      "K_primary"
    )
  ) |>
  dplyr::mutate(

    K_ca_jo =
      K_primary,

    vecm_short_run_lags =
      K_ca_jo - 1L,

    ecdet =
      "const",

    spec =
      "transitory",

    season =
      4L,

    dumvar = dplyr::case_when(

      model_key == "exports_expanded" ~
        "commodity_c",

      TRUE ~
        "Ninguna"
    ),

    serial_status = dplyr::case_when(

      pt_p_value > 0.10 &
        bg_p_value > 0.10 ~
        "Sin evidencia de autocorrelación al 10%",

      TRUE ~
        paste0(
          "Persiste autocorrelación; ",
          "la inferencia será indicativa"
        )
    ),

    formal_status = dplyr::case_when(

      model_key == "exports_classic" ~
        paste0(
          "Johansen indicativo por rechazo ",
          "de normalidad multivariada"
        ),

      model_key == "imports" ~
        paste0(
          "Johansen indicativo por autocorrelación, ",
          "no normalidad y heterocedasticidad"
        ),

      model_key == "exports_expanded" ~
        paste0(
          "Robustez indicativa con variable exógena I(0)"
        ),

      TRUE ~
        "Indicativo"
    )
  ) |>
  dplyr::select(
    model_key,
    model_label,
    K_ca_jo,
    vecm_short_run_lags,
    K_sensitivity,
    ecdet,
    spec,
    season,
    dumvar,
    pt_p_value,
    bg_p_value,
    maximum_root_modulus,
    serial_status,
    formal_status,
    johansen_role_final
  )


print(
  insumos_johansen_final,
  n = Inf,
  width = Inf
)


readr::write_csv(
  insumos_johansen_final,
  here::here(
    "outputs",
    "tables",
    "insumos_johansen_final.csv"
  )
)


saveRDS(
  insumos_johansen_final,
  here::here(
    "outputs",
    "models",
    "insumos_johansen_final.rds"
  )
)

