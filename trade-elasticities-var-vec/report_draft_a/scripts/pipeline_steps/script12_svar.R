# ============================================================
# TP3 - SERIES DE TIEMPO
# SCRIPT 12
#
# SVAR, FUNCIONES IMPULSO-RESPUESTA,
# DESCOMPOSICIÓN DE VARIANZA Y PROYECCIONES LOCALES
#
# Herramientas:
#   - svars::id.chol()
#   - vars::irf()
#   - vars::fevd()
#   - lpirfs::lp_lin()
#
# Modelos provenientes del Script 11:
#
# 1. Importaciones:
#    K = 2, rango de cointegración r = 2
#
# 2. Exportaciones clásicas:
#    K = 2, rango de cointegración r = 1
#
# 3. Exportaciones ampliadas:
#    K = 2, rango de cointegración r = 1
#
# 4. Exportaciones ampliadas, sensibilidad:
#    K = 4, rango de cointegración r = 1
#
# Horizonte:
#   12 trimestres
#
# Normalización:
#   shock positivo de 1% en la variable impulso
#
# Identificación Cholesky principal:
#   actividad -> ITCRM -> variable comercial
#
# Identificación alternativa:
#   ITCRM -> actividad -> variable comercial
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
  "zoo",
  "vars",
  "svars",
  "lpirfs",
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
  "\nPAQUETES DEL SCRIPT 12 DISPONIBLES",
  "\n============================================================",
  "\nsvars: ",
  as.character(
    packageVersion("svars")
  ),
  "\nvars: ",
  as.character(
    packageVersion("vars")
  ),
  "\nlpirfs: ",
  as.character(
    packageVersion("lpirfs")
  ),
  "\n============================================================\n"
)


# ------------------------------------------------------------
# 2. PARÁMETROS GENERALES
# ------------------------------------------------------------

set.seed(20260623)


# Horizonte h = 0, 1, ..., 12.

horizonte_irf <- 12L


# Shock de 1% en logaritmos.

shock_log <- 0.01


# Valor crítico aproximado para bandas de 90%.

confianza_lp <- 1.645


# Número de núcleos para lpirfs.

numero_nucleos <- 1L


# Las IRF y FEVD principales se calculan directamente a partir
# de la matriz B identificada por svars y las matrices A del VAR.
#
# También se generan los objetos nativos irf() y fevd(), pero un
# eventual error de compatibilidad no detiene el script.

calcular_objetos_nativos <- TRUE


# ------------------------------------------------------------
# 3. CARPETAS
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
# 4. CARGAR RESULTADOS DE LOS SCRIPTS 10 Y 11
# ------------------------------------------------------------

ruta_modelos_vec2var <- here::here(
  "outputs",
  "models",
  "modelos_vec2var_system.rds"
)

ruta_decision_rango <- here::here(
  "outputs",
  "models",
  "decision_rango_johansen.rds"
)

ruta_datos_sistemas <- here::here(
  "outputs",
  "models",
  "var_system_data.rds"
)


rutas_requeridas <- c(
  ruta_modelos_vec2var,
  ruta_decision_rango,
  ruta_datos_sistemas
)


rutas_faltantes <- rutas_requeridas[
  !file.exists(
    rutas_requeridas
  )
]


if (length(rutas_faltantes) > 0) {

  stop(
    paste0(
      "Faltan archivos de los scripts 10 y 11:\n\n",
      paste(
        rutas_faltantes,
        collapse = "\n"
      ),
      "\n\nEjecuta primero los scripts anteriores."
    )
  )
}


modelos_vec2var_system <- readRDS(
  ruta_modelos_vec2var
)

decision_rango <- readRDS(
  ruta_decision_rango
)

datos_sistemas <- readRDS(
  ruta_datos_sistemas
)


# ------------------------------------------------------------
# 5. VERIFICAR LOS OBJETOS CARGADOS
# ------------------------------------------------------------

if (!is.list(modelos_vec2var_system)) {

  stop(
    "modelos_vec2var_system no es una lista."
  )
}


if (!is.data.frame(decision_rango)) {

  stop(
    "decision_rango no es una tabla."
  )
}


if (!is.list(datos_sistemas)) {

  stop(
    "datos_sistemas no es una lista."
  )
}


columnas_requeridas <- c(
  "model_id",
  "model_key",
  "model_label_run",
  "K",
  "rank_system_vecm",
  "is_primary",
  "is_sensitivity",
  "formal_status"
)


columnas_faltantes <- setdiff(
  columnas_requeridas,
  names(decision_rango)
)


if (length(columnas_faltantes) > 0) {

  stop(
    paste0(
      "Faltan columnas en decision_rango:\n",
      paste(
        columnas_faltantes,
        collapse = ", "
      )
    )
  )
}


# ------------------------------------------------------------
# 6. ORDENAMIENTOS CONTEMPORÁNEOS
# ------------------------------------------------------------

orden_principal <- list(

  imports = c(
    "ln_pib_real",
    "ln_itcrm",
    "ln_importaciones_reales"
  ),

  exports_classic = c(
    "ln_pib_socios",
    "ln_itcrm",
    "ln_exportaciones_reales"
  ),

  exports_expanded = c(
    "ln_pib_socios",
    "ln_itcrm",
    "ln_exportaciones_reales"
  )
)


orden_alternativo <- list(

  imports = c(
    "ln_itcrm",
    "ln_pib_real",
    "ln_importaciones_reales"
  ),

  exports_classic = c(
    "ln_itcrm",
    "ln_pib_socios",
    "ln_exportaciones_reales"
  ),

  exports_expanded = c(
    "ln_itcrm",
    "ln_pib_socios",
    "ln_exportaciones_reales"
  )
)


variable_actividad <- c(

  imports =
    "ln_pib_real",

  exports_classic =
    "ln_pib_socios",

  exports_expanded =
    "ln_pib_socios"
)


variable_comercio <- c(

  imports =
    "ln_importaciones_reales",

  exports_classic =
    "ln_exportaciones_reales",

  exports_expanded =
    "ln_exportaciones_reales"
)


etiquetas_variables <- c(

  ln_importaciones_reales =
    "Importaciones reales",

  ln_exportaciones_reales =
    "Exportaciones reales",

  ln_pib_real =
    "PIB argentino",

  ln_pib_socios =
    "PIB de socios",

  ln_itcrm =
    "ITCRM",

  commodity_c =
    "Precio de commodities"
)


# ------------------------------------------------------------
# 7. CONFIGURACIÓN DE LOS MODELOS
# ------------------------------------------------------------

config_modelos_12 <- decision_rango |>
  dplyr::mutate(

    order_primary =
      purrr::map(
        model_key,
        ~ orden_principal[[.x]]
      ),

    order_alternative =
      purrr::map(
        model_key,
        ~ orden_alternativo[[.x]]
      ),

    activity_variable =
      purrr::map_chr(
        model_key,
        ~ unname(
          variable_actividad[[.x]]
        )
      ),

    trade_variable =
      purrr::map_chr(
        model_key,
        ~ unname(
          variable_comercio[[.x]]
        )
      ),

    model_short =
      dplyr::case_when(

        model_key == "imports" ~
          "Importaciones",

        model_key == "exports_classic" ~
          "Exportaciones clásicas",

        model_key == "exports_expanded" &
          is_primary ~
          "Exportaciones ampliadas",

        model_key == "exports_expanded" &
          is_sensitivity ~
          "Exportaciones ampliadas K=4",

        TRUE ~
          model_label_run
      ),

    lp_lags =
      pmax(
        K - 1L,
        1L
      ),

    recommended_model =
      is_primary
  )


print(
  config_modelos_12,
  n = Inf,
  width = Inf
)


stopifnot(
  nrow(config_modelos_12) == 4,
  all(config_modelos_12$K >= 2),
  all(config_modelos_12$rank_system_vecm >= 1)
)


# ------------------------------------------------------------
# 8. VERIFICAR VARIABLES Y MODELOS
# ------------------------------------------------------------

for (
  i in seq_len(
    nrow(config_modelos_12)
  )
) {

  model_id_i <-
    config_modelos_12$model_id[[i]]

  model_key_i <-
    config_modelos_12$model_key[[i]]


  modelo_i <-
    modelos_vec2var_system[[model_id_i]]


  if (is.null(modelo_i)) {

    stop(
      paste0(
        "No se encontró el modelo vec2var:\n",
        model_id_i
      )
    )
  }


  if (!inherits(
    modelo_i,
    "vec2var"
  )) {

    stop(
      paste0(
        "El objeto no es de clase vec2var:\n",
        model_id_i
      )
    )
  }


  variables_i <- colnames(
    datos_sistemas[[model_key_i]]$y
  )


  if (
    !setequal(
      variables_i,
      config_modelos_12$order_primary[[i]]
    )
  ) {

    stop(
      paste0(
        "El orden principal no coincide con las variables de:\n",
        model_id_i
      )
    )
  }


  if (
    !setequal(
      variables_i,
      config_modelos_12$order_alternative[[i]]
    )
  ) {

    stop(
      paste0(
        "El orden alternativo no coincide con las variables de:\n",
        model_id_i
      )
    )
  }
}


# ------------------------------------------------------------
# 9. FUNCIÓN PARA NOMBRES DE ARCHIVOS
# ------------------------------------------------------------

nombre_seguro <- function(
    texto
) {

  resultado <- gsub(
    pattern = "[^A-Za-z0-9]+",
    replacement = "_",
    x = texto
  )

  resultado <- gsub(
    pattern = "^_+|_+$",
    replacement = "",
    x = resultado
  )

  tolower(
    resultado
  )
}


# ------------------------------------------------------------
# 10. FUNCIÓN PARA ETIQUETAR VARIABLES
# ------------------------------------------------------------

agregar_etiquetas <- function(
    tabla
) {

  tabla |>
    dplyr::mutate(

      impulse_label =
        unname(
          etiquetas_variables[
            impulse
          ]
        ),

      response_label =
        unname(
          etiquetas_variables[
            response
          ]
        ),

      impulse_label =
        dplyr::if_else(
          is.na(impulse_label),
          impulse,
          impulse_label
        ),

      response_label =
        dplyr::if_else(
          is.na(response_label),
          response,
          response_label
        ),

      shock_label =
        paste0(
          "Shock de ",
          impulse_label,
          " (+1%)"
        )
    )
}


# ------------------------------------------------------------
# 11. IDENTIFICACIÓN CHOLESKY
# ------------------------------------------------------------

identificar_cholesky <- function(
    modelo,
    orden,
    variables
) {

  resultado <- svars::id.chol(

    x =
      modelo,

    order_k =
      orden
  )


  resultado$B <- as.matrix(
    resultado$B
  )


  if (
    is.null(
      rownames(
        resultado$B
      )
    )
  ) {

    rownames(
      resultado$B
    ) <- variables
  }


  if (
    is.null(
      colnames(
        resultado$B
      )
    )
  ) {

    colnames(
      resultado$B
    ) <- variables
  }


  resultado
}


svar_cholesky_primary <- list()

svar_cholesky_alternative <- list()

lista_estado_identificacion <- list()

contador_estado <- 1L


for (
  i in seq_len(
    nrow(config_modelos_12)
  )
) {

  model_id_i <-
    config_modelos_12$model_id[[i]]

  model_key_i <-
    config_modelos_12$model_key[[i]]

  modelo_i <-
    modelos_vec2var_system[[model_id_i]]

  variables_i <- colnames(
    datos_sistemas[[model_key_i]]$y
  )


  cat(
    "\n============================================================",
    "\nIDENTIFICACIÓN CHOLESKY",
    "\nModelo:",
    config_modelos_12$model_label_run[[i]],
    "\n============================================================\n"
  )


  principal_i <- tryCatch(

    identificar_cholesky(

      modelo =
        modelo_i,

      orden =
        config_modelos_12$order_primary[[i]],

      variables =
        variables_i
    ),

    error = function(e) {
      e
    }
  )


  if (!inherits(
    principal_i,
    "error"
  )) {

    svar_cholesky_primary[[model_id_i]] <-
      principal_i
  }


  lista_estado_identificacion[[
    contador_estado
  ]] <- tibble::tibble(

    model_id =
      model_id_i,

    model_key =
      model_key_i,

    model_label_run =
      config_modelos_12$model_label_run[[i]],

    identification =
      "Cholesky principal",

    causal_order =
      paste(
        config_modelos_12$order_primary[[i]],
        collapse = " -> "
      ),

    status = if (
      inherits(
        principal_i,
        "error"
      )
    ) {
      "ERROR"
    } else {
      "OK"
    },

    error = if (
      inherits(
        principal_i,
        "error"
      )
    ) {
      conditionMessage(
        principal_i
      )
    } else {
      NA_character_
    }
  )


  contador_estado <-
    contador_estado + 1L


  alternativo_i <- tryCatch(

    identificar_cholesky(

      modelo =
        modelo_i,

      orden =
        config_modelos_12$order_alternative[[i]],

      variables =
        variables_i
    ),

    error = function(e) {
      e
    }
  )


  if (!inherits(
    alternativo_i,
    "error"
  )) {

    svar_cholesky_alternative[[model_id_i]] <-
      alternativo_i
  }


  lista_estado_identificacion[[
    contador_estado
  ]] <- tibble::tibble(

    model_id =
      model_id_i,

    model_key =
      model_key_i,

    model_label_run =
      config_modelos_12$model_label_run[[i]],

    identification =
      "Cholesky alternativo",

    causal_order =
      paste(
        config_modelos_12$order_alternative[[i]],
        collapse = " -> "
      ),

    status = if (
      inherits(
        alternativo_i,
        "error"
      )
    ) {
      "ERROR"
    } else {
      "OK"
    },

    error = if (
      inherits(
        alternativo_i,
        "error"
      )
    ) {
      conditionMessage(
        alternativo_i
      )
    } else {
      NA_character_
    }
  )


  contador_estado <-
    contador_estado + 1L
}


estado_identificacion <- dplyr::bind_rows(
  lista_estado_identificacion
)


print(
  estado_identificacion,
  n = Inf,
  width = Inf
)


if (
  any(
    estado_identificacion$status[
      estado_identificacion$identification ==
      "Cholesky principal"
    ] != "OK"
  )
) {

  stop(
    paste0(
      "Falló alguna identificación Cholesky principal.\n",
      "Revisa el objeto estado_identificacion."
    )
  )
}


# ------------------------------------------------------------
# 12. EXTRAER MATRICES DE IMPACTO B
# ------------------------------------------------------------

extraer_matriz_B <- function(
    objeto_svar,
    model_id_arg,
    model_key_arg,
    model_label_arg,
    model_short_arg,
    method_code_arg,
    method_arg,
    causal_order_arg
) {

  B <- as.matrix(
    objeto_svar$B
  )


  indices <- expand.grid(

    response_index =
      seq_len(
        nrow(B)
      ),

    impulse_index =
      seq_len(
        ncol(B)
      ),

    KEEP.OUT.ATTRS =
      FALSE,

    stringsAsFactors =
      FALSE
  )


  tibble::tibble(

    model_id =
      model_id_arg,

    model_key =
      model_key_arg,

    model_label_run =
      model_label_arg,

    model_short =
      model_short_arg,

    method_code =
      method_code_arg,

    method =
      method_arg,

    causal_order =
      paste(
        causal_order_arg,
        collapse = " -> "
      ),

    response =
      rownames(B)[
        indices$response_index
      ],

    impulse =
      colnames(B)[
        indices$impulse_index
      ],

    impact_coefficient =
      as.numeric(
        B[
          cbind(
            indices$response_index,
            indices$impulse_index
          )
        ]
      )
  )
}


lista_matrices_B <- list()

contador_B <- 1L


for (
  i in seq_len(
    nrow(config_modelos_12)
  )
) {

  model_id_i <-
    config_modelos_12$model_id[[i]]


  lista_matrices_B[[
    contador_B
  ]] <- extraer_matriz_B(

    objeto_svar =
      svar_cholesky_primary[[model_id_i]],

    model_id_arg =
      model_id_i,

    model_key_arg =
      config_modelos_12$model_key[[i]],

    model_label_arg =
      config_modelos_12$model_label_run[[i]],

    model_short_arg =
      config_modelos_12$model_short[[i]],

    method_code_arg =
      "chol_primary",

    method_arg =
      "SVAR Cholesky principal",

    causal_order_arg =
      config_modelos_12$order_primary[[i]]
  )


  contador_B <-
    contador_B + 1L


  if (
    !is.null(
      svar_cholesky_alternative[[model_id_i]]
    )
  ) {

    lista_matrices_B[[
      contador_B
    ]] <- extraer_matriz_B(

      objeto_svar =
        svar_cholesky_alternative[[model_id_i]],

      model_id_arg =
        model_id_i,

      model_key_arg =
        config_modelos_12$model_key[[i]],

      model_label_arg =
        config_modelos_12$model_label_run[[i]],

      model_short_arg =
        config_modelos_12$model_short[[i]],

      method_code_arg =
        "chol_alternative",

      method_arg =
        "SVAR Cholesky alternativo",

      causal_order_arg =
        config_modelos_12$order_alternative[[i]]
    )


    contador_B <-
      contador_B + 1L
  }
}


matrices_impacto_B <- dplyr::bind_rows(
  lista_matrices_B
) |>
  agregar_etiquetas()


# ------------------------------------------------------------
# 13. OBJETOS NATIVOS irf() Y fevd()
# ------------------------------------------------------------

objetos_irf_nativos <- list()

objetos_fevd_nativos <- list()

lista_estado_nativo <- list()

contador_nativo <- 1L


for (
  i in seq_len(
    nrow(config_modelos_12)
  )
) {

  model_id_i <-
    config_modelos_12$model_id[[i]]


  identificaciones_i <- list(

    chol_primary =
      svar_cholesky_primary[[model_id_i]],

    chol_alternative =
      svar_cholesky_alternative[[model_id_i]]
  )


  etiquetas_i <- c(

    chol_primary =
      "SVAR Cholesky principal",

    chol_alternative =
      "SVAR Cholesky alternativo"
  )


  for (
    method_code_i in names(
      identificaciones_i
    )
  ) {

    objeto_i <-
      identificaciones_i[[method_code_i]]


    if (is.null(objeto_i)) {
      next
    }


    clave_i <- paste0(
      model_id_i,
      "__",
      method_code_i
    )


    if (calcular_objetos_nativos) {

      irf_i <- tryCatch(

        vars::irf(

          objeto_i,

          n.ahead =
            horizonte_irf
        ),

        error = function(e) {
          e
        }
      )


      fevd_i <- tryCatch(

        vars::fevd(

          objeto_i,

          n.ahead =
            horizonte_irf
        ),

        error = function(e) {
          e
        }
      )

    } else {

      irf_i <- structure(
        list(
          message =
            "No ejecutado"
        ),
        class =
          "native_not_run"
      )


      fevd_i <- structure(
        list(
          message =
            "No ejecutado"
        ),
        class =
          "native_not_run"
      )
    }


    if (
      calcular_objetos_nativos &&
      !inherits(
        irf_i,
        "error"
      )
    ) {

      objetos_irf_nativos[[clave_i]] <-
        irf_i
    }


    if (
      calcular_objetos_nativos &&
      !inherits(
        fevd_i,
        "error"
      )
    ) {

      objetos_fevd_nativos[[clave_i]] <-
        fevd_i
    }


    lista_estado_nativo[[
      contador_nativo
    ]] <- tibble::tibble(

      model_id =
        model_id_i,

      model_key =
        config_modelos_12$model_key[[i]],

      model_label_run =
        config_modelos_12$model_label_run[[i]],

      method_code =
        method_code_i,

      method =
        etiquetas_i[[method_code_i]],

      irf_status = dplyr::case_when(

        !calcular_objetos_nativos ~
          "NO EJECUTADO",

        inherits(
          irf_i,
          "error"
        ) ~
          "ERROR",

        TRUE ~
          "OK"
      ),

      irf_error = if (
        calcular_objetos_nativos &&
        inherits(
          irf_i,
          "error"
        )
      ) {
        conditionMessage(
          irf_i
        )
      } else {
        NA_character_
      },

      fevd_status = dplyr::case_when(

        !calcular_objetos_nativos ~
          "NO EJECUTADO",

        inherits(
          fevd_i,
          "error"
        ) ~
          "ERROR",

        TRUE ~
          "OK"
      ),

      fevd_error = if (
        calcular_objetos_nativos &&
        inherits(
          fevd_i,
          "error"
        )
      ) {
        conditionMessage(
          fevd_i
        )
      } else {
        NA_character_
      }
    )


    contador_nativo <-
      contador_nativo + 1L
  }
}


estado_objetos_nativos <- dplyr::bind_rows(
  lista_estado_nativo
)


print(
  estado_objetos_nativos,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 14. MATRICES DE MEDIA MÓVIL
# ------------------------------------------------------------

calcular_matrices_ma <- function(
    matrices_A,
    horizonte
) {

  numero_rezagos <- length(
    matrices_A
  )

  numero_variables <- nrow(
    matrices_A[[1]]
  )


  Phi <- vector(
    mode = "list",
    length = horizonte + 1L
  )


  Phi[[1]] <- diag(
    numero_variables
  )


  if (horizonte >= 1L) {

    for (
      h in seq_len(
        horizonte
      )
    ) {

      acumulador <- matrix(
        0,
        nrow =
          numero_variables,
        ncol =
          numero_variables
      )


      maximo_rezago <- min(
        numero_rezagos,
        h
      )


      for (
        rezago_i in seq_len(
          maximo_rezago
        )
      ) {

        acumulador <-
          acumulador +
          matrices_A[[rezago_i]] %*%
          Phi[[
            h - rezago_i + 1L
          ]]
      }


      Phi[[h + 1L]] <-
        acumulador
    }
  }


  Phi
}


# ------------------------------------------------------------
# 15. CALCULAR IRF ESTRUCTURALES
# ------------------------------------------------------------

calcular_irf_estructural <- function(
    modelo_vec2var,
    objeto_svar,
    horizonte,
    model_id_arg,
    model_key_arg,
    model_label_arg,
    model_short_arg,
    method_code_arg,
    method_arg
) {

  B <- as.matrix(
    objeto_svar$B
  )


  variables <- colnames(
    modelo_vec2var$y
  )


  if (is.null(variables)) {

    variables <- rownames(
      B
    )
  }


  rownames(B) <- variables

  colnames(B) <- variables


  matrices_A <- modelo_vec2var$A


  matrices_A <- lapply(

    matrices_A,

    function(A_i) {

      A_i <- as.matrix(
        A_i
      )

      rownames(A_i) <- variables

      colnames(A_i) <- variables

      A_i
    }
  )


  Phi <- calcular_matrices_ma(

    matrices_A =
      matrices_A,

    horizonte =
      horizonte
  )


  resultados <- list()

  contador <- 1L


  for (
    impulso_i in seq_along(
      variables
    )
  ) {

    impacto_propio <- B[
      impulso_i,
      impulso_i
    ]


    factor_normalizacion <- if (
      is.na(
        impacto_propio
      ) ||
      abs(
        impacto_propio
      ) <
      0.0000000001
    ) {

      NA_real_

    } else {

      shock_log /
        impacto_propio
    }


    for (
      h in 0:horizonte
    ) {

      respuesta_h <- Phi[[
        h + 1L
      ]] %*%
        B


      for (
        respuesta_i in seq_along(
          variables
        )
      ) {

        respuesta_log <-
          respuesta_h[
            respuesta_i,
            impulso_i
          ] *
          factor_normalizacion


        resultados[[
          contador
        ]] <- tibble::tibble(

          model_id =
            model_id_arg,

          model_key =
            model_key_arg,

          model_label_run =
            model_label_arg,

          model_short =
            model_short_arg,

          method_code =
            method_code_arg,

          method =
            method_arg,

          impulse =
            variables[
              impulso_i
            ],

          response =
            variables[
              respuesta_i
            ],

          horizon =
            h,

          own_impact =
            impacto_propio,

          normalization_factor =
            factor_normalizacion,

          shock_size_percent =
            100 *
            shock_log,

          response_log =
            respuesta_log,

          response_percent =
            100 *
            respuesta_log
        )


        contador <-
          contador + 1L
      }
    }
  }


  dplyr::bind_rows(
    resultados
  )
}


# ------------------------------------------------------------
# 16. CALCULAR FEVD ESTRUCTURAL
# ------------------------------------------------------------

calcular_fevd_estructural <- function(
    modelo_vec2var,
    objeto_svar,
    horizonte,
    model_id_arg,
    model_key_arg,
    model_label_arg,
    model_short_arg,
    method_code_arg,
    method_arg
) {

  B <- as.matrix(
    objeto_svar$B
  )


  variables <- colnames(
    modelo_vec2var$y
  )


  if (is.null(variables)) {

    variables <- rownames(
      B
    )
  }


  rownames(B) <- variables

  colnames(B) <- variables


  matrices_A <- lapply(

    modelo_vec2var$A,

    function(A_i) {

      A_i <- as.matrix(
        A_i
      )

      rownames(A_i) <- variables

      colnames(A_i) <- variables

      A_i
    }
  )


  Phi <- calcular_matrices_ma(

    matrices_A =
      matrices_A,

    horizonte =
      horizonte - 1L
  )


  resultados <- list()

  contador <- 1L


  for (
    h in seq_len(
      horizonte
    )
  ) {

    numeradores <- matrix(
      0,
      nrow =
        length(
          variables
        ),
      ncol =
        length(
          variables
        )
    )


    for (
      s in 0:(h - 1L)
    ) {

      theta_s <- Phi[[
        s + 1L
      ]] %*%
        B


      numeradores <-
        numeradores +
        theta_s^2
    }


    denominador <- rowSums(
      numeradores
    )


    participaciones <- sweep(

      numeradores,

      MARGIN =
        1,

      STATS =
        denominador,

      FUN =
        "/"
    )


    for (
      respuesta_i in seq_along(
        variables
      )
    ) {

      for (
        impulso_i in seq_along(
          variables
        )
      ) {

        resultados[[
          contador
        ]] <- tibble::tibble(

          model_id =
            model_id_arg,

          model_key =
            model_key_arg,

          model_label_run =
            model_label_arg,

          model_short =
            model_short_arg,

          method_code =
            method_code_arg,

          method =
            method_arg,

          response =
            variables[
              respuesta_i
            ],

          impulse =
            variables[
              impulso_i
            ],

          horizon =
            h,

          fevd_share =
            participaciones[
              respuesta_i,
              impulso_i
            ],

          fevd_percent =
            100 *
            participaciones[
              respuesta_i,
              impulso_i
            ]
        )


        contador <-
          contador + 1L
      }
    }
  }


  dplyr::bind_rows(
    resultados
  )
}


# ------------------------------------------------------------
# 17. IRF Y FEVD PARA TODOS LOS MODELOS
# ------------------------------------------------------------

lista_irf_svar <- list()

lista_fevd_svar <- list()

contador_irf <- 1L

contador_fevd <- 1L


for (
  i in seq_len(
    nrow(config_modelos_12)
  )
) {

  model_id_i <-
    config_modelos_12$model_id[[i]]

  modelo_i <-
    modelos_vec2var_system[[model_id_i]]


  identificaciones_i <- list(

    chol_primary = list(

      object =
        svar_cholesky_primary[[model_id_i]],

      label =
        "SVAR Cholesky principal"
    ),

    chol_alternative = list(

      object =
        svar_cholesky_alternative[[model_id_i]],

      label =
        "SVAR Cholesky alternativo"
    )
  )


  for (
    method_code_i in names(
      identificaciones_i
    )
  ) {

    objeto_i <-
      identificaciones_i[[
        method_code_i
      ]]$object


    if (is.null(objeto_i)) {
      next
    }


    etiqueta_i <-
      identificaciones_i[[
        method_code_i
      ]]$label


    lista_irf_svar[[
      contador_irf
    ]] <- calcular_irf_estructural(

      modelo_vec2var =
        modelo_i,

      objeto_svar =
        objeto_i,

      horizonte =
        horizonte_irf,

      model_id_arg =
        model_id_i,

      model_key_arg =
        config_modelos_12$model_key[[i]],

      model_label_arg =
        config_modelos_12$model_label_run[[i]],

      model_short_arg =
        config_modelos_12$model_short[[i]],

      method_code_arg =
        method_code_i,

      method_arg =
        etiqueta_i
    )


    contador_irf <-
      contador_irf + 1L


    lista_fevd_svar[[
      contador_fevd
    ]] <- calcular_fevd_estructural(

      modelo_vec2var =
        modelo_i,

      objeto_svar =
        objeto_i,

      horizonte =
        horizonte_irf,

      model_id_arg =
        model_id_i,

      model_key_arg =
        config_modelos_12$model_key[[i]],

      model_label_arg =
        config_modelos_12$model_label_run[[i]],

      model_short_arg =
        config_modelos_12$model_short[[i]],

      method_code_arg =
        method_code_i,

      method_arg =
        etiqueta_i
    )


    contador_fevd <-
      contador_fevd + 1L
  }
}


irf_svar <- dplyr::bind_rows(
  lista_irf_svar
) |>
  agregar_etiquetas()


fevd_svar <- dplyr::bind_rows(
  lista_fevd_svar
) |>
  agregar_etiquetas()


# ------------------------------------------------------------
# 18. CONTROLES PARA PROYECCIONES LOCALES
# ------------------------------------------------------------

crear_controles_lp <- function(
    model_key,
    y_diferencias
) {

  trimestre <- as.integer(
    stats::cycle(
      y_diferencias[, 1]
    )
  )


  controles <- data.frame(

    Q2 =
      as.integer(
        trimestre == 2
      ),

    Q3 =
      as.integer(
        trimestre == 3
      ),

    Q4 =
      as.integer(
        trimestre == 4
      )
  )


  exog_i <- datos_sistemas[[
    model_key
  ]]$exogen


  if (!is.null(exog_i)) {

    exog_alineada <- stats::window(

      exog_i,

      start =
        stats::start(
          y_diferencias
        ),

      end =
        stats::end(
          y_diferencias
        )
    )


    exog_alineada <- as.data.frame(
      exog_alineada
    )


    if (
      nrow(exog_alineada) !=
      nrow(controles)
    ) {

      stop(
        paste0(
          "La variable exógena no quedó alineada para:\n",
          model_key
        )
      )
    }


    controles <- cbind(
      controles,
      exog_alineada
    )
  }


  as.data.frame(
    controles
  )
}


# ------------------------------------------------------------
# 19. ESTIMAR PROYECCIONES LOCALES
# ------------------------------------------------------------

modelos_lpirfs <- list()

lista_estado_lpirfs <- list()

contador_lpirfs <- 1L


for (
  i in seq_len(
    nrow(config_modelos_12)
  )
) {

  model_id_i <-
    config_modelos_12$model_id[[i]]

  model_key_i <-
    config_modelos_12$model_key[[i]]


  y_niveles <- datos_sistemas[[
    model_key_i
  ]]$y


  y_diferencias <- diff(
    y_niveles
  )


  orden_i <-
    config_modelos_12$order_primary[[i]]


  y_diferencias_ordenadas <- y_diferencias[
    ,
    orden_i,
    drop = FALSE
  ]


  datos_endogenos_lp <- as.data.frame(
    y_diferencias_ordenadas
  )


  controles_lp <- crear_controles_lp(

    model_key =
      model_key_i,

    y_diferencias =
      y_diferencias_ordenadas
  )


  cat(
    "\n============================================================",
    "\nPROYECCIONES LOCALES",
    "\nModelo:",
    config_modelos_12$model_label_run[[i]],
    "\nRezagos:",
    config_modelos_12$lp_lags[[i]],
    "\n============================================================\n"
  )


  lp_i <- tryCatch(

    lpirfs::lp_lin(

      endog_data =
        datos_endogenos_lp,

      lags_endog_lin =
        as.integer(
          config_modelos_12$lp_lags[[i]]
        ),

      lags_criterion =
        NaN,

      max_lags =
        NaN,

      trend =
        0,

      shock_type =
        1,

      confint =
        confianza_lp,

      use_nw =
        TRUE,

      nw_lag =
        NULL,

      nw_prewhite =
        FALSE,

      adjust_se =
        TRUE,

      hor =
        horizonte_irf + 1L,

      exog_data =
        NULL,

      lags_exog =
        NULL,

      contemp_data =
        controles_lp,

      num_cores =
        numero_nucleos
    ),

    error = function(e) {
      e
    }
  )


  if (!inherits(
    lp_i,
    "error"
  )) {

    modelos_lpirfs[[model_id_i]] <-
      lp_i
  }


  lista_estado_lpirfs[[
    contador_lpirfs
  ]] <- tibble::tibble(

    model_id =
      model_id_i,

    model_key =
      model_key_i,

    model_label_run =
      config_modelos_12$model_label_run[[i]],

    causal_order =
      paste(
        orden_i,
        collapse = " -> "
      ),

    endogenous_lags =
      config_modelos_12$lp_lags[[i]],

    status = if (
      inherits(
        lp_i,
        "error"
      )
    ) {
      "ERROR"
    } else {
      "OK"
    },

    error = if (
      inherits(
        lp_i,
        "error"
      )
    ) {
      conditionMessage(
        lp_i
      )
    } else {
      NA_character_
    }
  )


  contador_lpirfs <-
    contador_lpirfs + 1L
}


estado_lpirfs <- dplyr::bind_rows(
  lista_estado_lpirfs
)


print(
  estado_lpirfs,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 20. EXTRAER RESULTADOS DE lpirfs
# ------------------------------------------------------------

extraer_lpirfs <- function(
    objeto_lp,
    orden_variables,
    model_id_arg,
    model_key_arg,
    model_label_arg,
    model_short_arg
) {

  media <- objeto_lp$irf_lin_mean

  inferior <- objeto_lp$irf_lin_low

  superior <- objeto_lp$irf_lin_up


  dimensiones <- dim(
    media
  )


  if (
    length(dimensiones) != 3L
  ) {

    stop(
      paste0(
        "irf_lin_mean no tiene tres dimensiones para:\n",
        model_id_arg
      )
    )
  }


  numero_respuestas <- dimensiones[1]

  numero_horizontes <- dimensiones[2]

  numero_shocks <- dimensiones[3]


  if (
    numero_respuestas !=
    length(orden_variables) ||
    numero_shocks !=
    length(orden_variables)
  ) {

    stop(
      paste0(
        "Las dimensiones de lpirfs no coinciden para:\n",
        model_id_arg
      )
    )
  }


  horizontes <- seq_len(
    numero_horizontes
  ) - 1L


  resultados <- list()

  contador <- 1L


  for (
    impulso_i in seq_len(
      numero_shocks
    )
  ) {

    for (
      respuesta_i in seq_len(
        numero_respuestas
      )
    ) {

      respuesta_diferencia <- as.numeric(
        media[
          respuesta_i,
          ,
          impulso_i
        ]
      )


      inferior_diferencia <- as.numeric(
        inferior[
          respuesta_i,
          ,
          impulso_i
        ]
      )


      superior_diferencia <- as.numeric(
        superior[
          respuesta_i,
          ,
          impulso_i
        ]
      )


      # Las variables se estiman en diferencias logarítmicas.
      # La acumulación aproxima la respuesta del logaritmo
      # en niveles.

      respuesta_acumulada <- cumsum(
        respuesta_diferencia
      )


      inferior_acumulado <- cumsum(
        inferior_diferencia
      )


      superior_acumulado <- cumsum(
        superior_diferencia
      )


      limite_bajo <- pmin(
        inferior_acumulado,
        superior_acumulado
      )


      limite_alto <- pmax(
        inferior_acumulado,
        superior_acumulado
      )


      resultados[[
        contador
      ]] <- tibble::tibble(

        model_id =
          model_id_arg,

        model_key =
          model_key_arg,

        model_label_run =
          model_label_arg,

        model_short =
          model_short_arg,

        method_code =
          "local_projection",

        method =
          "Proyecciones locales",

        impulse =
          orden_variables[
            impulso_i
          ],

        response =
          orden_variables[
            respuesta_i
          ],

        horizon =
          horizontes,

        response_difference_unit_shock =
          respuesta_diferencia,

        response_percent =
          respuesta_acumulada,

        lower_90 =
          limite_bajo,

        upper_90 =
          limite_alto,

        shock_size_percent =
          1,

        confidence_note =
          paste0(
            "Bandas acumuladas aproximadas a partir de ",
            "errores Newey-West de respuestas en diferencias."
          )
      )


      contador <-
        contador + 1L
    }
  }


  dplyr::bind_rows(
    resultados
  )
}


lista_lpirfs_tidy <- list()

contador_lpirfs_tidy <- 1L


for (
  i in seq_len(
    nrow(config_modelos_12)
  )
) {

  model_id_i <-
    config_modelos_12$model_id[[i]]


  objeto_lp_i <-
    modelos_lpirfs[[model_id_i]]


  if (is.null(objeto_lp_i)) {
    next
  }


  resultado_i <- tryCatch(

    extraer_lpirfs(

      objeto_lp =
        objeto_lp_i,

      orden_variables =
        config_modelos_12$order_primary[[i]],

      model_id_arg =
        model_id_i,

      model_key_arg =
        config_modelos_12$model_key[[i]],

      model_label_arg =
        config_modelos_12$model_label_run[[i]],

      model_short_arg =
        config_modelos_12$model_short[[i]]
    ),

    error = function(e) {

      warning(
        paste0(
          "No se pudieron extraer las proyecciones locales de ",
          model_id_i,
          ":\n",
          conditionMessage(e)
        )
      )

      NULL
    }
  )


  if (!is.null(resultado_i)) {

    lista_lpirfs_tidy[[
      contador_lpirfs_tidy
    ]] <- resultado_i

    contador_lpirfs_tidy <-
      contador_lpirfs_tidy + 1L
  }
}


if (length(lista_lpirfs_tidy) > 0) {

  lpirfs_tidy <- dplyr::bind_rows(
    lista_lpirfs_tidy
  ) |>
    dplyr::filter(
      horizon <=
        horizonte_irf
    ) |>
    agregar_etiquetas()

} else {

  lpirfs_tidy <- tibble::tibble(

    model_id =
      character(),

    model_key =
      character(),

    model_label_run =
      character(),

    model_short =
      character(),

    method_code =
      character(),

    method =
      character(),

    impulse =
      character(),

    response =
      character(),

    horizon =
      integer(),

    response_difference_unit_shock =
      double(),

    response_percent =
      double(),

    lower_90 =
      double(),

    upper_90 =
      double(),

    shock_size_percent =
      double(),

    confidence_note =
      character(),

    impulse_label =
      character(),

    response_label =
      character(),

    shock_label =
      character()
  )
}


# ------------------------------------------------------------
# 21. INFORMACIÓN DE LOS MODELOS
# ------------------------------------------------------------

config_variables <- config_modelos_12 |>
  dplyr::select(
    model_id,
    model_key,
    model_short,
    is_primary,
    is_sensitivity,
    recommended_model,
    activity_variable,
    trade_variable,
    K,
    rank_system_vecm,
    formal_status
  )


# ------------------------------------------------------------
# 22. RESPUESTAS DE LAS VARIABLES COMERCIALES
# ------------------------------------------------------------

irf_comercio <- irf_svar |>
  dplyr::left_join(

    config_variables,

    by = c(
      "model_id",
      "model_key",
      "model_short"
    )
  ) |>
  dplyr::filter(

    response ==
      trade_variable,

    impulse %in%
      c(
        activity_variable,
        "ln_itcrm"
      )
  )


fevd_comercio <- fevd_svar |>
  dplyr::left_join(

    config_variables,

    by = c(
      "model_id",
      "model_key",
      "model_short"
    )
  ) |>
  dplyr::filter(
    response ==
      trade_variable
  )


if (nrow(lpirfs_tidy) > 0) {

  lpirfs_comercio <- lpirfs_tidy |>
    dplyr::left_join(

      config_variables,

      by = c(
        "model_id",
        "model_key",
        "model_short"
      )
    ) |>
    dplyr::filter(

      response ==
        trade_variable,

      impulse %in%
        c(
          activity_variable,
          "ln_itcrm"
        )
    )

} else {

  lpirfs_comercio <- lpirfs_tidy
}


# ------------------------------------------------------------
# 23. UNIR SVAR Y PROYECCIONES LOCALES
# ------------------------------------------------------------

irf_comparacion <- dplyr::bind_rows(

  irf_comercio |>
    dplyr::transmute(
      model_id,
      model_key,
      model_short,
      is_primary,
      is_sensitivity,
      recommended_model,
      activity_variable,
      trade_variable,
      method_code,
      method,
      impulse,
      impulse_label,
      shock_label,
      response,
      response_label,
      horizon,
      response_percent,
      lower_90 =
        NA_real_,
      upper_90 =
        NA_real_
    ),

  lpirfs_comercio |>
    dplyr::transmute(
      model_id,
      model_key,
      model_short,
      is_primary,
      is_sensitivity,
      recommended_model,
      activity_variable,
      trade_variable,
      method_code,
      method,
      impulse,
      impulse_label,
      shock_label,
      response,
      response_label,
      horizon,
      response_percent,
      lower_90,
      upper_90
    )
)


# ------------------------------------------------------------
# 24. FUNCIONES AUXILIARES PARA EL RESUMEN
# ------------------------------------------------------------

valor_horizonte <- function(
    horizonte,
    valor,
    objetivo
) {

  datos_validos <- !is.na(
    horizonte
  ) &
    !is.na(
      valor
    )


  if (!any(datos_validos)) {

    return(
      NA_real_
    )
  }


  horizonte_valido <- horizonte[
    datos_validos
  ]

  valor_valido <- valor[
    datos_validos
  ]


  indice <- which.min(
    abs(
      horizonte_valido -
        objetivo
    )
  )


  as.numeric(
    valor_valido[
      indice
    ]
  )
}


valor_pico <- function(
    horizonte,
    valor
) {

  datos_validos <- !is.na(
    horizonte
  ) &
    !is.na(
      valor
    )


  if (!any(datos_validos)) {

    return(
      list(
        value =
          NA_real_,
        horizon =
          NA_integer_
      )
    )
  }


  horizonte_valido <- horizonte[
    datos_validos
  ]

  valor_valido <- valor[
    datos_validos
  ]


  indice <- which.max(
    abs(
      valor_valido
    )
  )


  list(

    value =
      as.numeric(
        valor_valido[
          indice
        ]
      ),

    horizon =
      as.integer(
        horizonte_valido[
          indice
        ]
      )
  )
}


# ------------------------------------------------------------
# 25. RESUMEN DE RESPUESTAS
# ------------------------------------------------------------

resumen_respuestas <- irf_comparacion |>
  dplyr::group_by(
    model_id,
    model_key,
    model_short,
    is_primary,
    is_sensitivity,
    recommended_model,
    activity_variable,
    trade_variable,
    method_code,
    method,
    impulse,
    impulse_label,
    response,
    response_label
  ) |>
  dplyr::summarise(

    impact_response =
      valor_horizonte(
        horizon,
        response_percent,
        0L
      ),

    response_h1 =
      valor_horizonte(
        horizon,
        response_percent,
        1L
      ),

    response_h4 =
      valor_horizonte(
        horizon,
        response_percent,
        4L
      ),

    response_h8 =
      valor_horizonte(
        horizon,
        response_percent,
        8L
      ),

    response_h12 =
      valor_horizonte(
        horizon,
        response_percent,
        12L
      ),

    maximum_absolute_response =
      valor_pico(
        horizon,
        response_percent
      )$value,

    peak_horizon =
      valor_pico(
        horizon,
        response_percent
      )$horizon,

    .groups =
      "drop"
  ) |>
  dplyr::mutate(

    expected_sign =
      dplyr::case_when(

        impulse ==
          activity_variable ~
          "Positivo",

        model_key ==
          "imports" &
          impulse ==
          "ln_itcrm" ~
          "Negativo",

        model_key %in%
          c(
            "exports_classic",
            "exports_expanded"
          ) &
          impulse ==
          "ln_itcrm" ~
          "Positivo",

        TRUE ~
          "No definido"
      ),

    observed_sign_h4 =
      dplyr::case_when(

        is.na(
          response_h4
        ) ~
          "No disponible",

        response_h4 >
          0 ~
          "Positivo",

        response_h4 <
          0 ~
          "Negativo",

        TRUE ~
          "Cero"
      ),

    expected_sign_h4 =
      dplyr::case_when(

        expected_sign ==
          "No definido" ~
          NA,

        observed_sign_h4 ==
          "No disponible" ~
          NA,

        TRUE ~
          expected_sign ==
          observed_sign_h4
      )
  )


print(
  resumen_respuestas,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 26. SENSIBILIDAD AL ORDENAMIENTO
# ------------------------------------------------------------

datos_sensibilidad <- resumen_respuestas |>
  dplyr::filter(
    method_code %in%
      c(
        "chol_primary",
        "chol_alternative"
      )
  )


if (
  all(
    c(
      "chol_primary",
      "chol_alternative"
    ) %in%
    datos_sensibilidad$method_code
  )
) {

  sensibilidad_orden <- datos_sensibilidad |>
    dplyr::select(
      model_id,
      model_key,
      model_short,
      impulse,
      impulse_label,
      response,
      response_label,
      method_code,
      impact_response,
      response_h4,
      response_h8,
      response_h12
    ) |>
    tidyr::pivot_wider(

      names_from =
        method_code,

      values_from = c(
        impact_response,
        response_h4,
        response_h8,
        response_h12
      ),

      names_glue =
        "{.value}_{method_code}"
    ) |>
    dplyr::mutate(

      difference_impact =
        impact_response_chol_alternative -
        impact_response_chol_primary,

      difference_h4 =
        response_h4_chol_alternative -
        response_h4_chol_primary,

      difference_h8 =
        response_h8_chol_alternative -
        response_h8_chol_primary,

      difference_h12 =
        response_h12_chol_alternative -
        response_h12_chol_primary,

      same_sign_h4 =
        sign(
          response_h4_chol_alternative
        ) ==
        sign(
          response_h4_chol_primary
        ),

      same_sign_h12 =
        sign(
          response_h12_chol_alternative
        ) ==
        sign(
          response_h12_chol_primary
        )
    )

} else {

  sensibilidad_orden <- tibble::tibble()
}


# ------------------------------------------------------------
# 27. COMPARACIÓN SVAR VS PROYECCIONES LOCALES
# ------------------------------------------------------------

datos_svar_lp <- resumen_respuestas |>
  dplyr::filter(

    is_primary,

    method_code %in%
      c(
        "chol_primary",
        "local_projection"
      )
  )


if (
  all(
    c(
      "chol_primary",
      "local_projection"
    ) %in%
    datos_svar_lp$method_code
  )
) {

  resumen_svar_lp <- datos_svar_lp |>
    dplyr::select(
      model_id,
      model_key,
      model_short,
      impulse,
      impulse_label,
      method_code,
      response_h4,
      response_h8,
      response_h12,
      expected_sign_h4
    ) |>
    tidyr::pivot_wider(

      names_from =
        method_code,

      values_from = c(
        response_h4,
        response_h8,
        response_h12,
        expected_sign_h4
      ),

      names_glue =
        "{.value}_{method_code}"
    ) |>
    dplyr::mutate(

      same_sign_h4 =
        sign(
          response_h4_chol_primary
        ) ==
        sign(
          response_h4_local_projection
        ),

      same_sign_h8 =
        sign(
          response_h8_chol_primary
        ) ==
        sign(
          response_h8_local_projection
        ),

      same_sign_h12 =
        sign(
          response_h12_chol_primary
        ) ==
        sign(
          response_h12_local_projection
        )
    )

} else {

  resumen_svar_lp <- tibble::tibble()
}


# ------------------------------------------------------------
# 28. FEVD EN HORIZONTES SELECCIONADOS
# ------------------------------------------------------------

fevd_comercio_seleccionada <- fevd_comercio |>
  dplyr::filter(

    method_code ==
      "chol_primary",

    horizon %in%
      c(
        1L,
        4L,
        8L,
        12L
      )
  )


# ------------------------------------------------------------
# 29. DECISIÓN FINAL PARA EL INFORME
# ------------------------------------------------------------

decision_irf_final <- config_modelos_12 |>
  dplyr::mutate(

    primary_identification =
      "SVAR recursivo mediante Cholesky",

    primary_order =
      purrr::map_chr(

        order_primary,

        ~ paste(
          .x,
          collapse = " -> "
        )
      ),

    alternative_order =
      purrr::map_chr(

        order_alternative,

        ~ paste(
          .x,
          collapse = " -> "
        )
      ),

    shock_normalization =
      "Shock positivo de 1% en la variable impulso",

    local_projection_specification =
      paste0(
        "Primeras diferencias, respuesta acumulada, ",
        "errores Newey-West y bandas aproximadas al 90%"
      ),

    report_use =
      dplyr::case_when(

        model_key ==
          "imports" &
          is_primary ~
          paste0(
            "IRF indicativas del VECM con rango r=2. ",
            "Interpretar con cautela por autocorrelación ",
            "y no normalidad."
          ),

        model_key ==
          "exports_classic" &
          is_primary ~
          paste0(
            "IRF principales de exportaciones. El sistema ",
            "no presenta autocorrelación, aunque se rechaza ",
            "normalidad y existen efectos ARCH."
          ),

        model_key ==
          "exports_expanded" &
          is_primary ~
          paste0(
            "Robustez con commodities como control ",
            "exógeno estacionario."
          ),

        model_key ==
          "exports_expanded" &
          is_sensitivity ~
          paste0(
            "Sensibilidad con K=4. No utilizar como ",
            "especificación central."
          ),

        TRUE ~
          "Resultado complementario."
      )
  ) |>
  dplyr::select(
    model_id,
    model_key,
    model_label_run,
    model_short,
    K,
    rank_system_vecm,
    primary_identification,
    primary_order,
    alternative_order,
    shock_normalization,
    local_projection_specification,
    formal_status,
    report_use,
    recommended_model
  )


print(
  decision_irf_final,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 30. GRÁFICO: IRF SVAR PRINCIPAL
# ------------------------------------------------------------

datos_grafico_irf <- irf_comercio |>
  dplyr::filter(

    is_primary,

    method_code ==
      "chol_primary"
  )


if (nrow(datos_grafico_irf) > 0) {

  grafico_irf_principal <- ggplot2::ggplot(

    datos_grafico_irf,

    ggplot2::aes(
      x =
        horizon,
      y =
        response_percent
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
      size = 1.4
    ) +
    ggplot2::facet_grid(
      model_short ~ impulse_label,
      scales = "free_y"
    ) +
    ggplot2::labs(

      title =
        "Funciones impulso-respuesta estructurales",

      subtitle =
        paste0(
          "Identificación Cholesky principal; ",
          "respuesta porcentual ante un shock de 1%"
        ),

      x =
        "Horizonte, trimestres",

      y =
        "Respuesta porcentual"
    ) +
    ggplot2::theme_minimal(
      base_size = 11
    )


  ggplot2::ggsave(

    filename = here::here(
      "outputs",
      "figures",
      "12_irf_svar_principal.png"
    ),

    plot =
      grafico_irf_principal,

    width =
      13,

    height =
      9,

    dpi =
      300
  )
}


# ------------------------------------------------------------
# 31. GRÁFICO: SENSIBILIDAD AL ORDENAMIENTO
# ------------------------------------------------------------

datos_grafico_orden <- irf_comercio |>
  dplyr::filter(

    is_primary,

    method_code %in%
      c(
        "chol_primary",
        "chol_alternative"
      )
  )


if (nrow(datos_grafico_orden) > 0) {

  grafico_orden <- ggplot2::ggplot(

    datos_grafico_orden,

    ggplot2::aes(
      x =
        horizon,
      y =
        response_percent,
      linetype =
        method
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    ggplot2::geom_line(
      linewidth = 0.8
    ) +
    ggplot2::facet_grid(
      model_short ~ impulse_label,
      scales = "free_y"
    ) +
    ggplot2::labs(

      title =
        "Sensibilidad al ordenamiento contemporáneo",

      subtitle =
        "Comparación de los dos ordenamientos Cholesky",

      x =
        "Horizonte, trimestres",

      y =
        "Respuesta porcentual",

      linetype =
        "Ordenamiento"
    ) +
    ggplot2::theme_minimal(
      base_size = 11
    ) +
    ggplot2::theme(
      legend.position = "bottom"
    )


  ggplot2::ggsave(

    filename = here::here(
      "outputs",
      "figures",
      "12_irf_sensibilidad_orden.png"
    ),

    plot =
      grafico_orden,

    width =
      13,

    height =
      9,

    dpi =
      300
  )
}


# ------------------------------------------------------------
# 32. GRÁFICO: FEVD
# ------------------------------------------------------------

datos_grafico_fevd <- fevd_comercio_seleccionada |>
  dplyr::filter(
    is_primary
  )


if (nrow(datos_grafico_fevd) > 0) {

  grafico_fevd <- ggplot2::ggplot(

    datos_grafico_fevd,

    ggplot2::aes(
      x =
        factor(
          horizon
        ),
      y =
        fevd_percent,
      fill =
        impulse_label
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(
      ~ model_short
    ) +
    ggplot2::labs(

      title =
        "Descomposición de la varianza del error de pronóstico",

      subtitle =
        paste0(
          "Participación de cada shock en la varianza ",
          "de la variable comercial"
        ),

      x =
        "Horizonte, trimestres",

      y =
        "Participación porcentual",

      fill =
        "Shock estructural"
    ) +
    ggplot2::theme_minimal(
      base_size = 11
    ) +
    ggplot2::theme(
      legend.position = "bottom"
    )


  ggplot2::ggsave(

    filename = here::here(
      "outputs",
      "figures",
      "12_fevd_variable_comercial.png"
    ),

    plot =
      grafico_fevd,

    width =
      13,

    height =
      7,

    dpi =
      300
  )
}


# ------------------------------------------------------------
# 33. GRÁFICO: PROYECCIONES LOCALES
# ------------------------------------------------------------

if (nrow(lpirfs_comercio) > 0) {

  datos_grafico_lp <- lpirfs_comercio |>
    dplyr::filter(
      is_primary
    )


  grafico_lp <- ggplot2::ggplot(

    datos_grafico_lp,

    ggplot2::aes(
      x =
        horizon,
      y =
        response_percent
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    ggplot2::geom_ribbon(

      ggplot2::aes(
        ymin =
          lower_90,
        ymax =
          upper_90
      ),

      alpha =
        0.20
    ) +
    ggplot2::geom_line(
      linewidth = 0.8
    ) +
    ggplot2::facet_grid(
      model_short ~ impulse_label,
      scales = "free_y"
    ) +
    ggplot2::labs(

      title =
        "Funciones impulso-respuesta mediante proyecciones locales",

      subtitle =
        paste0(
          "Respuestas acumuladas; bandas aproximadas al 90% ",
          "con errores Newey-West"
        ),

      x =
        "Horizonte, trimestres",

      y =
        "Respuesta porcentual"
    ) +
    ggplot2::theme_minimal(
      base_size = 11
    )


  ggplot2::ggsave(

    filename = here::here(
      "outputs",
      "figures",
      "12_lpirfs_variable_comercial.png"
    ),

    plot =
      grafico_lp,

    width =
      13,

    height =
      9,

    dpi =
      300
  )
}


# ------------------------------------------------------------
# 34. GRÁFICO: SVAR VS PROYECCIONES LOCALES
# ------------------------------------------------------------

datos_comparacion_grafico <- irf_comparacion |>
  dplyr::filter(

    is_primary,

    method_code %in%
      c(
        "chol_primary",
        "local_projection"
      )
  )


if (
  nrow(datos_comparacion_grafico) > 0 &&
  "local_projection" %in%
  datos_comparacion_grafico$method_code
) {

  datos_bandas_lp <- datos_comparacion_grafico |>
    dplyr::filter(
      method_code ==
        "local_projection"
    )


  grafico_comparacion <- ggplot2::ggplot(

    datos_comparacion_grafico,

    ggplot2::aes(
      x =
        horizon,
      y =
        response_percent,
      linetype =
        method
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    ggplot2::geom_ribbon(

      data =
        datos_bandas_lp,

      ggplot2::aes(
        x =
          horizon,
        ymin =
          lower_90,
        ymax =
          upper_90
      ),

      inherit.aes =
        FALSE,

      alpha =
        0.15
    ) +
    ggplot2::geom_line(
      linewidth = 0.8
    ) +
    ggplot2::facet_grid(
      model_short ~ impulse_label,
      scales = "free_y"
    ) +
    ggplot2::labs(

      title =
        "Comparación entre SVAR y proyecciones locales",

      subtitle =
        paste0(
          "Respuesta porcentual ante un shock de 1%; ",
          "la banda corresponde a las proyecciones locales"
        ),

      x =
        "Horizonte, trimestres",

      y =
        "Respuesta porcentual",

      linetype =
        "Método"
    ) +
    ggplot2::theme_minimal(
      base_size = 11
    ) +
    ggplot2::theme(
      legend.position = "bottom"
    )


  ggplot2::ggsave(

    filename = here::here(
      "outputs",
      "figures",
      "12_svar_vs_lpirfs.png"
    ),

    plot =
      grafico_comparacion,

    width =
      13,

    height =
      9,

    dpi =
      300
  )
}


# ------------------------------------------------------------
# 35. CONFIGURACIÓN PLANA PARA EXCEL
# ------------------------------------------------------------

config_excel <- config_modelos_12 |>
  dplyr::mutate(

    order_primary =
      purrr::map_chr(
        order_primary,
        ~ paste(
          .x,
          collapse = " -> "
        )
      ),

    order_alternative =
      purrr::map_chr(
        order_alternative,
        ~ paste(
          .x,
          collapse = " -> "
        )
      )
  )


# ------------------------------------------------------------
# 36. GUARDAR RESULTADOS EN EXCEL
# ------------------------------------------------------------

hojas_excel <- list(

  config =
    config_excel,

  id_status =
    estado_identificacion,

  native_status =
    estado_objetos_nativos,

  impact_B =
    matrices_impacto_B,

  irf_svar =
    irf_svar,

  irf_trade =
    irf_comercio,

  fevd_svar =
    fevd_svar,

  fevd_trade =
    fevd_comercio_seleccionada,

  lp_status =
    estado_lpirfs,

  irf_lp =
    lpirfs_tidy,

  lp_trade =
    lpirfs_comercio,

  response_summary =
    resumen_respuestas,

  final_decision =
    decision_irf_final
)


if (ncol(sensibilidad_orden) > 0) {

  hojas_excel$order_sensitivity <-
    sensibilidad_orden
}


if (ncol(resumen_svar_lp) > 0) {

  hojas_excel$svar_lp_summary <-
    resumen_svar_lp
}


writexl::write_xlsx(

  hojas_excel,

  path = here::here(
    "outputs",
    "tables",
    "svar_irf_fevd_lpirfs.xlsx"
  )
)


# ------------------------------------------------------------
# 37. GUARDAR OBJETOS RDS
# ------------------------------------------------------------

saveRDS(
  svar_cholesky_primary,
  here::here(
    "outputs",
    "models",
    "svar_cholesky_primary.rds"
  )
)


saveRDS(
  svar_cholesky_alternative,
  here::here(
    "outputs",
    "models",
    "svar_cholesky_alternative.rds"
  )
)


saveRDS(
  objetos_irf_nativos,
  here::here(
    "outputs",
    "models",
    "objetos_irf_svars.rds"
  )
)


saveRDS(
  objetos_fevd_nativos,
  here::here(
    "outputs",
    "models",
    "objetos_fevd_svars.rds"
  )
)


saveRDS(
  irf_svar,
  here::here(
    "outputs",
    "models",
    "irf_svar_normalizadas.rds"
  )
)


saveRDS(
  fevd_svar,
  here::here(
    "outputs",
    "models",
    "fevd_svar.rds"
  )
)


saveRDS(
  modelos_lpirfs,
  here::here(
    "outputs",
    "models",
    "modelos_lpirfs.rds"
  )
)


saveRDS(
  lpirfs_tidy,
  here::here(
    "outputs",
    "models",
    "irf_lpirfs.rds"
  )
)


saveRDS(
  resumen_respuestas,
  here::here(
    "outputs",
    "models",
    "resumen_respuestas_irf.rds"
  )
)


saveRDS(
  sensibilidad_orden,
  here::here(
    "outputs",
    "models",
    "sensibilidad_orden_irf.rds"
  )
)


saveRDS(
  resumen_svar_lp,
  here::here(
    "outputs",
    "models",
    "comparacion_svar_lpirfs.rds"
  )
)


saveRDS(
  decision_irf_final,
  here::here(
    "outputs",
    "models",
    "decision_irf_final.rds"
  )
)


# ------------------------------------------------------------
# 38. RESUMEN TEXTUAL
# ------------------------------------------------------------

texto_resumen <- c(

  "============================================================",

  "SCRIPT 12: SVAR, IRF, FEVD Y PROYECCIONES LOCALES",

  "============================================================",

  "",

  paste0(
    "Horizonte máximo: ",
    horizonte_irf,
    " trimestres."
  ),

  paste0(
    "Normalización: shock positivo de ",
    100 * shock_log,
    "%."
  ),

  paste0(
    "Bandas de proyecciones locales: aproximadamente 90%, ",
    "con errores Newey-West."
  ),

  "",

  "ESTADO DE LA IDENTIFICACIÓN:",

  capture.output(
    estado_identificacion |>
      print(
        n = Inf,
        width = Inf
      )
  ),

  "",

  "ESTADO DE OBJETOS NATIVOS:",

  capture.output(
    estado_objetos_nativos |>
      print(
        n = Inf,
        width = Inf
      )
  ),

  "",

  "ESTADO DE PROYECCIONES LOCALES:",

  capture.output(
    estado_lpirfs |>
      print(
        n = Inf,
        width = Inf
      )
  ),

  "",

  "RESUMEN DE RESPUESTAS:",

  capture.output(
    resumen_respuestas |>
      dplyr::select(
        model_short,
        method,
        impulse_label,
        impact_response,
        response_h4,
        response_h8,
        response_h12,
        expected_sign,
        expected_sign_h4
      ) |>
      print(
        n = Inf,
        width = Inf
      )
  ),

  "",

  "DECISIÓN PARA EL INFORME:",

  capture.output(
    decision_irf_final |>
      dplyr::select(
        model_short,
        primary_order,
        alternative_order,
        report_use,
        recommended_model
      ) |>
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
    "resumen_script_12.txt"
  )
)


capture.output(

  sessionInfo(),

  file = here::here(
    "outputs",
    "models",
    "session_info_script_12.txt"
  )
)


# ------------------------------------------------------------
# 39. CONTROLES FINALES
# ------------------------------------------------------------

stopifnot(

  nrow(config_modelos_12) ==
    4,

  length(
    svar_cholesky_primary
  ) ==
    4,

  nrow(
    irf_svar
  ) >
    0,

  nrow(
    fevd_svar
  ) >
    0,

  nrow(
    resumen_respuestas
  ) >
    0,

  nrow(
    decision_irf_final
  ) ==
    4
)


# ------------------------------------------------------------
# 40. RESULTADOS PRINCIPALES EN CONSOLA
# ------------------------------------------------------------

cat(
  "\n\n============================================================",
  "\nRESPUESTAS DE LA VARIABLE COMERCIAL",
  "\nCHOLESKY PRINCIPAL",
  "\n============================================================\n"
)


resumen_respuestas |>
  dplyr::filter(

    method_code ==
      "chol_primary",

    is_primary
  ) |>
  dplyr::select(
    model_short,
    impulse_label,
    impact_response,
    response_h1,
    response_h4,
    response_h8,
    response_h12,
    maximum_absolute_response,
    peak_horizon,
    expected_sign,
    expected_sign_h4
  ) |>
  print(
    n = Inf,
    width = Inf
  )


cat(
  "\n\n============================================================",
  "\nDESCOMPOSICIÓN DE VARIANZA A 12 TRIMESTRES",
  "\n============================================================\n"
)


fevd_comercio_seleccionada |>
  dplyr::filter(

    is_primary,

    horizon ==
      12L
  ) |>
  dplyr::select(
    model_short,
    response_label,
    impulse_label,
    fevd_percent
  ) |>
  dplyr::arrange(
    model_short,
    dplyr::desc(
      fevd_percent
    )
  ) |>
  print(
    n = Inf,
    width = Inf
  )


cat(
  "\n\n============================================================",
  "\nCOMPARACIÓN SVAR VS PROYECCIONES LOCALES",
  "\n============================================================\n"
)


if (ncol(resumen_svar_lp) > 0) {

  resumen_svar_lp |>
    print(
      n = Inf,
      width = Inf
    )

} else {

  cat(
    "No se pudo construir la comparación completa.\n",
    "Revisa estado_lpirfs.\n"
  )
}


cat(
  "\n\n============================================================",
  "\nSENSIBILIDAD AL ORDENAMIENTO",
  "\n============================================================\n"
)


if (ncol(sensibilidad_orden) > 0) {

  sensibilidad_orden |>
    dplyr::select(
      model_short,
      impulse_label,
      difference_impact,
      difference_h4,
      difference_h8,
      difference_h12,
      same_sign_h4,
      same_sign_h12
    ) |>
    print(
      n = Inf,
      width = Inf
    )

} else {

  cat(
    "No se pudo construir la sensibilidad al ordenamiento.\n"
  )
}


cat(
  "\n\n============================================================",
  "\nDECISIÓN FINAL",
  "\n============================================================\n"
)


decision_irf_final |>
  dplyr::select(
    model_short,
    K,
    rank_system_vecm,
    primary_order,
    report_use,
    recommended_model
  ) |>
  print(
    n = Inf,
    width = Inf
  )


# ------------------------------------------------------------
# 41. MENSAJE FINAL
# ------------------------------------------------------------

cat(
  "\n============================================================",
  "\nSCRIPT 12 FINALIZADO",
  "\n============================================================",

  "\nModelos analizados:",
  nrow(config_modelos_12),

  "\nIdentificaciones Cholesky principales:",
  length(svar_cholesky_primary),

  "\nIdentificaciones Cholesky alternativas:",
  length(svar_cholesky_alternative),

  "\nModelos lpirfs estimados:",
  length(modelos_lpirfs),

  "\nHorizonte máximo:",
  horizonte_irf,
  "trimestres",

  "\nShock normalizado:",
  "1%",

  "\n\nArchivos generados:",

  "\n- outputs/tables/svar_irf_fevd_lpirfs.xlsx",

  "\n- outputs/models/svar_cholesky_primary.rds",

  "\n- outputs/models/svar_cholesky_alternative.rds",

  "\n- outputs/models/objetos_irf_svars.rds",

  "\n- outputs/models/objetos_fevd_svars.rds",

  "\n- outputs/models/irf_svar_normalizadas.rds",

  "\n- outputs/models/fevd_svar.rds",

  "\n- outputs/models/modelos_lpirfs.rds",

  "\n- outputs/models/irf_lpirfs.rds",

  "\n- outputs/models/resumen_respuestas_irf.rds",

  "\n- outputs/models/sensibilidad_orden_irf.rds",

  "\n- outputs/models/comparacion_svar_lpirfs.rds",

  "\n- outputs/models/decision_irf_final.rds",

  "\n- outputs/models/resumen_script_12.txt",

  "\n- outputs/models/session_info_script_12.txt",

  "\n- figures/12_irf_svar_principal.png",

  "\n- figures/12_irf_sensibilidad_orden.png",

  "\n- figures/12_fevd_variable_comercial.png",

  "\n- figures/12_lpirfs_variable_comercial.png",

  "\n- figures/12_svar_vs_lpirfs.png",

  "\n============================================================",
  "\n"
)
