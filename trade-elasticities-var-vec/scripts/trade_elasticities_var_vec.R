#********************************************************
# UNIVERSIDAD DE BUENOS AIRES (UBA)
# Facultad de Ciencias Economicas
# Maestria en Economia Aplicada
#
# Trabajo Practico N. 3 - Series de Tiempo
# Elasticidades del comercio exterior argentino
# Extension multivariada VAR/VEC
#
# Fase base:
# 1. Cargar panel transformado.
# 2. Definir muestra de trabajo.
# 3. Verificar variables clave.
# 4. Crear sistemas de importaciones y exportaciones.
# 5. Correr ADF en niveles y diferencias.
# 6. Exportar tabla de raices unitarias.
#
# # El script maestro carga scripts auxiliares de clase cuando estan disponibles,
# mantiene esos archivos separados y exporta salidas tabulares reproducibles.
#********************************************************

# se limpia la sesion, se reinician graficos y se evita notacion cientifica.
rm(list = ls())
graphics.off()
options(scipen = 999)

# se identifica la libreria externa del trabajo para cargar paquetes fuera del proyecto.
course_r_lib <- "C:/Users/julla/R/trade-var-vec-library/4.3"

# se identifica la libreria de usuario de R como respaldo.
user_r_lib <- Sys.getenv("R_LIBS_USER")

# se priorizan las librerias externas antes de cargar paquetes. Si el script se
# relanza en una sesion RStudio con paquetes ya cargados, no se reordena la
# busqueda porque R podria intentar cambiar versiones de namespaces activos.
current_lib_paths <- .libPaths()
extra_lib_paths <- c(
  if (dir.exists(course_r_lib)) course_r_lib,
  if (nzchar(user_r_lib) && dir.exists(user_r_lib)) user_r_lib
)
loaded_package_namespaces <- c(
  "zoo", "dynlm", "forecast", "ivreg", "lmtest", "lpirfs",
  "mFilter", "nlme", "readxl", "sandwich", "sarima", "seasonal",
  "strucchange", "strucchangeRcpp", "urca", "vars", "svars"
)

if (any(vapply(loaded_package_namespaces, isNamespaceLoaded, logical(1)))) {
  .libPaths(unique(c(current_lib_paths, extra_lib_paths)))
} else {
  .libPaths(unique(c(extra_lib_paths, current_lib_paths)))
}

#********************************************************
# 1. Directorios
#********************************************************
# se calcula project_dir para usarlo en el paso siguiente.
project_dir <- "C:/Users/julla/GitHub/applied-time-series-econometrics/trade-elasticities-var-vec"

# se construye la ruta scripts_dir sin escribirla manualmente.
scripts_dir <- file.path(project_dir, "scripts")
required_scripts_dir <- file.path(scripts_dir, "required")
processed_data_dir <- file.path(project_dir, "data", "processed")
output_dir <- file.path(project_dir, "outputs")
figures_dir <- file.path(project_dir, "figures")

# se agrupan las salidas CSV por bloque para facilitar la revision manual.
output_subdirs <- c(
  section_00 = "00_setup",
  section_01 = "01_data_descriptives",
  section_02 = "02_unit_roots",
  section_03 = "03_var_lags",
  section_04 = "04_johansen_diagnostics",
  section_05 = "05_engle_granger_ecm",
  section_06 = "06_vecm_decisions",
  section_07 = "07_var_differences",
  section_08 = "08_final_diagnostics",
  section_09 = "09_wickens_breusch",
  section_10 = "10_class_extensions"
)

# se crea la carpeta necesaria si todavia no existe.
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
for (subdir in output_subdirs) {
  dir.create(file.path(output_dir, subdir), recursive = TRUE,
             showWarnings = FALSE)
}

# esta funcion centraliza la ruta de exportacion para cada CSV del trabajo.
output_path <- function(file_name) {
  section_key <- sub("^((section_[0-9]{2}).*)$", "\\2", file_name)
  subdir <- output_subdirs[[section_key]]

  if (is.null(subdir) || is.na(subdir)) {
    return(file.path(output_dir, file_name))
  }

  file.path(output_dir, subdir, file_name)
}

# devuelve la ruta relativa desde outputs para el manifiesto reproducible.
output_relative_path <- function(file_name) {
  section_key <- sub("^((section_[0-9]{2}).*)$", "\\2", file_name)
  subdir <- output_subdirs[[section_key]]

  if (is.null(subdir) || is.na(subdir)) {
    return(file_name)
  }

  file.path(subdir, file_name)
}

#********************************************************
# 1A. Paquetes utilizados durante la cursada
#********************************************************
# se listan los paquetes observados en los scripts teorico-practicos locales y
# el equivalente CRAN para la mencion SVAR de la consigna.
course_packages <- data.frame(
  package = c(
    "dynlm", "forecast", "ivreg", "lmtest", "lpirfs",
    "mFilter", "nlme", "readxl", "sandwich", "sarima",
    "seasonal", "strucchangeRcpp", "urca", "vars", "svars"
  ),
  source = c(
    "clase_01, clase_04, clase_05, clase_06",
    "clase_03, clase_04",
    "clase_01",
    "clase_01, clase_02, clase_03, clase_04",
    "clase_11",
    "clase_09",
    "clase_01",
    "clase_01, clase_04, clase_09",
    "clase_01",
    "clase_04",
    "clase_09",
    "clase_06",
    "clase_03, clase_04, clase_05, clase_06, clase_10, clase_11",
    "clase_07, clase_08, clase_09, clase_10, clase_11",
    "consigna TP3: paquete SVAR disponible en CRAN como svars"
  ),
  role = c(
    "Modelos dinamicos y ADF de clase",
    "ARIMA y pronosticos Box-Jenkins",
    "Variables instrumentales",
    "Contrastes y diagnosticos de regresion",
    "Proyecciones locales e IRF",
    "Filtro Hodrick-Prescott",
    "Modelos GLS y maxima verosimilitud",
    "Lectura de planillas Excel",
    "Errores estandar robustos",
    "Modelos SARIMA",
    "Ajuste estacional",
    "Quiebres estructurales con implementacion Rcpp",
    "Raices unitarias, Engle-Granger y Johansen",
    "VAR, VEC, IRF y SVAR de clase",
    "Modelos SVAR del ecosistema CRAN"
  ),
  required_for_alignment = c(
    rep(TRUE, 14),
    FALSE
  ),
  stringsAsFactors = FALSE
)

package_available <- vapply(
  course_packages$package,
  requireNamespace,
  logical(1),
  quietly = TRUE
)

package_version <- vapply(
  course_packages$package,
  function(pkg) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      as.character(utils::packageVersion(pkg))
    } else {
      NA_character_
    }
  },
  character(1)
)

course_packages_status <- data.frame(
  course_packages,
  installed = unname(package_available),
  version = unname(package_version),
  status = ifelse(package_available, "available", "missing"),
  stringsAsFactors = FALSE
)

missing_course_packages <- course_packages_status$package[
  !course_packages_status$installed &
    course_packages_status$required_for_alignment
]

if (length(missing_course_packages) > 0) {
  stop(
    "Faltan paquetes utilizados durante la cursada: ",
    paste(missing_course_packages, collapse = ", "),
    ". Revisar .libPaths() o instalarlos antes de ejecutar el script maestro."
  )
}

# Como en los scripts de clase, se cargan explicitamente los paquetes usados en
# la cursada. Las llamadas se mantienen separadas para que la consola de RStudio
# muestre con claridad que el entorno reproduce el de trabajo.
suppressPackageStartupMessages(library(dynlm))
suppressPackageStartupMessages(library(forecast))
suppressPackageStartupMessages(library(ivreg))
suppressPackageStartupMessages(library(lmtest))
suppressPackageStartupMessages(library(lpirfs))
suppressPackageStartupMessages(library(mFilter))
suppressPackageStartupMessages(library(nlme))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(sandwich))
suppressPackageStartupMessages(library(sarima))
suppressPackageStartupMessages(library(seasonal))
suppressPackageStartupMessages(library(strucchangeRcpp))
suppressPackageStartupMessages(library(urca))
suppressPackageStartupMessages(library(vars))
if (requireNamespace("svars", quietly = TRUE)) {
  suppressPackageStartupMessages(library(svars))
}

# se documenta la arquitectura docente que organiza el script maestro.
course_workflow_alignment <- data.frame(
  step = seq_len(12),
  class_workflow = c(
    "Cargar paquetes de la cursada con library()",
    "Cargar datos y convertir a objetos ts",
    "Graficar niveles y diferencias",
    "Evaluar raices unitarias con ADF de clase",
    "Estimar Engle-Granger y ECM",
    "Contrastar Wickens-Breusch",
    "Seleccionar rezagos con VARselect y estimar VAR",
    "Aplicar diagnosticos VAR de clase",
    "Estimar Johansen con ca.jo",
    "Recuperar VEC con cajorls",
    "Usar VAR en diferencias si no hay cointegracion",
    "Agregar extensiones IRF, FEVD, SVAR y LPIRF"
  ),
  teacher_reference = c(
    "scripts clase_01 a clase_11",
    "scripts VAR/VEC y ADF",
    "scripts ADF, VAR y VEC",
    "Test.ADF.Ver.3.R",
    "clase_05_cointegracion_ecm",
    "clase_05_cointegracion_ecm",
    "clase_07_var",
    "clase_07_var, clase_10_vec y scripts requeridos",
    "clase_10_vec",
    "clase_10_vec",
    "clase_07_var y consigna TP3",
    "clase_08_var_irf, clase_09_var_svar y clase_11_local_projections"
  ),
  script_block = c(
    "1A. Paquetes utilizados durante la cursada",
    "2. Carga de datos y muestra de trabajo",
    "4. Analisis descriptivo",
    "5B. ADF con script de clase",
    "6. Engle-Granger y ECM",
    "6A. Wickens-Breusch",
    "7. Seleccion de rezagos VAR",
    "8B/8C y Wald de exclusion",
    "8. Cointegracion de Johansen",
    "8D. Decisiones VECM/VAR",
    "9. VAR en diferencias",
    "10. Extensiones de clase"
  ),
  evidence_output = c(
    "00_setup/section_00_course_packages_status.csv",
    "01_data_descriptives/section_01_model_sample.csv",
    "figures/figure_01*_log_levels.png y figures/figure_02*_log_differences.png",
    "02_unit_roots/section_02_course_adf_ver3_results.csv",
    "05_engle_granger_ecm/section_05_*",
    "09_wickens_breusch/section_09_wickens_breusch_long_run_coefficients.csv",
    "03_var_lags/section_03_var_lag_selection.csv",
    "03_var_lags/section_03_var_lag_exclusion_wald.csv y 04_johansen_diagnostics/section_04_course_*",
    "04_johansen_diagnostics/section_04_johansen_*",
    "06_vecm_decisions/section_06_vecm_*",
    "07_var_differences/section_07_var_diff_*",
    "10_class_extensions/section_10_class_*"
  ),
  stringsAsFactors = FALSE
)

#********************************************************
# 1B. Scripts auxiliares de clase
#********************************************************
# se define la funcion auxiliar source_course_script.
source_course_script <- function(file_name, required_packages = character(),
                                 expected_function = NA_character_) {
  script_path <- file.path(required_scripts_dir, file_name)
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  # se evalua una condicion antes de decidir el siguiente paso.
  if (!file.exists(script_path)) {
    return(list(
      loaded = FALSE,
      file = file_name,
      path = script_path,
      expected_function = expected_function,
      status = "missing_file",
      message = paste("No se encontro el script de clase:", script_path)
    ))
  }

  # se evalua una condicion antes de decidir el siguiente paso.
  if (length(missing_packages) > 0) {
    return(list(
      loaded = FALSE,
      file = file_name,
      path = script_path,
      expected_function = expected_function,
      status = "missing_package",
      message = paste("Faltan paquetes requeridos:", paste(missing_packages, collapse = ", "))
    ))
  }

  # se construye source_result con las instrucciones de este minibloque.
  source_result <- tryCatch(
    {
      source(script_path, local = .GlobalEnv, encoding = "UTF-8")
      TRUE
    },
    error = function(e) e
  )

  # se evalua una condicion antes de decidir el siguiente paso.
  if (inherits(source_result, "error")) {
    return(list(
      loaded = FALSE,
      file = file_name,
      path = script_path,
      expected_function = expected_function,
      status = "source_error",
      message = source_result$message
    ))
  }

  expected_function_loaded <- is.na(expected_function) ||
    exists(expected_function, mode = "function")

  # se evalua una condicion antes de decidir el siguiente paso.
  if (!expected_function_loaded) {
    return(list(
      loaded = FALSE,
      file = file_name,
      path = script_path,
      expected_function = expected_function,
      status = "missing_function",
      message = paste("El script se cargo, pero no se encontro la funcion", expected_function)
    ))
  }

  # se ejecuta este bloque amplio del procedimiento.
  list(
    loaded = TRUE,
    file = file_name,
    path = script_path,
    expected_function = expected_function,
    status = "loaded",
    message = "Script de clase cargado correctamente."
  )
}

# se arma la lista course_script_loads para guardar resultados intermedios.
course_script_loads <- list(
  source_course_script(
    "Test.ADF_Ver.3.R",
    required_packages = c("dynlm", "urca"),
    expected_function = "Test.ADF.Ver.3"
  ),
  source_course_script(
    "vcorr_res.R",
    required_packages = c("vars"),
    expected_function = "vcorr_res"
  ),
  source_course_script(
    "VAR_white_no_cross.R",
    expected_function = "VAR_white_no_cross"
  ),
  source_course_script(
    "VAR_lag_exclusion_wald.R",
    expected_function = "VAR_lag_exclusion_wald"
  )
)

# se consolida course_scripts_status a partir de resultados parciales.
course_scripts_status <- do.call(
  rbind,
  lapply(course_script_loads, function(x) {
    data.frame(
      file = x$file,
      path = x$path,
      expected_function = x$expected_function,
      loaded = x$loaded,
      status = x$status,
      message = x$message,
      stringsAsFactors = FALSE
    )
  })
)

# se define la funcion auxiliar course_script_loaded.
course_script_loaded <- function(file_name) {
  isTRUE(course_scripts_status$loaded[course_scripts_status$file == file_name][1])
}

# se construye la ruta input_panel_file sin escribirla manualmente.
input_panel_file <- file.path(
  processed_data_dir,
  "trade_elasticities_var_vec_panel_transformed_2004_2025.csv"
)

# se evalua una condicion antes de decidir el siguiente paso.
if (!file.exists(input_panel_file)) {
  stop("No se encontro la base principal: ", input_panel_file)
}

#********************************************************
# 2. Carga de datos y muestra de trabajo
#********************************************************
# se importa la base que alimenta el objeto panel_data.
panel_data <- read.csv(input_panel_file, stringsAsFactors = FALSE)
panel_data$quarter_date <- as.Date(panel_data$quarter_date)
panel_data$year <- as.integer(panel_data$year)
panel_data$q <- as.integer(panel_data$q)
panel_data <- panel_data[order(panel_data$quarter_date), ]

# La muestra preferida replica el cierre usado en el TP 2. Si se decide ampliar
# el TP 3 a toda la cobertura disponible, cambiar a as.Date("2025-10-01").
# se construye model_sample_start con las instrucciones de este minibloque.
model_sample_start <- as.Date("2004-01-01")
model_sample_end_preferred <- as.Date("2022-10-01")

# se define el vector required_metadata_vars usado en este bloque.
required_metadata_vars <- c("quarter", "year", "q", "quarter_date")

# se define el vector required_level_vars usado en este bloque.
required_level_vars <- c(
  "ln_imports_real",
  "ln_exports_real",
  "ln_gdp_real",
  "ln_itcrm",
  "ln_pib_socios",
  "ln_commodity_price_index"
)

# se define el vector required_diff_vars usado en este bloque.
required_diff_vars <- c(
  "d_ln_imports_real",
  "d_ln_exports_real",
  "d_ln_gdp_real",
  "d_ln_itcrm",
  "d_ln_pib_socios",
  "d_ln_commodity_price_index"
)

# se define el vector required_vars usado en este bloque.
required_vars <- c(required_metadata_vars, required_level_vars, required_diff_vars)
missing_required_vars <- setdiff(required_vars, names(panel_data))

# se evalua una condicion antes de decidir el siguiente paso.
if (length(missing_required_vars) > 0) {
  stop(
    "Faltan variables requeridas en la base: ",
    paste(missing_required_vars, collapse = ", ")
  )
}

# se construye complete_key_rows con las instrucciones de este minibloque.
complete_key_rows <- complete.cases(panel_data[, c(required_level_vars, required_diff_vars)])
available_sample_end <- max(panel_data$quarter_date[complete_key_rows], na.rm = TRUE)
model_sample_end <- min(model_sample_end_preferred, available_sample_end)

# se construye work_data con las instrucciones de este minibloque.
work_data <- panel_data[
  panel_data$quarter_date >= model_sample_start &
    panel_data$quarter_date <= model_sample_end,
]
work_data <- work_data[order(work_data$quarter_date), ]

# se construyen objetos ts como en los scripts de clase para que el recorrido
# datos -> series de tiempo -> graficos -> ADF -> VAR/VEC sea transparente.
work_ts_start <- c(work_data$year[1], work_data$q[1])
ts_levels <- ts(
  work_data[, required_level_vars],
  start = work_ts_start,
  frequency = 4
)
ts_differences <- ts(
  work_data[, required_diff_vars],
  start = work_ts_start,
  frequency = 4
)

# se arma la tabla model_sample_definition con resultados de este paso.
model_sample_definition <- data.frame(
  model_sample_start = min(work_data$quarter),
  model_sample_end = max(work_data$quarter),
  n_quarters = nrow(work_data),
  input_file = input_panel_file,
  note = paste(
    "Muestra base TP3 para ADF y preparacion VAR/VEC;",
    "el cierre preferido replica TP2 y puede ampliarse a 2025Q4 si se decide."
  ),
  stringsAsFactors = FALSE
)

#********************************************************
# 3. Verificacion de variables y sistemas
#********************************************************
# se define el vector imports_system_vars usado en este bloque.
imports_system_vars <- c(
  "ln_imports_real",
  "ln_gdp_real",
  "ln_itcrm"
)

# se define el vector exports_system_vars usado en este bloque.
exports_system_vars <- c(
  "ln_exports_real",
  "ln_pib_socios",
  "ln_itcrm"
)

# se construye imports_system_data con las instrucciones de este minibloque.
imports_system_data <- work_data[
  complete.cases(work_data[, imports_system_vars]),
  c(required_metadata_vars, imports_system_vars)
]

# se construye exports_system_data con las instrucciones de este minibloque.
exports_system_data <- work_data[
  complete.cases(work_data[, exports_system_vars]),
  c(required_metadata_vars, exports_system_vars)
]

# se replican los objetos ts por sistema que el docente usa antes de estimar VAR.
imports_ts_levels <- ts(
  imports_system_data[, imports_system_vars],
  start = c(imports_system_data$year[1], imports_system_data$q[1]),
  frequency = 4
)
exports_ts_levels <- ts(
  exports_system_data[, exports_system_vars],
  start = c(exports_system_data$year[1], exports_system_data$q[1]),
  frequency = 4
)

# se arma la tabla system_sample_summary con resultados de este paso.
system_sample_summary <- data.frame(
  system = c("imports", "exports"),
  variables = c(
    paste(imports_system_vars, collapse = ", "),
    paste(exports_system_vars, collapse = ", ")
  ),
  first_quarter = c(min(imports_system_data$quarter), min(exports_system_data$quarter)),
  last_quarter = c(max(imports_system_data$quarter), max(exports_system_data$quarter)),
  n_quarters = c(nrow(imports_system_data), nrow(exports_system_data)),
  stringsAsFactors = FALSE
)

# se arma la tabla variable_check con resultados de este paso.
variable_check <- data.frame(
  variable = required_vars,
  exists_in_panel = required_vars %in% names(panel_data),
  non_missing_full_panel = vapply(
    required_vars,
    function(v) if (v %in% names(panel_data)) sum(!is.na(panel_data[[v]])) else NA_integer_,
    integer(1)
  ),
  non_missing_work_sample = vapply(
    required_vars,
    function(v) if (v %in% names(work_data)) sum(!is.na(work_data[[v]])) else NA_integer_,
    integer(1)
  ),
  stringsAsFactors = FALSE
)

#********************************************************
# 4. Analisis descriptivo
#********************************************************
# se arma la tabla descriptive_series con resultados de este paso.
descriptive_series <- data.frame(
  series_key = c("imports", "exports", "gdp_arg", "itcrm", "pib_socios", "commodities"),
  series_label = c(
    "Importaciones reales",
    "Exportaciones reales",
    "PIB real Argentina",
    "Tipo de cambio real multilateral",
    "PIB socios comerciales",
    "Indice de precios de commodities"
  ),
  level_var = c(
    "ln_imports_real",
    "ln_exports_real",
    "ln_gdp_real",
    "ln_itcrm",
    "ln_pib_socios",
    "ln_commodity_price_index"
  ),
  diff_var = c(
    "d_ln_imports_real",
    "d_ln_exports_real",
    "d_ln_gdp_real",
    "d_ln_itcrm",
    "d_ln_pib_socios",
    "d_ln_commodity_price_index"
  ),
  stringsAsFactors = FALSE
)

# se define la funcion auxiliar summarize_numeric_series.
summarize_numeric_series <- function(data, variable, series_key, series_label,
                                     transformation) {
  x <- as.numeric(data[[variable]])
  valid <- !is.na(x)
  x_valid <- x[valid]
  q_valid <- data$quarter[valid]

  # se ejecuta este bloque amplio del procedimiento.
  data.frame(
    series_key = series_key,
    series_label = series_label,
    variable = variable,
    transformation = transformation,
    first_quarter = if (length(q_valid) > 0) q_valid[1] else NA_character_,
    last_quarter = if (length(q_valid) > 0) q_valid[length(q_valid)] else NA_character_,
    n = length(x_valid),
    mean = if (length(x_valid) > 0) mean(x_valid) else NA_real_,
    sd = if (length(x_valid) > 1) stats::sd(x_valid) else NA_real_,
    min = if (length(x_valid) > 0) min(x_valid) else NA_real_,
    p25 = if (length(x_valid) > 0) unname(stats::quantile(x_valid, 0.25)) else NA_real_,
    median = if (length(x_valid) > 0) stats::median(x_valid) else NA_real_,
    p75 = if (length(x_valid) > 0) unname(stats::quantile(x_valid, 0.75)) else NA_real_,
    max = if (length(x_valid) > 0) max(x_valid) else NA_real_,
    start_value = if (length(x_valid) > 0) x_valid[1] else NA_real_,
    end_value = if (length(x_valid) > 0) x_valid[length(x_valid)] else NA_real_,
    range = if (length(x_valid) > 0) max(x_valid) - min(x_valid) else NA_real_,
    stringsAsFactors = FALSE
  )
}

# se consolida descriptive_stats a partir de resultados parciales.
descriptive_stats <- do.call(
  rbind,
  lapply(seq_len(nrow(descriptive_series)), function(i) {
    spec <- descriptive_series[i, ]
    rbind(
      summarize_numeric_series(
        work_data,
        spec$level_var,
        spec$series_key,
        spec$series_label,
        "log_level"
      ),
      summarize_numeric_series(
        work_data,
        spec$diff_var,
        spec$series_key,
        spec$series_label,
        "log_difference"
      )
    )
  })
)

# se ejecuta este minibloque del procedimiento.
descriptive_stats[, c(
  "mean", "sd", "min", "p25", "median", "p75", "max",
  "start_value", "end_value", "range"
)] <- round(descriptive_stats[, c(
  "mean", "sd", "min", "p25", "median", "p75", "max",
  "start_value", "end_value", "range"
)], 4)

# se consolida seasonal_diff_summary a partir de resultados parciales.
seasonal_diff_summary <- do.call(
  rbind,
  lapply(seq_len(nrow(descriptive_series)), function(i) {
    spec <- descriptive_series[i, ]
    values <- as.numeric(work_data[[spec$diff_var]])
    split_values <- split(values, work_data$q)

    # se combinan objetos parciales en una unica salida.
    do.call(rbind, lapply(names(split_values), function(q_i) {
      x <- split_values[[q_i]]
      x <- x[!is.na(x)]
      data.frame(
        series_key = spec$series_key,
        series_label = spec$series_label,
        variable = spec$diff_var,
        quarter = paste0("Q", q_i),
        n = length(x),
        mean = if (length(x) > 0) round(mean(x), 4) else NA_real_,
        sd = if (length(x) > 1) round(stats::sd(x), 4) else NA_real_,
        min = if (length(x) > 0) round(min(x), 4) else NA_real_,
        max = if (length(x) > 0) round(max(x), 4) else NA_real_,
        stringsAsFactors = FALSE
      )
    }))
  })
)

# se arma la tabla descriptive_interpretation con resultados de este paso.
descriptive_interpretation <- data.frame(
  topic = c(
    "tendencia",
    "estacionalidad",
    "volatilidad",
    "quiebres"
  ),
  evidence = c(
    paste(
      "Los graficos en niveles logaritmicos permiten observar tendencias",
      "comunes de largo plazo y posibles cambios de pendiente en las series."
    ),
    paste(
      "La tabla por trimestre de las primeras diferencias resume si algun",
      "trimestre muestra medias sistematicamente distintas."
    ),
    paste(
      "La desviacion estandar de las diferencias logaritmicas se usa como",
      "medida simple de volatilidad trimestral."
    ),
    paste(
      "Los maximos, minimos y graficos de diferencias ayudan a ubicar episodios",
      "de saltos asociados a crisis, shocks externos o cambios de regimen."
    )
  ),
  output = c(
    "figure_01_log_levels.png",
    "section_01_descriptive_seasonality_by_quarter.csv",
    "section_01_descriptive_statistics.csv",
    "figure_02_log_differences.png"
  ),
  stringsAsFactors = FALSE
)

# se define la funcion auxiliar plot_series_panel.
plot_series_panel <- function(data, variables, labels, ylab, file_name,
                              width = 1800, height = 1700,
                              event_dates = NULL) {
  output_file <- file.path(figures_dir, file_name)
  grDevices::png(output_file, width = width, height = height, res = 180)
  old_par <- par(no.readonly = TRUE)
  on.exit({
    par(old_par)
    grDevices::dev.off()
  })

  par(
    mfrow = c(length(variables), 1),
    mar = c(3.2, 4.2, 2.0, 1.2),
    oma = c(0.4, 0, 0.2, 0),
    pty = "m"
  )
  for (i in seq_along(variables)) {
    plot(
      data$quarter_date,
      as.numeric(data[[variables[i]]]),
      type = "l",
      col = "#1F4E79",
      lwd = 1.4,
      xlab = "",
      ylab = ylab,
      main = labels[i]
    )
    grid(col = "gray85")
    abline(h = 0, col = "gray60", lty = 3)
    if (!is.null(event_dates)) {
      abline(v = as.Date(event_dates), col = "gray75", lty = 3)
    }
  }
  invisible(output_file)
}

# se define una lista breve de eventos para ubicar quiebres visuales.
descriptive_event_dates <- as.Date(c("2008-10-01", "2020-04-01"))

# se ejecuta este bloque amplio del procedimiento.
plot_series_panel(
  work_data,
  descriptive_series$level_var,
  descriptive_series$series_label,
  "log",
  "figure_01_log_levels.png",
  event_dates = descriptive_event_dates
)

# se ejecuta este bloque amplio del procedimiento.
plot_series_panel(
  work_data,
  descriptive_series$diff_var,
  descriptive_series$series_label,
  "dif. log",
  "figure_02_log_differences.png",
  event_dates = descriptive_event_dates
)

# se separan los paneles por sistema para revisar una version mas legible.
imports_system_keys <- c("imports", "gdp_arg", "itcrm")
exports_system_keys <- c("exports", "pib_socios", "itcrm", "commodities")

select_descriptive_rows <- function(series_keys) {
  descriptive_series[match(series_keys, descriptive_series$series_key), ]
}

imports_descriptive_series <- select_descriptive_rows(imports_system_keys)
exports_descriptive_series <- select_descriptive_rows(exports_system_keys)

plot_series_panel(
  work_data,
  imports_descriptive_series$level_var,
  imports_descriptive_series$series_label,
  "log",
  "figure_01a_imports_log_levels.png",
  height = 1000,
  event_dates = descriptive_event_dates
)

plot_series_panel(
  work_data,
  exports_descriptive_series$level_var,
  exports_descriptive_series$series_label,
  "log",
  "figure_01b_exports_log_levels.png",
  height = 1200,
  event_dates = descriptive_event_dates
)

plot_series_panel(
  work_data,
  imports_descriptive_series$diff_var,
  imports_descriptive_series$series_label,
  "dif. log",
  "figure_02a_imports_log_differences.png",
  height = 1000,
  event_dates = descriptive_event_dates
)

plot_series_panel(
  work_data,
  exports_descriptive_series$diff_var,
  exports_descriptive_series$series_label,
  "dif. log",
  "figure_02b_exports_log_differences.png",
  height = 1200,
  event_dates = descriptive_event_dates
)

#********************************************************
# 5. ADF en niveles y diferencias
#********************************************************
# se calcula adf_specs para usarlo en el paso siguiente.
adf_specs <- descriptive_series

# se define la funcion auxiliar adf_critical_value_5pct.
adf_critical_value_5pct <- function(deterministic) {
  # Valores criticos aproximados para muestras cercanas a 75 observaciones.
  # Se usan solo para la corrida preliminar base; la version formal se contrasta
  # con urca y con el script de clase Test.ADF.Ver.3.R.
  # se evalua una condicion antes de decidir el siguiente paso.
  if (deterministic == "trend") {
    return(-3.45)
  }
  if (deterministic == "drift") {
    return(-2.89)
  }
  -1.95
}

# Run preliminar ADF: replica una prueba ADF simple con R base para tener una
# primera lectura antes de la salida formal con urca y scripts de clase.
# se define la funcion auxiliar run_adf_lm.
run_adf_lm <- function(data, variable, transformation, series_key, series_label) {
  x <- as.numeric(data[[variable]])
  x <- x[!is.na(x)]
  n_obs <- length(x)

  # se construye deterministic con las instrucciones de este minibloque.
  deterministic <- if (transformation == "log_level") "trend" else "drift"
  critical_value_5pct <- adf_critical_value_5pct(deterministic)

  # se evalua una condicion antes de decidir el siguiente paso.
  if (n_obs < 12) {
    return(data.frame(
      series_key = series_key,
      series_label = series_label,
      variable = variable,
      transformation = transformation,
      deterministic = deterministic,
      n_obs = n_obs,
      adf_lag = NA_integer_,
      statistic = NA_real_,
      p_value = NA_real_,
      critical_value_5pct = critical_value_5pct,
      null_hypothesis = "raiz unitaria",
      conclusion_5pct = "muestra insuficiente",
      note = "ADF no ejecutado por cantidad insuficiente de observaciones.",
      stringsAsFactors = FALSE
    ))
  }

  # se construye adf_lag con las instrucciones de este minibloque.
  adf_lag <- trunc((n_obs - 1)^(1 / 3))
  dx <- diff(x)
  n_dx <- length(dx)
  idx <- (adf_lag + 1):n_dx

  # se arma la tabla adf_data con resultados de este paso.
  adf_data <- data.frame(
    dy = dx[idx],
    y_lag = x[idx],
    stringsAsFactors = FALSE
  )

  # se evalua una condicion antes de decidir el siguiente paso.
  if (deterministic %in% c("drift", "trend")) {
    adf_data$const <- 1
  }
  if (deterministic == "trend") {
    adf_data$trend <- idx + 1
  }
  if (adf_lag > 0) {
    for (lag_i in seq_len(adf_lag)) {
      adf_data[[paste0("dylag_", lag_i)]] <- dx[idx - lag_i]
    }
  }

  # se define el vector rhs_vars usado en este bloque.
  rhs_vars <- c("y_lag")
  if (deterministic == "trend") {
    rhs_vars <- c("trend", rhs_vars)
  }
  if (adf_lag > 0) {
    rhs_vars <- c(rhs_vars, paste0("dylag_", seq_len(adf_lag)))
  }

  # se calcula adf_formula para usarlo en el paso siguiente.
  adf_formula <- as.formula(paste("dy ~", paste(rhs_vars, collapse = " + ")))

  # se construye fit con las instrucciones de este minibloque.
  fit <- tryCatch(
    lm(adf_formula, data = adf_data),
    error = function(e) e
  )

  # se evalua una condicion antes de decidir el siguiente paso.
  if (inherits(fit, "error")) {
    return(data.frame(
      series_key = series_key,
      series_label = series_label,
      variable = variable,
      transformation = transformation,
      deterministic = deterministic,
      n_obs = n_obs,
      adf_lag = adf_lag,
      statistic = NA_real_,
      p_value = NA_real_,
      critical_value_5pct = critical_value_5pct,
      null_hypothesis = "raiz unitaria",
      conclusion_5pct = "error en ADF",
      note = fit$message,
      stringsAsFactors = FALSE
    ))
  }

  # se construye coef_table con las instrucciones de este minibloque.
  coef_table <- summary(fit)$coefficients
  statistic <- unname(coef_table["y_lag", "t value"])
  p_value <- unname(coef_table["y_lag", "Pr(>|t|)"])

  # se ejecuta este bloque amplio del procedimiento.
  data.frame(
    series_key = series_key,
    series_label = series_label,
    variable = variable,
    transformation = transformation,
    deterministic = deterministic,
    n_obs = n_obs,
    adf_lag = adf_lag,
    statistic = statistic,
    p_value = p_value,
    critical_value_5pct = critical_value_5pct,
    null_hypothesis = "raiz unitaria",
    conclusion_5pct = ifelse(
      statistic < critical_value_5pct,
      "rechaza raiz unitaria al 5%",
      "no rechaza raiz unitaria al 5%"
    ),
    note = paste(
      "ADF por regresion auxiliar en R base;",
      "p-value OLS reportado solo como referencia no Dickey-Fuller."
    ),
    stringsAsFactors = FALSE
  )
}

# se arma la lista adf_results_list para guardar resultados intermedios.
adf_results_list <- list()

# se recorre cada elemento necesario para completar este paso.
for (i in seq_len(nrow(adf_specs))) {
  spec <- adf_specs[i, ]
  adf_results_list[[length(adf_results_list) + 1]] <- run_adf_lm(
    data = work_data,
    variable = spec$level_var,
    transformation = "log_level",
    series_key = spec$series_key,
    series_label = spec$series_label
  )
  adf_results_list[[length(adf_results_list) + 1]] <- run_adf_lm(
    data = work_data,
    variable = spec$diff_var,
    transformation = "log_difference",
    series_key = spec$series_key,
    series_label = spec$series_label
  )
}

# se consolida adf_results a partir de resultados parciales.
adf_results <- do.call(rbind, adf_results_list)

# se arma la tabla adf_order_summary con resultados de este paso.
adf_order_summary <- data.frame(
  series_key = adf_specs$series_key,
  series_label = adf_specs$series_label,
  level_conclusion_5pct = NA_character_,
  difference_conclusion_5pct = NA_character_,
  preliminary_order = NA_character_,
  interpretation = NA_character_,
  stringsAsFactors = FALSE
)

# se recorre cada elemento necesario para completar este paso.
for (i in seq_len(nrow(adf_order_summary))) {
  key <- adf_order_summary$series_key[i]
  level_conclusion <- adf_results$conclusion_5pct[
    adf_results$series_key == key & adf_results$transformation == "log_level"
  ]
  diff_conclusion <- adf_results$conclusion_5pct[
    adf_results$series_key == key & adf_results$transformation == "log_difference"
  ]

  # se ejecuta este minibloque del procedimiento.
  adf_order_summary$level_conclusion_5pct[i] <- level_conclusion
  adf_order_summary$difference_conclusion_5pct[i] <- diff_conclusion

  # se evalua una condicion antes de decidir el siguiente paso.
  if (level_conclusion == "rechaza raiz unitaria al 5%") {
    adf_order_summary$preliminary_order[i] <- "I(0)"
    adf_order_summary$interpretation[i] <-
      "Serie estacionaria en niveles segun ADF preliminar."
  } else if (
    level_conclusion == "no rechaza raiz unitaria al 5%" &&
      diff_conclusion == "rechaza raiz unitaria al 5%"
  ) {
    adf_order_summary$preliminary_order[i] <- "I(1)"
    adf_order_summary$interpretation[i] <-
      "Serie compatible con uso en pruebas de cointegracion."
  } else {
    adf_order_summary$preliminary_order[i] <- "indeterminado"
    adf_order_summary$interpretation[i] <-
      "Revisar especificacion ADF, rezagos y pruebas complementarias."
  }
}

#********************************************************
# 5. ADF formal con urca::ur.df
#********************************************************
# se evalua una condicion antes de decidir el siguiente paso.
if (!requireNamespace("urca", quietly = TRUE)) {
  stop(
    "Falta el paquete 'urca'. Revisar .libPaths() y la libreria de usuario R_LIBS_USER."
  )
}

# se evalua una condicion antes de decidir el siguiente paso.
if (!requireNamespace("vars", quietly = TRUE)) {
  stop(
    "Falta el paquete 'vars'. Revisar .libPaths() y la libreria de usuario R_LIBS_USER."
  )
}

# se define la funcion auxiliar extract_urdf_result.
extract_urdf_result <- function(urdf_object, deterministic) {
  teststat <- urdf_object@teststat
  cval <- urdf_object@cval

  # se evalua una condicion antes de decidir el siguiente paso.
  if (deterministic == "trend") {
    tau_name <- "tau3"
  } else if (deterministic == "drift") {
    tau_name <- "tau2"
  } else {
    tau_name <- "tau1"
  }

  # se construye statistic con las instrucciones de este minibloque.
  statistic <- as.numeric(teststat[1, tau_name])
  critical_1pct <- as.numeric(cval[tau_name, "1pct"])
  critical_5pct <- as.numeric(cval[tau_name, "5pct"])
  critical_10pct <- as.numeric(cval[tau_name, "10pct"])

  # se ejecuta este minibloque del procedimiento.
  list(
    statistic = statistic,
    critical_1pct = critical_1pct,
    critical_5pct = critical_5pct,
    critical_10pct = critical_10pct
  )
}

# Run formal ADF con urca: calcula la prueba Dickey-Fuller aumentada comparable
# con valores criticos estandar y alimenta las tablas formales del informe.
# se define la funcion auxiliar run_urca_adf.
run_urca_adf <- function(data, variable, transformation, series_key, series_label) {
  x <- as.numeric(data[[variable]])
  x <- x[!is.na(x)]
  n_obs <- length(x)
  deterministic <- if (transformation == "log_level") "trend" else "drift"

  # se evalua una condicion antes de decidir el siguiente paso.
  if (n_obs < 12) {
    return(data.frame(
      series_key = series_key,
      series_label = series_label,
      variable = variable,
      transformation = transformation,
      deterministic = deterministic,
      n_obs = n_obs,
      adf_lag = NA_integer_,
      statistic = NA_real_,
      critical_1pct = NA_real_,
      critical_5pct = NA_real_,
      critical_10pct = NA_real_,
      conclusion_5pct = "muestra insuficiente",
      note = "ADF formal no ejecutado por cantidad insuficiente de observaciones.",
      stringsAsFactors = FALSE
    ))
  }

  # se calcula adf_lag para usarlo en el paso siguiente.
  adf_lag <- trunc((n_obs - 1)^(1 / 3))

  # se construye urdf_fit mediante un bloque de calculo extendido.
  urdf_fit <- tryCatch(
    urca::ur.df(
      y = x,
      type = deterministic,
      lags = adf_lag,
      selectlags = "Fixed"
    ),
    error = function(e) e
  )

  # se evalua una condicion antes de decidir el siguiente paso.
  if (inherits(urdf_fit, "error")) {
    return(data.frame(
      series_key = series_key,
      series_label = series_label,
      variable = variable,
      transformation = transformation,
      deterministic = deterministic,
      n_obs = n_obs,
      adf_lag = adf_lag,
      statistic = NA_real_,
      critical_1pct = NA_real_,
      critical_5pct = NA_real_,
      critical_10pct = NA_real_,
      conclusion_5pct = "error en ADF formal",
      note = urdf_fit$message,
      stringsAsFactors = FALSE
    ))
  }

  # se calcula extracted para usarlo en el paso siguiente.
  extracted <- extract_urdf_result(urdf_fit, deterministic)

  # se ejecuta este bloque amplio del procedimiento.
  data.frame(
    series_key = series_key,
    series_label = series_label,
    variable = variable,
    transformation = transformation,
    deterministic = deterministic,
    n_obs = n_obs,
    adf_lag = adf_lag,
    statistic = extracted$statistic,
    critical_1pct = extracted$critical_1pct,
    critical_5pct = extracted$critical_5pct,
    critical_10pct = extracted$critical_10pct,
    conclusion_5pct = ifelse(
      extracted$statistic < extracted$critical_5pct,
      "rechaza raiz unitaria al 5%",
      "no rechaza raiz unitaria al 5%"
    ),
    note = "ADF formal con urca::ur.df; tendencia en niveles y constante en diferencias.",
    stringsAsFactors = FALSE
  )
}

# se arma la lista adf_urca_results_list para guardar resultados intermedios.
adf_urca_results_list <- list()

# se recorre cada elemento necesario para completar este paso.
for (i in seq_len(nrow(adf_specs))) {
  spec <- adf_specs[i, ]
  adf_urca_results_list[[length(adf_urca_results_list) + 1]] <- run_urca_adf(
    data = work_data,
    variable = spec$level_var,
    transformation = "log_level",
    series_key = spec$series_key,
    series_label = spec$series_label
  )
  adf_urca_results_list[[length(adf_urca_results_list) + 1]] <- run_urca_adf(
    data = work_data,
    variable = spec$diff_var,
    transformation = "log_difference",
    series_key = spec$series_key,
    series_label = spec$series_label
  )
}

# se consolida adf_urca_results a partir de resultados parciales.
adf_urca_results <- do.call(rbind, adf_urca_results_list)

# se arma la tabla adf_urca_order_summary con resultados de este paso.
adf_urca_order_summary <- data.frame(
  series_key = adf_specs$series_key,
  series_label = adf_specs$series_label,
  level_conclusion_5pct = NA_character_,
  difference_conclusion_5pct = NA_character_,
  formal_order = NA_character_,
  interpretation = NA_character_,
  stringsAsFactors = FALSE
)

# se recorre cada elemento necesario para completar este paso.
for (i in seq_len(nrow(adf_urca_order_summary))) {
  key <- adf_urca_order_summary$series_key[i]
  level_conclusion <- adf_urca_results$conclusion_5pct[
    adf_urca_results$series_key == key &
      adf_urca_results$transformation == "log_level"
  ]
  diff_conclusion <- adf_urca_results$conclusion_5pct[
    adf_urca_results$series_key == key &
      adf_urca_results$transformation == "log_difference"
  ]

  # se ejecuta este minibloque del procedimiento.
  adf_urca_order_summary$level_conclusion_5pct[i] <- level_conclusion
  adf_urca_order_summary$difference_conclusion_5pct[i] <- diff_conclusion

  # se evalua una condicion antes de decidir el siguiente paso.
  if (level_conclusion == "rechaza raiz unitaria al 5%") {
    adf_urca_order_summary$formal_order[i] <- "I(0)"
    adf_urca_order_summary$interpretation[i] <-
      "Serie estacionaria en niveles segun ADF formal."
  } else if (
    level_conclusion == "no rechaza raiz unitaria al 5%" &&
      diff_conclusion == "rechaza raiz unitaria al 5%"
  ) {
    adf_urca_order_summary$formal_order[i] <- "I(1)"
    adf_urca_order_summary$interpretation[i] <-
      "Serie compatible con uso en pruebas de cointegracion."
  } else {
    adf_urca_order_summary$formal_order[i] <- "indeterminado"
    adf_urca_order_summary$interpretation[i] <-
      "Revisar rezagos, especificacion deterministica y pruebas complementarias."
  }
}

#********************************************************
# 5B. ADF con script de clase Test.ADF.Ver.3.R
#********************************************************
# se define la funcion auxiliar parse_course_adf_output.
parse_course_adf_output <- function(output_lines, pattern) {
  match_line <- grep(pattern, output_lines, value = TRUE)
  if (length(match_line) == 0) {
    return(NA_character_)
  }
  match_line[1]
}

# se define la funcion auxiliar extract_last_number.
extract_last_number <- function(x) {
  if (is.na(x)) {
    return(NA_real_)
  }
  numeric_matches <- regmatches(
    x,
    gregexpr("-?[0-9]+\\.?[0-9]*", x)
  )[[1]]
  if (length(numeric_matches) == 0) {
    return(NA_real_)
  }
  as.numeric(numeric_matches[length(numeric_matches)])
}

# Run ADF de clase: ejecuta Test.ADF.Ver.3.R sin modificar el script del docente,
# captura su salida impresa y la convierte en una tabla reproducible.
# se define la funcion auxiliar run_course_adf.
run_course_adf <- function(data, variable, transformation, series_key,
                           series_label, mod, sel_lags = "BIC") {
  if (!course_script_loaded("Test.ADF_Ver.3.R") ||
      !exists("Test.ADF.Ver.3", mode = "function")) {
    return(data.frame(
      series_key = series_key,
      series_label = series_label,
      variable = variable,
      transformation = transformation,
      course_script = "Test.ADF_Ver.3.R",
      mod = mod,
      sel_lags = sel_lags,
      selected_lag = NA_integer_,
      max_lag = NA_integer_,
      tau_statistic = NA_real_,
      output_text = NA_character_,
      status = "not_run",
      message = course_scripts_status$message[
        course_scripts_status$file == "Test.ADF_Ver.3.R"
      ][1],
      stringsAsFactors = FALSE
    ))
  }

  # se construye x con las instrucciones de este minibloque.
  x <- as.numeric(data[[variable]])
  x <- x[!is.na(x)]

  # se construye result mediante un bloque de calculo extendido.
  result <- tryCatch(
    capture.output(
      Test.ADF.Ver.3(
        y = x,
        mod = mod,
        sel.lags = sel_lags,
        alfa = 0.10,
        reporte = FALSE
      )
    ),
    error = function(e) e
  )

  # se evalua una condicion antes de decidir el siguiente paso.
  if (inherits(result, "error")) {
    return(data.frame(
      series_key = series_key,
      series_label = series_label,
      variable = variable,
      transformation = transformation,
      course_script = "Test.ADF_Ver.3.R",
      mod = mod,
      sel_lags = sel_lags,
      selected_lag = NA_integer_,
      max_lag = NA_integer_,
      tau_statistic = NA_real_,
      output_text = NA_character_,
      status = "error",
      message = result$message,
      stringsAsFactors = FALSE
    ))
  }

  # se construye lag_line con las instrucciones de este minibloque.
  lag_line <- parse_course_adf_output(result, "lags =")
  max_lag_line <- parse_course_adf_output(result, "Cantidad máxima")
  tau_line <- parse_course_adf_output(result, "Estadístico Tau")

  # se ejecuta este bloque amplio del procedimiento.
  data.frame(
    series_key = series_key,
    series_label = series_label,
    variable = variable,
    transformation = transformation,
    course_script = "Test.ADF_Ver.3.R",
    mod = mod,
    sel_lags = sel_lags,
    selected_lag = as.integer(extract_last_number(lag_line)),
    max_lag = as.integer(extract_last_number(max_lag_line)),
    tau_statistic = extract_last_number(tau_line),
    output_text = paste(result, collapse = " | "),
    status = "ok",
    message = "ADF ejecutado con script de clase.",
    stringsAsFactors = FALSE
  )
}

# se consolida course_adf_results a partir de resultados parciales.
course_adf_results <- do.call(
  rbind,
  lapply(seq_len(nrow(adf_specs)), function(i) {
    spec <- adf_specs[i, ]
    rbind(
      run_course_adf(
        data = work_data,
        variable = spec$level_var,
        transformation = "log_level",
        series_key = spec$series_key,
        series_label = spec$series_label,
        mod = 3
      ),
      run_course_adf(
        data = work_data,
        variable = spec$diff_var,
        transformation = "log_difference",
        series_key = spec$series_key,
        series_label = spec$series_label,
        mod = 2
      )
    )
  })
)

#********************************************************
# 6. Engle-Granger y ECM
#********************************************************
# se define la funcion auxiliar format_model_formula.
format_model_formula <- function(dependent_var, regressors) {
  paste(dependent_var, "~", paste(regressors, collapse = " + "))
}

# se define la funcion auxiliar tidy_lm_coefficients_base.
tidy_lm_coefficients_base <- function(fit, model_key, model_label, horizon) {
  coef_table <- as.data.frame(summary(fit)$coefficients)
  coef_table$term <- rownames(coef_table)
  rownames(coef_table) <- NULL

  # se ejecuta este bloque amplio del procedimiento.
  data.frame(
    model_key = model_key,
    model_label = model_label,
    horizon = horizon,
    term = coef_table$term,
    estimate = coef_table$Estimate,
    std_error = coef_table$`Std. Error`,
    t_statistic = coef_table$`t value`,
    p_value = coef_table$`Pr(>|t|)`,
    stringsAsFactors = FALSE
  )
}

# se define la funcion auxiliar residual_adf_t_stat.
residual_adf_t_stat <- function(residuals, lag_order) {
  residuals <- as.numeric(residuals)
  dy <- diff(residuals)
  y_lag <- residuals[-length(residuals)]

  # se arma la tabla adf_data con resultados de este paso.
  adf_data <- data.frame(dy = dy, y_lag = y_lag)

  # se evalua una condicion antes de decidir el siguiente paso.
  if (lag_order > 0) {
    for (lag_i in seq_len(lag_order)) {
      adf_data[[paste0("dy_lag", lag_i)]] <- c(
        rep(NA_real_, lag_i),
        dy[seq_len(length(dy) - lag_i)]
      )
    }
  }

  # se calcula adf_data para usarlo en el paso siguiente.
  adf_data <- adf_data[complete.cases(adf_data), ]

  # se construye rhs con las instrucciones de este minibloque.
  rhs <- if (lag_order > 0) {
    paste(c("y_lag", paste0("dy_lag", seq_len(lag_order))), collapse = " + ")
  } else {
    "y_lag"
  }

  # se construye fit con las instrucciones de este minibloque.
  fit <- lm(as.formula(paste0("dy ~ ", rhs, " - 1")), data = adf_data)
  unname(summary(fit)$coefficients["y_lag", "t value"])
}

# se arma la tabla engle_granger_critical_values con resultados de este paso.
engle_granger_critical_values <- data.frame(
  q_series = c(2L, 2L, 2L, 3L, 3L, 3L),
  reference_n_obs = c(50L, 100L, 200L, 50L, 100L, 200L),
  critical_value_1pct = c(-4.32, -4.07, -4.00, -4.84, -4.45, -4.35),
  critical_value_5pct = c(-3.67, -3.37, -3.37, -4.11, -3.93, -3.78),
  critical_value_10pct = c(-3.28, -3.03, -3.02, -3.73, -3.59, -3.47),
  stringsAsFactors = FALSE
)

# Run Engle-Granger: estima la relacion de largo plazo, guarda residuos y aplica
# el ADF residual que define si corresponde continuar con un ECM.
# se define la funcion auxiliar run_engle_granger.
run_engle_granger <- function(data, model_key, model_label, dependent_var,
                              regressors) {
  model_vars <- c(dependent_var, regressors)
  model_data <- data[complete.cases(data[, model_vars]), c(required_metadata_vars, model_vars)]
  model_data <- model_data[order(model_data$quarter_date), ]

  # se construye fit mediante un bloque de calculo extendido.
  fit <- lm(reformulate(regressors, response = dependent_var), data = model_data)
  residuals <- resid(fit)
  n_obs <- length(residuals)
  adf_lag <- trunc((n_obs - 1)^(1 / 3))
  statistic <- residual_adf_t_stat(residuals, adf_lag)
  q_series <- length(regressors) + 1L
  reference_n_obs <- engle_granger_critical_values$reference_n_obs[
    which.min(abs(engle_granger_critical_values$reference_n_obs - n_obs))
  ]
  critical_row <- engle_granger_critical_values[
    engle_granger_critical_values$q_series == q_series &
      engle_granger_critical_values$reference_n_obs == reference_n_obs,
  ]

  # se ejecuta este bloque amplio del procedimiento.
  list(
    fit = fit,
    residuals = data.frame(
      quarter = model_data$quarter,
      quarter_date = model_data$quarter_date,
      model_key = model_key,
      residual = residuals,
      stringsAsFactors = FALSE
    ),
    equation = data.frame(
      model_key = model_key,
      model_label = model_label,
      method = "Engle-Granger first-stage OLS",
      formula = format_model_formula(dependent_var, regressors),
      n_obs = n_obs,
      first_quarter = min(model_data$quarter),
      last_quarter = max(model_data$quarter),
      r_squared = summary(fit)$r.squared,
      adj_r_squared = summary(fit)$adj.r.squared,
      stringsAsFactors = FALSE
    ),
    coefficients = tidy_lm_coefficients_base(
      fit = fit,
      model_key = model_key,
      model_label = model_label,
      horizon = "long_run"
    ),
    residual_test = data.frame(
      model_key = model_key,
      model_label = model_label,
      method = "Engle-Granger residual ADF without constant",
      n_obs = n_obs,
      adf_lag = adf_lag,
      q_series = q_series,
      reference_n_obs = reference_n_obs,
      statistic = statistic,
      critical_value_1pct = critical_row$critical_value_1pct,
      critical_value_5pct = critical_row$critical_value_5pct,
      critical_value_10pct = critical_row$critical_value_10pct,
      rejects_unit_root_5pct = statistic < critical_row$critical_value_5pct,
      conclusion_5pct = ifelse(
        statistic < critical_row$critical_value_5pct,
        "residuos estacionarios; evidencia de cointegracion",
        "no rechaza raiz unitaria en residuos"
      ),
      note = "Comparacion contra valores criticos Engle-Granger usados en clase.",
      stringsAsFactors = FALSE
    )
  )
}

# se construye imports_engle_granger con las instrucciones de este minibloque.
imports_engle_granger <- run_engle_granger(
  data = work_data,
  model_key = "imports",
  model_label = "Importaciones: comercio, PIB argentino e ITCRM",
  dependent_var = "ln_imports_real",
  regressors = c("ln_gdp_real", "ln_itcrm")
)

# se construye exports_engle_granger con las instrucciones de este minibloque.
exports_engle_granger <- run_engle_granger(
  data = work_data,
  model_key = "exports",
  model_label = "Exportaciones: comercio, PIB socios e ITCRM",
  dependent_var = "ln_exports_real",
  regressors = c("ln_pib_socios", "ln_itcrm")
)

# se unen filas para formar engle_granger_equations.
engle_granger_equations <- rbind(
  imports_engle_granger$equation,
  exports_engle_granger$equation
)

# se unen filas para formar engle_granger_long_run_coefficients.
engle_granger_long_run_coefficients <- rbind(
  imports_engle_granger$coefficients,
  exports_engle_granger$coefficients
)

# se unen filas para formar engle_granger_residual_tests.
engle_granger_residual_tests <- rbind(
  imports_engle_granger$residual_test,
  exports_engle_granger$residual_test
)

# se unen filas para formar engle_granger_residuals.
engle_granger_residuals <- rbind(
  imports_engle_granger$residuals,
  exports_engle_granger$residuals
)

# se reorganiza la estructura de datos en residuals_wide.
residuals_wide <- reshape(
  engle_granger_residuals,
  idvar = c("quarter", "quarter_date"),
  timevar = "model_key",
  direction = "wide"
)
names(residuals_wide) <- gsub("residual\\.", "resid_", names(residuals_wide))

# se cruza informacion para construir modeling_data.
modeling_data <- merge(
  work_data,
  residuals_wide[, c("quarter_date", "resid_imports", "resid_exports")],
  by = "quarter_date",
  all.x = TRUE
)
modeling_data <- modeling_data[order(modeling_data$quarter_date), ]
modeling_data$l1_resid_imports <- c(NA_real_, modeling_data$resid_imports[-nrow(modeling_data)])
modeling_data$l1_resid_exports <- c(NA_real_, modeling_data$resid_exports[-nrow(modeling_data)])

# se arma la tabla engle_granger_decision con resultados de este paso.
engle_granger_decision <- data.frame(
  model_key = engle_granger_residual_tests$model_key,
  model_label = engle_granger_residual_tests$model_label,
  use_ecm = engle_granger_residual_tests$rejects_unit_root_5pct,
  cointegration_evidence = engle_granger_residual_tests$conclusion_5pct,
  next_step = ifelse(
    engle_granger_residual_tests$rejects_unit_root_5pct,
    "estimar ECM con residuo Engle-Granger rezagado",
    "estimar modelo en primeras diferencias sin ECM"
  ),
  stringsAsFactors = FALSE
)

# Run de corto plazo: estima el modelo en diferencias y, cuando corresponde,
# incorpora el termino de correccion de error rezagado.
# se define la funcion auxiliar run_short_run_model.
run_short_run_model <- function(data, model_key, model_label, dependent_var,
                                regressors, model_type) {
  model_vars <- c(dependent_var, regressors)
  model_data <- data[complete.cases(data[, model_vars]), c(required_metadata_vars, model_vars)]
  model_data <- model_data[order(model_data$quarter_date), ]
  fit <- lm(reformulate(regressors, response = dependent_var), data = model_data)

  # se ejecuta este bloque amplio del procedimiento.
  list(
    fit = fit,
    summary = data.frame(
      model_key = model_key,
      model_label = model_label,
      model_type = model_type,
      formula = format_model_formula(dependent_var, regressors),
      n_obs = nobs(fit),
      first_quarter = min(model_data$quarter),
      last_quarter = max(model_data$quarter),
      r_squared = summary(fit)$r.squared,
      adj_r_squared = summary(fit)$adj.r.squared,
      aic = AIC(fit),
      bic = BIC(fit),
      stringsAsFactors = FALSE
    ),
    coefficients = tidy_lm_coefficients_base(
      fit = fit,
      model_key = model_key,
      model_label = model_label,
      horizon = model_type
    )
  )
}

# se construye imports_short_run mediante un bloque de calculo extendido.
imports_short_run <- if (
  engle_granger_decision$use_ecm[engle_granger_decision$model_key == "imports"]
) {
  run_short_run_model(
    data = modeling_data,
    model_key = "imports_ecm",
    model_label = "Importaciones - ECM",
    dependent_var = "d_ln_imports_real",
    regressors = c("d_ln_gdp_real", "d_ln_itcrm", "l1_resid_imports"),
    model_type = "ecm"
  )
} else {
  run_short_run_model(
    data = modeling_data,
    model_key = "imports_diff",
    model_label = "Importaciones - primeras diferencias",
    dependent_var = "d_ln_imports_real",
    regressors = c("d_ln_gdp_real", "d_ln_itcrm"),
    model_type = "short_run_diff"
  )
}

# se construye exports_short_run mediante un bloque de calculo extendido.
exports_short_run <- if (
  engle_granger_decision$use_ecm[engle_granger_decision$model_key == "exports"]
) {
  run_short_run_model(
    data = modeling_data,
    model_key = "exports_ecm",
    model_label = "Exportaciones - ECM",
    dependent_var = "d_ln_exports_real",
    regressors = c("d_ln_pib_socios", "d_ln_itcrm", "l1_resid_exports"),
    model_type = "ecm"
  )
} else {
  run_short_run_model(
    data = modeling_data,
    model_key = "exports_diff",
    model_label = "Exportaciones - primeras diferencias",
    dependent_var = "d_ln_exports_real",
    regressors = c("d_ln_pib_socios", "d_ln_itcrm", "d_ln_commodity_price_index"),
    model_type = "short_run_diff"
  )
}

# se unen filas para formar short_run_model_summary.
short_run_model_summary <- rbind(
  imports_short_run$summary,
  exports_short_run$summary
)

# se unen filas para formar short_run_coefficients.
short_run_coefficients <- rbind(
  imports_short_run$coefficients,
  exports_short_run$coefficients
)

# se construye ecm_adjustment_terms con las instrucciones de este minibloque.
ecm_adjustment_terms <- short_run_coefficients[
  short_run_coefficients$term %in% c("l1_resid_imports", "l1_resid_exports"),
]

# se arma la tabla ecm_adjustment_summary con resultados de este paso.
ecm_adjustment_summary <- data.frame(
  model_key = ecm_adjustment_terms$model_key,
  model_label = ecm_adjustment_terms$model_label,
  adjustment_term = ecm_adjustment_terms$term,
  adjustment_coefficient = ecm_adjustment_terms$estimate,
  p_value = ecm_adjustment_terms$p_value,
  adjustment_percent_per_quarter = ifelse(
    ecm_adjustment_terms$estimate < 0,
    abs(ecm_adjustment_terms$estimate) * 100,
    NA_real_
  ),
  interpretation = ifelse(
    ecm_adjustment_terms$estimate < 0,
    "corrige una fraccion del desequilibrio por trimestre",
    "no presenta signo de correccion hacia equilibrio"
  ),
  stringsAsFactors = FALSE
)

# se arma la tabla eg_ecm_elasticity_summary con resultados de este paso.
eg_ecm_elasticity_summary <- data.frame(
  flow = c("Importaciones", "Importaciones", "Exportaciones", "Exportaciones"),
  variable = c("PIB", "TCR", "PIB socios", "TCR"),
  long_run_term = c("ln_gdp_real", "ln_itcrm", "ln_pib_socios", "ln_itcrm"),
  short_run_term = c("d_ln_gdp_real", "d_ln_itcrm", "d_ln_pib_socios", "d_ln_itcrm"),
  stringsAsFactors = FALSE
)

# se agrega o actualiza la columna long_run_estimate en eg_ecm_elasticity_summary.
eg_ecm_elasticity_summary$long_run_estimate <- NA_real_
eg_ecm_elasticity_summary$short_run_estimate <- NA_real_
eg_ecm_elasticity_summary$interpretation <- NA_character_

# se recorre cada elemento necesario para completar este paso.
for (i in seq_len(nrow(eg_ecm_elasticity_summary))) {
  flow_key <- ifelse(eg_ecm_elasticity_summary$flow[i] == "Importaciones", "imports", "exports")
  long_run_model_key <- flow_key
  short_run_model_prefix <- ifelse(flow_key == "imports", "imports_", "exports_")

  # se ejecuta este minibloque del procedimiento.
  eg_ecm_elasticity_summary$long_run_estimate[i] <-
    engle_granger_long_run_coefficients$estimate[
      engle_granger_long_run_coefficients$model_key == long_run_model_key &
        engle_granger_long_run_coefficients$term == eg_ecm_elasticity_summary$long_run_term[i]
    ][1]

  # se ejecuta este minibloque del procedimiento.
  eg_ecm_elasticity_summary$short_run_estimate[i] <-
    short_run_coefficients$estimate[
      grepl(short_run_model_prefix, short_run_coefficients$model_key) &
        short_run_coefficients$term == eg_ecm_elasticity_summary$short_run_term[i]
    ][1]
}

# se agrega o actualiza la columna interpretation en eg_ecm_elasticity_summary.
eg_ecm_elasticity_summary$interpretation <- c(
  "Elasticidad ingreso positiva; ECM estimado por evidencia de cointegracion.",
  "Elasticidad cambiaria negativa; ECM estimado por evidencia de cointegracion.",
  "Elasticidad de corto plazo en diferencias; largo plazo solo indicativo.",
  "Elasticidad de corto plazo en diferencias; largo plazo solo indicativo."
)

#********************************************************
# 6A. Contraste Wickens-Breusch para Engle-Granger
#********************************************************
# Wickens-Breusch se implementa como ECM no restringido para recuperar
# elasticidades de largo plazo por cociente entre niveles rezagados.
wb_modeling_data <- modeling_data
wb_modeling_data$l1_ln_imports_real <- c(
  NA_real_,
  wb_modeling_data$ln_imports_real[-nrow(wb_modeling_data)]
)
wb_modeling_data$l1_ln_gdp_real <- c(
  NA_real_,
  wb_modeling_data$ln_gdp_real[-nrow(wb_modeling_data)]
)
wb_modeling_data$l1_ln_itcrm <- c(
  NA_real_,
  wb_modeling_data$ln_itcrm[-nrow(wb_modeling_data)]
)

# se define la funcion auxiliar get_eg_long_run_estimate.
get_eg_long_run_estimate <- function(model_key, term) {
  value <- engle_granger_long_run_coefficients$estimate[
    engle_granger_long_run_coefficients$model_key == model_key &
      engle_granger_long_run_coefficients$term == term
  ]
  if (length(value) == 0) {
    return(NA_real_)
  }
  as.numeric(value[1])
}

# se define la funcion auxiliar run_wickens_breusch_imports.
run_wickens_breusch_imports <- function(data) {
  model_vars <- c(
    "d_ln_imports_real",
    "d_ln_gdp_real",
    "d_ln_itcrm",
    "l1_ln_imports_real",
    "l1_ln_gdp_real",
    "l1_ln_itcrm"
  )
  model_data <- data[complete.cases(data[, model_vars]), c(required_metadata_vars, model_vars)]
  model_data <- model_data[order(model_data$quarter_date), ]

  fit <- tryCatch(
    lm(
      d_ln_imports_real ~ d_ln_gdp_real + d_ln_itcrm +
        l1_ln_imports_real + l1_ln_gdp_real + l1_ln_itcrm,
      data = model_data
    ),
    error = function(e) e
  )

  if (inherits(fit, "error")) {
    return(data.frame(
      system = "imports",
      model_key = "imports_wickens_breusch",
      variable = c("PIB", "TCR"),
      engle_granger_term = c("ln_gdp_real", "ln_itcrm"),
      wickens_breusch_level_term = c("l1_ln_gdp_real", "l1_ln_itcrm"),
      engle_granger_estimate = c(
        get_eg_long_run_estimate("imports", "ln_gdp_real"),
        get_eg_long_run_estimate("imports", "ln_itcrm")
      ),
      wickens_breusch_estimate = NA_real_,
      adjustment_coefficient = NA_real_,
      sign_consistency = NA_character_,
      relative_difference_percent = NA_real_,
      n_obs = nrow(model_data),
      first_quarter = ifelse(nrow(model_data) > 0, min(model_data$quarter), NA_character_),
      last_quarter = ifelse(nrow(model_data) > 0, max(model_data$quarter), NA_character_),
      status = "model_error",
      message = fit$message,
      stringsAsFactors = FALSE
    ))
  }

  coefficient_table <- coef(fit)
  adjustment <- as.numeric(coefficient_table["l1_ln_imports_real"])
  wb_gdp <- -as.numeric(coefficient_table["l1_ln_gdp_real"]) / adjustment
  wb_itcrm <- -as.numeric(coefficient_table["l1_ln_itcrm"]) / adjustment
  eg_values <- c(
    get_eg_long_run_estimate("imports", "ln_gdp_real"),
    get_eg_long_run_estimate("imports", "ln_itcrm")
  )
  wb_values <- c(wb_gdp, wb_itcrm)

  data.frame(
    system = "imports",
    model_key = "imports_wickens_breusch",
    variable = c("PIB", "TCR"),
    engle_granger_term = c("ln_gdp_real", "ln_itcrm"),
    wickens_breusch_level_term = c("l1_ln_gdp_real", "l1_ln_itcrm"),
    engle_granger_estimate = eg_values,
    wickens_breusch_estimate = wb_values,
    adjustment_coefficient = adjustment,
    sign_consistency = ifelse(sign(eg_values) == sign(wb_values), "mismo signo", "cambia signo"),
    relative_difference_percent = ifelse(
      is.finite(eg_values) & eg_values != 0,
      100 * (wb_values - eg_values) / abs(eg_values),
      NA_real_
    ),
    n_obs = nobs(fit),
    first_quarter = min(model_data$quarter),
    last_quarter = max(model_data$quarter),
    status = "ok",
    message = paste(
      "Elasticidades Wickens-Breusch calculadas desde ECM no restringido:",
      "-coeficiente de nivel explicativo rezagado dividido por coeficiente",
      "de variable dependiente rezagada."
    ),
    stringsAsFactors = FALSE
  )
}

wickens_breusch_imports <- run_wickens_breusch_imports(wb_modeling_data)

wickens_breusch_exports_note <- data.frame(
  system = "exports",
  model_key = "exports_wickens_breusch",
  variable = c("PIB socios", "TCR"),
  engle_granger_term = c("ln_pib_socios", "ln_itcrm"),
  wickens_breusch_level_term = NA_character_,
  engle_granger_estimate = c(
    get_eg_long_run_estimate("exports", "ln_pib_socios"),
    get_eg_long_run_estimate("exports", "ln_itcrm")
  ),
  wickens_breusch_estimate = NA_real_,
  adjustment_coefficient = NA_real_,
  sign_consistency = NA_character_,
  relative_difference_percent = NA_real_,
  n_obs = NA_integer_,
  first_quarter = NA_character_,
  last_quarter = NA_character_,
  status = "not_applicable",
  message = paste(
    "No se calcula como especificacion principal porque Engle-Granger no",
    "respalda cointegracion para exportaciones; el resultado de largo plazo",
    "se conserva solo como indicativo."
  ),
  stringsAsFactors = FALSE
)

wickens_breusch_long_run_coefficients <- rbind(
  wickens_breusch_imports,
  wickens_breusch_exports_note
)

#********************************************************
# 7. Seleccion de rezagos VAR
#********************************************************
# se construye var_lag_max con las instrucciones de este minibloque.
var_lag_max <- 8
var_type <- "const"
preferred_lag_criterion <- "SC"

# se define la funcion auxiliar prepare_var_matrix.
prepare_var_matrix <- function(data, variables) {
  matrix_data <- data[, variables]
  matrix_data <- matrix_data[complete.cases(matrix_data), ]
  as.matrix(matrix_data)
}

# se construye imports_var_matrix con las instrucciones de este minibloque.
imports_var_matrix <- prepare_var_matrix(work_data, imports_system_vars)
exports_var_matrix <- prepare_var_matrix(work_data, exports_system_vars)

# Run seleccion de rezagos VAR: aplica VARselect para cada sistema y deja tanto
# la decision por criterio como la grilla completa de valores.
# se define la funcion auxiliar run_var_lag_selection.
run_var_lag_selection <- function(system_name, matrix_data, lag_max, type) {
  selection_result <- tryCatch(
    vars::VARselect(matrix_data, lag.max = lag_max, type = type),
    error = function(e) e
  )

  # se evalua una condicion antes de decidir el siguiente paso.
  if (inherits(selection_result, "error")) {
    return(list(
      selection = data.frame(
        system = system_name,
        criterion = c("AIC", "HQ", "SC", "FPE"),
        selected_lag = NA_integer_,
        error = selection_result$message,
        stringsAsFactors = FALSE
      ),
      criteria = data.frame(
        system = system_name,
        criterion = NA_character_,
        lag = NA_integer_,
        value = NA_real_,
        error = selection_result$message,
        stringsAsFactors = FALSE
      )
    ))
  }

  # se construye selected mediante un bloque de calculo extendido.
  selected <- selection_result$selection
  selected_clean <- data.frame(
    system = system_name,
    criterion = gsub("\\(n\\)", "", names(selected)),
    selected_lag = as.integer(selected),
    error = NA_character_,
    stringsAsFactors = FALSE
  )

  # se construye criteria_matrix con las instrucciones de este minibloque.
  criteria_matrix <- selection_result$criteria
  criteria_long <- data.frame()

  # se recorre cada elemento necesario para completar este paso.
  for (criterion_name in rownames(criteria_matrix)) {
    for (lag_name in colnames(criteria_matrix)) {
      criteria_long <- rbind(
        criteria_long,
        data.frame(
          system = system_name,
          criterion = gsub("\\(n\\)", "", criterion_name),
          lag = as.integer(gsub("[^0-9]", "", lag_name)),
          value = as.numeric(criteria_matrix[criterion_name, lag_name]),
          error = NA_character_,
          stringsAsFactors = FALSE
        )
      )
    }
  }

  # se ejecuta este minibloque del procedimiento.
  list(selection = selected_clean, criteria = criteria_long)
}

# se construye imports_lag_selection con las instrucciones de este minibloque.
imports_lag_selection <- run_var_lag_selection(
  system_name = "imports",
  matrix_data = imports_var_matrix,
  lag_max = var_lag_max,
  type = var_type
)

# se construye exports_lag_selection con las instrucciones de este minibloque.
exports_lag_selection <- run_var_lag_selection(
  system_name = "exports",
  matrix_data = exports_var_matrix,
  lag_max = var_lag_max,
  type = var_type
)

# se unen filas para formar var_lag_selection.
var_lag_selection <- rbind(
  imports_lag_selection$selection,
  exports_lag_selection$selection
)

# se unen filas para formar var_lag_criteria.
var_lag_criteria <- rbind(
  imports_lag_selection$criteria,
  exports_lag_selection$criteria
)

# se construye var_lag_decision con las instrucciones de este minibloque.
var_lag_decision <- var_lag_selection[
  var_lag_selection$criterion == preferred_lag_criterion,
  c("system", "selected_lag")
]

# se ajustan nombres para que la salida sea legible.
names(var_lag_decision)[names(var_lag_decision) == "selected_lag"] <-
  "preferred_selected_lag"

# se agrega o actualiza la columna criteria_used en var_lag_decision.
var_lag_decision$criteria_used <- preferred_lag_criterion
var_lag_decision$lag_max <- var_lag_max
var_lag_decision$var_type <- var_type
var_lag_decision$note <- paste(
  "Criterio principal SC/BIC por parsimonia en muestra trimestral;",
  "AIC, HQ y FPE se reportan como contraste."
)

# esta funcion recupera el rezago base elegido por SC/BIC para cada sistema.
get_base_var_lag <- function(system_name) {
  lag_value <- var_lag_decision$preferred_selected_lag[
    var_lag_decision$system == system_name
  ]

  if (length(lag_value) != 1 || is.na(lag_value)) {
    stop("No se encontro rezago base para el sistema: ", system_name)
  }

  as.integer(lag_value)
}

# Run diagnostico de rezagos: evalua autocorrelacion residual para candidatos.
run_var_lag_diagnostic_grid <- function(system_name, matrix_data, base_lag,
                                        lag_max, type, diagnostic_lag = 12) {
  candidate_lags <- seq.int(base_lag, lag_max)

  do.call(
    rbind,
    lapply(candidate_lags, function(candidate_lag) {
      fit <- tryCatch(
        vars::VAR(matrix_data, p = candidate_lag, type = type),
        error = function(e) e
      )

      if (inherits(fit, "error")) {
        return(data.frame(
          system = system_name,
          base_selected_lag = base_lag,
          candidate_lag = candidate_lag,
          diagnostic_lag = NA_integer_,
          pt_adjusted_p_value = NA_real_,
          bg_p_value = NA_real_,
          passes_serial_diagnostic_5pct = FALSE,
          status = "var_error",
          note = fit$message,
          stringsAsFactors = FALSE
        ))
      }

      lags_to_use <- min(diagnostic_lag, floor(fit$obs / 3))
      pt_result <- tryCatch(
        vars::serial.test(fit, lags.pt = lags_to_use, type = "PT.adjusted"),
        error = function(e) e
      )
      bg_result <- tryCatch(
        vars::serial.test(fit, lags.bg = lags_to_use, type = "BG"),
        error = function(e) e
      )

      pt_p_value <- if (inherits(pt_result, "error")) {
        NA_real_
      } else {
        as.numeric(pt_result$serial$p.value)
      }
      bg_p_value <- if (inherits(bg_result, "error")) {
        NA_real_
      } else {
        as.numeric(bg_result$serial$p.value)
      }
      pt_p_value <- if (length(pt_p_value) == 1) pt_p_value else NA_real_
      bg_p_value <- if (length(bg_p_value) == 1) bg_p_value else NA_real_
      passes <- is.finite(pt_p_value) && is.finite(bg_p_value) &&
        pt_p_value >= 0.05 && bg_p_value >= 0.05

      data.frame(
        system = system_name,
        base_selected_lag = base_lag,
        candidate_lag = candidate_lag,
        diagnostic_lag = lags_to_use,
        pt_adjusted_p_value = pt_p_value,
        bg_p_value = bg_p_value,
        passes_serial_diagnostic_5pct = passes,
        status = "ok",
        note = ifelse(
          passes,
          "Candidato sin autocorrelacion residual al 5%.",
          "Candidato mantiene evidencia de autocorrelacion residual."
        ),
        stringsAsFactors = FALSE
      )
    })
  )
}

var_lag_diagnostic_grid <- rbind(
  run_var_lag_diagnostic_grid(
    system_name = "imports",
    matrix_data = imports_var_matrix,
    base_lag = get_base_var_lag("imports"),
    lag_max = var_lag_max,
    type = var_type
  ),
  run_var_lag_diagnostic_grid(
    system_name = "exports",
    matrix_data = exports_var_matrix,
    base_lag = get_base_var_lag("exports"),
    lag_max = var_lag_max,
    type = var_type
  )
)

# Se elige el menor rezago >= SC/BIC que pase autocorrelacion residual.
var_lag_diagnostic_decision <- do.call(
  rbind,
  lapply(unique(var_lag_diagnostic_grid$system), function(system_name) {
    system_grid <- var_lag_diagnostic_grid[
      var_lag_diagnostic_grid$system == system_name,
    ]
    base_lag <- unique(system_grid$base_selected_lag)[1]
    passing_lags <- system_grid$candidate_lag[
      system_grid$passes_serial_diagnostic_5pct
    ]
    final_lag <- if (length(passing_lags) > 0) {
      min(passing_lags)
    } else {
      base_lag
    }
    final_row <- system_grid[system_grid$candidate_lag == final_lag, ][1, ]
    diagnostics_pass <- isTRUE(final_row$passes_serial_diagnostic_5pct)

    data.frame(
      system = system_name,
      base_selected_lag = base_lag,
      final_selected_lag = final_lag,
      lag_max = var_lag_max,
      var_type = var_type,
      selection_rule = ifelse(
        diagnostics_pass,
        "menor rezago que pasa autocorrelacion residual",
        "SC/BIC conservado por falta de candidato sin autocorrelacion"
      ),
      pt_adjusted_p_value = final_row$pt_adjusted_p_value,
      bg_p_value = final_row$bg_p_value,
      diagnostics_pass_5pct = diagnostics_pass,
      diagnostic_status = ifelse(
        diagnostics_pass,
        "serial_diagnostic_pass",
        "residual_autocorrelation_warning"
      ),
      note = ifelse(
        diagnostics_pass,
        "El rezago final retroalimenta los diagnosticos de autocorrelacion.",
        "Ningun rezago candidato elimina autocorrelacion; resultados indicativos."
      ),
      stringsAsFactors = FALSE
    )
  })
)

# se define la funcion auxiliar plot_var_lag_serial_diagnostics.
plot_var_lag_serial_diagnostics <- function(diagnostic_grid, diagnostic_decision,
                                            file_name) {
  output_file <- file.path(figures_dir, file_name)
  system_names <- unique(diagnostic_grid$system)
  system_labels <- c(
    imports = "Importaciones",
    exports = "Exportaciones"
  )

  grDevices::png(output_file, width = 1800, height = 1050, res = 180)
  old_par <- par(no.readonly = TRUE)
  on.exit({
    par(old_par)
    grDevices::dev.off()
  })

  par(
    mfrow = c(length(system_names), 1),
    mar = c(4.2, 4.4, 2.2, 1.2),
    oma = c(0.2, 0, 0.2, 0),
    pty = "m"
  )

  for (system_name in system_names) {
    system_grid <- diagnostic_grid[diagnostic_grid$system == system_name, ]
    system_decision <- diagnostic_decision[
      diagnostic_decision$system == system_name,
    ][1, ]
    p_values <- c(system_grid$pt_adjusted_p_value, system_grid$bg_p_value)
    finite_p_values <- p_values[is.finite(p_values)]
    y_max <- if (length(finite_p_values) == 0) {
      0.10
    } else {
      min(1, max(0.10, max(finite_p_values, na.rm = TRUE) * 1.15))
    }
    label <- ifelse(
      system_name %in% names(system_labels),
      system_labels[[system_name]],
      system_name
    )

    plot(
      system_grid$candidate_lag,
      system_grid$pt_adjusted_p_value,
      type = "b",
      pch = 16,
      lwd = 1.6,
      col = "#1F4E79",
      ylim = c(0, y_max),
      xaxt = "n",
      xlab = "Rezago VAR candidato",
      ylab = "p-valor",
      main = label
    )
    lines(
      system_grid$candidate_lag,
      system_grid$bg_p_value,
      type = "b",
      pch = 17,
      lwd = 1.6,
      col = "#B45F06"
    )
    axis(1, at = system_grid$candidate_lag)
    grid(col = "gray85")
    abline(h = 0.05, col = "gray45", lty = 2)
    abline(v = system_decision$base_selected_lag, col = "gray45", lty = 3)
    abline(v = system_decision$final_selected_lag, col = "#38761D", lty = 4)

    legend(
      "topright",
      legend = c("PT ajustado", "BG", "Umbral 5%", "SC/BIC", "Rezago final"),
      col = c("#1F4E79", "#B45F06", "gray45", "gray45", "#38761D"),
      lty = c(1, 1, 2, 3, 4),
      pch = c(16, 17, NA, NA, NA),
      bty = "n",
      cex = 0.82
    )
  }

  invisible(output_file)
}

# se exporta una figura diagnostica para justificar la decision final de rezagos.
plot_var_lag_serial_diagnostics(
  var_lag_diagnostic_grid,
  var_lag_diagnostic_decision,
  "figure_03_var_lag_serial_diagnostics.png"
)

# esta funcion recupera el rezago VAR elegido para cada sistema.
get_preferred_var_lag <- function(system_name) {
  # se usa la decision final ajustada por diagnosticos residuales.
  lag_value <- var_lag_diagnostic_decision$final_selected_lag[
    var_lag_diagnostic_decision$system == system_name
  ]

  # se detiene la corrida si no existe una decision unica y valida.
  if (length(lag_value) != 1 || is.na(lag_value)) {
    stop("No se encontro rezago final para el sistema: ", system_name)
  }

  # se devuelve el rezago como entero para pasarlo a las rutinas VAR/VEC.
  as.integer(lag_value)
}

# Run Wald de exclusion: evalua si un rezago completo aporta al sistema VAR.
run_var_lag_exclusion_wald <- function(system_name, matrix_data, p, type) {
  if (!course_script_loaded("VAR_lag_exclusion_wald.R") ||
      !exists("VAR_lag_exclusion_wald", mode = "function")) {
    return(data.frame(
      system = system_name,
      var_lag = p,
      tested_lag = NA_integer_,
      statistic = NA_real_,
      df = NA_integer_,
      p_value = NA_real_,
      n_restrictions = NA_integer_,
      conclusion_5pct = NA_character_,
      note = "No se pudo interpretar porque el script requerido no cargo.",
      status = "script_not_loaded",
      message = course_scripts_status$message[
        course_scripts_status$file == "VAR_lag_exclusion_wald.R"
      ][1],
      stringsAsFactors = FALSE
    ))
  }

  var_fit <- tryCatch(
    vars::VAR(matrix_data, p = p, type = type),
    error = function(e) e
  )

  # Si falla la estimacion del VAR, se registra el error sin cortar la corrida.
  if (inherits(var_fit, "error")) {
    return(data.frame(
      system = system_name,
      var_lag = p,
      tested_lag = NA_integer_,
      statistic = NA_real_,
      df = NA_integer_,
      p_value = NA_real_,
      n_restrictions = NA_integer_,
      conclusion_5pct = NA_character_,
      note = "No se pudo interpretar porque el VAR no pudo estimarse.",
      status = "var_error",
      message = var_fit$message,
      stringsAsFactors = FALSE
    ))
  }

  wald_result <- tryCatch(
    VAR_lag_exclusion_wald(var_fit, lags = seq_len(p)),
    error = function(e) e
  )

  # Si falla el Wald, se conserva el mensaje para auditar el script requerido.
  if (inherits(wald_result, "error")) {
    return(data.frame(
      system = system_name,
      var_lag = p,
      tested_lag = NA_integer_,
      statistic = NA_real_,
      df = NA_integer_,
      p_value = NA_real_,
      n_restrictions = NA_integer_,
      conclusion_5pct = NA_character_,
      note = "No se pudo interpretar porque fallo el test Wald.",
      status = "wald_error",
      message = wald_result$message,
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    system = system_name,
    var_lag = p,
    tested_lag = wald_result$lag,
    statistic = wald_result$statistic,
    df = wald_result$df,
    p_value = wald_result$p_value,
    n_restrictions = wald_result$n_restrictions,
    conclusion_5pct = wald_result$conclusion_5pct,
    note = wald_result$note,
    status = "ok",
    message = "Lag exclusion Wald ejecutado con VAR_lag_exclusion_wald.R.",
    stringsAsFactors = FALSE
  )
}

# Se documenta si cada bloque de rezagos elegido aporta informacion conjunta.
var_lag_exclusion_wald_results <- rbind(
  run_var_lag_exclusion_wald(
    system_name = "imports",
    matrix_data = imports_var_matrix,
    p = get_preferred_var_lag("imports"),
    type = var_type
  ),
  run_var_lag_exclusion_wald(
    system_name = "exports",
    matrix_data = exports_var_matrix,
    p = get_preferred_var_lag("exports"),
    type = var_type
  )
)

#********************************************************
# 8. Cointegracion de Johansen
#********************************************************
# se fija la constante dentro de la relacion de cointegracion.
johansen_ecdet <- "const"

# se evaluan dos especificaciones deterministicas para robustez.
johansen_ecdet_specs <- c("const", "none")

# se usa la especificacion transitoria de ca.jo, como en aplicaciones VAR/VEC.
johansen_spec <- "transitory"

# se agregan dummies estacionales trimestrales.
johansen_season <- 4

# esta funcion adapta el rezago VAR al minimo requerido por Johansen.
get_johansen_lag <- function(system_name) {
  # ca.jo requiere K >= 2, por eso se impone ese piso.
  max(get_preferred_var_lag(system_name), 2L)
}

# esta funcion advierte si alguna variable no luce I(1) segun el ADF formal.
integration_warning_for_system <- function(system_name, system_vars) {
  # se recupera el orden de integracion formal de las variables del sistema.
  orders <- adf_urca_order_summary$formal_order[
    adf_urca_order_summary$series_key %in% names(system_vars)
  ]

  # se renombran los ordenes para que la advertencia sea legible.
  names(orders) <- names(system_vars)[
    names(system_vars) %in% adf_urca_order_summary$series_key
  ]

  # se identifican variables que no fueron clasificadas como I(1).
  non_i1 <- orders[orders != "I(1)"]

  # si todas son I(1), se devuelve una lectura favorable.
  if (length(non_i1) == 0) {
    return("Todas las variables del sistema son compatibles con I(1) segun ADF formal.")
  }

  # si hay excepciones, se arma una advertencia para el informe.
  paste(
    "Lectura cautelosa:",
    paste(names(non_i1), non_i1, sep = "=", collapse = ", "),
    "segun ADF formal."
  )
}

# esta funcion convierte el objeto ca.jo en una tabla plana exportable.
extract_johansen_results <- function(johansen_object, system_name, test_type) {
  # se extraen los estadisticos de prueba del objeto de urca.
  test_stats <- as.numeric(johansen_object@teststat)

  # se guardan los nombres de las hipotesis nulas de rango.
  test_names <- names(johansen_object@teststat)

  # se extraen los valores criticos provistos por ca.jo.
  critical_values <- johansen_object@cval

  # si el objeto no trae nombres, se usan las filas de valores criticos.
  if (is.null(test_names)) {
    test_names <- rownames(critical_values)
  }

  # se transforma cada hipotesis en el rango nulo correspondiente.
  null_rank <- as.integer(gsub(".*?(\\d+).*", "\\1", test_names))

  # se arma la tabla final de resultados Johansen para exportar a CSV.
  data.frame(
    system = system_name,
    test_type = test_type,
    null_hypothesis = test_names,
    null_rank = null_rank,
    statistic = test_stats,
    critical_10pct = as.numeric(critical_values[, "10pct"]),
    critical_5pct = as.numeric(critical_values[, "5pct"]),
    critical_1pct = as.numeric(critical_values[, "1pct"]),
    reject_5pct = test_stats > as.numeric(critical_values[, "5pct"]),
    ecdet = johansen_object@ecdet,
    spec = johansen_object@spec,
    lag_k = johansen_object@lag,
    season = johansen_season,
    stringsAsFactors = FALSE
  )
}

# esta funcion selecciona el rango segun el primer rango no rechazado.
select_johansen_rank <- function(johansen_results) {
  # se ordenan las hipotesis por rango para leer la secuencia de pruebas.
  ordered_results <- johansen_results[order(johansen_results$null_rank), ]

  # se guarda la secuencia de rechazos al 5%.
  rejected <- ordered_results$reject_5pct

  # si se rechazan todas las hipotesis, se toma el rango maximo disponible.
  if (all(rejected)) {
    return(nrow(ordered_results))
  }

  # el primer no rechazo determina el rango seleccionado.
  first_not_rejected <- which(!rejected)[1]

  # se devuelve el rango elegido para la decision del sistema.
  ordered_results$null_rank[first_not_rejected]
}

# Run Johansen: estima pruebas de traza y maximo autovalor para un sistema,
# una especificacion deterministica y el rezago K elegido.
# se define la funcion auxiliar run_johansen_system.
run_johansen_system <- function(system_name, matrix_data, lag_k, system_vars,
                                ecdet) {
  # se recuperan rezagos base y final para documentar la retroalimentacion.
  selected_var_lag <- get_preferred_var_lag(system_name)
  base_var_lag <- get_base_var_lag(system_name)

  # se documenta si el rezago de Johansen difiere por la restriccion K >= 2.
  lag_note <- if (lag_k != selected_var_lag) {
    paste(
      "El rezago final ajustado fue", selected_var_lag,
      "rezago VAR; ca.jo exige K >= 2, por lo que Johansen se estima con K =",
      lag_k,
      paste0("(base SC/BIC = ", base_var_lag, ").")
    )
  } else {
    paste(
      "Johansen se estima con el rezago VAR final ajustado: K =",
      lag_k,
      paste0("(base SC/BIC = ", base_var_lag, ").")
    )
  }

  # se estima la prueba de traza dentro de un tryCatch para capturar errores.
  trace_fit <- tryCatch(
    urca::ca.jo(
      x = matrix_data,
      type = "trace",
      ecdet = ecdet,
      K = lag_k,
      spec = johansen_spec,
      season = johansen_season
    ),
    error = function(e) e
  )

  # se estima la prueba de maximo autovalor con la misma especificacion.
  eigen_fit <- tryCatch(
    urca::ca.jo(
      x = matrix_data,
      type = "eigen",
      ecdet = ecdet,
      K = lag_k,
      spec = johansen_spec,
      season = johansen_season
    ),
    error = function(e) e
  )

  # si alguna prueba falla, se devuelve una salida vacia pero trazable.
  if (inherits(trace_fit, "error") || inherits(eigen_fit, "error")) {
    # se consolida el mensaje de error para dejarlo en el CSV.
    error_message <- paste(
      c(
        if (inherits(trace_fit, "error")) paste("trace:", trace_fit$message),
        if (inherits(eigen_fit, "error")) paste("eigen:", eigen_fit$message)
      ),
      collapse = " | "
    )

    # se crea una tabla vacia con la misma estructura que una corrida exitosa.
    empty_results <- data.frame(
      system = system_name,
      test_type = c("trace", "eigen"),
      null_hypothesis = NA_character_,
      null_rank = NA_integer_,
      statistic = NA_real_,
      critical_10pct = NA_real_,
      critical_5pct = NA_real_,
      critical_1pct = NA_real_,
      reject_5pct = NA,
      ecdet = ecdet,
      spec = johansen_spec,
      lag_k = lag_k,
      season = johansen_season,
      stringsAsFactors = FALSE
    )

    # se retorna la tabla vacia y una decision con la advertencia correspondiente.
    return(list(
      results = empty_results,
      decision = data.frame(
        system = system_name,
        selected_var_lag = selected_var_lag,
        lag_k = lag_k,
        selected_rank_trace_5pct = NA_integer_,
        selected_rank_eigen_5pct = NA_integer_,
        ecdet = ecdet,
        spec = johansen_spec,
        season = johansen_season,
        note = paste(lag_note, error_message, sep = " | "),
        integration_warning = integration_warning_for_system(system_name, system_vars),
        stringsAsFactors = FALSE
      ),
      trace_fit = NULL,
      eigen_fit = NULL
    ))
  }

  # se tabulan los resultados de la prueba de traza.
  trace_results <- extract_johansen_results(trace_fit, system_name, "trace")

  # se tabulan los resultados de la prueba de maximo autovalor.
  eigen_results <- extract_johansen_results(eigen_fit, system_name, "eigen")

  # se unen ambas pruebas para exportarlas en un unico archivo.
  combined_results <- rbind(trace_results, eigen_results)

  # se construye la decision de rango para el sistema y especificacion.
  decision <- data.frame(
    system = system_name,
    selected_var_lag = selected_var_lag,
    lag_k = lag_k,
    selected_rank_trace_5pct = select_johansen_rank(trace_results),
    selected_rank_eigen_5pct = select_johansen_rank(eigen_results),
    ecdet = ecdet,
    spec = johansen_spec,
    season = johansen_season,
    note = paste(
      lag_note,
      paste0("Johansen con ecdet = ", ecdet, ","),
      "especificacion transitory y dummies estacionales trimestrales."
    ),
    integration_warning = integration_warning_for_system(system_name, system_vars),
    stringsAsFactors = FALSE
  )

  # se devuelve tanto la tabla de pruebas como la decision resumida.
  list(
    results = combined_results,
    decision = decision,
    trace_fit = trace_fit,
    eigen_fit = eigen_fit
  )
}

# se vinculan las variables del sistema de importaciones con sus claves ADF.
imports_series_key_map <- c(
  imports = "ln_imports_real",
  gdp_arg = "ln_gdp_real",
  itcrm = "ln_itcrm"
)

# se vinculan las variables del sistema de exportaciones con sus claves ADF.
exports_series_key_map <- c(
  exports = "ln_exports_real",
  pib_socios = "ln_pib_socios",
  itcrm = "ln_itcrm"
)

# se inicializa la lista donde se guardan todas las corridas Johansen.
johansen_system_results <- list()

# se corre Johansen para cada especificacion deterministica.
for (ecdet_i in johansen_ecdet_specs) {
  # se agrega la corrida Johansen del sistema de importaciones.
  johansen_system_results[[length(johansen_system_results) + 1]] <-
    run_johansen_system(
      system_name = "imports",
      matrix_data = imports_var_matrix,
      lag_k = get_johansen_lag("imports"),
      system_vars = imports_series_key_map,
      ecdet = ecdet_i
    )

  # se agrega la corrida Johansen del sistema de exportaciones.
  johansen_system_results[[length(johansen_system_results) + 1]] <-
    run_johansen_system(
      system_name = "exports",
      matrix_data = exports_var_matrix,
      lag_k = get_johansen_lag("exports"),
      system_vars = exports_series_key_map,
      ecdet = ecdet_i
    )
}

# se combinan los resultados detallados de todas las corridas Johansen.
johansen_results <- do.call(
  rbind,
  lapply(johansen_system_results, function(result) result$results)
)

# se separan las pruebas de traza para una salida especifica.
johansen_trace_results <- johansen_results[johansen_results$test_type == "trace", ]

# se separan las pruebas de maximo autovalor para una salida especifica.
johansen_eigen_results <- johansen_results[johansen_results$test_type == "eigen", ]

# se combinan las decisiones de rango de todos los sistemas.
johansen_rank_decision <- do.call(
  rbind,
  lapply(johansen_system_results, function(result) result$decision)
)

# esta funcion recupera la corrida Johansen de traza para un sistema/especificacion.
get_johansen_trace_fit <- function(system_name, ecdet) {
  matches <- lapply(johansen_system_results, function(result) {
    decision <- result$decision
    if (nrow(decision) == 1 &&
        identical(decision$system[1], system_name) &&
        identical(decision$ecdet[1], ecdet)) {
      return(result$trace_fit)
    }
    NULL
  })

  matches <- matches[!vapply(matches, is.null, logical(1))]
  if (length(matches) == 0) {
    return(NULL)
  }

  matches[[1]]
}

# se transforma una matriz con nombres de filas/columnas en tabla larga.
matrix_to_long <- function(matrix_x, value_name) {
  if (is.null(matrix_x) || length(matrix_x) == 0) {
    return(data.frame(
      relation = NA_character_,
      term = NA_character_,
      value = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  matrix_x <- as.matrix(matrix_x)
  relation_names <- colnames(matrix_x)
  if (is.null(relation_names)) {
    relation_names <- paste0("relation_", seq_len(ncol(matrix_x)))
  }
  term_names <- rownames(matrix_x)
  if (is.null(term_names)) {
    term_names <- paste0("term_", seq_len(nrow(matrix_x)))
  }

  rows <- do.call(
    rbind,
    lapply(seq_len(ncol(matrix_x)), function(col_i) {
      data.frame(
        relation = relation_names[col_i],
        term = term_names,
        value = as.numeric(matrix_x[, col_i]),
        stringsAsFactors = FALSE
      )
    })
  )
  names(rows)[names(rows) == "value"] <- value_name
  rows
}

# se extraen errores estandar, estadisticos t y p-valores de un modelo mlm/lm.
extract_coefficient_statistics <- function(model_object) {
  summary_object <- tryCatch(
    summary(model_object),
    error = function(e) e
  )

  if (inherits(summary_object, "error") || !is.list(summary_object)) {
    return(data.frame(
      equation = character(),
      term = character(),
      std_error = numeric(),
      statistic = numeric(),
      p_value = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  equation_names <- names(summary_object)
  if (is.null(equation_names) || any(equation_names == "")) {
    equation_names <- colnames(stats::coef(model_object))
  }
  if (is.null(equation_names)) {
    equation_names <- paste0("equation_", seq_along(summary_object))
  }

  stats_rows <- do.call(
    rbind,
    lapply(seq_along(summary_object), function(eq_i) {
      coefficients <- summary_object[[eq_i]]$coefficients
      if (is.null(coefficients) || length(coefficients) == 0) {
        return(NULL)
      }

      coefficients <- as.matrix(coefficients)
      std_col <- grep("Std\\. Error|Std. Error", colnames(coefficients))[1]
      stat_col <- grep("t value|z value|statistic", colnames(coefficients),
                       ignore.case = TRUE)[1]
      p_col <- grep("Pr\\(|p.value|p_value", colnames(coefficients),
                    ignore.case = TRUE)[1]

      data.frame(
        equation = equation_names[eq_i],
        term = rownames(coefficients),
        std_error = if (!is.na(std_col)) {
          as.numeric(coefficients[, std_col])
        } else {
          NA_real_
        },
        statistic = if (!is.na(stat_col)) {
          as.numeric(coefficients[, stat_col])
        } else {
          NA_real_
        },
        p_value = if (!is.na(p_col)) {
          as.numeric(coefficients[, p_col])
        } else {
          NA_real_
        },
        stringsAsFactors = FALSE
      )
    })
  )

  if (is.null(stats_rows)) {
    return(data.frame(
      equation = character(),
      term = character(),
      std_error = numeric(),
      statistic = numeric(),
      p_value = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  stats_rows
}

# se quitan columnas inferenciales cuando cajorls no entrega informacion usable.
clean_empty_inference_columns <- function(data_frame) {
  inference_columns <- c("std_error", "statistic", "p_value")
  present_columns <- intersect(inference_columns, names(data_frame))

  if (length(present_columns) == 0) {
    data_frame$inference_available <- FALSE
    data_frame$inference_note <- "Salida sin columnas inferenciales."
    return(data_frame)
  }

  has_inference <- vapply(
    present_columns,
    function(column_name) {
      values <- data_frame[[column_name]]
      if (is.numeric(values) || is.integer(values)) {
        return(any(is.finite(values)))
      }
      any(!is.na(values) & nzchar(trimws(as.character(values))))
    },
    logical(1)
  )

  if (!any(has_inference)) {
    data_frame <- data_frame[, setdiff(names(data_frame), present_columns),
                             drop = FALSE]
    data_frame$inference_available <- FALSE
    data_frame$inference_note <- paste(
      "cajorls no entrego errores estandar, estadisticos ni p-valores",
      "en una estructura exportable; se reportan coeficientes estimados."
    )
  } else {
    data_frame$inference_available <- TRUE
    data_frame$inference_note <- "Inferencia disponible en columnas especificas."
  }

  data_frame
}

# se arma la salida VECM para el sistema de importaciones cuando hay rango positivo.
extract_vecm_outputs <- function(system_name, ecdet, rank_used, status_note) {
  trace_fit <- get_johansen_trace_fit(system_name, ecdet)
  selected_var_lag <- get_preferred_var_lag(system_name)
  lag_k <- get_johansen_lag(system_name)

  empty_vector <- data.frame(
    system = system_name,
    ecdet = ecdet,
    spec = johansen_spec,
    selected_var_lag = selected_var_lag,
    johansen_k = lag_k,
    rank_used = rank_used,
    relation = NA_character_,
    term = NA_character_,
    beta_coefficient = NA_real_,
    normalized_to = NA_character_,
    long_run_elasticity = NA_real_,
    status = "skipped",
    note = status_note,
    stringsAsFactors = FALSE
  )
  empty_adjustment <- data.frame(
    system = system_name,
    ecdet = ecdet,
    spec = johansen_spec,
    selected_var_lag = selected_var_lag,
    johansen_k = lag_k,
    rank_used = rank_used,
    equation = NA_character_,
    relation = NA_character_,
    adjustment_coefficient = NA_real_,
    std_error = NA_real_,
    statistic = NA_real_,
    p_value = NA_real_,
    status = "skipped",
    note = status_note,
    stringsAsFactors = FALSE
  )
  empty_short_run <- data.frame(
    system = system_name,
    ecdet = ecdet,
    spec = johansen_spec,
    selected_var_lag = selected_var_lag,
    johansen_k = lag_k,
    rank_used = rank_used,
    equation = NA_character_,
    term = NA_character_,
    coefficient = NA_real_,
    std_error = NA_real_,
    statistic = NA_real_,
    p_value = NA_real_,
    coefficient_block = NA_character_,
    status = "skipped",
    note = status_note,
    stringsAsFactors = FALSE
  )

  if (is.null(trace_fit) || is.na(rank_used) || rank_used < 1) {
    return(list(
      vectors = empty_vector,
      adjustment = empty_adjustment,
      short_run = empty_short_run
    ))
  }

  vecm_fit <- tryCatch(
    urca::cajorls(trace_fit, r = rank_used),
    error = function(e) e
  )

  if (inherits(vecm_fit, "error")) {
    empty_vector$status <- "vecm_error"
    empty_adjustment$status <- "vecm_error"
    empty_short_run$status <- "vecm_error"
    empty_vector$note <- vecm_fit$message
    empty_adjustment$note <- vecm_fit$message
    empty_short_run$note <- vecm_fit$message
    return(list(
      vectors = empty_vector,
      adjustment = empty_adjustment,
      short_run = empty_short_run
    ))
  }

  beta_long <- matrix_to_long(vecm_fit$beta, "beta_coefficient")
  normalized_to <- rownames(as.matrix(vecm_fit$beta))[1]
  normalizing_value <- as.numeric(as.matrix(vecm_fit$beta)[1, ])
  names(normalizing_value) <- colnames(as.matrix(vecm_fit$beta))
  beta_long$system <- system_name
  beta_long$ecdet <- ecdet
  beta_long$spec <- johansen_spec
  beta_long$selected_var_lag <- selected_var_lag
  beta_long$johansen_k <- lag_k
  beta_long$rank_used <- rank_used
  beta_long$normalized_to <- normalized_to
  beta_long$long_run_elasticity <- mapply(
    function(relation, term, coefficient) {
      norm_value <- normalizing_value[[relation]]
      if (is.na(norm_value) || norm_value == 0 ||
          identical(term, normalized_to) ||
          grepl("constant|const|trend", term, ignore.case = TRUE)) {
        return(NA_real_)
      }
      -coefficient / norm_value
    },
    beta_long$relation,
    beta_long$term,
    beta_long$beta_coefficient
  )
  beta_long$status <- "ok"
  beta_long$note <- status_note
  beta_long <- beta_long[, c(
    "system", "ecdet", "spec", "selected_var_lag", "johansen_k", "rank_used",
    "relation", "term", "beta_coefficient", "normalized_to",
    "long_run_elasticity", "status", "note"
  )]

  coef_matrix <- as.matrix(stats::coef(vecm_fit$rlm))
  coef_long <- matrix_to_long(coef_matrix, "coefficient")
  names(coef_long)[names(coef_long) == "relation"] <- "equation"
  coefficient_statistics <- extract_coefficient_statistics(vecm_fit$rlm)
  coef_long <- merge(
    coef_long,
    coefficient_statistics,
    by = c("equation", "term"),
    all.x = TRUE,
    sort = FALSE
  )
  coef_long$coefficient_block <- ifelse(
    grepl("^ect", coef_long$term, ignore.case = TRUE),
    "adjustment",
    ifelse(grepl("\\.dl[0-9]+$|\\.d$", coef_long$term), "short_run", "deterministic")
  )
  coef_long$system <- system_name
  coef_long$ecdet <- ecdet
  coef_long$spec <- johansen_spec
  coef_long$selected_var_lag <- selected_var_lag
  coef_long$johansen_k <- lag_k
  coef_long$rank_used <- rank_used
  coef_long$status <- "ok"
  coef_long$note <- status_note

  adjustment <- coef_long[coef_long$coefficient_block == "adjustment", ]
  if (nrow(adjustment) == 0) {
    adjustment <- empty_adjustment
    adjustment$status <- "no_adjustment_terms_found"
  } else {
    names(adjustment)[names(adjustment) == "term"] <- "relation"
    names(adjustment)[names(adjustment) == "coefficient"] <-
      "adjustment_coefficient"
    adjustment <- adjustment[, c(
      "system", "ecdet", "spec", "selected_var_lag", "johansen_k",
      "rank_used", "equation", "relation", "adjustment_coefficient",
      "std_error", "statistic", "p_value", "status", "note"
    )]
  }

  short_run <- coef_long[coef_long$coefficient_block != "adjustment", ]
  short_run <- short_run[, c(
    "system", "ecdet", "spec", "selected_var_lag", "johansen_k",
    "rank_used", "equation", "term", "coefficient", "std_error",
    "statistic", "p_value", "coefficient_block", "status", "note"
  )]

  list(
    vectors = beta_long,
    adjustment = adjustment,
    short_run = short_run
  )
}

imports_vecm_rank <- johansen_rank_decision$selected_rank_trace_5pct[
  johansen_rank_decision$system == "imports" &
    johansen_rank_decision$ecdet == johansen_ecdet
][1]

imports_vecm_outputs <- extract_vecm_outputs(
  system_name = "imports",
  ecdet = johansen_ecdet,
  rank_used = imports_vecm_rank,
  status_note = paste(
    "VECM estimado para importaciones con el rango de traza al 5%;",
    "se reporta como extension multivariada y respaldo del ECM uniecuacional."
  )
)

vecm_cointegration_vectors <- imports_vecm_outputs$vectors
vecm_adjustment_coefficients <- clean_empty_inference_columns(
  imports_vecm_outputs$adjustment
)
vecm_short_run_summary <- clean_empty_inference_columns(
  imports_vecm_outputs$short_run
)

#********************************************************
# 8B. Autocorrelacion residual con script de clase vcorr_res.R
#********************************************************
# Run autocorrelacion de clase: estima un VAR y aplica vcorr_res.R para obtener
# diagnosticos Q/BG de residuos en el formato usado en clase.
# se define la funcion auxiliar run_course_vcorr.
run_course_vcorr <- function(system_name, matrix_data, p, type, lags = 12,
                             test_type = "PT.adjusted") {
  if (!course_script_loaded("vcorr_res.R") ||
      !exists("vcorr_res", mode = "function")) {
    return(data.frame(
      system = system_name,
      course_script = "vcorr_res.R",
      var_lag = p,
      var_type = type,
      diagnostic_type = test_type,
      lag = NA_integer_,
      statistic = NA_real_,
      p_value = NA_character_,
      status = "not_run",
      message = course_scripts_status$message[
        course_scripts_status$file == "vcorr_res.R"
      ][1],
      stringsAsFactors = FALSE
    ))
  }

  # se ejecuta esta instruccion puntual del procedimiento.
  suppressPackageStartupMessages(library(vars))

  # se construye fit con las instrucciones de este minibloque.
  fit <- tryCatch(
    vars::VAR(matrix_data, p = p, type = type),
    error = function(e) e
  )

  # se evalua una condicion antes de decidir el siguiente paso.
  if (inherits(fit, "error")) {
    return(data.frame(
      system = system_name,
      course_script = "vcorr_res.R",
      var_lag = p,
      var_type = type,
      diagnostic_type = test_type,
      lag = NA_integer_,
      statistic = NA_real_,
      p_value = NA_character_,
      status = "var_error",
      message = fit$message,
      stringsAsFactors = FALSE
    ))
  }

  # se construye max_lags_allowed con las instrucciones de este minibloque.
  max_lags_allowed <- floor(fit$obs / 3)
  lags_to_use <- min(lags, max_lags_allowed)

  # se construye vcorr_result con las instrucciones de este minibloque.
  vcorr_result <- tryCatch(
    vcorr_res(fit, lags = lags_to_use, tipo = test_type),
    error = function(e) e
  )

  # se evalua una condicion antes de decidir el siguiente paso.
  if (inherits(vcorr_result, "error")) {
    return(data.frame(
      system = system_name,
      course_script = "vcorr_res.R",
      var_lag = p,
      var_type = type,
      diagnostic_type = test_type,
      lag = NA_integer_,
      statistic = NA_real_,
      p_value = NA_character_,
      status = "vcorr_error",
      message = vcorr_result$message,
      stringsAsFactors = FALSE
    ))
  }

  # se construye vcorr_df con las instrucciones de este minibloque.
  vcorr_df <- as.data.frame(vcorr_result, stringsAsFactors = FALSE)
  names(vcorr_df) <- c("lag", "statistic", "p_value")
  vcorr_df$lag <- as.integer(vcorr_df$lag)
  vcorr_df$statistic <- as.numeric(vcorr_df$statistic)
  vcorr_df$p_value <- as.character(vcorr_df$p_value)

  # se ejecuta este bloque amplio del procedimiento.
  data.frame(
    system = system_name,
    course_script = "vcorr_res.R",
    var_lag = p,
    var_type = type,
    diagnostic_type = test_type,
    lag = vcorr_df$lag,
    statistic = vcorr_df$statistic,
    p_value = vcorr_df$p_value,
    status = "ok",
    message = paste("Diagnostico", test_type, "ejecutado con script de clase."),
    stringsAsFactors = FALSE
  )
}

# se consolida course_vcorr_results a partir de resultados parciales.
course_vcorr_results <- do.call(
  rbind,
  list(
    run_course_vcorr(
      system_name = "imports",
      matrix_data = imports_var_matrix,
      p = get_preferred_var_lag("imports"),
      type = var_type,
      test_type = "PT.adjusted"
    ),
    run_course_vcorr(
      system_name = "imports",
      matrix_data = imports_var_matrix,
      p = get_preferred_var_lag("imports"),
      type = var_type,
      test_type = "BG"
    ),
    run_course_vcorr(
      system_name = "exports",
      matrix_data = exports_var_matrix,
      p = get_preferred_var_lag("exports"),
      type = var_type,
      test_type = "PT.adjusted"
    ),
    run_course_vcorr(
      system_name = "exports",
      matrix_data = exports_var_matrix,
      p = get_preferred_var_lag("exports"),
      type = var_type,
      test_type = "BG"
    )
  )
)

#********************************************************
# 8C. Heterocedasticidad residual con script VAR_white_no_cross.R
#********************************************************
# Run White no-cross: aplica el test de heterocedasticidad pedido por la consigna.
run_course_white_no_cross <- function(system_name, matrix_data, p, type) {
  if (!course_script_loaded("VAR_white_no_cross.R") ||
      !exists("VAR_white_no_cross", mode = "function")) {
    return(data.frame(
      system = system_name,
      course_script = "VAR_white_no_cross.R",
      var_lag = p,
      var_type = type,
      equation = NA_character_,
      statistic = NA_real_,
      df = NA_integer_,
      p_value = NA_real_,
      n_obs = NA_integer_,
      n_auxiliary_terms = NA_integer_,
      conclusion_5pct = NA_character_,
      note = "No se pudo interpretar porque el script requerido no cargo.",
      status = "not_run",
      message = course_scripts_status$message[
        course_scripts_status$file == "VAR_white_no_cross.R"
      ][1],
      stringsAsFactors = FALSE
    ))
  }

  suppressPackageStartupMessages(library(vars))

  fit <- tryCatch(
    vars::VAR(matrix_data, p = p, type = type),
    error = function(e) e
  )

  # Si falla la estimacion del VAR, se registra el error sin cortar la corrida.
  if (inherits(fit, "error")) {
    return(data.frame(
      system = system_name,
      course_script = "VAR_white_no_cross.R",
      var_lag = p,
      var_type = type,
      equation = NA_character_,
      statistic = NA_real_,
      df = NA_integer_,
      p_value = NA_real_,
      n_obs = NA_integer_,
      n_auxiliary_terms = NA_integer_,
      conclusion_5pct = NA_character_,
      note = "No se pudo interpretar porque el VAR no pudo estimarse.",
      status = "var_error",
      message = fit$message,
      stringsAsFactors = FALSE
    ))
  }

  white_result <- tryCatch(
    VAR_white_no_cross(fit),
    error = function(e) e
  )

  # Si falla el White, se conserva el mensaje para auditar el script requerido.
  if (inherits(white_result, "error")) {
    return(data.frame(
      system = system_name,
      course_script = "VAR_white_no_cross.R",
      var_lag = p,
      var_type = type,
      equation = NA_character_,
      statistic = NA_real_,
      df = NA_integer_,
      p_value = NA_real_,
      n_obs = NA_integer_,
      n_auxiliary_terms = NA_integer_,
      conclusion_5pct = NA_character_,
      note = "No se pudo interpretar porque fallo el test White no-cross.",
      status = "white_error",
      message = white_result$message,
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    system = system_name,
    course_script = "VAR_white_no_cross.R",
    var_lag = p,
    var_type = type,
    equation = white_result$equation,
    statistic = white_result$statistic,
    df = white_result$df,
    p_value = white_result$p_value,
    n_obs = white_result$n_obs,
    n_auxiliary_terms = white_result$n_auxiliary_terms,
    conclusion_5pct = white_result$conclusion_5pct,
    note = white_result$note,
    status = "ok",
    message = "White no-cross ejecutado con VAR_white_no_cross.R.",
    stringsAsFactors = FALSE
  )
}

# Se corre White no-cross sobre los VAR finales ajustados por diagnostico.
course_white_no_cross_results <- rbind(
  run_course_white_no_cross(
    system_name = "imports",
    matrix_data = imports_var_matrix,
    p = get_preferred_var_lag("imports"),
    type = var_type
  ),
  run_course_white_no_cross(
    system_name = "exports",
    matrix_data = exports_var_matrix,
    p = get_preferred_var_lag("exports"),
    type = var_type
  )
)

#********************************************************
# 8D. Decisiones VECM/VAR con retroalimentacion diagnostica
#********************************************************
# se construye johansen_robustness_summary mediante un bloque de calculo extendido.
johansen_robustness_summary <- johansen_rank_decision[, c(
  "system",
  "ecdet",
  "selected_var_lag",
  "lag_k",
  "selected_rank_trace_5pct",
  "selected_rank_eigen_5pct",
  "integration_warning"
)]

# se ajustan nombres para que la salida sea legible.
names(johansen_robustness_summary) <- c(
  "system",
  "deterministic_specification",
  "selected_var_lag",
  "johansen_k",
  "rank_trace_5pct",
  "rank_eigen_5pct",
  "integration_warning"
)

# se define la funcion auxiliar get_rank_decision_value.
get_rank_decision_value <- function(system_name, ecdet, rank_column) {
  value <- johansen_rank_decision[
    johansen_rank_decision$system == system_name &
      johansen_rank_decision$ecdet == ecdet,
    rank_column
  ]

  # se evalua una condicion antes de decidir el siguiente paso.
  if (length(value) == 0) {
    return(NA_integer_)
  }

  # se ejecuta este minibloque del procedimiento.
  as.integer(value[1])
}

# se define la funcion auxiliar get_eg_use_ecm.
get_eg_use_ecm <- function(system_name) {
  value <- engle_granger_decision$use_ecm[
    engle_granger_decision$model_key == system_name
  ]

  # se evalua una condicion antes de decidir el siguiente paso.
  if (length(value) == 0) {
    return(FALSE)
  }

  # se ejecuta este minibloque del procedimiento.
  isTRUE(value[1])
}

# se define la funcion auxiliar get_formal_order_for_key.
get_formal_order_for_key <- function(series_key) {
  value <- adf_urca_order_summary$formal_order[
    adf_urca_order_summary$series_key == series_key
  ]

  # se evalua una condicion antes de decidir el siguiente paso.
  if (length(value) == 0) {
    return(NA_character_)
  }

  # se ejecuta este minibloque del procedimiento.
  value[1]
}

# se define la funcion auxiliar get_lag_diagnostic_value.
get_lag_diagnostic_value <- function(system_name, column_name) {
  value <- var_lag_diagnostic_decision[
    var_lag_diagnostic_decision$system == system_name,
    column_name
  ]

  if (length(value) == 0) {
    return(NA)
  }

  value[1]
}

# se define la funcion auxiliar get_white_joint_value.
get_white_joint_value <- function(system_name, column_name) {
  value <- course_white_no_cross_results[
    course_white_no_cross_results$system == system_name &
      course_white_no_cross_results$equation == "joint",
    column_name
  ]

  if (length(value) == 0) {
    return(NA)
  }

  value[1]
}

# se define la funcion auxiliar combined_diagnostic_status.
combined_diagnostic_status <- function(system_name) {
  serial_status <- get_lag_diagnostic_value(system_name, "diagnostic_status")
  white_conclusion <- get_white_joint_value(system_name, "conclusion_5pct")
  white_rejects <- identical(white_conclusion, "reject homoskedasticity")

  if (identical(serial_status, "residual_autocorrelation_warning") &&
      white_rejects) {
    return("serial_and_heteroskedasticity_warning")
  }
  if (identical(serial_status, "residual_autocorrelation_warning")) {
    return("serial_autocorrelation_warning")
  }
  if (white_rejects) {
    return("heteroskedasticity_warning")
  }

  "diagnostics_pass"
}

# se arma la tabla var_vec_treatment_decision con resultados de este paso.
var_vec_treatment_decision <- data.frame(
  system = c("imports", "exports"),
  dependent_series_order = c(
    get_formal_order_for_key("imports"),
    get_formal_order_for_key("exports")
  ),
  base_var_lag = c(
    get_lag_diagnostic_value("imports", "base_selected_lag"),
    get_lag_diagnostic_value("exports", "base_selected_lag")
  ),
  final_var_lag = c(
    get_lag_diagnostic_value("imports", "final_selected_lag"),
    get_lag_diagnostic_value("exports", "final_selected_lag")
  ),
  residual_diagnostic_status = c(
    get_lag_diagnostic_value("imports", "diagnostic_status"),
    get_lag_diagnostic_value("exports", "diagnostic_status")
  ),
  white_joint_p_value = c(
    get_white_joint_value("imports", "p_value"),
    get_white_joint_value("exports", "p_value")
  ),
  white_joint_conclusion_5pct = c(
    get_white_joint_value("imports", "conclusion_5pct"),
    get_white_joint_value("exports", "conclusion_5pct")
  ),
  diagnostic_validity_status = c(
    combined_diagnostic_status("imports"),
    combined_diagnostic_status("exports")
  ),
  engle_granger_supports_cointegration = c(
    get_eg_use_ecm("imports"),
    get_eg_use_ecm("exports")
  ),
  johansen_const_trace_rank = c(
    get_rank_decision_value("imports", "const", "selected_rank_trace_5pct"),
    get_rank_decision_value("exports", "const", "selected_rank_trace_5pct")
  ),
  johansen_const_eigen_rank = c(
    get_rank_decision_value("imports", "const", "selected_rank_eigen_5pct"),
    get_rank_decision_value("exports", "const", "selected_rank_eigen_5pct")
  ),
  johansen_none_trace_rank = c(
    get_rank_decision_value("imports", "none", "selected_rank_trace_5pct"),
    get_rank_decision_value("exports", "none", "selected_rank_trace_5pct")
  ),
  johansen_none_eigen_rank = c(
    get_rank_decision_value("imports", "none", "selected_rank_eigen_5pct"),
    get_rank_decision_value("exports", "none", "selected_rank_eigen_5pct")
  ),
  recommended_var_vec_treatment = c(
    "estimar VECM con r=1 como extension multivariada; mantener ECM como respaldo",
    "estimar VAR en diferencias; reportar Johansen solo como evidencia indicativa"
  ),
  decision_note = c(
    paste(
      "Importaciones combina ADF compatible con I(1), evidencia Engle-Granger",
      "y rango 1 por traza en Johansen con constante; maximo autovalor no confirma.",
      get_lag_diagnostic_value("imports", "note"),
      get_white_joint_value("imports", "note")
    ),
    paste(
      "Exportaciones presenta alerta ADF I(0) y Engle-Granger no respalda",
      "cointegracion; por prudencia no se usara VECM como especificacion principal.",
      get_lag_diagnostic_value("exports", "note"),
      get_white_joint_value("exports", "note")
    )
  ),
  stringsAsFactors = FALSE
)

# se define la funcion auxiliar get_unique_lag_used.
get_unique_lag_used <- function(data, system_name, lag_column) {
  values <- unique(data[data$system == system_name, lag_column])
  values <- values[!is.na(values)]

  if (length(values) == 0) {
    return(NA_integer_)
  }
  if (length(values) > 1) {
    return(NA_integer_)
  }

  as.integer(values[1])
}

# Esta tabla verifica que las decisiones posteriores usen el rezago retroalimentado.
feedback_audit_rows <- list()
for (system_name in c("imports", "exports")) {
  base_lag <- as.integer(get_lag_diagnostic_value(system_name, "base_selected_lag"))
  final_lag <- as.integer(get_lag_diagnostic_value(system_name, "final_selected_lag"))
  stage_lags <- data.frame(
    downstream_block = c(
      "VAR_lag_exclusion_wald",
      "Johansen_rank_decision",
      "vcorr_residual_autocorrelation",
      "white_no_cross_heteroskedasticity",
      "final_var_vec_treatment"
    ),
    lag_used = c(
      get_unique_lag_used(var_lag_exclusion_wald_results, system_name, "var_lag"),
      get_unique_lag_used(johansen_rank_decision, system_name, "selected_var_lag"),
      get_unique_lag_used(course_vcorr_results, system_name, "var_lag"),
      get_unique_lag_used(course_white_no_cross_results, system_name, "var_lag"),
      as.integer(get_lag_diagnostic_value(system_name, "final_selected_lag"))
    ),
    stringsAsFactors = FALSE
  )

  feedback_audit_rows[[system_name]] <- data.frame(
    system = system_name,
    base_selected_lag = base_lag,
    final_selected_lag = final_lag,
    stage_lags,
    uses_final_lag = stage_lags$lag_used == final_lag,
    feedback_role = ifelse(
      stage_lags$downstream_block == "final_var_vec_treatment",
      "resume diagnosticos y decisiones econometricas",
      "usa rezago final ajustado por diagnosticos"
    ),
    stringsAsFactors = FALSE
  )
}

feedback_audit <- do.call(rbind, feedback_audit_rows)
rownames(feedback_audit) <- NULL

#********************************************************
# 9. VAR en diferencias para sistemas sin cointegracion robusta
#********************************************************
# se define el sistema operativo en diferencias para exportaciones.
exports_var_diff_vars <- c(
  "d_ln_exports_real",
  "d_ln_pib_socios",
  "d_ln_itcrm"
)

# se construye la matriz VAR en diferencias para exportaciones.
exports_var_diff_matrix <- prepare_var_matrix(work_data, exports_var_diff_vars)
exports_var_diff_lag <- get_preferred_var_lag("exports")
exports_var_diff_type <- var_type

# se estima el VAR en diferencias que opera como especificacion principal para
# exportaciones cuando no se verifica cointegracion.
exports_var_diff_fit <- tryCatch(
  vars::VAR(
    exports_var_diff_matrix,
    p = exports_var_diff_lag,
    type = exports_var_diff_type
  ),
  error = function(e) e
)

# se define la funcion auxiliar var_fit_failed_row.
var_fit_failed_row <- function(system_name, model_key, stage, fit_error) {
  data.frame(
    system = system_name,
    model_key = model_key,
    stage = stage,
    status = "var_error",
    message = fit_error$message,
    stringsAsFactors = FALSE
  )
}

# se define la funcion auxiliar extract_var_equation_coefficients.
extract_var_equation_coefficients <- function(fit, system_name, model_key,
                                              equation_name) {
  if (inherits(fit, "error")) {
    return(var_fit_failed_row(system_name, model_key, "coefficients", fit))
  }

  fit_summary <- summary(fit)
  equation_fit <- fit_summary$varresult[[equation_name]]
  if (is.null(equation_fit)) {
    return(data.frame(
      system = system_name,
      model_key = model_key,
      equation = equation_name,
      term = NA_character_,
      estimate = NA_real_,
      std_error = NA_real_,
      statistic = NA_real_,
      p_value = NA_real_,
      status = "equation_not_found",
      message = paste("No se encontro la ecuacion", equation_name),
      stringsAsFactors = FALSE
    ))
  }

  coefficient_matrix <- if (is.list(equation_fit) &&
                            is.matrix(equation_fit$coefficients)) {
    equation_fit$coefficients
  } else {
    coef(summary(equation_fit))
  }
  coefficient_df <- data.frame(
    system = system_name,
    model_key = model_key,
    equation = equation_name,
    term = rownames(coefficient_matrix),
    estimate = coefficient_matrix[, "Estimate"],
    std_error = coefficient_matrix[, "Std. Error"],
    statistic = coefficient_matrix[, "t value"],
    p_value = coefficient_matrix[, "Pr(>|t|)"],
    status = "ok",
    message = "Coeficientes extraidos de la ecuacion de exportaciones del VAR en diferencias.",
    stringsAsFactors = FALSE
  )
  rownames(coefficient_df) <- NULL
  coefficient_df
}

# se define la funcion auxiliar extract_var_model_summary.
extract_var_model_summary <- function(fit, system_name, model_key, matrix_data,
                                      variables, lag, type) {
  if (inherits(fit, "error")) {
    return(var_fit_failed_row(system_name, model_key, "model_summary", fit))
  }

  data.frame(
    system = system_name,
    model_key = model_key,
    variables = paste(variables, collapse = ", "),
    var_lag = lag,
    var_type = type,
    n_input_obs = nrow(matrix_data),
    n_estimation_obs = fit$obs,
    first_quarter = work_data$quarter[
      complete.cases(work_data[, variables])
    ][1],
    last_quarter = tail(
      work_data$quarter[complete.cases(work_data[, variables])],
      1
    ),
    status = "ok",
    message = paste(
      "VAR en diferencias estimado como especificacion operativa para",
      "exportaciones sin cointegracion robusta."
    ),
    stringsAsFactors = FALSE
  )
}

# se define la funcion auxiliar get_test_p_value.
get_test_p_value <- function(test_object, path) {
  value <- test_object
  for (name in path) {
    value <- value[[name]]
    if (is.null(value)) {
      return(NA_real_)
    }
  }
  value <- as.numeric(value)
  if (length(value) == 1) value else NA_real_
}

# se define la funcion auxiliar get_test_statistic_value.
get_test_statistic_value <- function(test_object, path) {
  value <- test_object
  for (name in path) {
    value <- value[[name]]
    if (is.null(value)) {
      return(NA_real_)
    }
  }
  value <- as.numeric(value)
  if (length(value) == 1) value else NA_real_
}

# se define la funcion auxiliar run_var_diff_diagnostics.
run_var_diff_diagnostics <- function(fit, system_name, model_key,
                                     diagnostic_lag = 12) {
  if (inherits(fit, "error")) {
    return(var_fit_failed_row(system_name, model_key, "diagnostics", fit))
  }

  lags_to_use <- min(diagnostic_lag, floor(fit$obs / 3))
  pt_result <- tryCatch(
    vars::serial.test(fit, lags.pt = lags_to_use, type = "PT.adjusted"),
    error = function(e) e
  )
  bg_result <- tryCatch(
    vars::serial.test(fit, lags.bg = lags_to_use, type = "BG"),
    error = function(e) e
  )
  arch_result <- tryCatch(
    vars::arch.test(fit, lags.multi = lags_to_use),
    error = function(e) e
  )
  normality_result <- tryCatch(
    vars::normality.test(fit),
    error = function(e) e
  )
  root_values <- tryCatch(
    vars::roots(fit, modulus = TRUE),
    error = function(e) e
  )

  pt_p_value <- if (inherits(pt_result, "error")) {
    NA_real_
  } else {
    as.numeric(pt_result$serial$p.value)
  }
  pt_statistic <- if (inherits(pt_result, "error")) {
    NA_real_
  } else {
    as.numeric(pt_result$serial$statistic)
  }
  bg_p_value <- if (inherits(bg_result, "error")) {
    NA_real_
  } else {
    as.numeric(bg_result$serial$p.value)
  }
  bg_statistic <- if (inherits(bg_result, "error")) {
    NA_real_
  } else {
    as.numeric(bg_result$serial$statistic)
  }
  arch_p_value <- if (inherits(arch_result, "error")) {
    NA_real_
  } else {
    get_test_p_value(arch_result, c("arch.mul", "p.value"))
  }
  arch_statistic <- if (inherits(arch_result, "error")) {
    NA_real_
  } else {
    get_test_statistic_value(arch_result, c("arch.mul", "statistic"))
  }
  jb_p_value <- if (inherits(normality_result, "error")) {
    NA_real_
  } else {
    get_test_p_value(normality_result, c("jb.mul", "JB", "p.value"))
  }
  jb_statistic <- if (inherits(normality_result, "error")) {
    NA_real_
  } else {
    get_test_statistic_value(normality_result, c("jb.mul", "JB", "statistic"))
  }
  max_root_modulus <- if (inherits(root_values, "error")) {
    NA_real_
  } else {
    max(as.numeric(root_values), na.rm = TRUE)
  }

  data.frame(
    system = system_name,
    model_key = model_key,
    var_lag = fit$p,
    diagnostic_lag = lags_to_use,
    pt_adjusted_statistic = pt_statistic,
    pt_adjusted_p_value = pt_p_value,
    bg_statistic = bg_statistic,
    bg_p_value = bg_p_value,
    arch_multivariate_statistic = arch_statistic,
    arch_multivariate_p_value = arch_p_value,
    jb_multivariate_statistic = jb_statistic,
    jb_multivariate_p_value = jb_p_value,
    max_root_modulus = max_root_modulus,
    serial_pass_5pct = is.finite(pt_p_value) && is.finite(bg_p_value) &&
      pt_p_value >= 0.05 && bg_p_value >= 0.05,
    arch_pass_5pct = is.finite(arch_p_value) && arch_p_value >= 0.05,
    normality_pass_5pct = is.finite(jb_p_value) && jb_p_value >= 0.05,
    stability_pass = is.finite(max_root_modulus) && max_root_modulus < 1,
    status = "ok",
    message = paste(
      "Diagnosticos VAR en diferencias: autocorrelacion, ARCH, normalidad",
      "multivariada y estabilidad."
    ),
    stringsAsFactors = FALSE
  )
}

# se define la funcion auxiliar extract_var_roots.
extract_var_roots <- function(fit, system_name, model_key) {
  if (inherits(fit, "error")) {
    return(var_fit_failed_row(system_name, model_key, "roots", fit))
  }

  root_values <- tryCatch(
    vars::roots(fit, modulus = TRUE),
    error = function(e) e
  )
  if (inherits(root_values, "error")) {
    return(var_fit_failed_row(system_name, model_key, "roots", root_values))
  }

  data.frame(
    system = system_name,
    model_key = model_key,
    root_id = seq_along(root_values),
    modulus = as.numeric(root_values),
    stable_root = as.numeric(root_values) < 1,
    status = "ok",
    message = "Raices del VAR en diferencias; estabilidad si todos los modulos son menores que uno.",
    stringsAsFactors = FALSE
  )
}

var_diff_model_summary <- extract_var_model_summary(
  fit = exports_var_diff_fit,
  system_name = "exports",
  model_key = "exports_var_diff",
  matrix_data = exports_var_diff_matrix,
  variables = exports_var_diff_vars,
  lag = exports_var_diff_lag,
  type = exports_var_diff_type
)

var_diff_exports_equation_coefficients <- extract_var_equation_coefficients(
  fit = exports_var_diff_fit,
  system_name = "exports",
  model_key = "exports_var_diff",
  equation_name = "d_ln_exports_real"
)

var_diff_diagnostics <- run_var_diff_diagnostics(
  fit = exports_var_diff_fit,
  system_name = "exports",
  model_key = "exports_var_diff"
)

var_diff_stability_roots <- extract_var_roots(
  fit = exports_var_diff_fit,
  system_name = "exports",
  model_key = "exports_var_diff"
)

#********************************************************
# 9B. Diagnosticos finales de normalidad y estabilidad
#********************************************************
# se define la funcion auxiliar estimate_var_for_final_diagnostics.
estimate_var_for_final_diagnostics <- function(matrix_data, p, type) {
  tryCatch(
    vars::VAR(matrix_data, p = p, type = type),
    error = function(e) e
  )
}

# se define la funcion auxiliar run_normality_stability_summary.
run_normality_stability_summary <- function(system_name, model_key, model_label,
                                            matrix_data, variables, p, type,
                                            treatment_role, validity_note) {
  fit <- estimate_var_for_final_diagnostics(matrix_data, p, type)

  if (inherits(fit, "error")) {
    return(data.frame(
      system = system_name,
      model_key = model_key,
      model_label = model_label,
      variables = paste(variables, collapse = ", "),
      var_lag = p,
      var_type = type,
      treatment_role = treatment_role,
      jb_multivariate_p_value = NA_real_,
      normality_decision_5pct = NA_character_,
      max_root_modulus = NA_real_,
      stability_decision = NA_character_,
      validity_note = validity_note,
      status = "var_error",
      message = fit$message,
      stringsAsFactors = FALSE
    ))
  }

  normality_result <- tryCatch(
    vars::normality.test(fit),
    error = function(e) e
  )
  root_values <- tryCatch(
    vars::roots(fit, modulus = TRUE),
    error = function(e) e
  )

  jb_p_value <- if (inherits(normality_result, "error")) {
    NA_real_
  } else {
    get_test_p_value(normality_result, c("jb.mul", "JB", "p.value"))
  }
  max_root_modulus <- if (inherits(root_values, "error")) {
    NA_real_
  } else {
    max(as.numeric(root_values), na.rm = TRUE)
  }

  normality_pass <- is.finite(jb_p_value) && jb_p_value >= 0.05
  stability_pass <- is.finite(max_root_modulus) && max_root_modulus < 1

  data.frame(
    system = system_name,
    model_key = model_key,
    model_label = model_label,
    variables = paste(variables, collapse = ", "),
    var_lag = p,
    var_type = type,
    treatment_role = treatment_role,
    jb_multivariate_p_value = jb_p_value,
    normality_decision_5pct = ifelse(
      normality_pass,
      "no rechaza normalidad",
      "rechaza normalidad"
    ),
    max_root_modulus = max_root_modulus,
    stability_decision = ifelse(
      stability_pass,
      "estable",
      "no estable"
    ),
    validity_note = validity_note,
    status = "ok",
    message = "Diagnostico final de normalidad multivariada y estabilidad VAR.",
    stringsAsFactors = FALSE
  )
}

normality_stability_summary <- rbind(
  run_normality_stability_summary(
    system_name = "imports",
    model_key = "imports_levels_var_vec",
    model_label = "Importaciones VAR/VEC en niveles",
    matrix_data = imports_var_matrix,
    variables = imports_system_vars,
    p = get_preferred_var_lag("imports"),
    type = var_type,
    treatment_role = "VECM indicativo con ECM Engle-Granger como respaldo",
    validity_note = paste(
      "Johansen/VECM se reporta como extension multivariada; la lectura final",
      "debe combinar normalidad, estabilidad y autocorrelacion residual."
    )
  ),
  run_normality_stability_summary(
    system_name = "exports",
    model_key = "exports_levels_johansen_var",
    model_label = "Exportaciones VAR/Johansen en niveles",
    matrix_data = exports_var_matrix,
    variables = exports_system_vars,
    p = get_preferred_var_lag("exports"),
    type = var_type,
    treatment_role = "Ejercicio indicativo de Johansen en niveles",
    validity_note = paste(
      "Exportaciones no respalda cointegracion; el sistema en niveles se",
      "conserva solo como ejercicio indicativo solicitado por la consigna."
    )
  ),
  run_normality_stability_summary(
    system_name = "exports",
    model_key = "exports_var_diff",
    model_label = "Exportaciones VAR en diferencias",
    matrix_data = exports_var_diff_matrix,
    variables = exports_var_diff_vars,
    p = exports_var_diff_lag,
    type = exports_var_diff_type,
    treatment_role = "Especificacion operativa de corto plazo",
    validity_note = paste(
      "Modelo principal para exportaciones cuando no se verifica cointegracion;",
      "sus coeficientes se interpretan como efectos dinamicos de corto plazo."
    )
  )
)

#********************************************************
# 10. Extensiones de clase: IRF, FEVD, SVAR y LPIRF
#********************************************************
# Estos ejercicios reproducen la arquitectura de las clases VAR/IRF/SVAR y
# proyecciones locales. Se reportan como extensiones indicativas y no sustituyen
# la decision principal de VAR/VEC/ECM.
class_extension_horizon <- 12

# se define la funcion auxiliar extract_irf_table.
extract_irf_table <- function(irf_object, system_name, model_key) {
  if (inherits(irf_object, "error")) {
    return(data.frame(
      system = system_name,
      model_key = model_key,
      impulse = NA_character_,
      response = NA_character_,
      horizon = NA_integer_,
      irf = NA_real_,
      lower = NA_real_,
      upper = NA_real_,
      status = "irf_error",
      message = irf_object$message,
      stringsAsFactors = FALSE
    ))
  }

  rows <- list()
  for (impulse_name in names(irf_object$irf)) {
    response_matrix <- as.matrix(irf_object$irf[[impulse_name]])
    lower_matrix <- if (!is.null(irf_object$Lower[[impulse_name]])) {
      as.matrix(irf_object$Lower[[impulse_name]])
    } else {
      matrix(NA_real_, nrow(response_matrix), ncol(response_matrix))
    }
    upper_matrix <- if (!is.null(irf_object$Upper[[impulse_name]])) {
      as.matrix(irf_object$Upper[[impulse_name]])
    } else {
      matrix(NA_real_, nrow(response_matrix), ncol(response_matrix))
    }
    response_names <- colnames(response_matrix)
    if (is.null(response_names)) {
      response_names <- paste0("response_", seq_len(ncol(response_matrix)))
    }

    for (response_i in seq_len(ncol(response_matrix))) {
      response_name <- response_names[response_i]
      rows[[length(rows) + 1]] <- data.frame(
        system = system_name,
        model_key = model_key,
        impulse = impulse_name,
        response = response_name,
        horizon = seq_len(nrow(response_matrix)) - 1L,
        irf = as.numeric(response_matrix[, response_i]),
        lower = as.numeric(lower_matrix[, response_i]),
        upper = as.numeric(upper_matrix[, response_i]),
        status = "ok",
        message = "IRF calculada con vars::irf como en clase.",
        stringsAsFactors = FALSE
      )
    }
  }

  do.call(rbind, rows)
}

# se define la funcion auxiliar extract_fevd_table.
extract_fevd_table <- function(fevd_object, system_name, model_key) {
  if (inherits(fevd_object, "error")) {
    return(data.frame(
      system = system_name,
      model_key = model_key,
      response = NA_character_,
      shock = NA_character_,
      horizon = NA_integer_,
      share = NA_real_,
      status = "fevd_error",
      message = fevd_object$message,
      stringsAsFactors = FALSE
    ))
  }

  rows <- list()
  for (response_name in names(fevd_object)) {
    fevd_matrix <- as.matrix(fevd_object[[response_name]])
    for (shock_name in colnames(fevd_matrix)) {
      rows[[length(rows) + 1]] <- data.frame(
        system = system_name,
        model_key = model_key,
        response = response_name,
        shock = shock_name,
        horizon = seq_len(nrow(fevd_matrix)),
        share = as.numeric(fevd_matrix[, shock_name]),
        status = "ok",
        message = "Descomposicion de varianza calculada con vars::fevd como en clase.",
        stringsAsFactors = FALSE
      )
    }
  }

  do.call(rbind, rows)
}

# se define la funcion auxiliar run_class_svar.
run_class_svar <- function(fit, system_name, model_key) {
  if (inherits(fit, "error")) {
    return(data.frame(
      system = system_name,
      model_key = model_key,
      variables = NA_character_,
      identification = NA_character_,
      status = "var_error",
      message = fit$message,
      stringsAsFactors = FALSE
    ))
  }

  var_names <- fit$y.names
  if (is.null(var_names) || length(var_names) == 0) {
    var_names <- colnames(fit$y)
  }
  if (is.null(var_names) || length(var_names) == 0) {
    var_names <- paste0("y", seq_len(ncol(fit$y)))
  }

  k_vars <- length(var_names)
  amat <- diag(1, k_vars)
  amat[lower.tri(amat)] <- NA

  svar_fit <- tryCatch(
    vars::SVAR(fit, estmethod = "scoring", Amat = amat),
    error = function(e) e
  )

  if (inherits(svar_fit, "error")) {
    return(data.frame(
      system = system_name,
      model_key = model_key,
      variables = paste(var_names, collapse = ", "),
      identification = "recursive A-model SVAR, lower-triangular A",
      status = "svar_error",
      message = svar_fit$message,
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    system = system_name,
    model_key = model_key,
    variables = paste(var_names, collapse = ", "),
    identification = "recursive A-model SVAR, lower-triangular A",
    status = "ok",
    message = "SVAR estimado con vars::SVAR; paquete svars disponible para extensiones SVAR.",
    stringsAsFactors = FALSE
  )
}

# se define la funcion auxiliar run_class_lpirfs.
run_class_lpirfs <- function(matrix_data, system_name, model_key, lag_order,
                             horizon) {
  if (requireNamespace("doParallel", quietly = TRUE)) {
    doParallel::stopImplicitCluster()
  }
  if (requireNamespace("foreach", quietly = TRUE)) {
    foreach::registerDoSEQ()
  }

  lp_result <- tryCatch(
    lpirfs::lp_lin(
      endog_data = as.data.frame(matrix_data),
      lags_endog_lin = lag_order,
      trend = 0,
      shock_type = 0,
      confint = 1.96,
      hor = horizon,
      num_cores = 1
    ),
    error = function(e) e
  )

  if (inherits(lp_result, "error")) {
    return(data.frame(
      system = system_name,
      model_key = model_key,
      package = "lpirfs",
      function_used = "lpirfs::lp_lin",
      lag_order = lag_order,
      horizon = horizon,
      status = "lpirfs_error",
      message = lp_result$message,
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    system = system_name,
    model_key = model_key,
    package = "lpirfs",
    function_used = "lpirfs::lp_lin",
    lag_order = lag_order,
    horizon = horizon,
    status = "ok",
    message = "Proyecciones locales calculadas con lpirfs::lp_lin como extension de clase.",
    stringsAsFactors = FALSE
  )
}

class_irf_object <- tryCatch(
  vars::irf(
    exports_var_diff_fit,
    n.ahead = class_extension_horizon,
    ortho = TRUE,
    cumulative = TRUE,
    boot = FALSE
  ),
  error = function(e) e
)

class_irf_results <- extract_irf_table(
  class_irf_object,
  system_name = "exports",
  model_key = "exports_var_diff_class_irf"
)

class_fevd_object <- tryCatch(
  vars::fevd(exports_var_diff_fit, n.ahead = class_extension_horizon),
  error = function(e) e
)

class_fevd_results <- extract_fevd_table(
  class_fevd_object,
  system_name = "exports",
  model_key = "exports_var_diff_class_fevd"
)

class_svar_status <- run_class_svar(
  exports_var_diff_fit,
  system_name = "exports",
  model_key = "exports_var_diff_class_svar"
)

class_lpirfs_status <- run_class_lpirfs(
  exports_var_diff_matrix,
  system_name = "exports",
  model_key = "exports_var_diff_class_lpirfs",
  lag_order = exports_var_diff_lag,
  horizon = class_extension_horizon
)

class_extensions_summary <- data.frame(
  extension = c("IRF", "FEVD", "SVAR", "LPIRF"),
  package = c("vars", "vars", "vars/svars", "lpirfs"),
  function_used = c("vars::irf", "vars::fevd", "vars::SVAR", "lpirfs::lp_lin"),
  system = "exports",
  model_key = c(
    "exports_var_diff_class_irf",
    "exports_var_diff_class_fevd",
    "exports_var_diff_class_svar",
    "exports_var_diff_class_lpirfs"
  ),
  status = c(
    unique(class_irf_results$status)[1],
    unique(class_fevd_results$status)[1],
    class_svar_status$status[1],
    class_lpirfs_status$status[1]
  ),
  message = c(
    unique(class_irf_results$message)[1],
    unique(class_fevd_results$message)[1],
    class_svar_status$message[1],
    class_lpirfs_status$message[1]
  ),
  stringsAsFactors = FALSE
)

#********************************************************
# 11. Exportacion
#********************************************************
# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  model_sample_definition,
  output_path("section_01_model_sample.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  variable_check,
  output_path("section_01_variable_check.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  system_sample_summary,
  output_path("section_01_system_samples.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  course_scripts_status,
  output_path("section_00_course_scripts_status.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  course_packages_status,
  output_path("section_00_course_packages_status.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  course_workflow_alignment,
  output_path("section_00_course_workflow_alignment.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  descriptive_stats,
  output_path("section_01_descriptive_statistics.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  seasonal_diff_summary,
  output_path("section_01_descriptive_seasonality_by_quarter.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  descriptive_interpretation,
  output_path("section_01_descriptive_interpretation_guide.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  adf_results,
  output_path("section_01_unit_root_adf_results.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  adf_order_summary,
  output_path("section_01_unit_root_adf_order_summary.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  adf_urca_results,
  output_path("section_02_unit_root_adf_urca_results.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  adf_urca_order_summary,
  output_path("section_02_unit_root_adf_urca_order_summary.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  course_adf_results,
  output_path("section_02_course_adf_ver3_results.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  var_lag_selection,
  output_path("section_03_var_lag_selection.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  var_lag_criteria,
  output_path("section_03_var_lag_criteria.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  var_lag_decision,
  output_path("section_03_var_lag_decision.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  var_lag_diagnostic_grid,
  output_path("section_03_var_lag_diagnostic_grid.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  var_lag_diagnostic_decision,
  output_path("section_03_var_lag_diagnostic_decision.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  var_lag_exclusion_wald_results,
  output_path("section_03_var_lag_exclusion_wald.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  johansen_trace_results,
  output_path("section_04_johansen_trace_results.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  johansen_eigen_results,
  output_path("section_04_johansen_eigen_results.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  johansen_rank_decision,
  output_path("section_04_johansen_rank_decision.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  course_vcorr_results,
  output_path("section_04_course_vcorr_residual_autocorrelation.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  course_white_no_cross_results,
  output_path("section_04_course_white_no_cross_heteroskedasticity.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  engle_granger_equations,
  output_path("section_05_engle_granger_equations.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  engle_granger_long_run_coefficients,
  output_path("section_05_engle_granger_long_run_coefficients.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  engle_granger_residual_tests,
  output_path("section_05_engle_granger_residual_tests.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  engle_granger_decision,
  output_path("section_05_engle_granger_decision.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  short_run_model_summary,
  output_path("section_05_ecm_short_run_model_summary.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  short_run_coefficients,
  output_path("section_05_ecm_short_run_coefficients.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  ecm_adjustment_summary,
  output_path("section_05_ecm_adjustment_summary.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  eg_ecm_elasticity_summary,
  output_path("section_05_eg_ecm_elasticity_summary.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  johansen_robustness_summary,
  output_path("section_06_johansen_robustness_summary.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  var_vec_treatment_decision,
  output_path("section_06_var_vec_treatment_decision.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  vecm_cointegration_vectors,
  output_path("section_06_vecm_cointegration_vectors.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  vecm_adjustment_coefficients,
  output_path("section_06_vecm_adjustment_coefficients.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  vecm_short_run_summary,
  output_path("section_06_vecm_short_run_summary.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  feedback_audit,
  output_path("section_06_feedback_audit.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  var_diff_model_summary,
  output_path("section_07_var_diff_model_summary.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  var_diff_exports_equation_coefficients,
  output_path("section_07_var_diff_exports_equation_coefficients.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  var_diff_diagnostics,
  output_path("section_07_var_diff_diagnostics.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  var_diff_stability_roots,
  output_path("section_07_var_diff_stability_roots.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  normality_stability_summary,
  output_path("section_08_normality_stability_summary.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  wickens_breusch_long_run_coefficients,
  output_path("section_09_wickens_breusch_long_run_coefficients.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  class_irf_results,
  output_path("section_10_class_irf_results.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  class_fevd_results,
  output_path("section_10_class_fevd_results.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  class_svar_status,
  output_path("section_10_class_svar_status.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  class_lpirfs_status,
  output_path("section_10_class_lpirfs_status.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  class_extensions_summary,
  output_path("section_10_class_extensions_summary.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se arma la tabla output_manifest con resultados de este paso.
output_manifest <- data.frame(
  file = c(
    "section_00_course_scripts_status.csv",
    "section_00_course_packages_status.csv",
    "section_00_course_workflow_alignment.csv",
    "section_01_outputs_manifest.csv",
    "section_01_model_sample.csv",
    "section_01_variable_check.csv",
    "section_01_system_samples.csv",
    "section_01_descriptive_statistics.csv",
    "section_01_descriptive_seasonality_by_quarter.csv",
    "section_01_descriptive_interpretation_guide.csv",
    "section_01_unit_root_adf_results.csv",
    "section_01_unit_root_adf_order_summary.csv",
    "section_02_unit_root_adf_urca_results.csv",
    "section_02_unit_root_adf_urca_order_summary.csv",
    "section_02_course_adf_ver3_results.csv",
    "section_03_var_lag_selection.csv",
    "section_03_var_lag_criteria.csv",
    "section_03_var_lag_decision.csv",
    "section_03_var_lag_diagnostic_grid.csv",
    "section_03_var_lag_diagnostic_decision.csv",
    "section_03_var_lag_exclusion_wald.csv",
    "section_04_johansen_trace_results.csv",
    "section_04_johansen_eigen_results.csv",
    "section_04_johansen_rank_decision.csv",
    "section_04_course_vcorr_residual_autocorrelation.csv",
    "section_04_course_white_no_cross_heteroskedasticity.csv",
    "section_05_engle_granger_equations.csv",
    "section_05_engle_granger_long_run_coefficients.csv",
    "section_05_engle_granger_residual_tests.csv",
    "section_05_engle_granger_decision.csv",
    "section_05_ecm_short_run_model_summary.csv",
    "section_05_ecm_short_run_coefficients.csv",
    "section_05_ecm_adjustment_summary.csv",
    "section_05_eg_ecm_elasticity_summary.csv",
    "section_06_johansen_robustness_summary.csv",
    "section_06_var_vec_treatment_decision.csv",
    "section_06_vecm_cointegration_vectors.csv",
    "section_06_vecm_adjustment_coefficients.csv",
    "section_06_vecm_short_run_summary.csv",
    "section_06_feedback_audit.csv",
    "section_07_var_diff_model_summary.csv",
    "section_07_var_diff_exports_equation_coefficients.csv",
    "section_07_var_diff_diagnostics.csv",
    "section_07_var_diff_stability_roots.csv",
    "section_08_normality_stability_summary.csv",
    "section_09_wickens_breusch_long_run_coefficients.csv",
    "section_10_class_irf_results.csv",
    "section_10_class_fevd_results.csv",
    "section_10_class_svar_status.csv",
    "section_10_class_lpirfs_status.csv",
    "section_10_class_extensions_summary.csv"
  ),
  description = c(
    "Estado de carga de scripts auxiliares provistos en clase",
    "Estado de disponibilidad de paquetes utilizados durante la cursada",
    "Mapa de alineacion entre arquitectura docente y bloques del script maestro",
    "Indice de salidas CSV generadas por el script maestro",
    "Definicion de muestra de trabajo",
    "Verificacion de variables requeridas",
    "Subconjuntos para sistemas de importaciones y exportaciones",
    "Estadisticas descriptivas de niveles logaritmicos y primeras diferencias",
    "Resumen por trimestre de primeras diferencias para revisar estacionalidad",
    "Guia de lectura para tendencia, estacionalidad, volatilidad y quiebres",
    "Resultados ADF en niveles y diferencias",
    "Resumen preliminar de orden de integracion",
    "Resultados ADF formales con urca::ur.df",
    "Resumen formal de orden de integracion con urca::ur.df",
    "Resultados ADF capturados desde Test.ADF.Ver.3.R",
    "Rezagos VAR sugeridos por AIC, HQ, SC/BIC y FPE",
    "Valores de criterios de informacion para rezagos VAR candidatos",
    "Decision base de rezagos VAR usando SC/BIC como criterio principal",
    "Grilla de autocorrelacion residual para retroalimentar rezagos VAR",
    "Decision final de rezagos VAR ajustada por diagnosticos residuales",
    "Tests Wald de exclusion conjunta por rezago en cada VAR; H0: bloque de rezago igual a cero",
    "Pruebas de cointegracion de Johansen por estadistico de traza",
    "Pruebas de cointegracion de Johansen por maximo autovalor",
    "Decision preliminar de rango de cointegracion por Johansen",
    "Diagnosticos de autocorrelacion residual VAR con vcorr_res.R",
    "Test White no-cross de heterocedasticidad residual VAR",
    "Ecuaciones de largo plazo Engle-Granger",
    "Coeficientes de largo plazo Engle-Granger",
    "Pruebas ADF sobre residuos Engle-Granger",
    "Decision operativa para ECM o primeras diferencias",
    "Resumen de modelos ECM o primeras diferencias",
    "Coeficientes de modelos ECM o primeras diferencias",
    "Velocidad de ajuste del ECM",
    "Resumen de elasticidades Engle-Granger y ECM para el informe",
    "Robustez de Johansen comparando ecdet const y none",
    "Decision operativa para VECM o VAR en diferencias",
    "Vectores de cointegracion normalizados del VECM de importaciones",
    "Coeficientes de ajuste del VECM de importaciones",
    "Coeficientes deterministas y de corto plazo del VECM de importaciones",
    "Auditoria de retroalimentacion entre diagnosticos y bloques posteriores",
    "Resumen del VAR en diferencias de exportaciones como especificacion operativa",
    "Coeficientes de la ecuacion de exportaciones del VAR en diferencias",
    "Diagnosticos del VAR en diferencias: autocorrelacion, ARCH, normalidad y estabilidad",
    "Raices de estabilidad del VAR en diferencias de exportaciones",
    "Resumen final de normalidad multivariada y estabilidad por modelo",
    "Contraste de elasticidades de largo plazo Engle-Granger y Wickens-Breusch",
    "Funciones impulso-respuesta acumuladas calculadas con vars::irf",
    "Descomposicion de varianza calculada con vars::fevd",
    "Estado del ejercicio SVAR calculado con vars::SVAR y svars disponible",
    "Estado del ejercicio de proyecciones locales calculado con lpirfs::lp_lin",
    "Resumen de extensiones de clase IRF, FEVD, SVAR y LPIRF"
  ),
  stringsAsFactors = FALSE
)
output_manifest$file <- vapply(
  output_manifest$file,
  output_relative_path,
  character(1)
)

# se exporta esta salida a CSV para documentar los resultados.
write.csv(
  output_manifest,
  output_path("section_01_outputs_manifest.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# se informa en consola el estado final de la corrida.
message("Fase base TP3 completada.")
message("Muestra: ", model_sample_definition$model_sample_start, " - ",
        model_sample_definition$model_sample_end)
message("Salidas CSV escritas en: ", output_dir)
