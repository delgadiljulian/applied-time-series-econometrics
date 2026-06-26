
# ============================================================
# TP3 - SERIES DE TIEMPO
# SCRIPT 02
#
# ITCRM DIARIO Y PONDERADORES DEL BCRA
#
# Salidas:
#   data/processed/arg_itcrm_trimestral_2004_2025.rds
#   data/processed/ponderadores_bcra_2004_2025.rds
#   data/processed/resumen_ponderadores_pib_socios.rds
#   data/processed/itcrm_y_ponderadores_bcra.xlsx
#   figures/itcrm_trimestral.png
#
# Este script es autónomo y no depende de objetos creados
# por otros scripts.
# ============================================================


# ------------------------------------------------------------
# 0. PAQUETES
# ------------------------------------------------------------

paquetes <- c(
  "tidyverse",
  "readxl",
  "janitor",
  "here",
  "lubridate",
  "zoo",
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


suppressPackageStartupMessages({
  
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(here)
  library(lubridate)
  library(zoo)
  library(writexl)
  
})


# ------------------------------------------------------------
# 1. CREAR CARPETAS NECESARIAS
# ------------------------------------------------------------

dir.create(
  here::here(
    "data",
    "processed"
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
# 2. RUTA DEL ARCHIVO DEL BCRA
# ------------------------------------------------------------

ruta_itcrm <- here::here(
  "data",
  "raw",
  "arg_itcrm_daily.xlsx"
)


if (!file.exists(ruta_itcrm)) {
  
  stop(
    paste0(
      "No se encontró el archivo:\n",
      ruta_itcrm
    ),
    call. = FALSE
  )
}


hojas_disponibles <- readxl::excel_sheets(
  ruta_itcrm
)


hojas_requeridas <- c(
  "ITCRM y bilaterales",
  "Ponderadores"
)


hojas_faltantes <- setdiff(
  hojas_requeridas,
  hojas_disponibles
)


if (length(hojas_faltantes) > 0) {
  
  stop(
    paste0(
      "Faltan hojas requeridas en el archivo Excel:\n",
      paste(
        hojas_faltantes,
        collapse = ", "
      ),
      "\n\nHojas disponibles:\n",
      paste(
        hojas_disponibles,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


cat(
  "\n============================================================",
  "\nSCRIPT 02: ITCRM Y PONDERADORES BCRA",
  "\n============================================================",
  "\nArchivo de entrada:",
  ruta_itcrm,
  "\nHojas disponibles:",
  paste(
    hojas_disponibles,
    collapse = " | "
  ),
  "\n============================================================\n"
)


# ============================================================
# 3. CARGA DEL ITCRM DIARIO
# ============================================================

itcrm_raw <- readxl::read_excel(
  path = ruta_itcrm,
  sheet = "ITCRM y bilaterales",
  skip = 1
) |>
  janitor::clean_names()


columnas_itcrm_requeridas <- c(
  "periodo",
  "itcrm"
)


columnas_itcrm_faltantes <- setdiff(
  columnas_itcrm_requeridas,
  names(itcrm_raw)
)


if (length(columnas_itcrm_faltantes) > 0) {
  
  stop(
    paste0(
      "Faltan columnas en la hoja 'ITCRM y bilaterales':\n",
      paste(
        columnas_itcrm_faltantes,
        collapse = ", "
      ),
      "\n\nColumnas disponibles:\n",
      paste(
        names(itcrm_raw),
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


cat(
  "\nColumnas de la hoja ITCRM:\n",
  paste(
    names(itcrm_raw),
    collapse = " | "
  ),
  "\n"
)


# ------------------------------------------------------------
# 4. DEPURAR LA SERIE DIARIA
# ------------------------------------------------------------

itcrm_diario <- itcrm_raw |>
  dplyr::transmute(
    
    fecha =
      as.Date(
        .data$periodo
      ),
    
    itcrm =
      suppressWarnings(
        as.numeric(
          .data$itcrm
        )
      )
  ) |>
  dplyr::filter(
    !is.na(.data$fecha),
    !is.na(.data$itcrm)
  ) |>
  dplyr::arrange(
    .data$fecha
  )


if (nrow(itcrm_diario) == 0) {
  
  stop(
    "La serie diaria del ITCRM quedó vacía después de la depuración.",
    call. = FALSE
  )
}


duplicados_itcrm <- itcrm_diario |>
  dplyr::count(
    .data$fecha,
    name = "n"
  ) |>
  dplyr::filter(
    .data$n > 1
  )


valores_no_positivos <- itcrm_diario |>
  dplyr::filter(
    .data$itcrm <= 0
  )


if (nrow(duplicados_itcrm) > 0) {
  
  stop(
    paste0(
      "Se encontraron fechas duplicadas en el ITCRM diario: ",
      nrow(duplicados_itcrm),
      "."
    ),
    call. = FALSE
  )
}


if (nrow(valores_no_positivos) > 0) {
  
  stop(
    paste0(
      "Se encontraron valores de ITCRM menores o iguales a cero: ",
      nrow(valores_no_positivos),
      "."
    ),
    call. = FALSE
  )
}


cat(
  "\nCobertura diaria original:",
  format(
    min(itcrm_diario$fecha),
    "%Y-%m-%d"
  ),
  "a",
  format(
    max(itcrm_diario$fecha),
    "%Y-%m-%d"
  ),
  "\nObservaciones diarias:",
  nrow(itcrm_diario),
  "\n"
)


# ------------------------------------------------------------
# 5. LIMITAR AL PERÍODO 2004-2025
# ------------------------------------------------------------

itcrm_diario_2004_2025 <- itcrm_diario |>
  dplyr::filter(
    .data$fecha >= as.Date("2004-01-01"),
    .data$fecha <= as.Date("2025-12-31")
  ) |>
  dplyr::mutate(
    
    anio =
      lubridate::year(
        .data$fecha
      ),
    
    trimestre =
      lubridate::quarter(
        .data$fecha
      ),
    
    periodo =
      zoo::as.yearqtr(
        .data$fecha
      )
  )


if (nrow(itcrm_diario_2004_2025) == 0) {
  
  stop(
    "No existen observaciones del ITCRM entre 2004 y 2025.",
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 6. CONVERTIR EL ITCRM A FRECUENCIA TRIMESTRAL
# ------------------------------------------------------------

itcrm_trimestral <- itcrm_diario_2004_2025 |>
  dplyr::group_by(
    .data$periodo,
    .data$anio,
    .data$trimestre
  ) |>
  dplyr::summarise(
    
    itcrm =
      mean(
        .data$itcrm,
        na.rm = TRUE
      ),
    
    observaciones_diarias =
      dplyr::n(),
    
    fecha_inicial =
      min(
        .data$fecha,
        na.rm = TRUE
      ),
    
    fecha_final =
      max(
        .data$fecha,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  ) |>
  dplyr::arrange(
    .data$periodo
  ) |>
  dplyr::mutate(
    
    fecha =
      as.Date(
        .data$periodo
      ),
    
    periodo_label =
      format(
        .data$periodo,
        "%Y Q%q"
      ),
    
    ln_itcrm =
      log(
        .data$itcrm
      )
  ) |>
  dplyr::select(
    .data$fecha,
    .data$periodo,
    .data$periodo_label,
    .data$anio,
    .data$trimestre,
    .data$itcrm,
    .data$ln_itcrm,
    .data$observaciones_diarias,
    .data$fecha_inicial,
    .data$fecha_final
  )


if (interactive()) {
  
  View(
    itcrm_trimestral
  )
}


cat(
  "\nResumen del ITCRM trimestral:\n"
)


print(
  summary(
    itcrm_trimestral |>
      dplyr::select(
        .data$itcrm,
        .data$ln_itcrm,
        .data$observaciones_diarias
      )
  )
)


# ------------------------------------------------------------
# 7. GRÁFICO DEL ITCRM TRIMESTRAL
# ------------------------------------------------------------

grafico_itcrm <- ggplot2::ggplot(
  
  data =
    itcrm_trimestral,
  
  mapping =
    ggplot2::aes(
      x = .data$fecha,
      y = .data$itcrm
    )
) +
  ggplot2::geom_line(
    linewidth = 0.7
  ) +
  ggplot2::labs(
    
    title =
      "Tipo de cambio real multilateral de Argentina",
    
    subtitle =
      "Promedio trimestral del índice diario",
    
    x =
      NULL,
    
    y =
      "ITCRM",
    
    caption =
      "Fuente: BCRA"
  ) +
  ggplot2::theme_minimal(
    base_size = 11
  )


print(
  grafico_itcrm
)


ggplot2::ggsave(
  
  filename = here::here(
    "outputs",
    "figures",
    "itcrm_trimestral.png"
  ),
  
  plot =
    grafico_itcrm,
  
  width =
    9,
  
  height =
    5,
  
  dpi =
    300
)


# ============================================================
# 8. PONDERADORES DEL ITCRM
# ============================================================

ponderadores_raw <- readxl::read_excel(
  path = ruta_itcrm,
  sheet = "Ponderadores",
  skip = 1
) |>
  janitor::clean_names()


columnas_ponderadores_requeridas <- c(
  "periodo",
  "brasil",
  "estados_unidos"
)


columnas_ponderadores_faltantes <- setdiff(
  columnas_ponderadores_requeridas,
  names(ponderadores_raw)
)


if (length(columnas_ponderadores_faltantes) > 0) {
  
  stop(
    paste0(
      "Faltan columnas en la hoja 'Ponderadores':\n",
      paste(
        columnas_ponderadores_faltantes,
        collapse = ", "
      ),
      "\n\nColumnas disponibles:\n",
      paste(
        names(ponderadores_raw),
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


cat(
  "\nColumnas de la hoja Ponderadores:\n",
  paste(
    names(ponderadores_raw),
    collapse = " | "
  ),
  "\n"
)


# ------------------------------------------------------------
# 9. DEPURAR PONDERADORES MENSUALES
# ------------------------------------------------------------

ponderadores_mensuales <- ponderadores_raw |>
  dplyr::transmute(
    
    fecha =
      as.Date(
        .data$periodo
      ),
    
    ponderador_brasil =
      suppressWarnings(
        as.numeric(
          .data$brasil
        )
      ),
    
    ponderador_usa =
      suppressWarnings(
        as.numeric(
          .data$estados_unidos
        )
      )
  ) |>
  dplyr::filter(
    !is.na(.data$fecha),
    !is.na(.data$ponderador_brasil),
    !is.na(.data$ponderador_usa),
    .data$fecha >= as.Date("2004-01-01"),
    .data$fecha <= as.Date("2025-12-31")
  ) |>
  dplyr::arrange(
    .data$fecha
  ) |>
  dplyr::mutate(
    
    anio =
      lubridate::year(
        .data$fecha
      ),
    
    mes =
      lubridate::month(
        .data$fecha
      ),
    
    periodo_mensual =
      format(
        .data$fecha,
        "%Y-%m"
      )
  )


if (nrow(ponderadores_mensuales) == 0) {
  
  stop(
    "La tabla de ponderadores mensuales quedó vacía.",
    call. = FALSE
  )
}


duplicados_ponderadores <- ponderadores_mensuales |>
  dplyr::count(
    .data$fecha,
    name = "n"
  ) |>
  dplyr::filter(
    .data$n > 1
  )


if (nrow(duplicados_ponderadores) > 0) {
  
  stop(
    paste0(
      "Se encontraron fechas duplicadas en los ponderadores: ",
      nrow(duplicados_ponderadores),
      "."
    ),
    call. = FALSE
  )
}


cat(
  "\nCobertura de los ponderadores:",
  format(
    min(ponderadores_mensuales$fecha),
    "%Y-%m-%d"
  ),
  "a",
  format(
    max(ponderadores_mensuales$fecha),
    "%Y-%m-%d"
  ),
  "\nObservaciones mensuales:",
  nrow(ponderadores_mensuales),
  "\n"
)


# ------------------------------------------------------------
# 10. PONDERADORES PROMEDIO 2004-2025
# ------------------------------------------------------------

ponderadores_promedio_2004_2025 <- ponderadores_mensuales |>
  dplyr::summarise(
    
    ponderador_brasil_original =
      mean(
        .data$ponderador_brasil,
        na.rm = TRUE
      ),
    
    ponderador_usa_original =
      mean(
        .data$ponderador_usa,
        na.rm = TRUE
      )
  ) |>
  dplyr::mutate(
    
    suma_ponderadores =
      .data$ponderador_brasil_original +
      .data$ponderador_usa_original,
    
    ponderador_brasil_normalizado =
      .data$ponderador_brasil_original /
      .data$suma_ponderadores,
    
    ponderador_usa_normalizado =
      .data$ponderador_usa_original /
      .data$suma_ponderadores,
    
    periodo_calculo =
      "2004-01 a 2025-12"
  )


# ------------------------------------------------------------
# 11. PONDERADORES PROMEDIO 2004-2022
# ------------------------------------------------------------

ponderadores_promedio_2004_2022 <- ponderadores_mensuales |>
  dplyr::filter(
    .data$fecha <= as.Date("2022-12-31")
  ) |>
  dplyr::summarise(
    
    ponderador_brasil_original =
      mean(
        .data$ponderador_brasil,
        na.rm = TRUE
      ),
    
    ponderador_usa_original =
      mean(
        .data$ponderador_usa,
        na.rm = TRUE
      )
  ) |>
  dplyr::mutate(
    
    suma_ponderadores =
      .data$ponderador_brasil_original +
      .data$ponderador_usa_original,
    
    ponderador_brasil_normalizado =
      .data$ponderador_brasil_original /
      .data$suma_ponderadores,
    
    ponderador_usa_normalizado =
      .data$ponderador_usa_original /
      .data$suma_ponderadores,
    
    periodo_calculo =
      "2004-01 a 2022-12"
  )


# ------------------------------------------------------------
# 12. TABLA COMPARATIVA DE PONDERADORES
# ------------------------------------------------------------

tabla_ponderadores <- dplyr::bind_rows(
  ponderadores_promedio_2004_2022,
  ponderadores_promedio_2004_2025
) |>
  dplyr::select(
    .data$periodo_calculo,
    .data$ponderador_brasil_original,
    .data$ponderador_usa_original,
    .data$ponderador_brasil_normalizado,
    .data$ponderador_usa_normalizado
  )


cat(
  "\nTabla de ponderadores normalizados:\n"
)


print(
  tabla_ponderadores,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------
# 13. GUARDAR ARCHIVOS RDS
# ------------------------------------------------------------

saveRDS(
  
  itcrm_trimestral,
  
  here::here(
    "data",
    "processed",
    "arg_itcrm_trimestral_2004_2025.rds"
  )
)


saveRDS(
  
  ponderadores_mensuales,
  
  here::here(
    "data",
    "processed",
    "ponderadores_bcra_2004_2025.rds"
  )
)


saveRDS(
  
  tabla_ponderadores,
  
  here::here(
    "data",
    "processed",
    "resumen_ponderadores_pib_socios.rds"
  )
)


# ------------------------------------------------------------
# 14. GUARDAR LIBRO EXCEL
# ------------------------------------------------------------

itcrm_exportar <- itcrm_trimestral |>
  dplyr::select(
    -.data$periodo
  )


writexl::write_xlsx(
  
  x = list(
    
    itcrm_trimestral =
      itcrm_exportar,
    
    ponderadores_mensuales =
      ponderadores_mensuales,
    
    resumen_ponderadores =
      tabla_ponderadores
  ),
  
  path = here::here(
    "data",
    "processed",
    "itcrm_y_ponderadores_bcra.xlsx"
  )
)


# ------------------------------------------------------------
# 15. CONTROLES FINALES
# ------------------------------------------------------------

suma_normalizada_2004_2025 <-
  ponderadores_promedio_2004_2025$
  ponderador_brasil_normalizado +
  ponderadores_promedio_2004_2025$
  ponderador_usa_normalizado


stopifnot(
  
  nrow(
    itcrm_trimestral
  ) ==
    88,
  
  !anyDuplicated(
    itcrm_trimestral$periodo
  ),
  
  all(
    itcrm_trimestral$itcrm >
      0
  ),
  
  all(
    is.finite(
      itcrm_trimestral$ln_itcrm
    )
  ),
  
  !anyDuplicated(
    ponderadores_mensuales$fecha
  ),
  
  abs(
    suma_normalizada_2004_2025 -
      1
  ) <
    1e-10,
  
  file.exists(
    here::here(
      "data",
      "processed",
      "arg_itcrm_trimestral_2004_2025.rds"
    )
  ),
  
  file.exists(
    here::here(
      "data",
      "processed",
      "ponderadores_bcra_2004_2025.rds"
    )
  ),
  
  file.exists(
    here::here(
      "data",
      "processed",
      "resumen_ponderadores_pib_socios.rds"
    )
  ),
  
  file.exists(
    here::here(
      "data",
      "processed",
      "itcrm_y_ponderadores_bcra.xlsx"
    )
  ),
  
  file.exists(
    here::here(
      "outputs",
      "figures",
      "itcrm_trimestral.png"
    )
  )
)


# ------------------------------------------------------------
# 16. MENSAJE FINAL
# ------------------------------------------------------------

cat(
  "\n============================================================",
  "\nSCRIPT 02 FINALIZADO CORRECTAMENTE",
  "\n============================================================",
  
  "\nTrimestres:",
  nrow(
    itcrm_trimestral
  ),
  
  "\nPeríodo inicial:",
  format(
    min(
      itcrm_trimestral$periodo
    ),
    "%Y Q%q"
  ),
  
  "\nPeríodo final:",
  format(
    max(
      itcrm_trimestral$periodo
    ),
    "%Y Q%q"
  ),
  
  "\nPonderador Brasil 2004-2025:",
  round(
    ponderadores_promedio_2004_2025$
      ponderador_brasil_normalizado,
    4
  ),
  
  "\nPonderador Estados Unidos 2004-2025:",
  round(
    ponderadores_promedio_2004_2025$
      ponderador_usa_normalizado,
    4
  ),
  
  "\n\nArchivos generados:",
  
  "\n- data/processed/arg_itcrm_trimestral_2004_2025.rds",
  
  "\n- data/processed/ponderadores_bcra_2004_2025.rds",
  
  "\n- data/processed/resumen_ponderadores_pib_socios.rds",
  
  "\n- data/processed/itcrm_y_ponderadores_bcra.xlsx",
  
  "\n- figures/itcrm_trimestral.png",
  
  "\n============================================================\n"
)

