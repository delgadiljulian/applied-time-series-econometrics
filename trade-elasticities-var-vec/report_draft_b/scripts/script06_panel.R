# ============================================================
# TP3 - SERIES DE TIEMPO
# Script 06: Construcción del panel maestro trimestral
# ============================================================


# ------------------------------------------------------------
# 0. LIMPIEZA DEL ENTORNO
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

library(tidyverse)
library(here)
library(zoo)
library(writexl)


# ------------------------------------------------------------
# 2. CREAR CARPETAS DE SALIDA
# ------------------------------------------------------------

dir.create(
  here::here("data", "processed"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here::here("outputs", "tables"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here::here("figures"),
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# 3. DEFINIR RUTAS DE LAS BASES PROCESADAS
# ------------------------------------------------------------

ruta_argentina <- here::here(
  "data",
  "processed",
  "argentina_quarterly_data_2004_2025.rds"
)

ruta_itcrm <- here::here(
  "data",
  "processed",
  "arg_itcrm_trimestral_2004_2025.rds"
)

ruta_pib_socios <- here::here(
  "data",
  "processed",
  "pib_socios_comerciales_2004_2025.rds"
)

ruta_commodities <- here::here(
  "data",
  "processed",
  "world_bank_commodity_index_quarterly_2004_2025.rds"
)


# ------------------------------------------------------------
# 4. VERIFICAR EXISTENCIA DE ARCHIVOS
# ------------------------------------------------------------

rutas_requeridas <- c(
  ruta_argentina,
  ruta_itcrm,
  ruta_pib_socios,
  ruta_commodities
)

archivos_faltantes <- rutas_requeridas[
  !file.exists(rutas_requeridas)
]

if (length(archivos_faltantes) > 0) {
  stop(
    paste0(
      "No se encontraron los siguientes archivos:\n",
      paste(
        archivos_faltantes,
        collapse = "\n"
      )
    )
  )
}


# ------------------------------------------------------------
# 5. CARGAR BASES
# ------------------------------------------------------------

datos_argentina <- readRDS(
  ruta_argentina
)

itcrm_trimestral <- readRDS(
  ruta_itcrm
)

pib_socios <- readRDS(
  ruta_pib_socios
)

commodities_trimestral <- readRDS(
  ruta_commodities
)


# ------------------------------------------------------------
# 6. REVISAR ESTRUCTURAS
# ------------------------------------------------------------

cat(
  "\nVariables de datos_argentina:\n"
)

print(
  names(datos_argentina)
)

cat(
  "\nVariables de itcrm_trimestral:\n"
)

print(
  names(itcrm_trimestral)
)

cat(
  "\nVariables de pib_socios:\n"
)

print(
  names(pib_socios)
)

cat(
  "\nVariables de commodities_trimestral:\n"
)

print(
  names(commodities_trimestral)
)


# ------------------------------------------------------------
# 7. VERIFICAR VARIABLES REQUERIDAS
# ------------------------------------------------------------

variables_argentina <- c(
  "periodo",
  "anio",
  "trimestre",
  "pib_real",
  "importaciones_reales",
  "exportaciones_reales"
)

variables_itcrm <- c(
  "periodo",
  "itcrm"
)

variables_socios <- c(
  "periodo",
  "pib_socios_indice"
)

variables_commodities <- c(
  "periodo",
  "commodity_price_index"
)

faltantes_argentina <- setdiff(
  variables_argentina,
  names(datos_argentina)
)

faltantes_itcrm <- setdiff(
  variables_itcrm,
  names(itcrm_trimestral)
)

faltantes_socios <- setdiff(
  variables_socios,
  names(pib_socios)
)

faltantes_commodities <- setdiff(
  variables_commodities,
  names(commodities_trimestral)
)

if (length(faltantes_argentina) > 0) {
  stop(
    paste(
      "Faltan variables en datos_argentina:",
      paste(
        faltantes_argentina,
        collapse = ", "
      )
    )
  )
}

if (length(faltantes_itcrm) > 0) {
  stop(
    paste(
      "Faltan variables en itcrm_trimestral:",
      paste(
        faltantes_itcrm,
        collapse = ", "
      )
    )
  )
}

if (length(faltantes_socios) > 0) {
  stop(
    paste(
      "Faltan variables en pib_socios:",
      paste(
        faltantes_socios,
        collapse = ", "
      )
    )
  )
}

if (length(faltantes_commodities) > 0) {
  stop(
    paste(
      "Faltan variables en commodities_trimestral:",
      paste(
        faltantes_commodities,
        collapse = ", "
      )
    )
  )
}


# ------------------------------------------------------------
# 8. NORMALIZAR LA CLASE DEL PERÍODO
# ------------------------------------------------------------

datos_argentina <- datos_argentina |>
  mutate(
    periodo = zoo::as.yearqtr(periodo)
  ) |>
  arrange(periodo)

itcrm_trimestral <- itcrm_trimestral |>
  mutate(
    periodo = zoo::as.yearqtr(periodo)
  ) |>
  arrange(periodo)

pib_socios <- pib_socios |>
  mutate(
    periodo = zoo::as.yearqtr(periodo)
  ) |>
  arrange(periodo)

commodities_trimestral <- commodities_trimestral |>
  mutate(
    periodo = zoo::as.yearqtr(periodo)
  ) |>
  arrange(periodo)


# ------------------------------------------------------------
# 9. VERIFICAR DUPLICADOS EN CADA BASE
# ------------------------------------------------------------

if (anyDuplicated(datos_argentina$periodo) > 0) {
  stop(
    "Existen períodos duplicados en datos_argentina."
  )
}

if (anyDuplicated(itcrm_trimestral$periodo) > 0) {
  stop(
    "Existen períodos duplicados en itcrm_trimestral."
  )
}

if (anyDuplicated(pib_socios$periodo) > 0) {
  stop(
    "Existen períodos duplicados en pib_socios."
  )
}

if (
  anyDuplicated(
    commodities_trimestral$periodo
  ) > 0
) {
  stop(
    "Existen períodos duplicados en commodities_trimestral."
  )
}


# ------------------------------------------------------------
# 10. COMPARAR COBERTURA TEMPORAL
# ------------------------------------------------------------

resumen_cobertura <- tibble(
  fuente = c(
    "Argentina",
    "ITCRM",
    "PIB socios",
    "Commodities"
  ),
  
  periodo_inicial = c(
    format(
      min(datos_argentina$periodo),
      "%Y Q%q"
    ),
    
    format(
      min(itcrm_trimestral$periodo),
      "%Y Q%q"
    ),
    
    format(
      min(pib_socios$periodo),
      "%Y Q%q"
    ),
    
    format(
      min(commodities_trimestral$periodo),
      "%Y Q%q"
    )
  ),
  
  periodo_final = c(
    format(
      max(datos_argentina$periodo),
      "%Y Q%q"
    ),
    
    format(
      max(itcrm_trimestral$periodo),
      "%Y Q%q"
    ),
    
    format(
      max(pib_socios$periodo),
      "%Y Q%q"
    ),
    
    format(
      max(commodities_trimestral$periodo),
      "%Y Q%q"
    )
  ),
  
  observaciones = c(
    nrow(datos_argentina),
    nrow(itcrm_trimestral),
    nrow(pib_socios),
    nrow(commodities_trimestral)
  )
)

print(
  resumen_cobertura
)


# ------------------------------------------------------------
# 11. VERIFICAR PERÍODOS QUE NO COINCIDEN
# ------------------------------------------------------------

periodos_argentina <- datos_argentina |>
  select(periodo)

periodos_itcrm <- itcrm_trimestral |>
  select(periodo)

periodos_socios <- pib_socios |>
  select(periodo)

periodos_commodities <- commodities_trimestral |>
  select(periodo)

faltan_en_itcrm <- anti_join(
  periodos_argentina,
  periodos_itcrm,
  by = "periodo"
)

faltan_en_socios <- anti_join(
  periodos_argentina,
  periodos_socios,
  by = "periodo"
)

faltan_en_commodities <- anti_join(
  periodos_argentina,
  periodos_commodities,
  by = "periodo"
)

cat(
  "\nPeríodos de Argentina ausentes en ITCRM:",
  nrow(faltan_en_itcrm),
  
  "\nPeríodos de Argentina ausentes en PIB socios:",
  nrow(faltan_en_socios),
  
  "\nPeríodos de Argentina ausentes en commodities:",
  nrow(faltan_en_commodities),
  
  "\n"
)


# ------------------------------------------------------------
# 12. SELECCIONAR VARIABLES NECESARIAS
# ------------------------------------------------------------

argentina_panel <- datos_argentina |>
  select(
    periodo,
    anio,
    trimestre,
    pib_real,
    importaciones_reales,
    exportaciones_reales
  )

itcrm_panel <- itcrm_trimestral |>
  select(
    periodo,
    itcrm
  )

socios_panel <- pib_socios |>
  select(
    periodo,
    pib_socios_indice
  )

commodities_panel <- commodities_trimestral |>
  select(
    periodo,
    commodity_price_index
  )


# ------------------------------------------------------------
# 13. CONSTRUIR PANEL MAESTRO
# ------------------------------------------------------------

panel_maestro <- argentina_panel |>
  full_join(
    itcrm_panel,
    by = "periodo"
  ) |>
  full_join(
    socios_panel,
    by = "periodo"
  ) |>
  full_join(
    commodities_panel,
    by = "periodo"
  ) |>
  arrange(periodo)


# ------------------------------------------------------------
# 14. RECONSTRUIR VARIABLES TEMPORALES
# ------------------------------------------------------------

panel_maestro <- panel_maestro |>
  mutate(
    anio = if_else(
      is.na(anio),
      as.integer(
        format(
          periodo,
          "%Y"
        )
      ),
      as.integer(anio)
    ),
    
    trimestre = if_else(
      is.na(trimestre),
      as.integer(
        cycle(
          ts(
            seq_len(n()),
            frequency = 4
          )
        )
      ),
      as.integer(trimestre)
    ),
    
    fecha = as.Date(periodo),
    
    periodo_label = format(
      periodo,
      "%Y Q%q"
    )
  )


# ------------------------------------------------------------
# 15. ORDENAR Y REVISAR VARIABLES EN NIVELES
# ------------------------------------------------------------

panel_maestro <- panel_maestro |>
  select(
    fecha,
    periodo,
    periodo_label,
    anio,
    trimestre,
    pib_real,
    importaciones_reales,
    exportaciones_reales,
    itcrm,
    pib_socios_indice,
    commodity_price_index
  ) |>
  arrange(periodo)


# ------------------------------------------------------------
# 16. VERIFICAR VALORES FALTANTES
# ------------------------------------------------------------

tabla_faltantes <- panel_maestro |>
  summarise(
    across(
      everything(),
      ~ sum(is.na(.x))
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "faltantes"
  )

print(
  tabla_faltantes
)


# ------------------------------------------------------------
# 17. VERIFICAR VALORES NO POSITIVOS
# ------------------------------------------------------------

valores_no_positivos <- panel_maestro |>
  filter(
    pib_real <= 0 |
      importaciones_reales <= 0 |
      exportaciones_reales <= 0 |
      itcrm <= 0 |
      pib_socios_indice <= 0 |
      commodity_price_index <= 0
  )

if (nrow(valores_no_positivos) > 0) {
  print(
    valores_no_positivos
  )
  
  stop(
    "Existen valores no positivos en variables que serán transformadas a logaritmos."
  )
}


# ------------------------------------------------------------
# 18. CREAR LOGARITMOS NATURALES
# ------------------------------------------------------------

panel_maestro <- panel_maestro |>
  mutate(
    ln_pib_real =
      log(pib_real),
    
    ln_importaciones_reales =
      log(importaciones_reales),
    
    ln_exportaciones_reales =
      log(exportaciones_reales),
    
    ln_itcrm =
      log(itcrm),
    
    ln_pib_socios =
      log(pib_socios_indice),
    
    ln_commodity_price_index =
      log(commodity_price_index)
  )


# ------------------------------------------------------------
# 19. CREAR PRIMERAS DIFERENCIAS LOGARÍTMICAS
# ------------------------------------------------------------

panel_maestro <- panel_maestro |>
  arrange(periodo) |>
  mutate(
    d_ln_pib_real =
      ln_pib_real -
      lag(ln_pib_real),
    
    d_ln_importaciones_reales =
      ln_importaciones_reales -
      lag(ln_importaciones_reales),
    
    d_ln_exportaciones_reales =
      ln_exportaciones_reales -
      lag(ln_exportaciones_reales),
    
    d_ln_itcrm =
      ln_itcrm -
      lag(ln_itcrm),
    
    d_ln_pib_socios =
      ln_pib_socios -
      lag(ln_pib_socios),
    
    d_ln_commodity_price_index =
      ln_commodity_price_index -
      lag(ln_commodity_price_index)
  )


# ------------------------------------------------------------
# 20. CREAR VARIACIONES PORCENTUALES APROXIMADAS
# ------------------------------------------------------------

panel_maestro <- panel_maestro |>
  mutate(
    crecimiento_pib_aprox =
      100 * d_ln_pib_real,
    
    crecimiento_importaciones_aprox =
      100 * d_ln_importaciones_reales,
    
    crecimiento_exportaciones_aprox =
      100 * d_ln_exportaciones_reales,
    
    variacion_itcrm_aprox =
      100 * d_ln_itcrm,
    
    crecimiento_pib_socios_aprox =
      100 * d_ln_pib_socios,
    
    variacion_commodities_aprox =
      100 * d_ln_commodity_price_index
  )


# ------------------------------------------------------------
# 21. CREAR VARIABLES ESTACIONALES
# ------------------------------------------------------------

panel_maestro <- panel_maestro |>
  mutate(
    trimestre_factor = factor(
      trimestre,
      levels = 1:4,
      labels = c(
        "Q1",
        "Q2",
        "Q3",
        "Q4"
      )
    ),
    
    dummy_q2 =
      as.integer(trimestre == 2),
    
    dummy_q3 =
      as.integer(trimestre == 3),
    
    dummy_q4 =
      as.integer(trimestre == 4)
  )


# ------------------------------------------------------------
# 22. CREAR DUMMIES DE EPISODIOS MACROECONÓMICOS
# ------------------------------------------------------------

panel_maestro <- panel_maestro |>
  mutate(
    dummy_crisis_global =
      as.integer(
        periodo >= zoo::as.yearqtr(
          "2008 Q3",
          format = "%Y Q%q"
        ) &
          periodo <= zoo::as.yearqtr(
            "2009 Q2",
            format = "%Y Q%q"
          )
      ),
    
    dummy_crisis_2018 =
      as.integer(
        periodo >= zoo::as.yearqtr(
          "2018 Q2",
          format = "%Y Q%q"
        ) &
          periodo <= zoo::as.yearqtr(
            "2019 Q4",
            format = "%Y Q%q"
          )
      ),
    
    dummy_covid =
      as.integer(
        periodo >= zoo::as.yearqtr(
          "2020 Q1",
          format = "%Y Q%q"
        ) &
          periodo <= zoo::as.yearqtr(
            "2020 Q4",
            format = "%Y Q%q"
          )
      )
  )


# ------------------------------------------------------------
# 23. CREAR BASE SIN LA PRIMERA OBSERVACIÓN
#     PARA MODELOS EN DIFERENCIAS
# ------------------------------------------------------------

panel_diferencias <- panel_maestro |>
  filter(
    !is.na(d_ln_pib_real),
    !is.na(d_ln_importaciones_reales),
    !is.na(d_ln_exportaciones_reales),
    !is.na(d_ln_itcrm),
    !is.na(d_ln_pib_socios),
    !is.na(d_ln_commodity_price_index)
  )


# ------------------------------------------------------------
# 24. VERIFICAR CONTINUIDAD TEMPORAL
# ------------------------------------------------------------

calendario_esperado <- tibble(
  periodo = seq(
    from = min(panel_maestro$periodo),
    to = max(panel_maestro$periodo),
    by = 0.25
  )
)

periodos_faltantes_panel <- anti_join(
  calendario_esperado,
  panel_maestro |>
    select(periodo),
  by = "periodo"
)

if (nrow(periodos_faltantes_panel) > 0) {
  print(
    periodos_faltantes_panel
  )
  
  stop(
    "El panel presenta trimestres faltantes."
  )
}


# ------------------------------------------------------------
# 25. CONTROLES AUTOMÁTICOS
# ------------------------------------------------------------

variables_niveles <- c(
  "pib_real",
  "importaciones_reales",
  "exportaciones_reales",
  "itcrm",
  "pib_socios_indice",
  "commodity_price_index"
)

variables_logs <- c(
  "ln_pib_real",
  "ln_importaciones_reales",
  "ln_exportaciones_reales",
  "ln_itcrm",
  "ln_pib_socios",
  "ln_commodity_price_index"
)

stopifnot(
  !anyDuplicated(
    panel_maestro$periodo
  ),
  
  !any(
    is.na(
      panel_maestro[
        variables_niveles
      ]
    )
  ),
  
  !any(
    is.na(
      panel_maestro[
        variables_logs
      ]
    )
  ),
  
  all(
    unlist(
      panel_maestro[
        variables_niveles
      ]
    ) > 0
  ),
  
  all(
    is.finite(
      unlist(
        panel_maestro[
          variables_logs
        ]
      )
    )
  ),
  
  nrow(panel_diferencias) ==
    nrow(panel_maestro) - 1
)


# ------------------------------------------------------------
# 26. CREAR OBJETOS DE SERIES DE TIEMPO
# ------------------------------------------------------------

anio_inicio <- min(
  panel_maestro$anio
)

trimestre_inicio <- panel_maestro |>
  slice(1) |>
  pull(trimestre)

ts_niveles <- ts(
  panel_maestro |>
    select(
      ln_importaciones_reales,
      ln_exportaciones_reales,
      ln_pib_real,
      ln_itcrm,
      ln_pib_socios,
      ln_commodity_price_index
    ),
  
  start = c(
    anio_inicio,
    trimestre_inicio
  ),
  
  frequency = 4
)

ts_diferencias <- ts(
  panel_diferencias |>
    select(
      d_ln_importaciones_reales,
      d_ln_exportaciones_reales,
      d_ln_pib_real,
      d_ln_itcrm,
      d_ln_pib_socios,
      d_ln_commodity_price_index
    ),
  
  start = c(
    panel_diferencias$anio[1],
    panel_diferencias$trimestre[1]
  ),
  
  frequency = 4
)


# ------------------------------------------------------------
# 27. CREAR SISTEMAS PARA IMPORTACIONES
# ------------------------------------------------------------

sistema_importaciones_niveles <- ts(
  panel_maestro |>
    select(
      ln_importaciones_reales,
      ln_pib_real,
      ln_itcrm
    ),
  
  start = c(
    anio_inicio,
    trimestre_inicio
  ),
  
  frequency = 4
)

colnames(
  sistema_importaciones_niveles
) <- c(
  "ln_importaciones",
  "ln_pib",
  "ln_itcrm"
)

sistema_importaciones_diferencias <- ts(
  panel_diferencias |>
    select(
      d_ln_importaciones_reales,
      d_ln_pib_real,
      d_ln_itcrm
    ),
  
  start = c(
    panel_diferencias$anio[1],
    panel_diferencias$trimestre[1]
  ),
  
  frequency = 4
)

colnames(
  sistema_importaciones_diferencias
) <- c(
  "d_ln_importaciones",
  "d_ln_pib",
  "d_ln_itcrm"
)


# ------------------------------------------------------------
# 28. CREAR SISTEMA CLÁSICO DE EXPORTACIONES
# ------------------------------------------------------------

sistema_exportaciones_clasico_niveles <- ts(
  panel_maestro |>
    select(
      ln_exportaciones_reales,
      ln_pib_socios,
      ln_itcrm
    ),
  
  start = c(
    anio_inicio,
    trimestre_inicio
  ),
  
  frequency = 4
)

colnames(
  sistema_exportaciones_clasico_niveles
) <- c(
  "ln_exportaciones",
  "ln_pib_socios",
  "ln_itcrm"
)

sistema_exportaciones_clasico_diferencias <- ts(
  panel_diferencias |>
    select(
      d_ln_exportaciones_reales,
      d_ln_pib_socios,
      d_ln_itcrm
    ),
  
  start = c(
    panel_diferencias$anio[1],
    panel_diferencias$trimestre[1]
  ),
  
  frequency = 4
)

colnames(
  sistema_exportaciones_clasico_diferencias
) <- c(
  "d_ln_exportaciones",
  "d_ln_pib_socios",
  "d_ln_itcrm"
)


# ------------------------------------------------------------
# 29. CREAR SISTEMA AMPLIADO DE EXPORTACIONES
# ------------------------------------------------------------

sistema_exportaciones_ampliado_niveles <- ts(
  panel_maestro |>
    select(
      ln_exportaciones_reales,
      ln_pib_socios,
      ln_itcrm,
      ln_commodity_price_index
    ),
  
  start = c(
    anio_inicio,
    trimestre_inicio
  ),
  
  frequency = 4
)

colnames(
  sistema_exportaciones_ampliado_niveles
) <- c(
  "ln_exportaciones",
  "ln_pib_socios",
  "ln_itcrm",
  "ln_commodities"
)

sistema_exportaciones_ampliado_diferencias <- ts(
  panel_diferencias |>
    select(
      d_ln_exportaciones_reales,
      d_ln_pib_socios,
      d_ln_itcrm,
      d_ln_commodity_price_index
    ),
  
  start = c(
    panel_diferencias$anio[1],
    panel_diferencias$trimestre[1]
  ),
  
  frequency = 4
)

colnames(
  sistema_exportaciones_ampliado_diferencias
) <- c(
  "d_ln_exportaciones",
  "d_ln_pib_socios",
  "d_ln_itcrm",
  "d_ln_commodities"
)


# ------------------------------------------------------------
# 30. REVISAR OBJETOS TS
# ------------------------------------------------------------

cat(
  "\nSistema de importaciones en niveles:\n"
)

print(
  start(
    sistema_importaciones_niveles
  )
)

print(
  end(
    sistema_importaciones_niveles
  )
)

print(
  frequency(
    sistema_importaciones_niveles
  )
)

cat(
  "\nSistema ampliado de exportaciones en niveles:\n"
)

print(
  start(
    sistema_exportaciones_ampliado_niveles
  )
)

print(
  end(
    sistema_exportaciones_ampliado_niveles
  )
)

print(
  frequency(
    sistema_exportaciones_ampliado_niveles
  )
)


# ------------------------------------------------------------
# 31. CREAR RESUMEN DEL PANEL
# ------------------------------------------------------------

resumen_panel <- tibble(
  indicador = c(
    "Período inicial",
    "Período final",
    "Observaciones en niveles",
    "Observaciones en diferencias",
    "Frecuencia",
    "Variables principales",
    "Faltantes en niveles",
    "Trimestres faltantes"
  ),
  
  resultado = c(
    format(
      min(panel_maestro$periodo),
      "%Y Q%q"
    ),
    
    format(
      max(panel_maestro$periodo),
      "%Y Q%q"
    ),
    
    as.character(
      nrow(panel_maestro)
    ),
    
    as.character(
      nrow(panel_diferencias)
    ),
    
    "Trimestral",
    
    "6",
    
    as.character(
      sum(
        is.na(
          panel_maestro[
            variables_niveles
          ]
        )
      )
    ),
    
    as.character(
      nrow(
        periodos_faltantes_panel
      )
    )
  )
)

print(
  resumen_panel
)


# ------------------------------------------------------------
# 32. GUARDAR PANEL MAESTRO COMO RDS
# ------------------------------------------------------------

saveRDS(
  panel_maestro,
  
  here::here(
    "data",
    "processed",
    "panel_maestro_2004_2025.rds"
  )
)

saveRDS(
  panel_diferencias,
  
  here::here(
    "data",
    "processed",
    "panel_diferencias_2004_2025.rds"
  )
)


# ------------------------------------------------------------
# 33. GUARDAR OBJETOS DE SERIES DE TIEMPO
# ------------------------------------------------------------

saveRDS(
  ts_niveles,
  
  here::here(
    "data",
    "processed",
    "ts_niveles_2004_2025.rds"
  )
)

saveRDS(
  ts_diferencias,
  
  here::here(
    "data",
    "processed",
    "ts_diferencias_2004_2025.rds"
  )
)

saveRDS(
  sistema_importaciones_niveles,
  
  here::here(
    "data",
    "processed",
    "sistema_importaciones_niveles.rds"
  )
)

saveRDS(
  sistema_importaciones_diferencias,
  
  here::here(
    "data",
    "processed",
    "sistema_importaciones_diferencias.rds"
  )
)

saveRDS(
  sistema_exportaciones_clasico_niveles,
  
  here::here(
    "data",
    "processed",
    "sistema_exportaciones_clasico_niveles.rds"
  )
)

saveRDS(
  sistema_exportaciones_clasico_diferencias,
  
  here::here(
    "data",
    "processed",
    "sistema_exportaciones_clasico_diferencias.rds"
  )
)

saveRDS(
  sistema_exportaciones_ampliado_niveles,
  
  here::here(
    "data",
    "processed",
    "sistema_exportaciones_ampliado_niveles.rds"
  )
)

saveRDS(
  sistema_exportaciones_ampliado_diferencias,
  
  here::here(
    "data",
    "processed",
    "sistema_exportaciones_ampliado_diferencias.rds"
  )
)


# ------------------------------------------------------------
# 34. PREPARAR PANEL PARA EXCEL
# ------------------------------------------------------------

panel_maestro_excel <- panel_maestro |>
  mutate(
    periodo = periodo_label,
    trimestre_factor =
      as.character(
        trimestre_factor
      )
  ) |>
  select(
    -periodo_label
  )

panel_diferencias_excel <- panel_diferencias |>
  mutate(
    periodo = periodo_label,
    trimestre_factor =
      as.character(
        trimestre_factor
      )
  ) |>
  select(
    -periodo_label
  )


# ------------------------------------------------------------
# 35. GUARDAR PANEL EN EXCEL
# ------------------------------------------------------------

writexl::write_xlsx(
  list(
    panel_niveles =
      panel_maestro_excel,
    
    panel_diferencias =
      panel_diferencias_excel,
    
    cobertura =
      resumen_cobertura,
    
    resumen =
      resumen_panel,
    
    faltantes =
      tabla_faltantes
  ),
  
  path = here::here(
    "data",
    "processed",
    "panel_maestro_2004_2025.xlsx"
  )
)


# ------------------------------------------------------------
# 36. GUARDAR TABLAS DE CONTROL
# ------------------------------------------------------------

writexl::write_xlsx(
  resumen_cobertura,
  
  path = here::here(
    "outputs",
    "tables",
    "tabla_cobertura_series.xlsx"
  )
)

writexl::write_xlsx(
  resumen_panel,
  
  path = here::here(
    "outputs",
    "tables",
    "tabla_resumen_panel.xlsx"
  )
)


# ------------------------------------------------------------
# 37. MENSAJE FINAL
# ------------------------------------------------------------

cat(
  "\n============================================================",
  "\nPANEL MAESTRO CONSTRUIDO CORRECTAMENTE",
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
  
  "\nObservaciones en niveles:",
  nrow(panel_maestro),
  
  "\nObservaciones en diferencias:",
  nrow(panel_diferencias),
  
  "\nFaltantes variables principales:",
  sum(
    is.na(
      panel_maestro[
        variables_niveles
      ]
    )
  ),
  
  "\nTrimestres faltantes:",
  nrow(
    periodos_faltantes_panel
  ),
  
  "\n\nArchivos principales generados:",
  
  "\n- data/processed/panel_maestro_2004_2025.rds",
  
  "\n- data/processed/panel_diferencias_2004_2025.rds",
  
  "\n- data/processed/panel_maestro_2004_2025.xlsx",
  
  "\n- data/processed/sistema_importaciones_niveles.rds",
  
  "\n- data/processed/sistema_exportaciones_clasico_niveles.rds",
  
  "\n- data/processed/sistema_exportaciones_ampliado_niveles.rds",
  
  "\n============================================================",
  "\n"
)

