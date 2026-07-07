# ============================================================
# TP3 - SERIES DE TIEMPO
# Script 05: Construcción del PIB de socios comerciales
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


library(tidyverse)
library(here)
library(zoo)
library(writexl)


# ------------------------------------------------------------
# 2. CARPETAS DEL PROYECTO
# ------------------------------------------------------------

dir.create(
  here::here("data", "processed"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here::here("data", "processed", "final_data_panel"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here::here("data", "processed", "auxiliary_data"),
  recursive = TRUE,
  showWarnings = FALSE
)


dir.create(
  here::here("figures"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here::here("outputs", "tables"),
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# 3. RUTAS DE LAS BASES PROCESADAS
# ------------------------------------------------------------

ruta_pib_externo <- here::here(
  "data",
  "processed",
  "auxiliary_data",
    "pib_usa_brasil_trimestral.rds"
)

ruta_ponderadores <- here::here(
  "data",
  "processed",
  "auxiliary_data",
    "ponderadores_bcra_2004_2025.rds"
)


# ------------------------------------------------------------
# 4. VERIFICAR EXISTENCIA DE ARCHIVOS
# ------------------------------------------------------------

if (!file.exists(ruta_pib_externo)) {
  stop(
    paste0(
      "No se encontró la base de PIB externos:\n",
      ruta_pib_externo
    )
  )
}

if (!file.exists(ruta_ponderadores)) {
  stop(
    paste0(
      "No se encontró la base de ponderadores:\n",
      ruta_ponderadores
    )
  )
}


# ------------------------------------------------------------
# 5. CARGAR LAS BASES
# ------------------------------------------------------------

pib_externo <- readRDS(
  ruta_pib_externo
)

ponderadores_mensuales <- readRDS(
  ruta_ponderadores
)


# ------------------------------------------------------------
# 6. VERIFICAR VARIABLES REQUERIDAS
# ------------------------------------------------------------

variables_pib_requeridas <- c(
  "periodo",
  "anio",
  "trimestre",
  "pib_usa_real",
  "pib_brasil_real"
)

variables_ponderadores_requeridas <- c(
  "fecha",
  "ponderador_brasil",
  "ponderador_usa"
)

variables_pib_faltantes <- setdiff(
  variables_pib_requeridas,
  names(pib_externo)
)

variables_ponderadores_faltantes <- setdiff(
  variables_ponderadores_requeridas,
  names(ponderadores_mensuales)
)

if (length(variables_pib_faltantes) > 0) {
  stop(
    paste0(
      "Faltan las siguientes variables en pib_externo: ",
      paste(
        variables_pib_faltantes,
        collapse = ", "
      )
    )
  )
}

if (length(variables_ponderadores_faltantes) > 0) {
  stop(
    paste0(
      "Faltan las siguientes variables en ponderadores_mensuales: ",
      paste(
        variables_ponderadores_faltantes,
        collapse = ", "
      )
    )
  )
}


# ------------------------------------------------------------
# 7. NORMALIZAR TIPOS DE VARIABLES
# ------------------------------------------------------------

pib_externo <- pib_externo |>
  mutate(
    periodo = zoo::as.yearqtr(periodo),
    anio = as.integer(anio),
    trimestre = as.integer(trimestre),
    pib_usa_real = as.numeric(pib_usa_real),
    pib_brasil_real = as.numeric(pib_brasil_real)
  ) |>
  arrange(periodo)

ponderadores_mensuales <- ponderadores_mensuales |>
  mutate(
    fecha = as.Date(fecha),
    ponderador_brasil = as.numeric(
      ponderador_brasil
    ),
    ponderador_usa = as.numeric(
      ponderador_usa
    )
  ) |>
  arrange(fecha)


# ------------------------------------------------------------
# 8. VERIFICAR COBERTURA Y CALIDAD DE LOS PIB
# ------------------------------------------------------------

resumen_pib_externo <- pib_externo |>
  summarise(
    periodo_inicial = min(periodo),
    periodo_final = max(periodo),
    observaciones = n(),
    faltantes_usa = sum(
      is.na(pib_usa_real)
    ),
    faltantes_brasil = sum(
      is.na(pib_brasil_real)
    ),
    duplicados = sum(
      duplicated(periodo)
    )
  )

print(resumen_pib_externo)

if (anyDuplicated(pib_externo$periodo) > 0) {
  stop(
    "Existen períodos trimestrales duplicados en pib_externo."
  )
}

if (
  any(is.na(pib_externo$pib_usa_real)) ||
  any(is.na(pib_externo$pib_brasil_real))
) {
  stop(
    "Existen valores faltantes en los PIB externos."
  )
}

if (
  any(pib_externo$pib_usa_real <= 0) ||
  any(pib_externo$pib_brasil_real <= 0)
) {
  stop(
    "Existen valores no positivos en los PIB externos."
  )
}


# ------------------------------------------------------------
# 9. FILTRAR PONDERADORES AL PERÍODO 2004-2025
# ------------------------------------------------------------

ponderadores_muestra <- ponderadores_mensuales |>
  filter(
    fecha >= as.Date("2004-01-01"),
    fecha <= as.Date("2025-12-31")
  )

if (nrow(ponderadores_muestra) == 0) {
  stop(
    "No existen ponderadores dentro del período 2004-2025."
  )
}


# ------------------------------------------------------------
# 10. CALCULAR PONDERADORES PROMEDIO
# ------------------------------------------------------------

ponderadores_pib_socios <- ponderadores_muestra |>
  summarise(
    ponderador_brasil_original = mean(
      ponderador_brasil,
      na.rm = TRUE
    ),

    ponderador_usa_original = mean(
      ponderador_usa,
      na.rm = TRUE
    ),

    observaciones_mensuales = n()
  ) |>
  mutate(
    suma_ponderadores =
      ponderador_brasil_original +
      ponderador_usa_original,

    ponderador_brasil_normalizado =
      ponderador_brasil_original /
      suma_ponderadores,

    ponderador_usa_normalizado =
      ponderador_usa_original /
      suma_ponderadores,

    periodo_calculo =
      "2004-01 a 2025-12"
  )

print(ponderadores_pib_socios)


# ------------------------------------------------------------
# 11. EXTRAER PONDERADORES COMO ESCALARES
# ------------------------------------------------------------

w_brasil <- ponderadores_pib_socios |>
  pull(
    ponderador_brasil_normalizado
  )

w_usa <- ponderadores_pib_socios |>
  pull(
    ponderador_usa_normalizado
  )

if (length(w_brasil) != 1) {
  stop(
    "El ponderador de Brasil no es un valor escalar."
  )
}

if (length(w_usa) != 1) {
  stop(
    "El ponderador de Estados Unidos no es un valor escalar."
  )
}

if (
  abs(
    w_brasil + w_usa - 1
  ) > 1e-10
) {
  stop(
    "Los ponderadores normalizados no suman uno."
  )
}

cat(
  "\nPonderadores normalizados:",
  "\nBrasil:", round(w_brasil, 6),
  "\nEstados Unidos:", round(w_usa, 6),
  "\nSuma:", round(w_brasil + w_usa, 6),
  "\n"
)


# ------------------------------------------------------------
# 12. DEFINIR EL PERÍODO BASE
# ------------------------------------------------------------

periodo_base <- zoo::as.yearqtr(
  "2004 Q1",
  format = "%Y Q%q"
)


# ------------------------------------------------------------
# 13. EXTRAER VALORES BASE 2004Q1
# ------------------------------------------------------------

pib_usa_base <- pib_externo |>
  filter(
    periodo == periodo_base
  ) |>
  pull(
    pib_usa_real
  )

pib_brasil_base <- pib_externo |>
  filter(
    periodo == periodo_base
  ) |>
  pull(
    pib_brasil_real
  )

if (length(pib_usa_base) != 1) {
  stop(
    "No se encontró exactamente una observación de PIB USA para 2004Q1."
  )
}

if (length(pib_brasil_base) != 1) {
  stop(
    "No se encontró exactamente una observación de PIB Brasil para 2004Q1."
  )
}

if (
  is.na(pib_usa_base) ||
  is.na(pib_brasil_base)
) {
  stop(
    "El valor base de alguno de los PIB es faltante."
  )
}

if (
  pib_usa_base <= 0 ||
  pib_brasil_base <= 0
) {
  stop(
    "El valor base de alguno de los PIB no es positivo."
  )
}


# ------------------------------------------------------------
# 14. CONSTRUIR ÍNDICES BASE 2004Q1 = 100
# ------------------------------------------------------------

pib_socios <- pib_externo |>
  mutate(
    pib_usa_indice =
      100 *
      pib_usa_real /
      pib_usa_base,

    pib_brasil_indice =
      100 *
      pib_brasil_real /
      pib_brasil_base
  )


# ------------------------------------------------------------
# 15. CALCULAR LOGARITMOS DE LOS ÍNDICES
# ------------------------------------------------------------

pib_socios <- pib_socios |>
  mutate(
    ln_pib_usa_indice =
      log(pib_usa_indice),

    ln_pib_brasil_indice =
      log(pib_brasil_indice)
  )


# ------------------------------------------------------------
# 16. CONSTRUIR PIB DE SOCIOS
#     PROMEDIO GEOMÉTRICO PONDERADO
# ------------------------------------------------------------

pib_socios <- pib_socios |>
  mutate(
    ln_pib_socios =
      w_brasil *
      ln_pib_brasil_indice +
      w_usa *
      ln_pib_usa_indice,

    pib_socios_indice =
      exp(ln_pib_socios)
  )


# ------------------------------------------------------------
# 17. AÑADIR FECHA Y ETIQUETA TRIMESTRAL
# ------------------------------------------------------------

pib_socios <- pib_socios |>
  mutate(
    fecha = as.Date(periodo),

    periodo_label = format(
      periodo,
      "%Y Q%q"
    )
  ) |>
  select(
    fecha,
    periodo,
    periodo_label,
    anio,
    trimestre,
    pib_usa_real,
    pib_brasil_real,
    pib_usa_indice,
    pib_brasil_indice,
    pib_socios_indice,
    ln_pib_usa_indice,
    ln_pib_brasil_indice,
    ln_pib_socios
  ) |>
  arrange(periodo)


# ------------------------------------------------------------
# 18. REVISAR RESULTADOS
# ------------------------------------------------------------

print(
  pib_socios |>
    slice_head(n = 8)
)

print(
  pib_socios |>
    slice_tail(n = 8)
)

print(
  pib_socios |>
    filter(
      periodo == periodo_base
    ) |>
    select(
      periodo_label,
      pib_usa_indice,
      pib_brasil_indice,
      pib_socios_indice,
      ln_pib_socios
    )
)


# ------------------------------------------------------------
# 19. CONTROLES AUTOMÁTICOS
# ------------------------------------------------------------

if (nrow(pib_socios) != 88) {
  warning(
    paste0(
      "La base tiene ",
      nrow(pib_socios),
      " observaciones y no 88."
    )
  )
}

stopifnot(
  !anyDuplicated(
    pib_socios$periodo
  ),

  !any(
    is.na(
      pib_socios[
        c(
          "pib_usa_indice",
          "pib_brasil_indice",
          "pib_socios_indice",
          "ln_pib_socios"
        )
      ]
    )
  ),

  all(
    pib_socios$pib_usa_indice > 0
  ),

  all(
    pib_socios$pib_brasil_indice > 0
  ),

  all(
    pib_socios$pib_socios_indice > 0
  ),

  all(
    is.finite(
      pib_socios$ln_pib_socios
    )
  ),

  abs(
    w_brasil + w_usa - 1
  ) < 1e-10
)

valor_base_socios <- pib_socios |>
  filter(
    periodo == periodo_base
  ) |>
  pull(
    pib_socios_indice
  )

stopifnot(
  length(valor_base_socios) == 1,
  abs(valor_base_socios - 100) < 1e-8
)


# ------------------------------------------------------------
# 20. CREAR TABLA DE TRAZABILIDAD
# ------------------------------------------------------------

tabla_ponderadores_pib_socios <- tibble(
  pais = c(
    "Brasil",
    "Estados Unidos"
  ),

  variable_fuente = c(
    "PIB real de Brasil",
    "PIB real de Estados Unidos"
  ),

  ponderador_original = c(
    ponderadores_pib_socios$
      ponderador_brasil_original,

    ponderadores_pib_socios$
      ponderador_usa_original
  ),

  ponderador_normalizado = c(
    w_brasil,
    w_usa
  ),

  periodo_ponderacion = c(
    "2004-01 a 2025-12",
    "2004-01 a 2025-12"
  ),

  fuente_ponderador = c(
    "BCRA, hoja Ponderadores del ITCRM",
    "BCRA, hoja Ponderadores del ITCRM"
  )
)

print(
  tabla_ponderadores_pib_socios
)


# ------------------------------------------------------------
# 21. PREPARAR DATOS PARA EL GRÁFICO
# ------------------------------------------------------------

pib_socios_long <- pib_socios |>
  select(
    fecha,

    `PIB Brasil` =
      pib_brasil_indice,

    `PIB Estados Unidos` =
      pib_usa_indice,

    `PIB socios` =
      pib_socios_indice
  ) |>
  pivot_longer(
    cols = -fecha,
    names_to = "serie",
    values_to = "indice"
  )


# ------------------------------------------------------------
# 22. CREAR GRÁFICO
# ------------------------------------------------------------

grafico_pib_socios <- ggplot(
  pib_socios_long,
  aes(
    x = fecha,
    y = indice,
    linetype = serie
  )
) +
  geom_line(
    linewidth = 0.8
  ) +
  labs(
    title =
      "PIB real de los socios comerciales de Argentina",

    subtitle =
      "Índices base 2004Q1 = 100",

    x = NULL,

    y = "Índice",

    linetype = NULL,

    caption = paste0(
      "Fuente: FRED y BCRA. ",
      "Brasil: ",
      round(
        100 * w_brasil,
        1
      ),
      "%; Estados Unidos: ",
      round(
        100 * w_usa,
        1
      ),
      "%."
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

print(
  grafico_pib_socios
)


# ------------------------------------------------------------
# 23. GUARDAR GRÁFICO
# ------------------------------------------------------------

ggsave(
  filename = here::here(
    "outputs",
    "figures",
    "pib_socios_comerciales.png"
  ),

  plot = grafico_pib_socios,

  width = 9,

  height = 5.5,

  dpi = 300
)


# ------------------------------------------------------------
# 24. GUARDAR BASE COMO RDS
# ------------------------------------------------------------

saveRDS(
  pib_socios,

  here::here(
    "data",
    "processed",
    "auxiliary_data",
    "pib_socios_comerciales_2004_2025.rds"
  )
)

saveRDS(
  tabla_ponderadores_pib_socios,

  here::here(
    "data",
    "processed",
    "auxiliary_data",
    "ponderadores_pib_socios.rds"
  )
)


# ------------------------------------------------------------
# 25. PREPARAR BASE PARA EXCEL
# ------------------------------------------------------------

pib_socios_excel <- pib_socios |>
  mutate(
    periodo = periodo_label
  ) |>
  select(
    -periodo_label
  )


# ------------------------------------------------------------
# 26. GUARDAR BASE Y PONDERADORES EN EXCEL
# ------------------------------------------------------------

writexl::write_xlsx(
  list(
    pib_socios =
      pib_socios_excel,

    ponderadores =
      tabla_ponderadores_pib_socios
  ),

  path = here::here(
    "data",
    "processed",
    "auxiliary_data",
    "pib_socios_comerciales_2004_2025.xlsx"
  )
)


# ------------------------------------------------------------
# 27. GUARDAR TABLA DE PONDERADORES POR SEPARADO
# ------------------------------------------------------------

writexl::write_xlsx(
  tabla_ponderadores_pib_socios,

  path = here::here(
    "outputs",
    "tables",
    "tabla_ponderadores_pib_socios.xlsx"
  )
)


# ------------------------------------------------------------
# 28. MENSAJE FINAL
# ------------------------------------------------------------

cat(
  "\n============================================================",
  "\nPIB DE SOCIOS CONSTRUIDO CORRECTAMENTE",
  "\n============================================================",

  "\nObservaciones:",
  nrow(pib_socios),

  "\nPeríodo inicial:",
  format(
    min(pib_socios$periodo),
    "%Y Q%q"
  ),

  "\nPeríodo final:",
  format(
    max(pib_socios$periodo),
    "%Y Q%q"
  ),

  "\nPonderador Brasil:",
  round(
    w_brasil,
    4
  ),

  "\nPonderador Estados Unidos:",
  round(
    w_usa,
    4
  ),

  "\nSuma de ponderadores:",
  round(
    w_brasil + w_usa,
    4
  ),

  "\nÍndice PIB socios en 2004Q1:",
  round(
    valor_base_socios,
    4
  ),

  "\nFaltantes PIB socios:",
  sum(
    is.na(
      pib_socios$pib_socios_indice
    )
  ),

  "\n\nArchivos generados:",

  "\n- data/processed/pib_socios_comerciales_2004_2025.rds",

  "\n- data/processed/pib_socios_comerciales_2004_2025.xlsx",

  "\n- data/processed/ponderadores_pib_socios.rds",

  "\n- outputs/tables/tabla_ponderadores_pib_socios.xlsx",

  "\n- figures/pib_socios_comerciales.png",

  "\n============================================================",
  "\n"
)


