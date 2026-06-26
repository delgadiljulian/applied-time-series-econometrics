# ============================================================
# TP3 - SERIES DE TIEMPO
# Script 07: Análisis descriptivo y estacionalidad
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
  "writexl",
  "scales"
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
library(writexl)
library(scales)


# ------------------------------------------------------------
# 2. CARPETAS DE SALIDA
# ------------------------------------------------------------

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

dir.create(
  here::here("outputs", "models"),
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
  arrange(periodo)


# ------------------------------------------------------------
# 4. VERIFICAR VARIABLES NECESARIAS
# ------------------------------------------------------------

variables_requeridas <- c(
  "fecha",
  "periodo",
  "periodo_label",
  "anio",
  "trimestre",
  
  "pib_real",
  "importaciones_reales",
  "exportaciones_reales",
  "itcrm",
  "pib_socios_indice",
  "commodity_price_index",
  
  "ln_pib_real",
  "ln_importaciones_reales",
  "ln_exportaciones_reales",
  "ln_itcrm",
  "ln_pib_socios",
  "ln_commodity_price_index",
  
  "d_ln_pib_real",
  "d_ln_importaciones_reales",
  "d_ln_exportaciones_reales",
  "d_ln_itcrm",
  "d_ln_pib_socios",
  "d_ln_commodity_price_index"
)

variables_faltantes <- setdiff(
  variables_requeridas,
  names(panel_maestro)
)

if (length(variables_faltantes) > 0) {
  stop(
    paste0(
      "Faltan variables en el panel maestro: ",
      paste(
        variables_faltantes,
        collapse = ", "
      )
    )
  )
}


# ------------------------------------------------------------
# 5. DICCIONARIO DE VARIABLES
# ------------------------------------------------------------

diccionario_series <- tibble(
  variable_nivel = c(
    "importaciones_reales",
    "exportaciones_reales",
    "pib_real",
    "itcrm",
    "pib_socios_indice",
    "commodity_price_index"
  ),
  
  variable_log = c(
    "ln_importaciones_reales",
    "ln_exportaciones_reales",
    "ln_pib_real",
    "ln_itcrm",
    "ln_pib_socios",
    "ln_commodity_price_index"
  ),
  
  variable_diferencia = c(
    "d_ln_importaciones_reales",
    "d_ln_exportaciones_reales",
    "d_ln_pib_real",
    "d_ln_itcrm",
    "d_ln_pib_socios",
    "d_ln_commodity_price_index"
  ),
  
  etiqueta = c(
    "Importaciones reales",
    "Exportaciones reales",
    "PIB real de Argentina",
    "ITCRM",
    "PIB de socios comerciales",
    "Índice de commodities"
  ),
  
  fuente = c(
    "INDEC",
    "INDEC",
    "INDEC",
    "BCRA",
    "FRED y BCRA",
    "Banco Mundial"
  )
)

print(diccionario_series)


# ------------------------------------------------------------
# 6. FUNCIÓN DE ESTADÍSTICOS DESCRIPTIVOS
# ------------------------------------------------------------

calcular_descriptivos <- function(
    datos,
    variable,
    etiqueta,
    transformacion
) {
  
  x <- datos[[variable]]
  
  tibble(
    variable = etiqueta,
    transformacion = transformacion,
    
    observaciones = sum(
      !is.na(x)
    ),
    
    faltantes = sum(
      is.na(x)
    ),
    
    media = mean(
      x,
      na.rm = TRUE
    ),
    
    mediana = median(
      x,
      na.rm = TRUE
    ),
    
    desviacion_estandar = sd(
      x,
      na.rm = TRUE
    ),
    
    minimo = min(
      x,
      na.rm = TRUE
    ),
    
    percentil_25 = quantile(
      x,
      probs = 0.25,
      na.rm = TRUE,
      names = FALSE
    ),
    
    percentil_75 = quantile(
      x,
      probs = 0.75,
      na.rm = TRUE,
      names = FALSE
    ),
    
    maximo = max(
      x,
      na.rm = TRUE
    )
  )
}


# ------------------------------------------------------------
# 7. ESTADÍSTICOS DESCRIPTIVOS EN NIVELES
# ------------------------------------------------------------

tabla_descriptivos_niveles <- purrr::pmap_dfr(
  diccionario_series |>
    select(
      variable_nivel,
      etiqueta
    ),
  
  function(
    variable_nivel,
    etiqueta
  ) {
    
    calcular_descriptivos(
      datos = panel_maestro,
      variable = variable_nivel,
      etiqueta = etiqueta,
      transformacion = "Nivel"
    )
  }
)

print(tabla_descriptivos_niveles)


# ------------------------------------------------------------
# 8. ESTADÍSTICOS DESCRIPTIVOS EN LOGARITMOS
# ------------------------------------------------------------

tabla_descriptivos_logs <- purrr::pmap_dfr(
  diccionario_series |>
    select(
      variable_log,
      etiqueta
    ),
  
  function(
    variable_log,
    etiqueta
  ) {
    
    calcular_descriptivos(
      datos = panel_maestro,
      variable = variable_log,
      etiqueta = etiqueta,
      transformacion = "Logaritmo natural"
    )
  }
)

print(tabla_descriptivos_logs)


# ------------------------------------------------------------
# 9. ESTADÍSTICOS DESCRIPTIVOS EN DIFERENCIAS
# ------------------------------------------------------------

tabla_descriptivos_diferencias <- purrr::pmap_dfr(
  diccionario_series |>
    select(
      variable_diferencia,
      etiqueta
    ),
  
  function(
    variable_diferencia,
    etiqueta
  ) {
    
    calcular_descriptivos(
      datos = panel_maestro,
      variable = variable_diferencia,
      etiqueta = etiqueta,
      transformacion =
        "Primera diferencia logarítmica"
    )
  }
)

print(tabla_descriptivos_diferencias)


# ------------------------------------------------------------
# 10. TABLA DESCRIPTIVA COMPLETA
# ------------------------------------------------------------

tabla_descriptivos_completa <- bind_rows(
  tabla_descriptivos_niveles,
  tabla_descriptivos_logs,
  tabla_descriptivos_diferencias
)

print(tabla_descriptivos_completa)


# ------------------------------------------------------------
# 11. BASE LARGA DE SERIES EN NIVELES
# ------------------------------------------------------------

series_niveles_long <- panel_maestro |>
  select(
    fecha,
    periodo_label,
    all_of(
      diccionario_series$variable_nivel
    )
  ) |>
  pivot_longer(
    cols = -c(
      fecha,
      periodo_label
    ),
    names_to = "variable_nivel",
    values_to = "valor"
  ) |>
  left_join(
    diccionario_series |>
      select(
        variable_nivel,
        etiqueta
      ),
    by = "variable_nivel"
  )


# ------------------------------------------------------------
# 12. BASE LARGA DE SERIES EN LOGARITMOS
# ------------------------------------------------------------

series_logs_long <- panel_maestro |>
  select(
    fecha,
    periodo_label,
    all_of(
      diccionario_series$variable_log
    )
  ) |>
  pivot_longer(
    cols = -c(
      fecha,
      periodo_label
    ),
    names_to = "variable_log",
    values_to = "valor"
  ) |>
  left_join(
    diccionario_series |>
      select(
        variable_log,
        etiqueta
      ),
    by = "variable_log"
  )


# ------------------------------------------------------------
# 13. BASE LARGA DE PRIMERAS DIFERENCIAS
# ------------------------------------------------------------

series_diferencias_long <- panel_maestro |>
  select(
    fecha,
    periodo_label,
    trimestre,
    all_of(
      diccionario_series$variable_diferencia
    )
  ) |>
  pivot_longer(
    cols = all_of(
      diccionario_series$variable_diferencia
    ),
    names_to = "variable_diferencia",
    values_to = "valor"
  ) |>
  left_join(
    diccionario_series |>
      select(
        variable_diferencia,
        etiqueta
      ),
    by = "variable_diferencia"
  ) |>
  filter(
    !is.na(valor)
  ) |>
  mutate(
    variacion_porcentual_aprox =
      100 * valor
  )


# ------------------------------------------------------------
# 14. GRÁFICO DE SERIES EN NIVELES
# ------------------------------------------------------------

grafico_series_niveles <- ggplot(
  series_niveles_long,
  aes(
    x = fecha,
    y = valor
  )
) +
  geom_line(
    linewidth = 0.65
  ) +
  facet_wrap(
    ~ etiqueta,
    scales = "free_y",
    ncol = 2
  ) +
  labs(
    title =
      "Series macroeconómicas y de comercio exterior",
    
    subtitle =
      "Valores trimestrales en niveles",
    
    x = NULL,
    
    y = NULL,
    
    caption =
      "Fuente: INDEC, BCRA, FRED y Banco Mundial."
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(
      face = "bold"
    ),
    
    plot.title = element_text(
      face = "bold"
    )
  )

print(
  grafico_series_niveles
)


# ------------------------------------------------------------
# 15. GUARDAR GRÁFICO DE NIVELES
# ------------------------------------------------------------

ggsave(
  filename = here::here(
    "outputs",
    "figures",
    "figure_series_levels.png"
  ),
  
  plot = grafico_series_niveles,
  
  width = 11,
  
  height = 8,
  
  dpi = 300
)


# ------------------------------------------------------------
# 16. GRÁFICO DE SERIES EN LOGARITMOS
# ------------------------------------------------------------

grafico_series_logs <- ggplot(
  series_logs_long,
  aes(
    x = fecha,
    y = valor
  )
) +
  geom_line(
    linewidth = 0.7
  ) +
  facet_wrap(
    ~ etiqueta,
    scales = "free_y",
    ncol = 2
  ) +
  labs(
    title =
      "Series utilizadas en los modelos econométricos",
    
    subtitle =
      "Variables expresadas en logaritmos naturales",
    
    x = NULL,
    
    y = "Logaritmo natural",
    
    caption =
      "Fuente: elaboración propia con datos de INDEC, BCRA, FRED y Banco Mundial."
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(
      face = "bold"
    ),
    
    plot.title = element_text(
      face = "bold"
    )
  )

print(
  grafico_series_logs
)


# ------------------------------------------------------------
# 17. GUARDAR GRÁFICO DE LOGARITMOS
# ------------------------------------------------------------

ggsave(
  filename = here::here(
    "outputs",
    "figures",
    "figure_log_series.png"
  ),
  
  plot = grafico_series_logs,
  
  width = 11,
  
  height = 8,
  
  dpi = 300
)


# ------------------------------------------------------------
# 18. GRÁFICO DE PRIMERAS DIFERENCIAS LOGARÍTMICAS
# ------------------------------------------------------------

grafico_series_diferencias <- ggplot(
  series_diferencias_long,
  aes(
    x = fecha,
    y = variacion_porcentual_aprox
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  geom_line(
    linewidth = 0.65
  ) +
  facet_wrap(
    ~ etiqueta,
    scales = "free_y",
    ncol = 2
  ) +
  labs(
    title =
      "Variaciones trimestrales aproximadas",
    
    subtitle =
      "Primeras diferencias logarítmicas multiplicadas por 100",
    
    x = NULL,
    
    y = "Variación trimestral aproximada (%)",
    
    caption =
      "Fuente: elaboración propia."
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(
      face = "bold"
    ),
    
    plot.title = element_text(
      face = "bold"
    )
  )

print(
  grafico_series_diferencias
)


# ------------------------------------------------------------
# 19. GUARDAR GRÁFICO DE DIFERENCIAS
# ------------------------------------------------------------

ggsave(
  filename = here::here(
    "outputs",
    "figures",
    "figure_first_differences.png"
  ),
  
  plot = grafico_series_diferencias,
  
  width = 11,
  
  height = 8,
  
  dpi = 300
)


# ------------------------------------------------------------
# 20. CONSTRUIR ÍNDICES BASE 2004Q1 = 100
# ------------------------------------------------------------

series_indice_base <- panel_maestro |>
  select(
    fecha,
    periodo,
    all_of(
      diccionario_series$variable_nivel
    )
  ) |>
  pivot_longer(
    cols = all_of(
      diccionario_series$variable_nivel
    ),
    names_to = "variable_nivel",
    values_to = "valor"
  ) |>
  group_by(
    variable_nivel
  ) |>
  arrange(
    periodo,
    .by_group = TRUE
  ) |>
  mutate(
    valor_base = first(
      valor[
        !is.na(valor)
      ]
    ),
    
    indice_2004q1 =
      100 * valor / valor_base
  ) |>
  ungroup() |>
  left_join(
    diccionario_series |>
      select(
        variable_nivel,
        etiqueta
      ),
    by = "variable_nivel"
  )


# ------------------------------------------------------------
# 21. GRÁFICO DE ÍNDICES BASE 2004Q1 = 100
# ------------------------------------------------------------

grafico_indices_base <- ggplot(
  series_indice_base,
  aes(
    x = fecha,
    y = indice_2004q1,
    linetype = etiqueta
  )
) +
  geom_hline(
    yintercept = 100,
    linetype = "dotted",
    linewidth = 0.4
  ) +
  geom_line(
    linewidth = 0.75
  ) +
  labs(
    title =
      "Evolución comparada de las series",
    
    subtitle =
      "Índices base 2004Q1 = 100",
    
    x = NULL,
    
    y = "Índice",
    
    linetype = NULL,
    
    caption =
      "Fuente: elaboración propia."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    
    legend.text = element_text(
      size = 8
    ),
    
    plot.title = element_text(
      face = "bold"
    )
  )

print(
  grafico_indices_base
)


# ------------------------------------------------------------
# 22. GUARDAR GRÁFICO DE ÍNDICES
# ------------------------------------------------------------

ggsave(
  filename = here::here(
    "outputs",
    "figures",
    "figure_index_base_2004q1.png"
  ),
  
  plot = grafico_indices_base,
  
  width = 11,
  
  height = 6,
  
  dpi = 300
)


# ------------------------------------------------------------
# 23. GRÁFICOS ESPECÍFICOS DE IMPORTACIONES
# ------------------------------------------------------------

grafico_importaciones <- panel_maestro |>
  select(
    fecha,
    
    `Importaciones reales` =
      ln_importaciones_reales,
    
    `PIB real de Argentina` =
      ln_pib_real,
    
    `ITCRM` =
      ln_itcrm
  ) |>
  pivot_longer(
    cols = -fecha,
    names_to = "serie",
    values_to = "valor"
  ) |>
  ggplot(
    aes(
      x = fecha,
      y = valor,
      linetype = serie
    )
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  labs(
    title =
      "Importaciones, actividad económica e ITCRM",
    
    subtitle =
      "Series trimestrales en logaritmos naturales",
    
    x = NULL,
    
    y = "Logaritmo natural",
    
    linetype = NULL,
    
    caption =
      "Fuente: INDEC y BCRA."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    
    plot.title = element_text(
      face = "bold"
    )
  )

print(
  grafico_importaciones
)

ggsave(
  filename = here::here(
    "outputs",
    "figures",
    "figure_import_function.png"
  ),
  
  plot = grafico_importaciones,
  
  width = 10,
  
  height = 5.5,
  
  dpi = 300
)


# ------------------------------------------------------------
# 24. GRÁFICO ESPECÍFICO DE EXPORTACIONES
# ------------------------------------------------------------

grafico_exportaciones <- panel_maestro |>
  select(
    fecha,
    
    `Exportaciones reales` =
      ln_exportaciones_reales,
    
    `PIB de socios` =
      ln_pib_socios,
    
    `ITCRM` =
      ln_itcrm,
    
    `Commodities` =
      ln_commodity_price_index
  ) |>
  pivot_longer(
    cols = -fecha,
    names_to = "serie",
    values_to = "valor"
  ) |>
  ggplot(
    aes(
      x = fecha,
      y = valor,
      linetype = serie
    )
  ) +
  geom_line(
    linewidth = 0.8
  ) +
  labs(
    title =
      "Exportaciones y sus determinantes externos",
    
    subtitle =
      "Series trimestrales en logaritmos naturales",
    
    x = NULL,
    
    y = "Logaritmo natural",
    
    linetype = NULL,
    
    caption =
      "Fuente: INDEC, BCRA, FRED y Banco Mundial."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    
    plot.title = element_text(
      face = "bold"
    )
  )

print(
  grafico_exportaciones
)

ggsave(
  filename = here::here(
    "outputs",
    "figures",
    "figure_export_function.png"
  ),
  
  plot = grafico_exportaciones,
  
  width = 10,
  
  height = 5.5,
  
  dpi = 300
)


# ------------------------------------------------------------
# 25. FUNCIÓN PARA PRUEBA DE ESTACIONALIDAD
# ------------------------------------------------------------

prueba_estacionalidad <- function(
    datos,
    variable,
    etiqueta
) {
  
  datos_prueba <- datos |>
    transmute(
      trimestre = factor(
        trimestre,
        levels = 1:4
      ),
      
      valor = .data[[variable]]
    ) |>
    filter(
      !is.na(valor)
    )
  
  # Modelo sin efectos trimestrales
  modelo_restringido <- lm(
    valor ~ 1,
    data = datos_prueba
  )
  
  # Modelo con dummies trimestrales
  modelo_estacional <- lm(
    valor ~ trimestre,
    data = datos_prueba
  )
  
  comparacion <- anova(
    modelo_restringido,
    modelo_estacional
  )
  
  p_valor_f <- comparacion$
    `Pr(>F)`[2]
  
  prueba_kw <- kruskal.test(
    valor ~ trimestre,
    data = datos_prueba
  )
  
  p_valor_kw <- prueba_kw$p.value
  
  tibble(
    variable = etiqueta,
    
    serie_evaluada =
      "Primera diferencia logarítmica",
    
    observaciones =
      nrow(datos_prueba),
    
    p_valor_test_f =
      as.numeric(p_valor_f),
    
    p_valor_kruskal_wallis =
      as.numeric(p_valor_kw),
    
    evidencia_estacional_5pct =
      case_when(
        p_valor_f < 0.05 &
          p_valor_kw < 0.05 ~
          "Sí: evidencia en ambos contrastes",
        
        p_valor_f < 0.05 |
          p_valor_kw < 0.05 ~
          "Evidencia mixta",
        
        TRUE ~
          "No se detecta evidencia"
      ),
    
    decision_metodologica =
      case_when(
        p_valor_f < 0.05 |
          p_valor_kw < 0.05 ~
          paste0(
            "Evaluar dummies trimestrales ",
            "y rezagos estacionales"
          ),
        
        TRUE ~
          "No requiere ajuste estacional adicional"
      )
  )
}


# ------------------------------------------------------------
# 26. EJECUTAR PRUEBAS DE ESTACIONALIDAD
# ------------------------------------------------------------

tabla_estacionalidad <- purrr::pmap_dfr(
  diccionario_series |>
    select(
      variable_diferencia,
      etiqueta
    ),
  
  function(
    variable_diferencia,
    etiqueta
  ) {
    
    prueba_estacionalidad(
      datos = panel_maestro,
      variable = variable_diferencia,
      etiqueta = etiqueta
    )
  }
)

print(
  tabla_estacionalidad
)


# ------------------------------------------------------------
# 27. BOXPLOTS POR TRIMESTRE
# ------------------------------------------------------------

grafico_estacionalidad <- series_diferencias_long |>
  mutate(
    trimestre_label = factor(
      trimestre,
      levels = 1:4,
      labels = c(
        "Q1",
        "Q2",
        "Q3",
        "Q4"
      )
    )
  ) |>
  ggplot(
    aes(
      x = trimestre_label,
      y = variacion_porcentual_aprox
    )
  ) +
  geom_boxplot() +
  facet_wrap(
    ~ etiqueta,
    scales = "free_y",
    ncol = 2
  ) +
  labs(
    title =
      "Distribución de las variaciones por trimestre",
    
    subtitle =
      "Diagnóstico gráfico de posibles patrones estacionales",
    
    x = "Trimestre",
    
    y = "Variación aproximada (%)",
    
    caption =
      "Fuente: elaboración propia."
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(
      face = "bold"
    ),
    
    plot.title = element_text(
      face = "bold"
    )
  )

print(
  grafico_estacionalidad
)

ggsave(
  filename = here::here(
    "outputs",
    "figures",
    "figure_seasonality_boxplots.png"
  ),
  
  plot = grafico_estacionalidad,
  
  width = 11,
  
  height = 8,
  
  dpi = 300
)


# ------------------------------------------------------------
# 28. MATRIZ DE CORRELACIONES EN DIFERENCIAS
# ------------------------------------------------------------

base_correlaciones <- panel_maestro |>
  select(
    Importaciones =
      d_ln_importaciones_reales,
    
    Exportaciones =
      d_ln_exportaciones_reales,
    
    PIB_Argentina =
      d_ln_pib_real,
    
    ITCRM =
      d_ln_itcrm,
    
    PIB_socios =
      d_ln_pib_socios,
    
    Commodities =
      d_ln_commodity_price_index
  )

matriz_correlaciones <- cor(
  base_correlaciones,
  use = "complete.obs"
)

tabla_correlaciones <- as.data.frame(
  matriz_correlaciones
) |>
  rownames_to_column(
    var = "variable"
  ) |>
  as_tibble()

print(
  tabla_correlaciones
)


# ------------------------------------------------------------
# 29. TABLA DE COBERTURA FINAL
# ------------------------------------------------------------

tabla_cobertura_descriptiva <- diccionario_series |>
  mutate(
    periodo_inicial =
      format(
        min(panel_maestro$periodo),
        "%Y Q%q"
      ),
    
    periodo_final =
      format(
        max(panel_maestro$periodo),
        "%Y Q%q"
      ),
    
    observaciones_niveles =
      nrow(panel_maestro),
    
    observaciones_diferencias =
      nrow(panel_maestro) - 1
  ) |>
  select(
    etiqueta,
    fuente,
    periodo_inicial,
    periodo_final,
    observaciones_niveles,
    observaciones_diferencias
  )

print(
  tabla_cobertura_descriptiva
)


# ------------------------------------------------------------
# 30. GUARDAR TABLAS EN EXCEL
# ------------------------------------------------------------

writexl::write_xlsx(
  list(
    descriptivos_niveles =
      tabla_descriptivos_niveles,
    
    descriptivos_logs =
      tabla_descriptivos_logs,
    
    descriptivos_diferencias =
      tabla_descriptivos_diferencias,
    
    estacionalidad =
      tabla_estacionalidad,
    
    correlaciones =
      tabla_correlaciones,
    
    cobertura =
      tabla_cobertura_descriptiva,
    
    diccionario =
      diccionario_series
  ),
  
  path = here::here(
    "outputs",
    "tables",
    "analisis_descriptivo_series.xlsx"
  )
)


# ------------------------------------------------------------
# 31. GUARDAR OBJETOS COMO RDS
# ------------------------------------------------------------

saveRDS(
  tabla_descriptivos_completa,
  
  here::here(
    "outputs",
    "tables",
    "tabla_descriptivos_completa.rds"
  )
)

saveRDS(
  tabla_estacionalidad,
  
  here::here(
    "outputs",
    "tables",
    "tabla_estacionalidad.rds"
  )
)

saveRDS(
  matriz_correlaciones,
  
  here::here(
    "outputs",
    "tables",
    "matriz_correlaciones_diferencias.rds"
  )
)


# ------------------------------------------------------------
# 32. MENSAJE FINAL
# ------------------------------------------------------------

cat(
  "\n============================================================",
  "\nANÁLISIS DESCRIPTIVO FINALIZADO CORRECTAMENTE",
  "\n============================================================",
  
  "\nPeríodo:",
  format(
    min(panel_maestro$periodo),
    "%Y Q%q"
  ),
  "a",
  format(
    max(panel_maestro$periodo),
    "%Y Q%q"
  ),
  
  "\nObservaciones en niveles:",
  nrow(panel_maestro),
  
  "\nObservaciones en diferencias:",
  sum(
    complete.cases(
      panel_maestro[
        diccionario_series$
          variable_diferencia
      ]
    )
  ),
  
  "\nSeries analizadas:",
  nrow(diccionario_series),
  
  "\n\nArchivos generados:",
  
  "\n- outputs/tables/analisis_descriptivo_series.xlsx",
  
  "\n- figures/figure_series_levels.png",
  
  "\n- figures/figure_log_series.png",
  
  "\n- figures/figure_first_differences.png",
  
  "\n- figures/figure_index_base_2004q1.png",
  
  "\n- figures/figure_import_function.png",
  
  "\n- figures/figure_export_function.png",
  
  "\n- figures/figure_seasonality_boxplots.png",
  
  "\n============================================================",
  "\n"
)

