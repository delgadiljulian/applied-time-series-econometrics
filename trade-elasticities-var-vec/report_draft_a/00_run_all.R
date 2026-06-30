# ============================================================
# TP3 - SERIES DE TIEMPO
# SCRIPT MAESTRO CORREGIDO
#
# Archivo:
#   00_run_all.R
#
# Ubicación:
#   C:/Users/andre/Desktop/TP3_Series_Tiempo/00_run_all.R
#
# Este script:
#   1. Ejecuta los scripts 01 a 13 en el orden correcto.
#   2. Usa el entorno global compartido.
#   3. Evita que rm(list = ls()) borre objetos intermedios.
#   4. Inicializa diagnosticos_candidatos cuando no existe.
#   5. Registra errores, advertencias y duración.
#   6. Verifica las salidas finales del Script 13.
# ============================================================

#=================================================
# FIJAR DIRECTORIO DEL PROYECTO
#=================================================

normalizePath(".")

dir.exists("scripts")

list.files("scripts")

commandArgs()

setwd(
  "C:/Users/julla/GitHub/applied-time-series-econometrics/trade-elasticities-var-vec/report_draft_a"
)

cat("Working directory:\n")
print(getwd())
getwd()

# ------------------------------------------------------------
# 0. SALIR DEL MODO DE DEPURACIÓN
# ------------------------------------------------------------

options(
  error = NULL,
  browserNLdisabled = TRUE,
  scipen = 999,
  stringsAsFactors = FALSE,
  dplyr.summarise.inform = FALSE,
  warn = 1
)


# ------------------------------------------------------------
# 1. LIMPIAR EL ENTORNO ANTES DE CREAR EL MAESTRO
# ------------------------------------------------------------

base::rm(
  list = ls(
    envir = .GlobalEnv,
    all.names = TRUE
  ),
  envir = .GlobalEnv
)

invisible(
  gc()
)


# ------------------------------------------------------------
# 2. CONFIGURACIÓN
# ------------------------------------------------------------

LIMPIAR_SALIDAS <- FALSE

DETENER_EN_ADVERTENCIAS <- FALSE

SEMILLA_GLOBAL <- 20260623L

set.seed(
  SEMILLA_GLOBAL
)


# ------------------------------------------------------------
# 3. ENTORNO DE CONTROL DEL PIPELINE
# ------------------------------------------------------------

.PIPELINE_CTRL <- new.env(
  parent = emptyenv()
)


.PIPELINE_CTRL$limpiar_salidas <-
  LIMPIAR_SALIDAS


.PIPELINE_CTRL$detener_en_advertencias <-
  DETENER_EN_ADVERTENCIAS


.PIPELINE_CTRL$semilla <-
  SEMILLA_GLOBAL


# ------------------------------------------------------------
# 4. DETECTAR EL ARCHIVO ACTIVO
# ------------------------------------------------------------

obtener_archivo_activo <- function() {

  argumentos <- commandArgs(
    trailingOnly = FALSE
  )


  argumento_archivo <- grep(
    pattern = "^--file=",
    x = argumentos,
    value = TRUE
  )


  if (length(argumento_archivo) > 0) {

    ruta <- sub(
      pattern = "^--file=",
      replacement = "",
      x = argumento_archivo[1]
    )


    if (file.exists(ruta)) {

      return(
        normalizePath(
          ruta,
          winslash = "/",
          mustWork = TRUE
        )
      )
    }
  }


  if (
    requireNamespace(
      "rstudioapi",
      quietly = TRUE
    ) &&
    rstudioapi::isAvailable()
  ) {

    ruta <- tryCatch(

      rstudioapi::getActiveDocumentContext()$path,

      error = function(e) {
        ""
      }
    )


    if (
      length(ruta) == 1 &&
      nzchar(ruta) &&
      file.exists(ruta)
    ) {

      return(
        normalizePath(
          ruta,
          winslash = "/",
          mustWork = TRUE
        )
      )
    }
  }


  NA_character_
}


# ------------------------------------------------------------
# 5. DETECTAR LA RAÍZ DEL PROYECTO
# ------------------------------------------------------------

detectar_raiz_proyecto <- function() {

  archivo_activo <- obtener_archivo_activo()


  candidatos <- character()


  if (!is.na(
    archivo_activo
  )) {

    directorio_archivo <- dirname(
      archivo_activo
    )


    candidatos <- c(
      candidatos,
      directorio_archivo
    )
  }


  candidatos <- c(
    candidatos,
    getwd(),
    dirname(
      getwd()
    )
  )


  candidatos <- unique(
    candidatos
  )


  candidatos <- candidatos[
    dir.exists(
      candidatos
    )
  ]


  for (
    candidato in candidatos
  ) {

    candidato <- normalizePath(
      candidato,
      winslash = "/",
      mustWork = TRUE
    )


    if (
      file.exists(
        file.path(
          candidato,
          "00_run_all.R"
        )
      ) &&
      dir.exists(
        file.path(
          candidato,
          "scripts"
        )
      )
    ) {

      return(
        candidato
      )
    }
  }


  stop(
    paste0(
      "No se pudo detectar la raíz del proyecto.\n\n",
      "00_run_all.R debe estar junto a la carpeta scripts.\n\n",
      "Directorio actual:\n",
      getwd()
    ),
    call. = FALSE
  )
}


.PIPELINE_CTRL$raiz <-
  detectar_raiz_proyecto()


setwd(
  .PIPELINE_CTRL$raiz
)


cat(
  "\n============================================================",
  "\nRAÍZ DEL PROYECTO",
  "\n============================================================\n",
  .PIPELINE_CTRL$raiz,
  "\n"
)


# ------------------------------------------------------------
# 6. FIJAR LA RAÍZ PARA HERE
# ------------------------------------------------------------

if (
  requireNamespace(
    "here",
    quietly = TRUE
  ) &&
  file.exists(
    file.path(
      .PIPELINE_CTRL$raiz,
      "00_run_all.R"
    )
  )
) {

  try(
    here::i_am(
      "00_run_all.R"
    ),
    silent = TRUE
  )
}


# ------------------------------------------------------------
# 7. DIRECTORIOS
# ------------------------------------------------------------

.PIPELINE_CTRL$directorio_scripts <- file.path(
  .PIPELINE_CTRL$raiz,
  "scripts"
)


.PIPELINE_CTRL$directorio_outputs <- file.path(
  .PIPELINE_CTRL$raiz,
  "outputs"
)


.PIPELINE_CTRL$directorio_tablas <- file.path(
  .PIPELINE_CTRL$directorio_outputs,
  "tables"
)


.PIPELINE_CTRL$directorio_modelos <- file.path(
  .PIPELINE_CTRL$directorio_outputs,
  "models"
)


.PIPELINE_CTRL$directorio_figuras <- file.path(
  .PIPELINE_CTRL$directorio_outputs,
  "figures"
)


.PIPELINE_CTRL$directorio_logs <- file.path(
  .PIPELINE_CTRL$directorio_outputs,
  "logs"
)


directorios_necesarios <- c(
  .PIPELINE_CTRL$directorio_outputs,
  .PIPELINE_CTRL$directorio_tablas,
  .PIPELINE_CTRL$directorio_modelos,
  .PIPELINE_CTRL$directorio_figuras,
  .PIPELINE_CTRL$directorio_logs
)


for (
  directorio in directorios_necesarios
) {

  dir.create(
    directorio,
    recursive = TRUE,
    showWarnings = FALSE
  )
}


# ------------------------------------------------------------
# 8. LIMPIEZA OPCIONAL DE SALIDAS
# ------------------------------------------------------------

limpiar_directorio <- function(
    directorio
) {

  if (!dir.exists(
    directorio
  )) {

    return(
      invisible(
        NULL
      )
    )
  }


  elementos <- list.files(
    path = directorio,
    full.names = TRUE,
    all.files = TRUE,
    recursive = FALSE
  )


  elementos <- elementos[
    !basename(
      elementos
    ) %in%
      c(
        ".",
        ".."
      )
  ]


  if (length(elementos) > 0) {

    unlink(
      elementos,
      recursive = TRUE,
      force = TRUE
    )
  }


  invisible(
    NULL
  )
}


if (isTRUE(
  .PIPELINE_CTRL$limpiar_salidas
)) {

  cat(
    "\n============================================================",
    "\nLIMPIANDO SALIDAS ANTERIORES",
    "\n============================================================\n"
  )


  limpiar_directorio(
    .PIPELINE_CTRL$directorio_tablas
  )


  limpiar_directorio(
    .PIPELINE_CTRL$directorio_modelos
  )


  limpiar_directorio(
    .PIPELINE_CTRL$directorio_figuras
  )


  dir.create(
    .PIPELINE_CTRL$directorio_tablas,
    recursive = TRUE,
    showWarnings = FALSE
  )


  dir.create(
    .PIPELINE_CTRL$directorio_modelos,
    recursive = TRUE,
    showWarnings = FALSE
  )


  dir.create(
    .PIPELINE_CTRL$directorio_figuras,
    recursive = TRUE,
    showWarnings = FALSE
  )


  cat(
    "Salidas anteriores eliminadas.\n"
  )

} else {

  cat(
    "\n============================================================",
    "\nSE CONSERVAN LAS SALIDAS EXISTENTES",
    "\n============================================================\n"
  )
}


# ------------------------------------------------------------
# 9. LISTA EXACTA DE SCRIPTS
# ------------------------------------------------------------

.PIPELINE_CTRL$scripts <- c(

  "script01_load_data.R",

  "script02_daily_itcrm.R",

  "script03_ipm_ipx_gdp.R",

  "script04_commodities.R",

  "script05_partner_gdp.R",

  "script06_panel.R",

  "script07_seasonality.R",

  "script08_unit_roots.R",

  "script09_engle_granger.R",

  "script10_selection_diagnostics.R",

  "script11_johansen_vecm.R",

  "script12_svar.R",

  "script13_compilation.R"
)


.PIPELINE_CTRL$rutas_scripts <- file.path(
  .PIPELINE_CTRL$directorio_scripts,
  .PIPELINE_CTRL$scripts
)


# ------------------------------------------------------------
# 10. VERIFICAR LOS ARCHIVOS
# ------------------------------------------------------------

scripts_existentes <- file.exists(
  .PIPELINE_CTRL$rutas_scripts
)


if (any(
  !scripts_existentes
)) {

  scripts_faltantes <- .PIPELINE_CTRL$scripts[
    !scripts_existentes
  ]


  stop(
    paste0(
      "Faltan los siguientes scripts:\n\n",
      paste(
        scripts_faltantes,
        collapse = "\n"
      ),
      "\n\nCarpeta revisada:\n",
      .PIPELINE_CTRL$directorio_scripts
    ),
    call. = FALSE
  )
}


.PIPELINE_CTRL$rutas_scripts <- normalizePath(
  .PIPELINE_CTRL$rutas_scripts,
  winslash = "/",
  mustWork = TRUE
)


inventario_scripts <- data.frame(

  paso =
    seq_along(
      .PIPELINE_CTRL$scripts
    ),

  script =
    .PIPELINE_CTRL$scripts,

  ruta =
    .PIPELINE_CTRL$rutas_scripts,

  existe =
    scripts_existentes,

  stringsAsFactors =
    FALSE
)


cat(
  "\n============================================================",
  "\nORDEN DE EJECUCIÓN",
  "\n============================================================\n"
)


print(
  inventario_scripts[
    c(
      "paso",
      "script",
      "existe"
    )
  ],
  row.names = FALSE
)


write.csv(

  inventario_scripts,

  file = file.path(
    .PIPELINE_CTRL$directorio_logs,
    "00_inventario_scripts.csv"
  ),

  row.names = FALSE,

  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------
# 11. IMPEDIR QUE LOS SCRIPTS BORREN EL ENTORNO
# ------------------------------------------------------------

# Guardar cualquier objeto global previo llamado rm o remove.

.PIPELINE_CTRL$existia_rm_global <- exists(
  "rm",
  envir = .GlobalEnv,
  inherits = FALSE
)


if (
  .PIPELINE_CTRL$existia_rm_global
) {

  .PIPELINE_CTRL$rm_global_anterior <- get(
    "rm",
    envir = .GlobalEnv,
    inherits = FALSE
  )
}


.PIPELINE_CTRL$existia_remove_global <- exists(
  "remove",
  envir = .GlobalEnv,
  inherits = FALSE
)


if (
  .PIPELINE_CTRL$existia_remove_global
) {

  .PIPELINE_CTRL$remove_global_anterior <- get(
    "remove",
    envir = .GlobalEnv,
    inherits = FALSE
  )
}


rm_pipeline_seguro <- function(
    ...,
    list = character(),
    pos = -1,
    envir = as.environment(pos),
    inherits = FALSE
) {

  cat(
    "\n[PIPELINE] Se omitió rm() para conservar ",
    "los objetos creados por scripts anteriores.\n",
    sep = ""
  )


  invisible(
    NULL
  )
}


assign(
  "rm",
  rm_pipeline_seguro,
  envir = .GlobalEnv
)


assign(
  "remove",
  rm_pipeline_seguro,
  envir = .GlobalEnv
)


# ------------------------------------------------------------
# 12. OBJETOS COMPARTIDOS INICIALES
# ------------------------------------------------------------

# script02_daily_itcrm.R utiliza este objeto.
# Se inicializa como lista vacía para evitar que falte.
# Si script01 lo genera, su valor será reemplazado.

if (
  !exists(
    "diagnosticos_candidatos",
    envir = .GlobalEnv,
    inherits = FALSE
  )
) {

  assign(
    "diagnosticos_candidatos",
    list(),
    envir = .GlobalEnv
  )
}


# Variables de rutas disponibles para todos los scripts.

assign(
  "RAIZ_PROYECTO",
  .PIPELINE_CTRL$raiz,
  envir = .GlobalEnv
)


assign(
  "DIRECTORIO_SCRIPTS",
  .PIPELINE_CTRL$directorio_scripts,
  envir = .GlobalEnv
)


assign(
  "DIRECTORIO_OUTPUTS",
  .PIPELINE_CTRL$directorio_outputs,
  envir = .GlobalEnv
)


assign(
  "DIRECTORIO_TABLAS",
  .PIPELINE_CTRL$directorio_tablas,
  envir = .GlobalEnv
)


assign(
  "DIRECTORIO_MODELOS",
  .PIPELINE_CTRL$directorio_modelos,
  envir = .GlobalEnv
)


assign(
  "DIRECTORIO_FIGURAS",
  .PIPELINE_CTRL$directorio_figuras,
  envir = .GlobalEnv
)


# ------------------------------------------------------------
# 13. ARCHIVOS DE REGISTRO
# ------------------------------------------------------------

marca_tiempo <- format(
  Sys.time(),
  "%Y%m%d_%H%M%S"
)


.PIPELINE_CTRL$archivo_log <- file.path(
  .PIPELINE_CTRL$directorio_logs,
  paste0(
    "00_run_all_",
    marca_tiempo,
    ".log"
  )
)


.PIPELINE_CTRL$archivo_manifiesto <- file.path(
  .PIPELINE_CTRL$directorio_logs,
  paste0(
    "00_manifiesto_ejecucion_",
    marca_tiempo,
    ".csv"
  )
)


.PIPELINE_CTRL$archivo_validacion <- file.path(
  .PIPELINE_CTRL$directorio_logs,
  paste0(
    "00_validacion_salidas_",
    marca_tiempo,
    ".csv"
  )
)


.PIPELINE_CTRL$archivo_session_info <- file.path(
  .PIPELINE_CTRL$directorio_logs,
  paste0(
    "00_session_info_",
    marca_tiempo,
    ".txt"
  )
)


.PIPELINE_CTRL$archivo_confirmacion <- file.path(
  .PIPELINE_CTRL$directorio_logs,
  paste0(
    "00_pipeline_completado_",
    marca_tiempo,
    ".txt"
  )
)


# ------------------------------------------------------------
# 14. RESTAURAR LOS OBJETOS RM AL FINAL
# ------------------------------------------------------------

restaurar_rm_global <- function() {

  if (
    .PIPELINE_CTRL$existia_rm_global
  ) {

    assign(
      "rm",
      .PIPELINE_CTRL$rm_global_anterior,
      envir = .GlobalEnv
    )

  } else if (
    exists(
      "rm",
      envir = .GlobalEnv,
      inherits = FALSE
    )
  ) {

    base::rm(
      list = "rm",
      envir = .GlobalEnv
    )
  }


  if (
    .PIPELINE_CTRL$existia_remove_global
  ) {

    assign(
      "remove",
      .PIPELINE_CTRL$remove_global_anterior,
      envir = .GlobalEnv
    )

  } else if (
    exists(
      "remove",
      envir = .GlobalEnv,
      inherits = FALSE
    )
  ) {

    base::rm(
      list = "remove",
      envir = .GlobalEnv
    )
  }


  invisible(
    NULL
  )
}


# ------------------------------------------------------------
# 15. EJECUTAR UN SCRIPT EN EL ENTORNO GLOBAL
# ------------------------------------------------------------

ejecutar_script <- function(
    paso,
    archivo
) {

  nombre_script <- basename(
    archivo
  )


  inicio <- Sys.time()

  advertencias <- character()

  estado <- "OK"

  mensaje_error <- NA_character_


  objetos_antes <- ls(
    envir = .GlobalEnv,
    all.names = TRUE
  )


  cat(
    "\n\n============================================================",
    "\nINICIO DEL SCRIPT ",
    sprintf(
      "%02d",
      paso
    ),
    "\n",
    nombre_script,
    "\n============================================================\n",
    sep = ""
  )


  directorio_anterior <- getwd()


  tryCatch(

    withCallingHandlers(

      {

        setwd(
          .PIPELINE_CTRL$raiz
        )


        set.seed(
          .PIPELINE_CTRL$semilla +
            paso
        )


        # Antes de ejecutar el Script 02, garantizar
        # que el acumulador exista.

        if (
          paso == 2 &&
          !exists(
            "diagnosticos_candidatos",
            envir = .GlobalEnv,
            inherits = FALSE
          )
        ) {

          assign(
            "diagnosticos_candidatos",
            list(),
            envir = .GlobalEnv
          )
        }


        source(
          file = archivo,
          local = .GlobalEnv,
          echo = FALSE,
          print.eval = FALSE,
          chdir = FALSE,
          encoding = "UTF-8"
        )
      },

      warning = function(w) {

        mensaje <- conditionMessage(
          w
        )


        advertencias <<- c(
          advertencias,
          mensaje
        )


        cat(
          "\n[ADVERTENCIA EN ",
          nombre_script,
          "]\n",
          mensaje,
          "\n",
          sep = ""
        )


        if (
          isTRUE(
            .PIPELINE_CTRL$detener_en_advertencias
          )
        ) {

          stop(
            mensaje,
            call. = FALSE
          )
        }


        invokeRestart(
          "muffleWarning"
        )
      }
    ),

    error = function(e) {

      estado <<- "ERROR"

      mensaje_error <<- conditionMessage(
        e
      )
    },

    finally = {

      setwd(
        directorio_anterior
      )
    }
  )


  fin <- Sys.time()


  duracion <- as.numeric(
    difftime(
      fin,
      inicio,
      units = "secs"
    )
  )


  advertencias <- unique(
    advertencias
  )


  objetos_despues <- ls(
    envir = .GlobalEnv,
    all.names = TRUE
  )


  objetos_nuevos <- setdiff(
    objetos_despues,
    objetos_antes
  )


  texto_advertencias <- if (
    length(advertencias) == 0
  ) {

    NA_character_

  } else {

    paste(
      advertencias,
      collapse = " || "
    )
  }


  texto_objetos_nuevos <- if (
    length(objetos_nuevos) == 0
  ) {

    NA_character_

  } else {

    paste(
      objetos_nuevos,
      collapse = " | "
    )
  }


  if (
    estado ==
    "OK"
  ) {

    cat(
      "\n------------------------------------------------------------",
      "\nSCRIPT ",
      sprintf(
        "%02d",
        paso
      ),
      " FINALIZADO CORRECTAMENTE",
      "\nDuración: ",
      round(
        duracion,
        2
      ),
      " segundos",
      "\nAdvertencias: ",
      length(
        advertencias
      ),
      "\nObjetos nuevos: ",
      length(
        objetos_nuevos
      ),
      "\nObjetos globales disponibles: ",
      length(
        objetos_despues
      ),
      "\n------------------------------------------------------------\n",
      sep = ""
    )

  } else {

    cat(
      "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!",
      "\nERROR EN SCRIPT ",
      sprintf(
        "%02d",
        paso
      ),
      "\nArchivo: ",
      nombre_script,
      "\nMensaje: ",
      mensaje_error,
      "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n",
      sep = ""
    )
  }


  list(

    exitoso =
      estado ==
      "OK",

    registro =
      data.frame(

        paso =
          paso,

        script =
          nombre_script,

        ruta =
          archivo,

        estado =
          estado,

        inicio =
          format(
            inicio,
            "%Y-%m-%d %H:%M:%S"
          ),

        fin =
          format(
            fin,
            "%Y-%m-%d %H:%M:%S"
          ),

        duracion_segundos =
          round(
            duracion,
            2
          ),

        numero_advertencias =
          length(
            advertencias
          ),

        advertencias =
          texto_advertencias,

        numero_objetos_nuevos =
          length(
            objetos_nuevos
          ),

        objetos_nuevos =
          texto_objetos_nuevos,

        error =
          mensaje_error,

        stringsAsFactors =
          FALSE
      )
  )
}


# ------------------------------------------------------------
# 16. FUNCIÓN PRINCIPAL
# ------------------------------------------------------------

ejecutar_pipeline <- function() {

  nivel_sink_inicial <- sink.number()


  sink(
    file = .PIPELINE_CTRL$archivo_log,
    append = FALSE,
    split = TRUE
  )


  on.exit(
    {

      while (
        sink.number() >
        nivel_sink_inicial
      ) {

        sink()
      }


      restaurar_rm_global()
    },
    add = TRUE
  )


  inicio_pipeline <- Sys.time()


  manifiesto <- data.frame(

    paso =
      integer(),

    script =
      character(),

    ruta =
      character(),

    estado =
      character(),

    inicio =
      character(),

    fin =
      character(),

    duracion_segundos =
      numeric(),

    numero_advertencias =
      integer(),

    advertencias =
      character(),

    numero_objetos_nuevos =
      integer(),

    objetos_nuevos =
      character(),

    error =
      character(),

    stringsAsFactors =
      FALSE
  )


  cat(
    "\n============================================================",
    "\nPIPELINE MAESTRO DEL TP3",
    "\n============================================================",
    "\nInicio: ",
    format(
      inicio_pipeline,
      "%Y-%m-%d %H:%M:%S"
    ),
    "\nRaíz: ",
    .PIPELINE_CTRL$raiz,
    "\nScripts: ",
    length(
      .PIPELINE_CTRL$scripts
    ),
    "\nEntorno compartido: .GlobalEnv",
    "\nObjeto diagnosticos_candidatos inicializado: ",
    exists(
      "diagnosticos_candidatos",
      envir = .GlobalEnv,
      inherits = FALSE
    ),
    "\n============================================================\n",
    sep = ""
  )


  for (
    i in seq_along(
      .PIPELINE_CTRL$rutas_scripts
    )
  ) {

    resultado <- ejecutar_script(

      paso =
        i,

      archivo =
        .PIPELINE_CTRL$rutas_scripts[i]
    )


    manifiesto <- rbind(
      manifiesto,
      resultado$registro
    )


    write.csv(

      manifiesto,

      file =
        .PIPELINE_CTRL$archivo_manifiesto,

      row.names =
        FALSE,

      fileEncoding =
        "UTF-8"
    )


    if (!isTRUE(
      resultado$exitoso
    )) {

      stop(
        paste0(
          "El pipeline se detuvo en el Script ",
          sprintf(
            "%02d",
            i
          ),
          ": ",
          resultado$registro$script,
          "\n\n",
          "Mensaje del error:\n",
          resultado$registro$error,
          "\n\n",
          "Revisa el log:\n",
          .PIPELINE_CTRL$archivo_log
        ),
        call. = FALSE
      )
    }
  }


  # ----------------------------------------------------------
  # 17. VALIDAR SALIDAS FINALES
  # ----------------------------------------------------------

  salidas_relativas <- c(

    "outputs/tables/13_resultados_finales_tp3.xlsx",

    "outputs/tables/13_decision_final_modelos.csv",

    "outputs/tables/13_elasticidades_largo_plazo.csv",

    "outputs/tables/13_comparacion_elasticidades_metodos.csv",

    "outputs/tables/13_ajustes_ecm_vecm.csv",

    "outputs/tables/13_irf_principales.csv",

    "outputs/tables/13_fevd_h12.csv",

    "outputs/tables/13_estimaciones_preferidas.csv",

    "outputs/tables/13_catalogo_salidas.csv",

    "outputs/models/ajuste_ecm_correcto.rds",

    "outputs/models/resultados_finales_tp3.rds",

    "outputs/models/estimaciones_preferidas_tp3.rds",

    "outputs/models/decision_final_modelos_tp3.rds",

    "outputs/models/ajustes_ecm_vecm_final.rds",

    "outputs/models/13_resumen_resultados_finales.txt",

    "figures/13_elasticidades_largo_plazo.png",

    "figures/13_coeficientes_ajuste.png",

    "figures/13_irf_horizontes_seleccionados.png",

    "figures/13_fevd_h12.png"
  )


  salidas_completas <- file.path(
    .PIPELINE_CTRL$raiz,
    salidas_relativas
  )


  existen <- file.exists(
    salidas_completas
  )


  tamanios <- rep(
    NA_real_,
    length(
      salidas_completas
    )
  )


  tamanios[existen] <- as.numeric(
    file.info(
      salidas_completas[existen]
    )$size
  )


  validacion_salidas <- data.frame(

    archivo =
      basename(
        salidas_completas
      ),

    ruta_relativa =
      salidas_relativas,

    existe =
      existen,

    tamanio_kb =
      round(
        tamanios /
          1024,
        3
      ),

    valida =
      existen &
      !is.na(
        tamanios
      ) &
      tamanios >
      0,

    stringsAsFactors =
      FALSE
  )


  write.csv(

    validacion_salidas,

    file =
      .PIPELINE_CTRL$archivo_validacion,

    row.names =
      FALSE,

    fileEncoding =
      "UTF-8"
  )


  cat(
    "\n\n============================================================",
    "\nVALIDACIÓN DE SALIDAS FINALES",
    "\n============================================================\n"
  )


  print(
    validacion_salidas,
    row.names = FALSE
  )


  if (any(
    !validacion_salidas$valida
  )) {

    salidas_invalidas <- validacion_salidas$ruta_relativa[
      !validacion_salidas$valida
    ]


    stop(
      paste0(
        "Faltan salidas finales o existen archivos vacíos:\n\n",
        paste(
          salidas_invalidas,
          collapse = "\n"
        )
      ),
      call. = FALSE
    )
  }


  capture.output(

    sessionInfo(),

    file =
      .PIPELINE_CTRL$archivo_session_info
  )


  fin_pipeline <- Sys.time()


  duracion_minutos <- as.numeric(
    difftime(
      fin_pipeline,
      inicio_pipeline,
      units = "mins"
    )
  )


  texto_confirmacion <- c(

    "============================================================",

    "PIPELINE TP3 COMPLETADO CORRECTAMENTE",

    "============================================================",

    paste0(
      "Inicio: ",
      format(
        inicio_pipeline,
        "%Y-%m-%d %H:%M:%S"
      )
    ),

    paste0(
      "Finalización: ",
      format(
        fin_pipeline,
        "%Y-%m-%d %H:%M:%S"
      )
    ),

    paste0(
      "Duración total en minutos: ",
      round(
        duracion_minutos,
        2
      )
    ),

    paste0(
      "Scripts ejecutados: ",
      nrow(
        manifiesto
      )
    ),

    paste0(
      "Scripts correctos: ",
      sum(
        manifiesto$estado ==
          "OK"
      )
    ),

    paste0(
      "Advertencias: ",
      sum(
        manifiesto$numero_advertencias
      )
    ),

    paste0(
      "Salidas verificadas: ",
      sum(
        validacion_salidas$valida
      )
    ),

    paste0(
      "Log: ",
      .PIPELINE_CTRL$archivo_log
    ),

    "============================================================"
  )


  writeLines(

    texto_confirmacion,

    con =
      .PIPELINE_CTRL$archivo_confirmacion
  )


  cat(
    "\n\n============================================================",
    "\nPIPELINE COMPLETADO CORRECTAMENTE",
    "\n============================================================",

    "\nScripts ejecutados: ",
    nrow(
      manifiesto
    ),

    "\nScripts correctos: ",
    sum(
      manifiesto$estado ==
        "OK"
    ),

    "\nAdvertencias: ",
    sum(
      manifiesto$numero_advertencias
    ),

    "\nSalidas verificadas: ",
    sum(
      validacion_salidas$valida
    ),

    "\nDuración total: ",
    round(
      duracion_minutos,
      2
    ),
    " minutos",

    "\n\nLog:\n",
    .PIPELINE_CTRL$archivo_log,

    "\n\nManifiesto:\n",
    .PIPELINE_CTRL$archivo_manifiesto,

    "\n\nValidación:\n",
    .PIPELINE_CTRL$archivo_validacion,

    "\n============================================================\n",
    sep = ""
  )


  invisible(

    list(

      manifiesto =
        manifiesto,

      validacion =
        validacion_salidas,

      log =
        .PIPELINE_CTRL$archivo_log
    )
  )
}


# ------------------------------------------------------------
# 18. EJECUTAR EL PIPELINE
# ------------------------------------------------------------

resultado_pipeline <- ejecutar_pipeline()


# ============================================================
# REVISAR ADVERTENCIAS DEL ÚLTIMO PIPELINE
# ============================================================

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# ------------------------------------------------------------
# Carpeta de logs del proyecto
# ------------------------------------------------------------

carpeta_logs <- file.path(
  getwd(),
  "outputs",
  "logs"
)

if (!dir.exists(carpeta_logs)) {

  stop(
    paste0(
      "La carpeta de logs no existe:\n",
      carpeta_logs
    )
  )

}

# ------------------------------------------------------------
# Buscar el manifiesto más reciente
# ------------------------------------------------------------

archivos_manifiesto <- list.files(
  path = carpeta_logs,
  pattern = "^00_manifiesto_ejecucion_.*\\.csv$",
  full.names = TRUE
)

if (length(archivos_manifiesto) == 0) {

  stop(
    paste0(
      "No se encontraron manifiestos de ejecución en:\n",
      carpeta_logs
    )
  )

}

archivo_mas_reciente <- archivos_manifiesto[
  which.max(file.info(archivos_manifiesto)$mtime)
]

cat(
  "\n============================================================",
  "\nMANIFIESTO ANALIZADO",
  "\n============================================================\n",
  archivo_mas_reciente,
  "\n\n"
)

# ------------------------------------------------------------
# Leer manifiesto
# ------------------------------------------------------------

manifiesto <- readr::read_csv(
  archivo_mas_reciente,
  show_col_types = FALSE
)

# ------------------------------------------------------------
# Resumen de advertencias
# ------------------------------------------------------------

resumen_advertencias <- manifiesto |>
  dplyr::select(
    paso,
    script,
    estado,
    numero_advertencias,
    advertencias
  ) |>
  dplyr::arrange(
    dplyr::desc(numero_advertencias)
  )

cat(
  "\n============================================================",
  "\nRESUMEN DE ADVERTENCIAS",
  "\n============================================================\n"
)

print(
  resumen_advertencias,
  n = Inf,
  width = Inf
)

# ------------------------------------------------------------
# Detalle de advertencias
# ------------------------------------------------------------

detalle_advertencias <- manifiesto |>
  dplyr::filter(
    numero_advertencias > 0,
    !is.na(advertencias)
  ) |>
  tidyr::separate_rows(
    advertencias,
    sep = " \\|\\| "
  ) |>
  dplyr::mutate(
    advertencias = stringr::str_squish(advertencias)
  ) |>
  dplyr::count(
    paso,
    script,
    advertencias,
    sort = TRUE,
    name = "frecuencia"
  )

cat(
  "\n============================================================",
  "\nDETALLE DE ADVERTENCIAS",
  "\n============================================================\n"
)

print(
  detalle_advertencias,
  n = Inf,
  width = Inf
)

# ------------------------------------------------------------
# Guardar reporte
# ------------------------------------------------------------

archivo_reporte <- file.path(
  carpeta_logs,
  "00_resumen_advertencias.csv"
)

readr::write_csv(
  detalle_advertencias,
  archivo_reporte
)

cat(
  "\n============================================================",
  "\nREPORTE GENERADO",
  "\n============================================================\n",
  archivo_reporte,
  "\n"
)
