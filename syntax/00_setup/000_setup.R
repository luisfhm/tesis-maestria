# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 000_setup.R
# Objetivo:
#   - Configurar rutas del proyecto
#   - Crear carpetas necesarias
#   - Instalar y cargar paquetes
#   - Definir opciones generales de trabajo
#
# Regla:
#   Este script no modifica datos.
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. Limpiar entorno
# ------------------------------------------------------------------------------

rm(list = ls(all.names = TRUE))

graphics.off()

invisible(gc())


# ------------------------------------------------------------------------------
# 1. Paquetes mínimos para configurar el proyecto
# ------------------------------------------------------------------------------

paquetes_setup <- c(
  "here",
  "fs"
)

paquetes_faltantes_setup <- paquetes_setup[
  !paquetes_setup %in% rownames(installed.packages())
]

if (length(paquetes_faltantes_setup) > 0) {
  message(
    "Instalando paquetes de configuración: ",
    paste(paquetes_faltantes_setup, collapse = ", ")
  )

  install.packages(
    paquetes_faltantes_setup,
    dependencies = TRUE,
    repos = "https://cloud.r-project.org"
  )
}

invisible(
  lapply(
    paquetes_setup,
    library,
    character.only = TRUE
  )
)


# ------------------------------------------------------------------------------
# 2. Localizar la raíz del proyecto
# ------------------------------------------------------------------------------

# here() busca un archivo que identifique la raíz del proyecto.
# Se recomienda crear un archivo .here en TESIS-MAESTRIA.

ruta_proyecto <- here::here()

message("Raíz del proyecto: ", ruta_proyecto)


# Validación básica
archivos_raiz <- fs::dir_ls(
  path = ruta_proyecto,
  type = "any",
  recurse = FALSE
)

if (!any(grepl("syntax", archivos_raiz, ignore.case = TRUE))) {
  warning(
    "No se encontró la carpeta 'syntax' en la raíz detectada. ",
    "Verifica que abriste el proyecto desde TESIS-MAESTRIA."
  )
}


# ------------------------------------------------------------------------------
# 3. Definir estructura de carpetas
# ------------------------------------------------------------------------------

rutas <- list(

  # Raíz
  proyecto = ruta_proyecto,

  # Datos
  data = fs::path(ruta_proyecto, "data"),
  data_legacy = fs::path(ruta_proyecto, "data", "legacy"),
  data_raw = fs::path(ruta_proyecto, "data", "raw"),
  data_processed = fs::path(ruta_proyecto, "data", "processed"),
  data_final = fs::path(ruta_proyecto, "data", "final"),

  # Código
  syntax = fs::path(ruta_proyecto, "syntax"),
  syntax_legacy = fs::path(ruta_proyecto, "syntax", "legacy"),
  syntax_setup = fs::path(ruta_proyecto, "syntax", "00_setup"),
  syntax_exploracion = fs::path(ruta_proyecto, "syntax", "01_exploracion"),
  syntax_limpieza = fs::path(ruta_proyecto, "syntax", "02_limpieza"),
  syntax_construccion = fs::path(ruta_proyecto, "syntax", "03_construccion"),
  syntax_descriptivos = fs::path(ruta_proyecto, "syntax", "04_descriptivos"),
  syntax_modelos = fs::path(ruta_proyecto, "syntax", "05_modelos"),
  syntax_robustez = fs::path(ruta_proyecto, "syntax", "06_robustez"),

  # Resultados
  output = fs::path(ruta_proyecto, "output"),
  output_figures = fs::path(ruta_proyecto, "output", "figures"),
  output_tables = fs::path(ruta_proyecto, "output", "tables"),
  output_maps = fs::path(ruta_proyecto, "output", "maps"),
  output_logs = fs::path(ruta_proyecto, "output", "logs")
)


# ------------------------------------------------------------------------------
# 4. Crear carpetas faltantes
# ------------------------------------------------------------------------------

rutas_a_crear <- unname(unlist(rutas))

for (ruta in rutas_a_crear) {
  if (!fs::dir_exists(ruta)) {
    fs::dir_create(
      path = ruta,
      recurse = TRUE
    )

    message("Carpeta creada: ", ruta)
  }
}


# ------------------------------------------------------------------------------
# 5. Crear subcarpetas de datos raw
# ------------------------------------------------------------------------------

subcarpetas_raw <- c(
  "accidentes_c5",
  "hechos_transito_ssc",
  "personas_hechos_transito",
  "vehiculos_hechos_transito",
  "infracciones",
  "fotomultas_pre2019",
  "fotocivicas",
  "radares",
  "infovial",
  "peatones_ciclistas",
  "depositos_vehiculares",
  "accidentes_alcaldias",
  "vehiculos_registrados",
  "controles"
)

rutas_raw <- fs::path(
  rutas$data_raw,
  subcarpetas_raw
)

for (ruta in rutas_raw) {
  if (!fs::dir_exists(ruta)) {
    fs::dir_create(
      path = ruta,
      recurse = TRUE
    )

    message("Carpeta raw creada: ", ruta)
  }
}


# ------------------------------------------------------------------------------
# 6. Paquetes generales de la tesis
# ------------------------------------------------------------------------------

paquetes_tesis <- c(

  # Manipulación de datos
  "tidyverse",
  "data.table",
  "janitor",
  "lubridate",
  "readxl",
  "arrow",

  # Archivos y rutas
  "here",
  "fs",

  # Datos espaciales
  "sf",
  "terra",
  "units",

  # Modelos econométricos
  "fixest",
  "modelsummary",
  "broom",

  # Tablas y reportes
  "gt",
  "knitr",

  # Gráficas
  "scales",
  "patchwork"
)

paquetes_faltantes <- paquetes_tesis[
  !paquetes_tesis %in% rownames(installed.packages())
]

if (length(paquetes_faltantes) > 0) {

  message(
    "Los siguientes paquetes no están instalados:\n",
    paste0("  - ", paquetes_faltantes, collapse = "\n")
  )

  respuesta <- readline(
    prompt = "¿Deseas instalarlos ahora? [s/n]: "
  )

  if (tolower(trimws(respuesta)) %in% c("s", "si", "sí", "y", "yes")) {

    install.packages(
      paquetes_faltantes,
      dependencies = TRUE,
      repos = "https://cloud.r-project.org"
    )

  } else {
    warning(
      "No se instalaron los paquetes faltantes. ",
      "Algunos scripts podrían no funcionar."
    )
  }
}


# ------------------------------------------------------------------------------
# 7. Cargar paquetes
# ------------------------------------------------------------------------------

paquetes_disponibles <- paquetes_tesis[
  paquetes_tesis %in% rownames(installed.packages())
]

suppressPackageStartupMessages(
  invisible(
    lapply(
      paquetes_disponibles,
      library,
      character.only = TRUE
    )
  )
)


# ------------------------------------------------------------------------------
# 8. Opciones generales
# ------------------------------------------------------------------------------

options(
  scipen = 999,
  digits = 4,
  dplyr.summarise.inform = FALSE,
  readr.show_col_types = FALSE,
  stringsAsFactors = FALSE
)

Sys.setenv(
  TZ = "America/Mexico_City"
)

set.seed(2026)


# ------------------------------------------------------------------------------
# 9. Función auxiliar para construir rutas
# ------------------------------------------------------------------------------

ruta <- function(...) {
  fs::path(ruta_proyecto, ...)
}


# Ejemplos:
#
# ruta("data", "raw", "infracciones")
# ruta("output", "figures", "grafica_infracciones.png")


# ------------------------------------------------------------------------------
# 10. Función auxiliar para validar archivos
# ------------------------------------------------------------------------------

validar_archivo <- function(path) {

  if (!fs::file_exists(path)) {
    stop(
      "No se encontró el archivo:\n",
      path,
      call. = FALSE
    )
  }

  invisible(TRUE)
}


# Ejemplo:
#
# archivo <- ruta(
#   "data",
#   "raw",
#   "infracciones",
#   "infracciones_2020_2023.csv"
# )
#
# validar_archivo(archivo)


# ------------------------------------------------------------------------------
# 11. Guardar información de la sesión
# ------------------------------------------------------------------------------

archivo_sesion <- fs::path(
  rutas$output_logs,
  paste0(
    "session_info_",
    format(Sys.Date(), "%Y%m%d"),
    ".txt"
  )
)

capture.output(
  sessionInfo(),
  file = archivo_sesion
)


# ------------------------------------------------------------------------------
# 12. Resumen
# ------------------------------------------------------------------------------

message("")
message("==============================================================")
message("Configuración completada")
message("==============================================================")
message("Proyecto:       ", rutas$proyecto)
message("Datos raw:      ", rutas$data_raw)
message("Datos proces.:  ", rutas$data_processed)
message("Datos finales:  ", rutas$data_final)
message("Outputs:        ", rutas$output)
message("R versión:      ", R.version.string)
message("==============================================================")