# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 201_limpiar_incidentes_c5.R
# Objetivo:
#   - Leer y unir los archivos de incidentes viales C5
#   - Homologar variables entre periodos
#   - Eliminar duplicados derivados del traslape temporal entre archivos
#   - Generar geometría espacial a partir de longitud y latitud
#
# Entrada:
#   data/raw/01_incidentes_c5/*.csv
#
# Salida:
#   data/processed/incidentes_c5_limpios.gpkg
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------------------------

source(
  here::here(
    "syntax",
    "00_setup",
    "000_setup.R"
  ),
  encoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 1. Localizar archivos C5
# ------------------------------------------------------------------------------

ruta_c5 <- fs::path(
  rutas$data_raw,
  "01_incidentes_c5"
)

archivos_c5 <- fs::dir_ls(
  path = ruta_c5,
  regexp = "\\.csv$",
  type = "file"
)

if (length(archivos_c5) == 0) {
  stop("No se encontraron archivos C5.")
}

message("Archivos C5 encontrados: ", length(archivos_c5))


# ------------------------------------------------------------------------------
# 2. Función de lectura y homologación
# ------------------------------------------------------------------------------

leer_c5 <- function(archivo) {

  datos <- readr::read_csv(
    archivo,
    show_col_types = FALSE,
    progress = FALSE
  )

  # Homologar nombre de colonia
  if ("colonia_catalogo" %in% names(datos)) {

    datos <- datos |>
      dplyr::mutate(
        colonia_fuente = colonia_catalogo
      )

  } else if ("colonia" %in% names(datos)) {

    datos <- datos |>
      dplyr::mutate(
        colonia_fuente = colonia
      )

  } else {

    datos <- datos |>
      dplyr::mutate(
        colonia_fuente = NA_character_
      )
  }

  # Homologar alcaldía de catálogo si existe
  if ("alcaldia_catalogo" %in% names(datos)) {

    datos <- datos |>
      dplyr::mutate(
        alcaldia_fuente = alcaldia_catalogo
      )

  } else {

    datos <- datos |>
      dplyr::mutate(
        alcaldia_fuente = alcaldia_inicio
      )
  }

  datos |>
    dplyr::transmute(
      folio,
      fecha_creacion,
      hora_creacion,
      dia_semana,
      fecha_cierre,
      hora_cierre,
      tipo_incidente_c4,
      incidente_c4,
      alcaldia_inicio,
      alcaldia_cierre,
      colonia_fuente,
      alcaldia_fuente,
      longitud,
      latitud,
      codigo_cierre,
      clas_con_f_alarma,
      tipo_entrada,
      archivo_origen = fs::path_file(archivo)
    )
}


# ------------------------------------------------------------------------------
# 3. Leer y unir archivos
# ------------------------------------------------------------------------------

incidentes_raw <- purrr::map_dfr(
  archivos_c5,
  leer_c5
)

message(
  "Observaciones antes de deduplicar: ",
  nrow(incidentes_raw)
)


# ------------------------------------------------------------------------------
# 4. Diagnóstico de duplicados por folio
# ------------------------------------------------------------------------------

duplicados_folio <- incidentes_raw |>
  dplyr::count(
    folio,
    name = "n"
  ) |>
  dplyr::filter(
    n > 1
  )

message(
  "Folios repetidos entre archivos: ",
  nrow(duplicados_folio)
)


# ------------------------------------------------------------------------------
# 5. Resolver traslapes entre archivos
# ------------------------------------------------------------------------------

# El archivo más reciente se considera preferible cuando un mismo folio
# aparece en más de un corte de la base.
#
# Priorizamos:
#   2022_2024 > 2022_2023 > 2019_2021 > 2016_2018 > 2014_2015

incidentes_raw <- incidentes_raw |>
  dplyr::mutate(
    prioridad_archivo = dplyr::case_when(
      archivo_origen == "inViales_2022_2024.csv" ~ 5L,
      archivo_origen == "inViales_2022_2023.csv" ~ 4L,
      archivo_origen == "inViales_2019_2021.csv" ~ 3L,
      archivo_origen == "inViales_2016_2018.csv" ~ 2L,
      archivo_origen == "inViales_2014_2015.csv" ~ 1L,
      TRUE ~ 0L
    )
  ) |>
  dplyr::arrange(
    folio,
    dplyr::desc(prioridad_archivo)
  ) |>
  dplyr::distinct(
    folio,
    .keep_all = TRUE
  )


message(
  "Observaciones después de deduplicar: ",
  nrow(incidentes_raw)
)


# ------------------------------------------------------------------------------
# 6. Validar coordenadas
# ------------------------------------------------------------------------------

incidentes_raw <- incidentes_raw |>
  dplyr::mutate(
    coord_valida =
      !is.na(longitud) &
      !is.na(latitud) &
      dplyr::between(longitud, -100, -98) &
      dplyr::between(latitud, 18, 20)
  )

message(
  "Coordenadas válidas: ",
  sum(incidentes_raw$coord_valida)
)

message(
  "Coordenadas no válidas: ",
  sum(!incidentes_raw$coord_valida)
)


# ------------------------------------------------------------------------------
# 7. Conservar observaciones georreferenciables
# ------------------------------------------------------------------------------

incidentes_geo <- incidentes_raw |>
  dplyr::filter(
    coord_valida
  )


# ------------------------------------------------------------------------------
# 8. Crear objeto espacial
# ------------------------------------------------------------------------------

incidentes_geo <- sf::st_as_sf(
  incidentes_geo,
  coords = c(
    "longitud",
    "latitud"
  ),
  crs = 4326,
  remove = FALSE
)


# ------------------------------------------------------------------------------
# 9. Transformar a CRS de trabajo
# ------------------------------------------------------------------------------

incidentes_geo <- incidentes_geo |>
  sf::st_transform(32614)


# ------------------------------------------------------------------------------
# 10. Variables temporales auxiliares
# ------------------------------------------------------------------------------

incidentes_geo <- incidentes_geo |>
  dplyr::mutate(
    mes = lubridate::floor_date(
      fecha_creacion,
      unit = "month"
    ),
    anio = lubridate::year(
      fecha_creacion
    )
  )


# ------------------------------------------------------------------------------
# 11. Diagnóstico temporal
# ------------------------------------------------------------------------------

message("")
message("==============================================================")
message("INCIDENTES C5 LIMPIOS")
message("==============================================================")
message("Observaciones:      ", nrow(incidentes_geo))
message("Folios únicos:      ", dplyr::n_distinct(incidentes_geo$folio))
message(
  "Fecha mínima:       ",
  min(incidentes_geo$fecha_creacion, na.rm = TRUE)
)
message(
  "Fecha máxima:       ",
  max(incidentes_geo$fecha_creacion, na.rm = TRUE)
)
message(
  "CRS:                ",
  sf::st_crs(incidentes_geo)$input
)
message("==============================================================")

# ------------------------------------------------------------------------------
# 12. Preparar variables para almacenamiento
# ------------------------------------------------------------------------------

incidentes_geo <- incidentes_geo |>
  dplyr::mutate(
    hora_creacion = as.character(hora_creacion),
    hora_cierre   = as.character(hora_cierre)
  )
# ------------------------------------------------------------------------------
# 13. Guardar
# ------------------------------------------------------------------------------

archivo_salida <- fs::path(
  rutas$data_processed,
  "incidentes_c5_limpios.gpkg"
)

sf::st_write(
  incidentes_geo,
  archivo_salida,
  delete_dsn = TRUE,
  quiet = TRUE
)

message(
  "Archivo guardado en: ",
  archivo_salida
)
