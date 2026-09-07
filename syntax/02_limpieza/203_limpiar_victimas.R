# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 203_limpiar_victimas.R
# Objetivo:
#   - Leer y unir las víctimas registradas en carpetas de investigación (FGJ)
#   - Filtrar a víctimas de hechos de tránsito (lesiones y homicidios culposos)
#   - Definir el periodo con fecha_hecho
#   - Clasificar gravedad (lesionado / fallecido) y tipo de siniestro
#   - Depurar coordenadas
#
# Entrada:
#   data/raw/15_victimas/victimasFGJ_*.csv
#
# Salida:
#   data/processed/victimas_transito_limpias.parquet
#
# Nota:
#   Esta fuente cubre fecha_hecho hasta julio de 2024, por lo que permite
#   extender la ventana posterior del evento de septiembre de 2023 más allá
#   de la cobertura de incidentes C5 (febrero de 2024).
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
# 1. Localizar archivos
# ------------------------------------------------------------------------------

ruta_victimas <- fs::path(
  rutas$data_raw,
  "15_victimas"
)

archivos <- fs::dir_ls(
  path = ruta_victimas,
  regexp = "victimasFGJ_[0-9]{4}\\.csv$"
)

if (length(archivos) == 0) {
  stop("No se encontraron archivos de víctimas FGJ.", call. = FALSE)
}

message("Archivos de víctimas encontrados: ", length(archivos))


# ------------------------------------------------------------------------------
# 2. Lectura
# ------------------------------------------------------------------------------

columnas <- c(
  "anio_hecho", "mes_hecho", "fecha_hecho",
  "delito", "categoria_delito",
  "sexo", "edad", "tipo_persona", "calidad_juridica",
  "colonia_hecho", "colonia_catalogo",
  "alcaldia_hecho", "alcaldia_catalogo",
  "latitud", "longitud"
)

leer_victimas <- function(archivo) {
  readr::read_csv(
    archivo,
    show_col_types = FALSE,
    progress = FALSE,
    locale = readr::locale(encoding = "UTF-8"),
    col_select = tidyselect::any_of(columnas),
    col_types = readr::cols(.default = readr::col_character())
  )
}

victimas <- purrr::map_dfr(archivos, leer_victimas)

message("Registros leídos (bruto): ", nrow(victimas))


# ------------------------------------------------------------------------------
# 3. Normalizar texto de delito
# ------------------------------------------------------------------------------

a_ascii <- function(x) {
  x |>
    stringr::str_to_upper() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_squish()
}

victimas <- victimas |>
  dplyr::mutate(
    delito_norm = a_ascii(delito)
  )


# ------------------------------------------------------------------------------
# 4. Filtrar a hechos de tránsito
# ------------------------------------------------------------------------------

# Se conservan lesiones y homicidios culposos derivados de tránsito vehicular.
# Se excluye robo de/ a vehículos y daño a propiedad (no son seguridad vial
# en el sentido de víctimas físicas).

patron_transito <- paste(
  "LESIONES CULPOSAS POR TRANSITO",
  "HOMICIDIO CULPOSO POR TRANSITO",
  "CAIDA DE VEHICULO EN MOVIMIENTO",
  sep = "|"
)

victimas_transito <- victimas |>
  dplyr::filter(
    stringr::str_detect(delito_norm, patron_transito)
  )

message("Víctimas de hechos de tránsito: ", nrow(victimas_transito))


# ------------------------------------------------------------------------------
# 5. Fecha y periodo
# ------------------------------------------------------------------------------

victimas_transito <- victimas_transito |>
  dplyr::mutate(
    fecha = suppressWarnings(as.Date(fecha_hecho)),
    mes   = lubridate::floor_date(fecha, unit = "month")
  )

rango_min <- as.Date("2018-01-01")
rango_max <- as.Date("2024-12-31")

fuera_rango <- sum(
  is.na(victimas_transito$fecha) |
    victimas_transito$fecha < rango_min |
    victimas_transito$fecha > rango_max
)

message("Víctimas con fecha ausente o fuera de rango: ", fuera_rango)

victimas_transito <- victimas_transito |>
  dplyr::filter(
    !is.na(fecha),
    fecha >= rango_min,
    fecha <= rango_max
  )


# ------------------------------------------------------------------------------
# 6. Clasificación de gravedad y tipo de siniestro
# ------------------------------------------------------------------------------

victimas_transito <- victimas_transito |>
  dplyr::mutate(
    gravedad = dplyr::if_else(
      stringr::str_detect(delito_norm, "HOMICIDIO"),
      "fallecido",
      "lesionado"
    ),

    tipo_siniestro = dplyr::case_when(
      stringr::str_detect(delito_norm, "ATROPELL")  ~ "atropellado",
      stringr::str_detect(delito_norm, "COLISION")  ~ "colision",
      stringr::str_detect(delito_norm, "CAIDA")     ~ "caida",
      TRUE ~ "otro"
    ),

    edad_num = suppressWarnings(as.integer(edad))
  )


# ------------------------------------------------------------------------------
# 7. Depurar coordenadas
# ------------------------------------------------------------------------------

bbox_cdmx <- list(
  lon_min = -99.40, lon_max = -98.90,
  lat_min = 19.00,  lat_max = 19.65
)

victimas_transito <- victimas_transito |>
  dplyr::mutate(
    longitud = suppressWarnings(as.numeric(longitud)),
    latitud  = suppressWarnings(as.numeric(latitud)),

    coord_valida =
      !is.na(longitud) & !is.na(latitud) &
      dplyr::between(longitud, bbox_cdmx$lon_min, bbox_cdmx$lon_max) &
      dplyr::between(latitud,  bbox_cdmx$lat_min, bbox_cdmx$lat_max)
  )


# ------------------------------------------------------------------------------
# 8. Normalizar colonia / alcaldía
# ------------------------------------------------------------------------------

victimas_transito <- victimas_transito |>
  dplyr::mutate(
    colonia_norm  = a_ascii(dplyr::coalesce(colonia_catalogo, colonia_hecho)) |>
      stringr::str_replace_all("[^A-Z0-9 ]", " ") |>
      stringr::str_squish(),
    alcaldia_norm = a_ascii(dplyr::coalesce(alcaldia_catalogo, alcaldia_hecho)) |>
      stringr::str_replace_all("[^A-Z0-9 ]", " ") |>
      stringr::str_squish()
  )


# ------------------------------------------------------------------------------
# 9. Diagnóstico
# ------------------------------------------------------------------------------

resumen_anual <- victimas_transito |>
  dplyr::mutate(anio = lubridate::year(fecha)) |>
  dplyr::group_by(anio) |>
  dplyr::summarise(
    n = dplyr::n(),
    fallecidos = sum(gravedad == "fallecido"),
    atropellados = sum(tipo_siniestro == "atropellado"),
    pct_coord = round(100 * mean(coord_valida), 1),
    .groups = "drop"
  )

message("")
message("==============================================================")
message("VÍCTIMAS DE TRÁNSITO LIMPIAS")
message("==============================================================")
message("Observaciones:  ", nrow(victimas_transito))
message("Periodo:        ", min(victimas_transito$mes), " a ", max(victimas_transito$mes))
message("Fallecidos:     ", sum(victimas_transito$gravedad == "fallecido"))
message("Coord. válidas: ", round(100 * mean(victimas_transito$coord_valida), 1), "%")
message("==============================================================")
print(resumen_anual)


# ------------------------------------------------------------------------------
# 10. Guardar
# ------------------------------------------------------------------------------

archivo_salida <- fs::path(
  rutas$data_processed,
  "victimas_transito_limpias.parquet"
)

victimas_transito |>
  dplyr::select(
    fecha, mes,
    delito, delito_norm, gravedad, tipo_siniestro,
    sexo, edad_num, tipo_persona, calidad_juridica,
    colonia_norm, alcaldia_norm,
    longitud, latitud, coord_valida
  ) |>
  arrow::write_parquet(archivo_salida)

message("")
message("Archivo guardado en: ", archivo_salida)
