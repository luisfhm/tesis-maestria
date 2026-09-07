# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 202_limpiar_infracciones.R
# Objetivo:
#   - Leer y unir los archivos de infracciones al Reglamento de Tránsito
#   - Homologar los dos esquemas de publicación (2020-abr2021 / mayo2021+)
#   - Definir el periodo de cada observación con fecha_infraccion
#   - Clasificar la infracción (artículo 9 = velocidad vs. resto)
#   - Depurar coordenadas y marcar las observaciones georreferenciables
#
# Entrada:
#   data/raw/05_infracciones/generales/*.csv
#
# Salida:
#   data/processed/infracciones_limpias.parquet
#
# Reglas:
#   - El periodo se toma de fecha_infraccion, no del nombre del archivo.
#   - Este script NO asigna colonia ni define tratamiento.
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

ruta_infracciones <- fs::path(
  rutas$data_raw,
  "05_infracciones",
  "generales"
)

archivos <- fs::dir_ls(
  path = ruta_infracciones,
  regexp = "\\.csv$",
  type = "file"
)

if (length(archivos) == 0) {
  stop("No se encontraron archivos de infracciones.", call. = FALSE)
}

message("Archivos de infracciones encontrados: ", length(archivos))


# ------------------------------------------------------------------------------
# 2. Función de lectura y homologación
# ------------------------------------------------------------------------------

# Variables longitudinalmente estables entre ambos esquemas
# (ver docs/03_exploracion_datos/104_explorar_infracciones.md).

columnas_utiles <- c(
  "id_infraccion", "id_folio", "fecha_infraccion",
  "articulo", "fraccion", "inciso", "parrafo", "categoria",
  "placa", "marca",
  "colonia", "alcaldia", "codigo_postal", "longitud", "latitud"
)

leer_infraccion <- function(archivo) {

  encabezado <- readr::read_csv(
    archivo,
    n_max = 0,
    show_col_types = FALSE,
    name_repair = "minimal"
  )

  nombres <- names(encabezado) |>
    stringr::str_trim() |>
    stringr::str_to_lower()

  datos <- readr::read_csv(
    archivo,
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "minimal",
    col_select = tidyselect::all_of(which(nombres %in% columnas_utiles)),
    col_types = readr::cols(.default = readr::col_character())
  )

  names(datos) <- names(datos) |>
    stringr::str_trim() |>
    stringr::str_to_lower()

  col_id <- dplyr::case_when(
    "id_infraccion" %in% names(datos) ~ "id_infraccion",
    "id_folio"      %in% names(datos) ~ "id_folio",
    TRUE ~ NA_character_
  )

  tomar <- function(nombre) {
    if (nombre %in% names(datos)) datos[[nombre]] else NA
  }

  tibble::tibble(
    id             = as.character(datos[[col_id]]),
    fecha_infraccion = as.character(tomar("fecha_infraccion")),
    articulo       = as.character(tomar("articulo")),
    fraccion       = as.character(tomar("fraccion")),
    inciso         = as.character(tomar("inciso")),
    parrafo        = as.character(tomar("parrafo")),
    categoria      = as.character(tomar("categoria")),
    placa          = as.character(tomar("placa")),
    marca          = as.character(tomar("marca")),
    colonia        = as.character(tomar("colonia")),
    alcaldia       = as.character(tomar("alcaldia")),
    codigo_postal  = as.character(tomar("codigo_postal")),
    longitud       = suppressWarnings(as.numeric(tomar("longitud"))),
    latitud        = suppressWarnings(as.numeric(tomar("latitud"))),
    archivo_origen = fs::path_file(archivo)
  )
}


# ------------------------------------------------------------------------------
# 3. Leer y unir
# ------------------------------------------------------------------------------

infracciones <- purrr::map_dfr(archivos, leer_infraccion)

message("Registros leídos (bruto): ", nrow(infracciones))


# ------------------------------------------------------------------------------
# 4. Fecha y periodo
# ------------------------------------------------------------------------------

infracciones <- infracciones |>
  dplyr::mutate(
    fecha = suppressWarnings(as.Date(fecha_infraccion)),
    mes   = lubridate::floor_date(fecha, unit = "month")
  )

# Rango plausible: la fuente cubre ene-2020 a abr-2023.
# Se conserva un pequeño margen para registros fechados a inicios de 2023
# que aparecen en archivos de 2022 (ver doc de exploración).

rango_min <- as.Date("2019-06-01")
rango_max <- as.Date("2023-12-31")

fuera_rango <- sum(
  is.na(infracciones$fecha) |
    infracciones$fecha < rango_min |
    infracciones$fecha > rango_max
)

message("Registros con fecha ausente o fuera de rango: ", fuera_rango)

infracciones <- infracciones |>
  dplyr::filter(
    !is.na(fecha),
    fecha >= rango_min,
    fecha <= rango_max
  )


# ------------------------------------------------------------------------------
# 5. Eliminar duplicados por identificador
# ------------------------------------------------------------------------------

n_antes <- nrow(infracciones)

infracciones <- infracciones |>
  dplyr::distinct(id, .keep_all = TRUE)

message("Duplicados por id eliminados: ", n_antes - nrow(infracciones))


# ------------------------------------------------------------------------------
# 6. Clasificación normativa
# ------------------------------------------------------------------------------

# El artículo 9 (fracciones I y II) concentra ~77% de las infracciones y
# corresponde a exceso de velocidad. Es la clasificación longitudinal más
# estable (articulo + fraccion).

infracciones <- infracciones |>
  dplyr::mutate(
    articulo_num = suppressWarnings(as.integer(articulo)),

    tipo_infraccion = dplyr::case_when(
      articulo_num == 9 ~ "velocidad",
      is.na(articulo_num) ~ "sin_clasificar",
      TRUE ~ "otra"
    )
  )


# ------------------------------------------------------------------------------
# 7. Depurar coordenadas
# ------------------------------------------------------------------------------

# Bounding box aproximado de la CDMX.

bbox_cdmx <- list(
  lon_min = -99.40, lon_max = -98.90,
  lat_min = 19.00,  lat_max = 19.65
)

infracciones <- infracciones |>
  dplyr::mutate(
    coord_valida =
      !is.na(longitud) & !is.na(latitud) &
      dplyr::between(longitud, bbox_cdmx$lon_min, bbox_cdmx$lon_max) &
      dplyr::between(latitud,  bbox_cdmx$lat_min, bbox_cdmx$lat_max)
  )


# ------------------------------------------------------------------------------
# 8. Normalizar texto de colonia y alcaldía
# ------------------------------------------------------------------------------

normalizar_texto <- function(x) {
  x |>
    stringr::str_to_upper() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_replace_all("[^A-Z0-9 ]", " ") |>
    stringr::str_squish()
}

infracciones <- infracciones |>
  dplyr::mutate(
    colonia_norm  = normalizar_texto(colonia),
    alcaldia_norm = normalizar_texto(alcaldia)
  )


# ------------------------------------------------------------------------------
# 9. Diagnóstico
# ------------------------------------------------------------------------------

resumen_anual <- infracciones |>
  dplyr::mutate(anio = lubridate::year(fecha)) |>
  dplyr::group_by(anio) |>
  dplyr::summarise(
    n = dplyr::n(),
    pct_coord   = round(100 * mean(coord_valida), 1),
    pct_colonia = round(100 * mean(!is.na(colonia_norm) & colonia_norm != ""), 1),
    pct_velocidad = round(100 * mean(tipo_infraccion == "velocidad"), 1),
    .groups = "drop"
  )

meses_presentes <- sort(unique(infracciones$mes))
rango_meses <- seq(min(meses_presentes), max(meses_presentes), by = "month")
meses_faltantes <- as.character(rango_meses[!rango_meses %in% meses_presentes])

message("")
message("==============================================================")
message("INFRACCIONES LIMPIAS")
message("==============================================================")
message("Observaciones: ", nrow(infracciones))
message("Periodo:        ", min(infracciones$mes), " a ", max(infracciones$mes))
message("Coord. válidas: ", round(100 * mean(infracciones$coord_valida), 1), "%")
message(
  "Meses sin registros: ",
  if (length(meses_faltantes) == 0) "ninguno" else paste(meses_faltantes, collapse = ", ")
)
message("==============================================================")
print(resumen_anual)


# ------------------------------------------------------------------------------
# 10. Guardar
# ------------------------------------------------------------------------------

archivo_salida <- fs::path(
  rutas$data_processed,
  "infracciones_limpias.parquet"
)

infracciones |>
  dplyr::select(
    id, fecha, mes,
    articulo, articulo_num, fraccion, tipo_infraccion, categoria,
    colonia, colonia_norm, alcaldia, alcaldia_norm, codigo_postal,
    longitud, latitud, coord_valida,
    archivo_origen
  ) |>
  arrow::write_parquet(archivo_salida)

message("")
message("Archivo guardado en: ", archivo_salida)
