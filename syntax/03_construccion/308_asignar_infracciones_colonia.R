# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 308_asignar_infracciones_colonia.R
# Objetivo:
#   - Asignar cada infracción a una unidad territorial del IECM
#   - Vía A: join espacial cuando hay coordenadas válidas (principalmente 2020)
#   - Vía B: coincidencia por nombre (alcaldía + colonia) para el resto
#   - Diagnosticar la tasa de asignación por año y por método
#
# Entradas:
#   data/processed/infracciones_limpias.parquet
#   data/processed/colonias_limpias.gpkg
#
# Salida:
#   data/processed/infracciones_colonia.parquet
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

sf::sf_use_s2(FALSE)


# ------------------------------------------------------------------------------
# 1. Leer bases (solo columnas necesarias)
# ------------------------------------------------------------------------------

archivo_infracciones <- fs::path(
  rutas$data_processed,
  "infracciones_limpias.parquet"
)

archivo_colonias <- fs::path(
  rutas$data_processed,
  "colonias_limpias.gpkg"
)

validar_archivo(archivo_infracciones)
validar_archivo(archivo_colonias)

infracciones <- arrow::read_parquet(
  archivo_infracciones,
  col_select = c(
    "id", "fecha", "mes", "tipo_infraccion", "articulo_num",
    "colonia_norm", "alcaldia_norm",
    "longitud", "latitud", "coord_valida"
  )
) |>
  data.table::as.data.table()

colonias <- sf::st_read(archivo_colonias, quiet = TRUE) |>
  sf::st_transform(32614)


# ------------------------------------------------------------------------------
# 2. Vía A — join espacial (solo id + geometría)
# ------------------------------------------------------------------------------

con_coord <- infracciones[coord_valida == TRUE, .(id, longitud, latitud)]
message("Infracciones con coordenadas válidas: ", nrow(con_coord))

puntos <- sf::st_as_sf(
  con_coord,
  coords = c("longitud", "latitud"),
  crs = 4326
) |>
  sf::st_transform(32614)

rm(con_coord)

# st_intersects devuelve, para cada punto, el índice de la colonia que lo
# contiene (más ligero que st_join porque no duplica atributos).
idx <- sf::st_intersects(puntos, colonias)

id_colonia_coord <- vapply(
  idx,
  function(i) if (length(i) >= 1) colonias$id_colonia[i[1]] else NA_character_,
  character(1)
)

asignacion_coord <- data.table::data.table(
  id = puntos$id,
  id_colonia_coord = id_colonia_coord
)

rm(puntos, idx, id_colonia_coord)
invisible(gc())


# ------------------------------------------------------------------------------
# 3. Vía B — coincidencia por nombre (alcaldía + colonia)
# ------------------------------------------------------------------------------

a_ascii <- function(x) {
  x |>
    stringr::str_to_upper() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_replace_all("[^A-Z0-9 ]", " ") |>
    stringr::str_squish()
}

catalogo_nombre <- colonias |>
  sf::st_drop_geometry() |>
  dplyr::transmute(
    alcaldia_key = a_ascii(alcaldia),
    colonia_key  = a_ascii(colonia),
    id_colonia_nombre = id_colonia
  ) |>
  dplyr::add_count(alcaldia_key, colonia_key, name = "n_ut") |>
  dplyr::filter(n_ut == 1) |>
  dplyr::select(-n_ut) |>
  data.table::as.data.table()

infracciones[, `:=`(
  alcaldia_key = a_ascii(alcaldia_norm),
  colonia_key  = a_ascii(colonia_norm)
)]

infracciones <- catalogo_nombre[
  infracciones,
  on = c("alcaldia_key", "colonia_key")
]

infracciones <- asignacion_coord[infracciones, on = "id"]


# ------------------------------------------------------------------------------
# 4. Combinar métodos (prioridad: coordenadas)
# ------------------------------------------------------------------------------

infracciones[, `:=`(
  id_colonia = data.table::fifelse(
    !is.na(id_colonia_coord), id_colonia_coord, id_colonia_nombre
  ),
  metodo_asignacion = data.table::fcase(
    !is.na(id_colonia_coord),  "coordenadas",
    !is.na(id_colonia_nombre), "nombre",
    default = "sin_asignar"
  )
)]


# ------------------------------------------------------------------------------
# 5. Diagnóstico
# ------------------------------------------------------------------------------

infracciones[, anio := lubridate::year(fecha)]

resumen <- infracciones[, .(
  n = .N,
  pct_coordenadas = round(100 * mean(metodo_asignacion == "coordenadas"), 1),
  pct_nombre      = round(100 * mean(metodo_asignacion == "nombre"), 1),
  pct_sin_asignar = round(100 * mean(metodo_asignacion == "sin_asignar"), 1)
), by = anio][order(anio)]

tasa_global <- round(100 * mean(infracciones$metodo_asignacion != "sin_asignar"), 1)

message("")
message("==============================================================")
message("ASIGNACIÓN DE INFRACCIONES A COLONIA")
message("==============================================================")
message("Tasa global de asignación: ", tasa_global, "%")
message("==============================================================")
print(resumen)

combos_sin_match <- infracciones[
  metodo_asignacion == "sin_asignar",
  .N,
  by = .(alcaldia_key, colonia_key)
][order(-N)][1:20]

message("")
message("Combinaciones alcaldía-colonia sin asignar más frecuentes:")
print(combos_sin_match)


# ------------------------------------------------------------------------------
# 6. Guardar
# ------------------------------------------------------------------------------

archivo_salida <- fs::path(
  rutas$data_processed,
  "infracciones_colonia.parquet"
)

arrow::write_parquet(
  infracciones[, .(
    id, fecha, mes, tipo_infraccion, articulo_num,
    id_colonia, metodo_asignacion
  )],
  archivo_salida
)

message("")
message("Archivo guardado en: ", archivo_salida)
