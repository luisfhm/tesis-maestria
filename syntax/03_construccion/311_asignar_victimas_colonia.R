# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 311_asignar_victimas_colonia.R
# Objetivo:
#   - Asignar cada víctima de tránsito a una unidad territorial del IECM
#   - Vía A: join espacial con coordenadas válidas (~88%)
#   - Vía B: coincidencia por nombre (alcaldía + colonia) para el resto
#
# Entradas:
#   data/processed/victimas_transito_limpias.parquet
#   data/processed/colonias_limpias.gpkg
#
# Salida:
#   data/processed/victimas_transito_colonia.parquet
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------------------------

source(
  here::here("syntax", "00_setup", "000_setup.R"),
  encoding = "UTF-8"
)

sf::sf_use_s2(FALSE)


# ------------------------------------------------------------------------------
# 1. Leer bases
# ------------------------------------------------------------------------------

archivo_victimas <- fs::path(
  rutas$data_processed,
  "victimas_transito_limpias.parquet"
)

archivo_colonias <- fs::path(
  rutas$data_processed,
  "colonias_limpias.gpkg"
)

validar_archivo(archivo_victimas)
validar_archivo(archivo_colonias)

victimas <- arrow::read_parquet(archivo_victimas) |>
  data.table::as.data.table()

victimas[, .fila := .I]

colonias <- sf::st_read(archivo_colonias, quiet = TRUE) |>
  sf::st_transform(32614)


# ------------------------------------------------------------------------------
# 2. Vía A — join espacial
# ------------------------------------------------------------------------------

con_coord <- victimas[coord_valida == TRUE, .(.fila, longitud, latitud)]
message("Víctimas con coordenadas válidas: ", nrow(con_coord))

puntos <- sf::st_as_sf(
  con_coord, coords = c("longitud", "latitud"), crs = 4326
) |>
  sf::st_transform(32614)

idx <- sf::st_intersects(puntos, colonias)

asignacion_coord <- data.table::data.table(
  .fila = puntos$.fila,
  id_colonia_coord = vapply(
    idx,
    function(i) if (length(i) >= 1) colonias$id_colonia[i[1]] else NA_character_,
    character(1)
  )
)

rm(puntos, idx, con_coord)
invisible(gc())


# ------------------------------------------------------------------------------
# 3. Vía B — coincidencia por nombre
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

victimas[, `:=`(
  alcaldia_key = a_ascii(alcaldia_norm),
  colonia_key  = a_ascii(colonia_norm)
)]

victimas <- catalogo_nombre[victimas, on = c("alcaldia_key", "colonia_key")]
victimas <- asignacion_coord[victimas, on = ".fila"]


# ------------------------------------------------------------------------------
# 4. Combinar (prioridad: coordenadas)
# ------------------------------------------------------------------------------

victimas[, `:=`(
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

victimas[, anio := lubridate::year(fecha)]

resumen <- victimas[, .(
  n = .N,
  pct_coordenadas = round(100 * mean(metodo_asignacion == "coordenadas"), 1),
  pct_nombre      = round(100 * mean(metodo_asignacion == "nombre"), 1),
  pct_sin_asignar = round(100 * mean(metodo_asignacion == "sin_asignar"), 1)
), by = anio][order(anio)]

message("")
message("==============================================================")
message("ASIGNACIÓN DE VÍCTIMAS A COLONIA")
message("==============================================================")
message(
  "Tasa global: ",
  round(100 * mean(victimas$metodo_asignacion != "sin_asignar"), 1), "%"
)
message("==============================================================")
print(resumen)


# ------------------------------------------------------------------------------
# 6. Guardar
# ------------------------------------------------------------------------------

archivo_salida <- fs::path(
  rutas$data_processed,
  "victimas_transito_colonia.parquet"
)

arrow::write_parquet(
  victimas[, .(
    fecha, mes, gravedad, tipo_siniestro, tipo_persona,
    id_colonia, metodo_asignacion
  )],
  archivo_salida
)

message("")
message("Archivo guardado en: ", archivo_salida)
