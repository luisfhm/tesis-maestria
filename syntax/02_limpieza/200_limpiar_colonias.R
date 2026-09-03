# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 200_limpiar_colonias.R
# Objetivo:
#   - Leer la capa administrativa de colonias
#   - Validar y estandarizar geometrías y variables
#   - Generar una capa limpia para la construcción del panel colonia-mes
#
# Entrada:
#   data/raw/colonias/...
#
# Salida:
#   data/processed/colonias_limpias.gpkg
#
# Regla:
#   Este script no incorpora tratamiento, radares ni outcomes.
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------------------------

source("syntax/00_setup/000_setup.R")


# ------------------------------------------------------------------------------
# 1. Definir archivo de entrada
# ------------------------------------------------------------------------------

archivo_colonias <- fs::path(
  rutas$data_raw,
  "14_otros",
  "colonias_iecm.shp"
)

validar_archivo(archivo_colonias)


# ------------------------------------------------------------------------------
# 2. Leer capa de colonias
# ------------------------------------------------------------------------------

colonias <- sf::st_read(
  archivo_colonias,
  quiet = TRUE
)

# ------------------------------------------------------------------------------
# 2. Leer capa
# ------------------------------------------------------------------------------

colonias <- sf::st_read(
  archivo_colonias,
  quiet = TRUE
)


# ------------------------------------------------------------------------------
# 3. Diagnóstico inicial
# ------------------------------------------------------------------------------

message("Número de registros: ", nrow(colonias))
message("CRS original: ", sf::st_crs(colonias)$input)

print(names(colonias))

message(
  "Geometrías inválidas: ",
  sum(!sf::st_is_valid(colonias))
)


# ------------------------------------------------------------------------------
# 4. Reparar geometrías
# ------------------------------------------------------------------------------

colonias <- colonias |>
  sf::st_make_valid()


# ------------------------------------------------------------------------------
# 5. Seleccionar y estandarizar variables
# ------------------------------------------------------------------------------

colonias <- colonias |>
  dplyr::transmute(
    id_colonia = as.character(CVEUT),
    colonia    = stringr::str_squish(NOMUT),
    id_alcaldia = as.character(CVEDT),
    alcaldia   = stringr::str_squish(NOMDT),
    geometry
  ) |>
  dplyr::mutate(
    colonia = stringr::str_to_upper(colonia),
    alcaldia = stringr::str_to_upper(alcaldia)
  )


# ------------------------------------------------------------------------------
# 6. Validar identificadores
# ------------------------------------------------------------------------------

duplicados <- colonias |>
  sf::st_drop_geometry() |>
  dplyr::count(id_colonia) |>
  dplyr::filter(n > 1)

message(
  "IDs de colonia duplicados: ",
  nrow(duplicados)
)

if (nrow(duplicados) > 0) {
  print(duplicados)
}

message(
  "Colonias sin identificador: ",
  sum(is.na(colonias$id_colonia))
)

message(
  "Número de IDs únicos: ",
  dplyr::n_distinct(colonias$id_colonia)
)


# ------------------------------------------------------------------------------
# 7. Garantizar CRS de trabajo
# ------------------------------------------------------------------------------

colonias <- colonias |>
  sf::st_transform(32614)


# ------------------------------------------------------------------------------
# 8. Crear variables auxiliares
# ------------------------------------------------------------------------------

colonias <- colonias |>
  dplyr::mutate(
    area_km2 = as.numeric(sf::st_area(geometry)) / 1e6
  )



# ------------------------------------------------------------------------------
# 9. Diagnóstico final
# ------------------------------------------------------------------------------

message("")
message("==============================================================")
message("CAPA DE COLONIAS LIMPIA")
message("==============================================================")
message("Unidades territoriales: ", nrow(colonias))
message("IDs únicos:              ", dplyr::n_distinct(colonias$id_colonia))
message("Alcaldías:               ", dplyr::n_distinct(colonias$id_alcaldia))
message("Geometrías inválidas:    ", sum(!sf::st_is_valid(colonias)))
message(
  "Área total km2:          ",
  round(sum(colonias$area_km2, na.rm = TRUE), 2)
)
message("==============================================================")

# ------------------------------------------------------------------------------
# 10. Guardar
# ------------------------------------------------------------------------------

archivo_salida <- fs::path(
  rutas$data_processed,
  "colonias_limpias.gpkg"
)

sf::st_write(
  colonias,
  archivo_salida,
  delete_dsn = TRUE,
  quiet = TRUE
)

message("Archivo guardado en: ", archivo_salida)