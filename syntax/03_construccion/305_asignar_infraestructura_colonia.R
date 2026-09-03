# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 305_asignar_infraestructura_colonia.R
# Objetivo:
#   - Leer la infraestructura histórica de Fotocívicas
#   - Asignar cada punto de infraestructura a una unidad territorial del IECM
#   - Diagnosticar puntos sin colonia o con asignación múltiple
#   - Generar una base de infraestructura por colonia
#
# Entradas:
#   data/processed/colonias_limpias.gpkg
#   data/raw/07_fotocivicas/ubicacion/fotocivicas-ubicacion-puntos.zip
#
# Salidas:
#   data/processed/fotocivicas_2022_colonia.gpkg
#   output/tables/305_fotocivicas_por_colonia.csv
#
# Regla:
#   Este script NO define tratamiento, Post ni eventos.
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
# 1. Definir archivos
# ------------------------------------------------------------------------------

archivo_colonias <- fs::path(
  rutas$data_processed,
  "colonias_limpias.gpkg"
)

archivo_zip <- fs::path(
  rutas$data_raw,
  "07_fotocivicas",
  "ubicacion",
  "fotocivicas-ubicacion-puntos.zip"
)

validar_archivo(archivo_colonias)
validar_archivo(archivo_zip)


# ------------------------------------------------------------------------------
# 2. Extraer shapefile de puntos
# ------------------------------------------------------------------------------

ruta_extraida <- fs::path(
  rutas$data_processed,
  "tmp_fotocivicas_2022"
)

if (fs::dir_exists(ruta_extraida)) {
  fs::dir_delete(ruta_extraida)
}

fs::dir_create(
  ruta_extraida,
  recurse = TRUE
)

utils::unzip(
  zipfile = archivo_zip,
  exdir = ruta_extraida
)

archivo_shp <- fs::dir_ls(
  ruta_extraida,
  recurse = TRUE,
  regexp = "\\.shp$",
  type = "file"
)

if (length(archivo_shp) != 1) {
  stop(
    "Se esperaba exactamente un shapefile de puntos en el ZIP.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 3. Leer bases
# ------------------------------------------------------------------------------

colonias <- sf::st_read(
  archivo_colonias,
  quiet = TRUE
)

infraestructura <- sf::st_read(
  archivo_shp,
  quiet = TRUE
)


# ------------------------------------------------------------------------------
# 4. Diagnóstico inicial
# ------------------------------------------------------------------------------

message("")
message("==============================================================")
message("DIAGNÓSTICO INICIAL DE INFRAESTRUCTURA")
message("==============================================================")
message("Puntos Fotocívicas: ", nrow(infraestructura))
message("Colonias IECM:      ", nrow(colonias))
message(
  "CRS infraestructura: ",
  sf::st_crs(infraestructura)$input
)
message(
  "CRS colonias:        ",
  sf::st_crs(colonias)$input
)
message(
  "Geometrías vacías:    ",
  sum(sf::st_is_empty(infraestructura))
)
message(
  "Geometrías inválidas: ",
  sum(!sf::st_is_valid(infraestructura))
)
message("==============================================================")


# ------------------------------------------------------------------------------
# 5. Validar y limpiar geometrías
# ------------------------------------------------------------------------------

infraestructura <- infraestructura |>
  sf::st_make_valid() |>
  dplyr::filter(
    !sf::st_is_empty(geometry)
  )


# ------------------------------------------------------------------------------
# 6. Crear identificador interno
# ------------------------------------------------------------------------------

# No asumimos que la fuente tenga un ID único y estable.
# Creamos uno únicamente para controlar el join espacial.

infraestructura <- infraestructura |>
  dplyr::mutate(
    id_infraestructura = dplyr::row_number(),
    corte_infraestructura = as.Date("2022-12-07")
  )


# ------------------------------------------------------------------------------
# 7. Homologar CRS
# ------------------------------------------------------------------------------

infraestructura <- infraestructura |>
  sf::st_transform(32614)

colonias <- colonias |>
  sf::st_transform(32614)


# ------------------------------------------------------------------------------
# 8. Preparar capa de colonias
# ------------------------------------------------------------------------------

colonias_join <- colonias |>
  dplyr::select(
    id_colonia,
    colonia,
    id_alcaldia,
    alcaldia
  )


# ------------------------------------------------------------------------------
# 9. Asignar infraestructura a colonia
# ------------------------------------------------------------------------------

infraestructura_colonia <- infraestructura |>
  sf::st_join(
    colonias_join,
    join = sf::st_within,
    left = TRUE
  )


# ------------------------------------------------------------------------------
# 10. Identificar asignaciones múltiples
# ------------------------------------------------------------------------------

infra_multiple <- infraestructura_colonia |>
  sf::st_drop_geometry() |>
  dplyr::count(
    id_infraestructura,
    name = "n_colonias"
  ) |>
  dplyr::filter(
    n_colonias > 1
  )

message("")
message(
  "Puntos con más de una colonia: ",
  nrow(infra_multiple)
)


# ------------------------------------------------------------------------------
# 11. Diagnóstico de asignación
# ------------------------------------------------------------------------------

n_original <- nrow(infraestructura)

n_con_colonia <- infraestructura_colonia |>
  sf::st_drop_geometry() |>
  dplyr::filter(
    !is.na(id_colonia)
  ) |>
  dplyr::distinct(id_infraestructura) |>
  nrow()

n_sin_colonia <- infraestructura_colonia |>
  sf::st_drop_geometry() |>
  dplyr::filter(
    is.na(id_colonia)
  ) |>
  dplyr::distinct(id_infraestructura) |>
  nrow()

message("")
message("==============================================================")
message("ASIGNACIÓN FOTOCÍVICAS -> COLONIA")
message("==============================================================")
message("Infraestructura original: ", n_original)
message("Con colonia asignada:     ", n_con_colonia)
message(
  "Porcentaje asignado:      ",
  round(100 * n_con_colonia / n_original, 2),
  "%"
)
message("Sin colonia asignada:     ", n_sin_colonia)
message("Asignación múltiple:      ", nrow(infra_multiple))
message("==============================================================")


# ------------------------------------------------------------------------------
# 12. Guardar casos ambiguos para auditoría
# ------------------------------------------------------------------------------

if (nrow(infra_multiple) > 0) {

  detalle_multiple <- infraestructura_colonia |>
    dplyr::semi_join(
      infra_multiple,
      by = "id_infraestructura"
    ) |>
    sf::st_drop_geometry() |>
    dplyr::arrange(
      id_infraestructura,
      id_colonia
    )

  readr::write_csv(
    detalle_multiple,
    fs::path(
      rutas$output_tables,
      "305_fotocivicas_asignacion_ambigua.csv"
    )
  )
}


# ------------------------------------------------------------------------------
# 13. Excluir asignaciones ambiguas y observaciones sin colonia
# ------------------------------------------------------------------------------

ids_ambiguos <- infra_multiple |>
  dplyr::pull(id_infraestructura)

infraestructura_final <- infraestructura_colonia |>
  dplyr::filter(
    !is.na(id_colonia),
    !id_infraestructura %in% ids_ambiguos
  )


# ------------------------------------------------------------------------------
# 14. Validar unicidad
# ------------------------------------------------------------------------------

duplicados_final <- infraestructura_final |>
  sf::st_drop_geometry() |>
  dplyr::count(
    id_infraestructura
  ) |>
  dplyr::filter(
    n > 1
  )

if (nrow(duplicados_final) > 0) {
  stop(
    "La infraestructura final contiene IDs duplicados.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 15. Construir resumen por colonia
# ------------------------------------------------------------------------------

infraestructura_por_colonia <- infraestructura_final |>
  sf::st_drop_geometry() |>
  dplyr::count(
    id_colonia,
    colonia,
    id_alcaldia,
    alcaldia,
    name = "n_fotocivicas_2022"
  ) |>
  dplyr::arrange(
    dplyr::desc(n_fotocivicas_2022)
  )


# ------------------------------------------------------------------------------
# 16. Diagnóstico final
# ------------------------------------------------------------------------------

message("")
message("==============================================================")
message("INFRAESTRUCTURA FINAL")
message("==============================================================")
message(
  "Puntos utilizables:        ",
  nrow(infraestructura_final)
)
message(
  "Colonias con infraestructura: ",
  dplyr::n_distinct(infraestructura_final$id_colonia)
)
message(
  "Máximo de puntos en una colonia: ",
  max(infraestructura_por_colonia$n_fotocivicas_2022)
)
message("==============================================================")


# ------------------------------------------------------------------------------
# 17. Guardar resultados
# ------------------------------------------------------------------------------

archivo_salida <- fs::path(
  rutas$data_processed,
  "fotocivicas_2022_colonia.gpkg"
)

sf::st_write(
  infraestructura_final,
  archivo_salida,
  delete_dsn = TRUE,
  quiet = TRUE
)

archivo_resumen <- fs::path(
  rutas$output_tables,
  "305_fotocivicas_por_colonia.csv"
)

readr::write_csv(
  infraestructura_por_colonia,
  archivo_resumen
)

message("")
message("Archivo espacial guardado en: ", archivo_salida)
message("Resumen guardado en:          ", archivo_resumen)


# ------------------------------------------------------------------------------
# 18. Limpiar archivos temporales
# ------------------------------------------------------------------------------

if (fs::dir_exists(ruta_extraida)) {
  fs::dir_delete(ruta_extraida)
}