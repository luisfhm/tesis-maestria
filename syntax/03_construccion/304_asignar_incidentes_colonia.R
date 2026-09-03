# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 304_asignar_incidentes_colonia.R
# Objetivo:
#   - Asignar cada incidente C5 a una unidad territorial del IECM
#   - Identificar incidentes sin colonia asignada
#   - Identificar y excluir asignaciones espaciales ambiguas
#   - Generar una base final lista para construir el panel colonia-mes
#
# Entradas:
#   data/processed/colonias_limpias.gpkg
#   data/processed/incidentes_c5_limpios.gpkg
#
# Salida:
#   data/processed/incidentes_c5_colonia.gpkg
#
# Regla:
#   Este script no define tratamiento ni periodos Post.
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

archivo_c5 <- fs::path(
  rutas$data_processed,
  "incidentes_c5_limpios.gpkg"
)

validar_archivo(archivo_colonias)
validar_archivo(archivo_c5)


# ------------------------------------------------------------------------------
# 2. Leer bases
# ------------------------------------------------------------------------------

colonias <- sf::st_read(
  archivo_colonias,
  quiet = TRUE
)

incidentes <- sf::st_read(
  archivo_c5,
  quiet = TRUE
)


# ------------------------------------------------------------------------------
# 3. Diagnóstico inicial
# ------------------------------------------------------------------------------

message("")
message("==============================================================")
message("DIAGNÓSTICO INICIAL")
message("==============================================================")
message("Incidentes C5: ", nrow(incidentes))
message("Colonias IECM: ", nrow(colonias))
message("CRS C5:        ", sf::st_crs(incidentes)$input)
message("CRS colonias:  ", sf::st_crs(colonias)$input)
message("==============================================================")


# ------------------------------------------------------------------------------
# 4. Homologar CRS
# ------------------------------------------------------------------------------

incidentes <- incidentes |>
  sf::st_transform(32614)

colonias <- colonias |>
  sf::st_transform(32614)


# ------------------------------------------------------------------------------
# 5. Preparar capa administrativa
# ------------------------------------------------------------------------------

colonias_join <- colonias |>
  dplyr::select(
    id_colonia,
    colonia,
    id_alcaldia,
    alcaldia
  )


# ------------------------------------------------------------------------------
# 6. Asignar colonia mediante join espacial
# ------------------------------------------------------------------------------

incidentes_colonia <- incidentes |>
  sf::st_join(
    colonias_join,
    join = sf::st_within,
    left = TRUE
  )


# ------------------------------------------------------------------------------
# 7. Diagnóstico preliminar de asignación
# ------------------------------------------------------------------------------

n_total_join <- nrow(incidentes_colonia)

n_asignados <- sum(
  !is.na(incidentes_colonia$id_colonia)
)

n_sin_colonia <- sum(
  is.na(incidentes_colonia$id_colonia)
)

message("")
message("==============================================================")
message("ASIGNACIÓN C5 -> COLONIA")
message("==============================================================")
message("Filas después del join:   ", n_total_join)
message("Con colonia asignada:     ", n_asignados)
message(
  "Porcentaje asignado:      ",
  round(100 * n_asignados / n_total_join, 2),
  "%"
)
message("Sin colonia asignada:     ", n_sin_colonia)
message(
  "Porcentaje sin colonia:   ",
  round(100 * n_sin_colonia / n_total_join, 2),
  "%"
)
message("==============================================================")


# ------------------------------------------------------------------------------
# 8. Identificar asignaciones múltiples
# ------------------------------------------------------------------------------

folios_multiples <- incidentes_colonia |>
  sf::st_drop_geometry() |>
  dplyr::count(
    folio,
    name = "n_colonias"
  ) |>
  dplyr::filter(
    n_colonias > 1
  )

message(
  "Folios con más de una colonia: ",
  nrow(folios_multiples)
)

if (nrow(folios_multiples) > 0) {

  resumen_multiples <- folios_multiples |>
    dplyr::count(
      n_colonias,
      name = "n_folios"
    )

  print(resumen_multiples)
}


# ------------------------------------------------------------------------------
# 9. Guardar diagnóstico de casos ambiguos
# ------------------------------------------------------------------------------

if (nrow(folios_multiples) > 0) {

  detalle_multiples <- incidentes_colonia |>
    dplyr::semi_join(
      folios_multiples,
      by = "folio"
    ) |>
    sf::st_drop_geometry() |>
    dplyr::select(
      folio,
      fecha_creacion,
      colonia_fuente,
      alcaldia_fuente,
      id_colonia,
      colonia,
      id_alcaldia,
      alcaldia
    ) |>
    dplyr::arrange(
      folio,
      id_colonia
    )

  archivo_ambiguos <- fs::path(
    rutas$output_tables,
    "304_incidentes_asignacion_ambigua.csv"
  )

  readr::write_csv(
    detalle_multiples,
    archivo_ambiguos
  )

  message(
    "Diagnóstico de casos ambiguos guardado en: ",
    archivo_ambiguos
  )
}


# ------------------------------------------------------------------------------
# 10. Construir base final
# ------------------------------------------------------------------------------

folios_ambiguos <- folios_multiples |>
  dplyr::pull(folio)

incidentes_colonia_final <- incidentes_colonia |>
  dplyr::filter(
    !is.na(id_colonia),
    !folio %in% folios_ambiguos
  )


# ------------------------------------------------------------------------------
# 11. Validar unicidad de folio
# ------------------------------------------------------------------------------

duplicados_final <- incidentes_colonia_final |>
  sf::st_drop_geometry() |>
  dplyr::count(
    folio,
    name = "n"
  ) |>
  dplyr::filter(
    n > 1
  )

if (nrow(duplicados_final) > 0) {

  stop(
    "La base final todavía contiene folios duplicados.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 12. Diagnóstico final
# ------------------------------------------------------------------------------

n_original <- nrow(incidentes)

n_final <- nrow(incidentes_colonia_final)

n_ambiguos <- nrow(folios_multiples)

n_excluidos <- n_original - n_final

message("")
message("==============================================================")
message("BASE FINAL C5 -> COLONIA")
message("==============================================================")
message("Incidentes originales:     ", n_original)
message("Sin colonia:               ", n_sin_colonia)
message("Asignación ambigua:        ", n_ambiguos)
message("Incidentes excluidos:      ", n_excluidos)
message("Incidentes utilizables:    ", n_final)
message(
  "Porcentaje utilizable:     ",
  round(100 * n_final / n_original, 2),
  "%"
)
message(
  "Folios únicos finales:     ",
  dplyr::n_distinct(incidentes_colonia_final$folio)
)
message("==============================================================")


# ------------------------------------------------------------------------------
# 13. Resumen por colonia
# ------------------------------------------------------------------------------

resumen_colonias <- incidentes_colonia_final |>
  sf::st_drop_geometry() |>
  dplyr::count(
    id_colonia,
    colonia,
    id_alcaldia,
    alcaldia,
    name = "n_incidentes"
  ) |>
  dplyr::arrange(
    dplyr::desc(n_incidentes)
  )

archivo_resumen <- fs::path(
  rutas$output_tables,
  "304_incidentes_por_colonia.csv"
)

readr::write_csv(
  resumen_colonias,
  archivo_resumen
)


# ------------------------------------------------------------------------------
# 14. Guardar base final
# ------------------------------------------------------------------------------

archivo_salida <- fs::path(
  rutas$data_processed,
  "incidentes_c5_colonia.gpkg"
)

sf::st_write(
  incidentes_colonia_final,
  archivo_salida,
  delete_dsn = TRUE,
  quiet = TRUE
)

message("")
message("Archivo guardado en: ", archivo_salida)
message("Resumen guardado en: ", archivo_resumen)