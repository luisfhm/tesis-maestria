# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 306_construir_panel_colonia_mes.R
# Objetivo:
#   - Construir un panel balanceado colonia-mes
#   - Agregar los incidentes C5 a nivel colonia-mes
#   - Completar con cero los meses sin incidentes
#
# Entradas:
#   data/processed/colonias_limpias.gpkg
#   data/processed/incidentes_c5_colonia.gpkg
#
# Salida:
#   data/final/panel_colonia_mes.parquet
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

archivo_incidentes <- fs::path(
  rutas$data_processed,
  "incidentes_c5_colonia.gpkg"
)

validar_archivo(archivo_colonias)
validar_archivo(archivo_incidentes)


# ------------------------------------------------------------------------------
# 2. Leer bases
# ------------------------------------------------------------------------------

colonias <- sf::st_read(
  archivo_colonias,
  quiet = TRUE
)

incidentes <- sf::st_read(
  archivo_incidentes,
  quiet = TRUE
)


# ------------------------------------------------------------------------------
# 3. Preparar catálogo de colonias
# ------------------------------------------------------------------------------

catalogo_colonias <- colonias |>
  sf::st_drop_geometry() |>
  dplyr::select(
    id_colonia,
    colonia,
    id_alcaldia,
    alcaldia,
    area_km2
  ) |>
  dplyr::distinct()


# ------------------------------------------------------------------------------
# 4. Definir periodo temporal
# ------------------------------------------------------------------------------

# Usamos meses completos observados en C5.

fecha_min <- lubridate::floor_date(
  min(incidentes$fecha_creacion, na.rm = TRUE),
  unit = "month"
)

fecha_max <- lubridate::floor_date(
  max(incidentes$fecha_creacion, na.rm = TRUE),
  unit = "month"
)

message("")
message("==============================================================")
message("PERIODO DEL PANEL")
message("==============================================================")
message("Mes inicial: ", fecha_min)
message("Mes final:   ", fecha_max)
message("==============================================================")


# ------------------------------------------------------------------------------
# 5. Construir secuencia de meses
# ------------------------------------------------------------------------------

meses <- tibble::tibble(
  mes = seq.Date(
    from = fecha_min,
    to = fecha_max,
    by = "month"
  )
)


# ------------------------------------------------------------------------------
# 6. Construir panel balanceado colonia-mes
# ------------------------------------------------------------------------------

panel_base <- tidyr::crossing(
  catalogo_colonias,
  meses
)

message(
  "Observaciones panel base: ",
  nrow(panel_base)
)


# ------------------------------------------------------------------------------
# 7. Agregar incidentes a nivel colonia-mes
# ------------------------------------------------------------------------------

incidentes_mes <- incidentes |>
  sf::st_drop_geometry() |>
  dplyr::mutate(
    mes = lubridate::floor_date(
      fecha_creacion,
      unit = "month"
    )
  ) |>
  dplyr::count(
    id_colonia,
    mes,
    name = "n_incidentes"
  )


# ------------------------------------------------------------------------------
# 8. Incorporar outcomes al panel
# ------------------------------------------------------------------------------

panel <- panel_base |>
  dplyr::left_join(
    incidentes_mes,
    by = c(
      "id_colonia",
      "mes"
    )
  ) |>
  dplyr::mutate(
    n_incidentes = tidyr::replace_na(
      n_incidentes,
      0L
    )
  )


# ------------------------------------------------------------------------------
# 9. Crear variables temporales auxiliares
# ------------------------------------------------------------------------------

panel <- panel |>
  dplyr::mutate(
    anio = lubridate::year(mes),
    numero_mes = lubridate::month(mes)
  )


# ------------------------------------------------------------------------------
# 10. Validar panel balanceado
# ------------------------------------------------------------------------------

n_colonias <- dplyr::n_distinct(
  panel$id_colonia
)

n_meses <- dplyr::n_distinct(
  panel$mes
)

n_esperado <- n_colonias * n_meses

message("")
message("==============================================================")
message("VALIDACIÓN DEL PANEL")
message("==============================================================")
message("Colonias:                  ", n_colonias)
message("Meses:                     ", n_meses)
message("Observaciones esperadas:   ", n_esperado)
message("Observaciones observadas:  ", nrow(panel))
message("==============================================================")


if (nrow(panel) != n_esperado) {
  stop(
    "El panel no está balanceado.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 11. Validar unicidad colonia-mes
# ------------------------------------------------------------------------------

duplicados_panel <- panel |>
  dplyr::count(
    id_colonia,
    mes
  ) |>
  dplyr::filter(
    n > 1
  )

if (nrow(duplicados_panel) > 0) {
  stop(
    "Existen combinaciones colonia-mes duplicadas.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 12. Diagnóstico de outcomes
# ------------------------------------------------------------------------------

message("")
message("==============================================================")
message("OUTCOME C5")
message("==============================================================")
message(
  "Incidentes totales en panel: ",
  sum(panel$n_incidentes)
)
message(
  "Media colonia-mes:           ",
  round(mean(panel$n_incidentes), 2)
)
message(
  "Mediana colonia-mes:         ",
  median(panel$n_incidentes)
)
message(
  "Máximo colonia-mes:          ",
  max(panel$n_incidentes)
)
message(
  "Observaciones con cero:      ",
  sum(panel$n_incidentes == 0)
)
message(
  "Porcentaje con cero:         ",
  round(
    100 * mean(panel$n_incidentes == 0),
    2
  ),
  "%"
)
message("==============================================================")


# ------------------------------------------------------------------------------
# 13. Ordenar panel
# ------------------------------------------------------------------------------

panel <- panel |>
  dplyr::arrange(
    id_colonia,
    mes
  )


# ------------------------------------------------------------------------------
# 14. Guardar panel
# ------------------------------------------------------------------------------

archivo_salida <- fs::path(
  rutas$data_final,
  "panel_colonia_mes.parquet"
)

arrow::write_parquet(
  panel,
  archivo_salida
)

message("")
message(
  "Panel guardado en: ",
  archivo_salida
)