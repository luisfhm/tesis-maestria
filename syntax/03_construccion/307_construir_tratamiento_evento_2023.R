# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 307_construir_tratamiento_evento_2023.R
# Objetivo:
#   - Incorporar infraestructura preexistente al panel colonia-mes
#   - Definir colonias tratadas antes del evento de septiembre de 2023
#   - Construir Post y tiempo relativo al evento
#
# Entradas:
#   data/final/panel_colonia_mes.parquet
#   data/processed/fotocivicas_2022_colonia.gpkg
#
# Salida:
#   data/final/panel_evento_2023.parquet
#
# Unidad de observación:
#   colonia-mes
#
# Tratamiento:
#   T_i = 1 si la colonia tenía al menos una instalación de Fotocívicas
#         en el corte de diciembre de 2022.
#
# Evento:
#   Reforma para motociclistas de septiembre de 2023.
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

archivo_panel <- fs::path(
  rutas$data_final,
  "panel_colonia_mes.parquet"
)

archivo_infraestructura <- fs::path(
  rutas$data_processed,
  "fotocivicas_2022_colonia.gpkg"
)

validar_archivo(archivo_panel)
validar_archivo(archivo_infraestructura)


# ------------------------------------------------------------------------------
# 2. Leer bases
# ------------------------------------------------------------------------------

panel <- arrow::read_parquet(
  archivo_panel
) |>
  tibble::as_tibble()

infraestructura <- sf::st_read(
  archivo_infraestructura,
  quiet = TRUE
)


# ------------------------------------------------------------------------------
# 3. Construir infraestructura por colonia
# ------------------------------------------------------------------------------

infra_colonia <- infraestructura |>
  sf::st_drop_geometry() |>
  dplyr::count(
    id_colonia,
    name = "n_fotocivicas_2022"
  )


# ------------------------------------------------------------------------------
# 4. Incorporar infraestructura al panel
# ------------------------------------------------------------------------------

panel_evento <- panel |>
  dplyr::left_join(
    infra_colonia,
    by = "id_colonia"
  ) |>
  dplyr::mutate(
    n_fotocivicas_2022 = tidyr::replace_na(
      n_fotocivicas_2022,
      0L
    ),

    tratado = as.integer(
      n_fotocivicas_2022 > 0
    )
  )


# ------------------------------------------------------------------------------
# 5. Definir evento
# ------------------------------------------------------------------------------

fecha_evento <- as.Date("2023-09-01")

panel_evento <- panel_evento |>
  dplyr::mutate(
    post = as.integer(
      mes >= fecha_evento
    )
  )


# ------------------------------------------------------------------------------
# 6. Construir tiempo relativo al evento
# ------------------------------------------------------------------------------

# Número de meses respecto a septiembre de 2023.
#
# Ejemplos:
#   agosto 2023     = -1
#   septiembre 2023 =  0
#   octubre 2023    =  1

panel_evento <- panel_evento |>
  dplyr::mutate(
    tiempo_evento =
      (lubridate::year(mes) - lubridate::year(fecha_evento)) * 12 +
      (lubridate::month(mes) - lubridate::month(fecha_evento))
  )


# ------------------------------------------------------------------------------
# 7. Validar que el tratamiento sea fijo dentro de colonia
# ------------------------------------------------------------------------------

variacion_tratamiento <- panel_evento |>
  dplyr::group_by(
    id_colonia
  ) |>
  dplyr::summarise(
    n_valores_tratado = dplyr::n_distinct(tratado),
    .groups = "drop"
  ) |>
  dplyr::filter(
    n_valores_tratado != 1
  )

if (nrow(variacion_tratamiento) > 0) {
  stop(
    "El indicador de tratamiento varía dentro de colonia.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 8. Diagnóstico de grupos
# ------------------------------------------------------------------------------

resumen_tratamiento <- panel_evento |>
  dplyr::distinct(
    id_colonia,
    colonia,
    alcaldia,
    tratado,
    n_fotocivicas_2022
  ) |>
  dplyr::count(
    tratado,
    name = "n_colonias"
  )

print(resumen_tratamiento)


# ------------------------------------------------------------------------------
# 9. Diagnóstico de infraestructura
# ------------------------------------------------------------------------------

resumen_infra <- panel_evento |>
  dplyr::distinct(
    id_colonia,
    n_fotocivicas_2022
  ) |>
  dplyr::count(
    n_fotocivicas_2022,
    name = "n_colonias"
  ) |>
  dplyr::arrange(
    n_fotocivicas_2022
  )

print(resumen_infra)


# ------------------------------------------------------------------------------
# 10. Diagnóstico temporal
# ------------------------------------------------------------------------------

message("")
message("==============================================================")
message("EVENTO SEPTIEMBRE 2023")
message("==============================================================")
message(
  "Colonias totales:       ",
  dplyr::n_distinct(panel_evento$id_colonia)
)
message(
  "Colonias tratadas:      ",
  dplyr::n_distinct(
    panel_evento$id_colonia[
      panel_evento$tratado == 1
    ]
  )
)
message(
  "Colonias control:       ",
  dplyr::n_distinct(
    panel_evento$id_colonia[
      panel_evento$tratado == 0
    ]
  )
)
message(
  "Meses pre:              ",
  dplyr::n_distinct(
    panel_evento$mes[
      panel_evento$tiempo_evento < 0
    ]
  )
)
message(
  "Meses post:             ",
  dplyr::n_distinct(
    panel_evento$mes[
      panel_evento$tiempo_evento >= 0
    ]
  )
)
message(
  "Observaciones:          ",
  nrow(panel_evento)
)
message("==============================================================")


# ------------------------------------------------------------------------------
# 11. Comparación descriptiva básica
# ------------------------------------------------------------------------------

descriptivos_grupo <- panel_evento |>
  dplyr::group_by(
    tratado
  ) |>
  dplyr::summarise(
    colonias = dplyr::n_distinct(id_colonia),
    media_incidentes = mean(n_incidentes),
    mediana_incidentes = median(n_incidentes),
    sd_incidentes = sd(n_incidentes),
    .groups = "drop"
  )

print(descriptivos_grupo)


# ------------------------------------------------------------------------------
# 12. Ordenar
# ------------------------------------------------------------------------------

panel_evento <- panel_evento |>
  dplyr::arrange(
    id_colonia,
    mes
  )


# ------------------------------------------------------------------------------
# 13. Guardar
# ------------------------------------------------------------------------------

archivo_salida <- fs::path(
  rutas$data_final,
  "panel_evento_2023.parquet"
)

arrow::write_parquet(
  panel_evento,
  archivo_salida
)

message("")
message(
  "Panel del evento guardado en: ",
  archivo_salida
)