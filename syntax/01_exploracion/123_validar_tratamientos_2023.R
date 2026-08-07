# ==============================================================================
# 123_validar_tratamientos_2023.R
# Validación espacial final de tratamientos candidatos de 2023
# ==============================================================================

source("syntax/00_setup/000_setup.R")


# ------------------------------------------------------------------------------
# 1. Leer panel anual
# ------------------------------------------------------------------------------

radares_anuales <- sf::st_read(
  fs::path(
    rutas$data_processed,
    "120_radares_ubicaciones_anuales.gpkg"
  ),
  quiet = TRUE
) |>
  sf::st_transform(32614)


# ------------------------------------------------------------------------------
# 2. Leer validación histórica
# ------------------------------------------------------------------------------

validacion <- readr::read_csv(
  fs::path(
    rutas$output_tables,
    "122_radares_validacion_historica_2023.csv"
  ),
  show_col_types = FALSE
)


# ------------------------------------------------------------------------------
# 3. Clasificar tratamientos
# ------------------------------------------------------------------------------

validacion <- validacion |>
  dplyr::mutate(
    tratamiento_100m =
      distancia_historica_m > 100,

    tratamiento_250m =
      distancia_historica_m > 250,

    tratamiento_500m =
      distancia_historica_m > 500,

    tratamiento_1000m =
      distancia_historica_m > 1000
  )


# ------------------------------------------------------------------------------
# 4. Crear geometrías
# ------------------------------------------------------------------------------

tratamientos_sf <- validacion |>
  sf::st_as_sf(
    coords = c(
      "x_utm",
      "y_utm"
    ),
    crs = 32614,
    remove = FALSE
  )


# ------------------------------------------------------------------------------
# 5. Recuperar antecedente histórico exacto
# ------------------------------------------------------------------------------

antecedentes_sf <- radares_anuales |>
  dplyr::filter(
    id_ubicacion_anual %in%
      validacion$id_historico_cercano
  )


# ------------------------------------------------------------------------------
# 6. Guardar capas para inspección
# ------------------------------------------------------------------------------

ruta_salida <- fs::path(
  rutas$data_processed,
  "123_validacion_tratamientos_2023.gpkg"
)


sf::st_write(
  tratamientos_sf,
  ruta_salida,
  layer = "candidatos_2023",
  delete_dsn = TRUE,
  quiet = TRUE
)

sf::st_write(
  antecedentes_sf,
  ruta_salida,
  layer = "antecedentes_historicos",
  append = TRUE,
  quiet = TRUE
)


# ------------------------------------------------------------------------------
# 7. Resumen final de tratamientos
# ------------------------------------------------------------------------------

resumen_tratamientos <- tibble::tibble(
  criterio = c(
    ">100 m",
    ">250 m",
    ">500 m",
    ">1000 m"
  ),

  radares = c(
    sum(validacion$tratamiento_100m),
    sum(validacion$tratamiento_250m),
    sum(validacion$tratamiento_500m),
    sum(validacion$tratamiento_1000m)
  )
)


cat("\nTratamientos candidatos según definición:\n\n")

print(
  resumen_tratamientos,
  n = Inf
)


readr::write_csv(
  resumen_tratamientos,
  fs::path(
    rutas$output_tables,
    "123_radares_resumen_tratamientos_2023.csv"
  )
)


message(
  "Validación espacial preparada."
)