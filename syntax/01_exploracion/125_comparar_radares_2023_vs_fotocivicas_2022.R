# ==============================================================================
# 125_comparar_radares_2023_vs_fotocivicas_2022.R
# Comparación de candidatos 2023 contra puntos Fotocívicas de dic-2022
# ==============================================================================

source("syntax/00_setup/000_setup.R")


# ------------------------------------------------------------------------------
# 1. Leer candidatos finales >500 m
# ------------------------------------------------------------------------------

tratamientos_2023 <- readr::read_csv(
  fs::path(
    rutas$output_tables,
    "124_radares_tratamientos_finales_2023.csv"
  ),
  show_col_types = FALSE
)

tratamientos_2023_sf <- tratamientos_2023 |>
  sf::st_as_sf(
    coords = c("x_utm", "y_utm"),
    crs = 32614,
    remove = FALSE
  )


# ------------------------------------------------------------------------------
# 2. Leer puntos Fotocívicas dic-2022
# ------------------------------------------------------------------------------

ruta_zip_puntos <- fs::path(
  rutas$data_raw,
  "07_fotocivicas",
  "ubicacion",
  "fotocivicas-ubicacion-puntos.zip"
)

dir_puntos <- fs::path_temp(
  "fotocivicas_ubicacion_puntos"
)

if (fs::dir_exists(dir_puntos)) {
  fs::dir_delete(dir_puntos)
}

fs::dir_create(dir_puntos)

utils::unzip(
  ruta_zip_puntos,
  exdir = dir_puntos
)

shp_puntos <- fs::dir_ls(
  dir_puntos,
  recurse = TRUE,
  regexp = "\\.shp$",
  type = "file"
)

fotocivicas_puntos <- sf::st_read(
  shp_puntos,
  quiet = TRUE
)

fotocivicas_puntos_utm <- fotocivicas_puntos |>
  sf::st_transform(32614)


cat(
  "\nPuntos Fotocívicas dic-2022:",
  nrow(fotocivicas_puntos),
  "\n"
)
# ------------------------------------------------------------------------------
# 3. Distancia de cada tratamiento a punto Fotocívica 2022 más cercano
# ------------------------------------------------------------------------------

distancias_2022 <- sf::st_distance(
  tratamientos_2023_sf,
  fotocivicas_puntos_utm
)

indice_2022 <- apply(
  distancias_2022,
  1,
  which.min
)

distancia_2022 <- apply(
  distancias_2022,
  1,
  min
)


# ------------------------------------------------------------------------------
# 4. Incorporar antecedente 2022 más cercano
# ------------------------------------------------------------------------------

comparacion_2022 <- tratamientos_2023 |>
  dplyr::mutate(
    distancia_fotocivica_2022_m =
      as.numeric(distancia_2022),

    ubi_2022 =
      fotocivicas_puntos_utm$ubi[
        indice_2022
      ],

    no_2022 =
      fotocivicas_puntos_utm$no[
        indice_2022
      ],

    via_2022 =
      fotocivicas_puntos_utm$via_princi[
        indice_2022
      ],

    ubicacion_2022 =
      fotocivicas_puntos_utm$ubicacion[
        indice_2022
      ],

    sentido_2022 =
      fotocivicas_puntos_utm$sentido[
        indice_2022
      ]
  )


# ------------------------------------------------------------------------------
# 5. Clasificación por tolerancia
# ------------------------------------------------------------------------------

comparacion_2022 <- comparacion_2022 |>
  dplyr::mutate(
    rango_2022 = dplyr::case_when(
      distancia_fotocivica_2022_m <= 10 ~ "≤10 m",
      distancia_fotocivica_2022_m <= 25 ~ "10–25 m",
      distancia_fotocivica_2022_m <= 50 ~ "25–50 m",
      distancia_fotocivica_2022_m <= 100 ~ "50–100 m",
      distancia_fotocivica_2022_m <= 250 ~ "100–250 m",
      distancia_fotocivica_2022_m <= 500 ~ "250–500 m",
      distancia_fotocivica_2022_m <= 1000 ~ "500–1000 m",
      TRUE ~ ">1000 m"
    ),

    ausente_2022_100m =
      distancia_fotocivica_2022_m > 100,

    ausente_2022_250m =
      distancia_fotocivica_2022_m > 250,

    ausente_2022_500m =
      distancia_fotocivica_2022_m > 500
  )


# ------------------------------------------------------------------------------
# 6. Resumen
# ------------------------------------------------------------------------------

resumen_comparacion_2022 <- tibble::tibble(
  criterio = c(
    ">100 m",
    ">250 m",
    ">500 m",
    ">1000 m"
  ),

  candidatos_ausentes = c(
    sum(comparacion_2022$distancia_fotocivica_2022_m > 100),
    sum(comparacion_2022$distancia_fotocivica_2022_m > 250),
    sum(comparacion_2022$distancia_fotocivica_2022_m > 500),
    sum(comparacion_2022$distancia_fotocivica_2022_m > 1000)
  )
)


cat("\nResumen comparación contra Fotocívicas dic-2022:\n\n")

print(
  resumen_comparacion_2022,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 7. Tabla detallada
# ------------------------------------------------------------------------------

revision_comparacion_2022 <- comparacion_2022 |>
  dplyr::select(
    id_raw,
    calle1_raw,
    calle2_raw,
    distancia_historica_m,
    distancia_fotocivica_2022_m,
    ubi_2022,
    no_2022,
    via_2022,
    ubicacion_2022,
    sentido_2022,
    rango_2022
  ) |>
  dplyr::arrange(
    dplyr::desc(
      distancia_fotocivica_2022_m
    )
  )


cat("\nDetalle comparación 2023 vs dic-2022:\n\n")

print(
  revision_comparacion_2022,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 8. Guardar resultados
# ------------------------------------------------------------------------------

readr::write_csv(
  comparacion_2022,
  fs::path(
    rutas$output_tables,
    "125_radares_comparacion_fotocivicas_2022.csv"
  )
)

readr::write_csv(
  resumen_comparacion_2022,
  fs::path(
    rutas$output_tables,
    "125_radares_resumen_fotocivicas_2022.csv"
  )
)

readr::write_csv(
  revision_comparacion_2022,
  fs::path(
    rutas$output_tables,
    "125_radares_revision_fotocivicas_2022.csv"
  )
)


message(
  "Comparación de candidatos 2023 contra puntos Fotocívicas 2022 terminada."
)