# ==============================================================================
# 126_validar_radares_2023_vs_lineas_2022.R
# Validación de candidatos 2023 contra tramos Fotocívicas de dic-2022
# ==============================================================================

source("syntax/00_setup/000_setup.R")


# ------------------------------------------------------------------------------
# 1. Leer comparación contra puntos 2022
# ------------------------------------------------------------------------------

comparacion_puntos <- readr::read_csv(
  fs::path(
    rutas$output_tables,
    "125_radares_comparacion_fotocivicas_2022.csv"
  ),
  show_col_types = FALSE
)


# Candidatos que sobreviven al criterio de 250 m contra puntos.
# Conservamos 10 para evaluar sensibilidad; el principal seguirá siendo 500 m.

candidatos <- comparacion_puntos |>
  dplyr::filter(
    distancia_fotocivica_2022_m > 250
  )


cat(
  "\nCandidatos >250 m de puntos Fotocívicas dic-2022:",
  nrow(candidatos),
  "\n"
)

cat(
  "Candidatos >500 m de puntos Fotocívicas dic-2022:",
  sum(candidatos$distancia_fotocivica_2022_m > 500),
  "\n"
)


# ------------------------------------------------------------------------------
# 2. Reconstruir geometría de candidatos
# ------------------------------------------------------------------------------

candidatos_sf <- candidatos |>
  sf::st_as_sf(
    coords = c(
      "x_utm",
      "y_utm"
    ),
    crs = 32614,
    remove = FALSE
  )


# ------------------------------------------------------------------------------
# 3. Leer capa de líneas Fotocívicas de diciembre de 2022
# ------------------------------------------------------------------------------

ruta_zip_lineas <- fs::path(
  rutas$data_raw,
  "07_fotocivicas",
  "ubicacion",
  "fotocivicas-ubicacion-lineas.zip"
)


dir_lineas <- fs::path_temp(
  "fotocivicas_ubicacion_lineas_126"
)


if (fs::dir_exists(dir_lineas)) {
  fs::dir_delete(dir_lineas)
}

fs::dir_create(dir_lineas)


utils::unzip(
  ruta_zip_lineas,
  exdir = dir_lineas
)


shp_lineas <- fs::dir_ls(
  dir_lineas,
  recurse = TRUE,
  regexp = "\\.shp$",
  type = "file"
)


fotocivicas_lineas <- sf::st_read(
  shp_lineas,
  quiet = TRUE
)


cat(
  "\nTramos Fotocívicas dic-2022:",
  nrow(fotocivicas_lineas),
  "\n"
)


# ------------------------------------------------------------------------------
# 4. Transformar líneas al CRS métrico
# ------------------------------------------------------------------------------

fotocivicas_lineas_utm <- fotocivicas_lineas |>
  sf::st_transform(32614)


# ------------------------------------------------------------------------------
# 5. Calcular distancia al tramo 2022 más cercano
# ------------------------------------------------------------------------------

distancias_lineas <- sf::st_distance(
  candidatos_sf,
  fotocivicas_lineas_utm
)


indice_linea <- apply(
  distancias_lineas,
  1,
  which.min
)


distancia_linea <- apply(
  distancias_lineas,
  1,
  min
)


# ------------------------------------------------------------------------------
# 6. Incorporar información del tramo más cercano
# ------------------------------------------------------------------------------

comparacion_lineas <- candidatos |>
  dplyr::mutate(
    distancia_linea_2022_m =
      as.numeric(distancia_linea),

    ubi_linea_2022 =
      fotocivicas_lineas_utm$ubi[
        indice_linea
      ],

    no_linea_2022 =
      fotocivicas_lineas_utm$no[
        indice_linea
      ],

    via_linea_2022 =
      fotocivicas_lineas_utm$via_princi[
        indice_linea
      ],

    ubicacion_linea_2022 =
      fotocivicas_lineas_utm$ubicacion[
        indice_linea
      ],

    sentido_linea_2022 =
      fotocivicas_lineas_utm$sentido[
        indice_linea
      ]
  )


# ------------------------------------------------------------------------------
# 7. Clasificar distancia a líneas
# ------------------------------------------------------------------------------

comparacion_lineas <- comparacion_lineas |>
  dplyr::mutate(
    rango_linea_2022 = dplyr::case_when(
      distancia_linea_2022_m <= 10 ~ "≤10 m",
      distancia_linea_2022_m <= 25 ~ "10–25 m",
      distancia_linea_2022_m <= 50 ~ "25–50 m",
      distancia_linea_2022_m <= 100 ~ "50–100 m",
      distancia_linea_2022_m <= 250 ~ "100–250 m",
      distancia_linea_2022_m <= 500 ~ "250–500 m",
      distancia_linea_2022_m <= 1000 ~ "500–1000 m",
      TRUE ~ ">1000 m"
    )
  )


# ------------------------------------------------------------------------------
# 8. Distancia mínima a cualquier infraestructura 2022
#    (punto o línea)
# ------------------------------------------------------------------------------

comparacion_lineas <- comparacion_lineas |>
  dplyr::mutate(
    distancia_infraestructura_2022_m =
      pmin(
        distancia_fotocivica_2022_m,
        distancia_linea_2022_m,
        na.rm = TRUE
      ),

    tratamiento_100m =
      distancia_infraestructura_2022_m > 100,

    tratamiento_250m =
      distancia_infraestructura_2022_m > 250,

    tratamiento_500m =
      distancia_infraestructura_2022_m > 500,

    tratamiento_1000m =
      distancia_infraestructura_2022_m > 1000
  )


# ------------------------------------------------------------------------------
# 9. Resumen final según distancia a cualquier infraestructura 2022
# ------------------------------------------------------------------------------

resumen_final <- tibble::tibble(
  criterio = c(
    ">100 m",
    ">250 m",
    ">500 m",
    ">1000 m"
  ),

  candidatos = c(
    sum(comparacion_lineas$tratamiento_100m),
    sum(comparacion_lineas$tratamiento_250m),
    sum(comparacion_lineas$tratamiento_500m),
    sum(comparacion_lineas$tratamiento_1000m)
  )
)


cat("\nCandidatos según distancia a cualquier infraestructura dic-2022:\n\n")

print(
  resumen_final,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 10. Tabla detallada
# ------------------------------------------------------------------------------

revision_final <- comparacion_lineas |>
  dplyr::select(
    id_raw,
    calle1_raw,
    calle2_raw,

    distancia_historica_m,

    distancia_fotocivica_2022_m,
    via_2022,
    ubicacion_2022,

    distancia_linea_2022_m,
    via_linea_2022,
    ubicacion_linea_2022,

    distancia_infraestructura_2022_m,

    tratamiento_250m,
    tratamiento_500m,
    tratamiento_1000m
  ) |>
  dplyr::arrange(
    dplyr::desc(
      distancia_infraestructura_2022_m
    )
  )


cat("\nValidación final contra infraestructura dic-2022:\n\n")

print(
  revision_final,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 11. Tratamientos principales >500 m
# ------------------------------------------------------------------------------

tratamientos_finales <- comparacion_lineas |>
  dplyr::filter(
    tratamiento_500m
  ) |>
  dplyr::arrange(
    dplyr::desc(
      distancia_infraestructura_2022_m
    )
  )


cat(
  "\nNúmero final de tratamientos >500 m:",
  nrow(tratamientos_finales),
  "\n"
)


print(
  tratamientos_finales |>
    dplyr::select(
      id_raw,
      calle1_raw,
      calle2_raw,
      distancia_fotocivica_2022_m,
      distancia_linea_2022_m,
      distancia_infraestructura_2022_m
    ),
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 12. Guardar resultados
# ------------------------------------------------------------------------------

readr::write_csv(
  comparacion_lineas,
  fs::path(
    rutas$output_tables,
    "126_radares_comparacion_lineas_2022.csv"
  )
)

readr::write_csv(
  resumen_final,
  fs::path(
    rutas$output_tables,
    "126_radares_resumen_validacion_final.csv"
  )
)

readr::write_csv(
  revision_final,
  fs::path(
    rutas$output_tables,
    "126_radares_revision_validacion_final.csv"
  )
)

readr::write_csv(
  tratamientos_finales,
  fs::path(
    rutas$output_tables,
    "126_radares_tratamientos_finales.csv"
  )
)


message(
  "Validación contra infraestructura Fotocívicas de diciembre de 2022 terminada."
)