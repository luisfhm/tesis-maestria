# ==============================================================================
# 122_clasificar_entradas_2023.R
# Validación histórica de las entradas candidatas de radares en 2023
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
# 2. Leer candidatos identificados por 121
# ------------------------------------------------------------------------------

candidatos_entrada <- readr::read_csv(
  fs::path(
    rutas$output_tables,
    "121_radares_candidatos_entrada_100m.csv"
  ),
  show_col_types = FALSE
)


candidatos_2023 <- candidatos_entrada |>
  dplyr::filter(
    anio_actual == 2023
  )


cat(
  "\nCandidatos 2023 respecto de 2022:",
  nrow(candidatos_2023),
  "\n"
)


# ------------------------------------------------------------------------------
# 3. Reconstruir geometría de candidatos 2023
# ------------------------------------------------------------------------------

candidatos_2023_sf <- candidatos_2023 |>
  sf::st_as_sf(
    coords = c(
      "x_utm",
      "y_utm"
    ),
    crs = 32614,
    remove = FALSE
  )


# ------------------------------------------------------------------------------
# 4. Inventario histórico anterior a 2023
# ------------------------------------------------------------------------------

historico_pre2023 <- radares_anuales |>
  dplyr::filter(
    anio < 2023
  )


# ------------------------------------------------------------------------------
# 5. Distancia de cada candidato a cualquier ubicación histórica
# ------------------------------------------------------------------------------

distancias_historicas <- sf::st_distance(
  candidatos_2023_sf,
  historico_pre2023
)


indice_historico <- apply(
  distancias_historicas,
  1,
  which.min
)

distancia_historica <- apply(
  distancias_historicas,
  1,
  min
)


# ------------------------------------------------------------------------------
# 6. Incorporar antecedente histórico más cercano
# ------------------------------------------------------------------------------

validacion_2023 <- candidatos_2023 |>
  dplyr::mutate(
    distancia_historica_m =
      as.numeric(distancia_historica),

    anio_historico_cercano =
      historico_pre2023$anio[
        indice_historico
      ],

    id_historico_cercano =
      historico_pre2023$id_ubicacion_anual[
        indice_historico
      ],

    ids_historicos =
      historico_pre2023$ids[
        indice_historico
      ],

    tipos_historicos =
      historico_pre2023$tipos[
        indice_historico
      ],

    calle1_historica =
      historico_pre2023$calle1[
        indice_historico
      ],

    calle2_historica =
      historico_pre2023$calle2[
        indice_historico
      ]
  )


# ------------------------------------------------------------------------------
# 7. Clasificación preliminar
# ------------------------------------------------------------------------------

validacion_2023 <- validacion_2023 |>
  dplyr::mutate(
    clasificacion_100m = dplyr::case_when(
      distancia_historica_m <= 100 ~
        "reaparicion_historica",

      distancia_historica_m > 100 ~
        "nueva_historica_candidata"
    ),

    rango_historico = dplyr::case_when(
      distancia_historica_m <= 10  ~ "≤10 m",
      distancia_historica_m <= 25  ~ "10–25 m",
      distancia_historica_m <= 50  ~ "25–50 m",
      distancia_historica_m <= 100 ~ "50–100 m",
      distancia_historica_m <= 250 ~ "100–250 m",
      distancia_historica_m <= 500 ~ "250–500 m",
      distancia_historica_m <= 1000 ~ "500–1000 m",
      TRUE ~ ">1000 m"
    )
  )


# ------------------------------------------------------------------------------
# 8. Resumen según tolerancia
# ------------------------------------------------------------------------------

sensibilidad_historica <- tibble::tibble(
  tolerancia_m = c(
    10,
    25,
    50,
    100,
    250,
    500,
    1000
  ),

  nuevas = purrr::map_int(
    tolerancia_m,
    ~ sum(
      validacion_2023$distancia_historica_m > .x
    )
  )
) |>
  dplyr::mutate(
    total = nrow(validacion_2023),

    porcentaje_nuevas = round(
      nuevas / total * 100,
      2
    )
  )


cat("\nSensibilidad histórica:\n\n")

print(
  sensibilidad_historica,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 9. Mostrar candidatos ordenados
# ------------------------------------------------------------------------------

revision_2023 <- validacion_2023 |>
  dplyr::select(
    ids,
    tipos,
    calle1,
    calle2,

    distancia_anterior_m,

    distancia_historica_m,
    anio_historico_cercano,

    ids_historicos,
    tipos_historicos,

    calle1_historica,
    calle2_historica,

    rango_historico,
    clasificacion_100m
  ) |>
  dplyr::arrange(
    dplyr::desc(
      distancia_historica_m
    )
  )


cat("\nValidación histórica de entradas 2023:\n\n")

print(
  revision_2023,
  n = Inf
)


# ------------------------------------------------------------------------------
# 10. Guardar
# ------------------------------------------------------------------------------

readr::write_csv(
  validacion_2023,
  fs::path(
    rutas$output_tables,
    "122_radares_validacion_historica_2023.csv"
  )
)

readr::write_csv(
  sensibilidad_historica,
  fs::path(
    rutas$output_tables,
    "122_radares_sensibilidad_historica_2023.csv"
  )
)

readr::write_csv(
  revision_2023,
  fs::path(
    rutas$output_tables,
    "122_radares_revision_entradas_2023.csv"
  )
)


message(
  "Validación histórica de entradas 2023 terminada."
)