# ==============================================================================
# 121_validar_cambios_radares.R
# Validación de cambios espaciales entre inventarios anuales de radares
# ==============================================================================

source("syntax/00_setup/000_setup.R")


# ------------------------------------------------------------------------------
# 1. Leer panel anual construido en 120
# ------------------------------------------------------------------------------

ruta_panel_radares <- fs::path(
  rutas$data_processed,
  "120_radares_ubicaciones_anuales.gpkg"
)

radares_anuales <- sf::st_read(
  ruta_panel_radares,
  quiet = TRUE
) |>
  sf::st_transform(32614)


cat("\nObservaciones del panel anual:", nrow(radares_anuales), "\n")

cat("\nAños disponibles:\n")

print(
  sort(unique(radares_anuales$anio))
)


# ------------------------------------------------------------------------------
# 2. Función de matching entre dos años consecutivos
# ------------------------------------------------------------------------------

comparar_anios <- function(datos, anio_origen, anio_destino) {

  origen <- datos |>
    dplyr::filter(
      anio == anio_origen
    )

  destino <- datos |>
    dplyr::filter(
      anio == anio_destino
    )


  cat(
    "\nComparando",
    anio_origen,
    "→",
    anio_destino,
    "\n"
  )


  # Matriz de distancias
  matriz_distancias <- sf::st_distance(
    origen,
    destino
  )


  # Para cada ubicación de origen:
  # encontrar ubicación más cercana en el siguiente año
  indice_destino <- apply(
    matriz_distancias,
    1,
    which.min
  )

  distancia_min <- apply(
    matriz_distancias,
    1,
    min
  )


  resultado <- sf::st_drop_geometry(
    origen
  ) |>
    dplyr::mutate(
      anio_origen = anio_origen,
      anio_destino = anio_destino,

      distancia_siguiente_m =
        as.numeric(distancia_min),

      id_destino =
        destino$id_ubicacion_anual[
          indice_destino
        ],

      x_destino =
        destino$x_utm[
          indice_destino
        ],

      y_destino =
        destino$y_utm[
          indice_destino
        ],

      ids_destino =
        destino$ids[
          indice_destino
        ],

      tipos_destino =
        destino$tipos[
          indice_destino
        ],

      calle1_destino =
        destino$calle1[
          indice_destino
        ],

      calle2_destino =
        destino$calle2[
          indice_destino
        ]
    )

  resultado
}


# ------------------------------------------------------------------------------
# 3. Ejecutar matching para años consecutivos
# ------------------------------------------------------------------------------

pares_anios <- tibble::tibble(
  anio_origen = 2019:2023,
  anio_destino = 2020:2024
)


matching_anual <- purrr::map2_dfr(
  pares_anios$anio_origen,
  pares_anios$anio_destino,
  ~ comparar_anios(
    radares_anuales,
    .x,
    .y
  )
)


cat("\nMatching anual construido.\n")


# ------------------------------------------------------------------------------
# 4. Clasificar distancias
# ------------------------------------------------------------------------------

matching_anual <- matching_anual |>
  dplyr::mutate(
    rango_distancia = dplyr::case_when(
      distancia_siguiente_m <= 10 ~ "≤10 m",
      distancia_siguiente_m <= 25 ~ "10–25 m",
      distancia_siguiente_m <= 50 ~ "25–50 m",
      distancia_siguiente_m <= 100 ~ "50–100 m",
      TRUE ~ ">100 m"
    )
  )


# ------------------------------------------------------------------------------
# 5. Resumen por transición anual
# ------------------------------------------------------------------------------

resumen_matching_anual <- matching_anual |>
  dplyr::count(
    anio_origen,
    anio_destino,
    rango_distancia,
    name = "ubicaciones"
  ) |>
  dplyr::group_by(
    anio_origen,
    anio_destino
  ) |>
  dplyr::mutate(
    porcentaje = round(
      ubicaciones /
        sum(ubicaciones) *
        100,
      2
    )
  ) |>
  dplyr::ungroup()


cat("\nDistribución de distancias entre inventarios:\n\n")

print(
  resumen_matching_anual,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 6. Indicadores acumulados por tolerancia
# ------------------------------------------------------------------------------

sensibilidad_matching <- matching_anual |>
  dplyr::group_by(
    anio_origen,
    anio_destino
  ) |>
  dplyr::summarise(
    ubicaciones_origen = dplyr::n(),

    pct_10m = round(
      mean(
        distancia_siguiente_m <= 10
      ) * 100,
      2
    ),

    pct_25m = round(
      mean(
        distancia_siguiente_m <= 25
      ) * 100,
      2
    ),

    pct_50m = round(
      mean(
        distancia_siguiente_m <= 50
      ) * 100,
      2
    ),

    pct_100m = round(
      mean(
        distancia_siguiente_m <= 100
      ) * 100,
      2
    ),

    distancia_mediana = round(
      median(
        distancia_siguiente_m
      ),
      2
    ),

    distancia_p90 = round(
      quantile(
        distancia_siguiente_m,
        0.90
      ),
      2
    ),

    .groups = "drop"
  )


cat("\nSensibilidad según tolerancia:\n\n")

print(
  sensibilidad_matching,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 7. Matching inverso
#    ¿Cada ubicación del año siguiente tiene antecedente cercano?
# ------------------------------------------------------------------------------

comparar_anios_inverso <- function(
    datos,
    anio_anterior,
    anio_actual
) {

  anterior <- datos |>
    dplyr::filter(
      anio == anio_anterior
    )

  actual <- datos |>
    dplyr::filter(
      anio == anio_actual
    )


  matriz_distancias <- sf::st_distance(
    actual,
    anterior
  )


  indice_anterior <- apply(
    matriz_distancias,
    1,
    which.min
  )

  distancia_min <- apply(
    matriz_distancias,
    1,
    min
  )


  sf::st_drop_geometry(
    actual
  ) |>
    dplyr::mutate(
      anio_anterior = anio_anterior,
      anio_actual = anio_actual,

      distancia_anterior_m =
        as.numeric(distancia_min),

      id_anterior =
        anterior$id_ubicacion_anual[
          indice_anterior
        ],

      ids_anterior =
        anterior$ids[
          indice_anterior
        ],

      tipos_anterior =
        anterior$tipos[
          indice_anterior
        ]
    )
}


matching_inverso <- purrr::map2_dfr(
  pares_anios$anio_origen,
  pares_anios$anio_destino,
  ~ comparar_anios_inverso(
    radares_anuales,
    .x,
    .y
  )
)


# ------------------------------------------------------------------------------
# 8. Posibles entradas según distintas tolerancias
# ------------------------------------------------------------------------------

entradas_por_tolerancia <- matching_inverso |>
  dplyr::group_by(
    anio_actual
  ) |>
  dplyr::summarise(
    ubicaciones = dplyr::n(),

    nuevas_10m = sum(
      distancia_anterior_m > 10
    ),

    nuevas_25m = sum(
      distancia_anterior_m > 25
    ),

    nuevas_50m = sum(
      distancia_anterior_m > 50
    ),

    nuevas_100m = sum(
      distancia_anterior_m > 100
    ),

    .groups = "drop"
  )


cat("\nPosibles entradas por tolerancia:\n\n")

print(
  entradas_por_tolerancia,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 9. Posibles salidas según distintas tolerancias
# ------------------------------------------------------------------------------

salidas_por_tolerancia <- matching_anual |>
  dplyr::group_by(
    anio_origen
  ) |>
  dplyr::summarise(
    ubicaciones = dplyr::n(),

    salidas_10m = sum(
      distancia_siguiente_m > 10
    ),

    salidas_25m = sum(
      distancia_siguiente_m > 25
    ),

    salidas_50m = sum(
      distancia_siguiente_m > 50
    ),

    salidas_100m = sum(
      distancia_siguiente_m > 100
    ),

    .groups = "drop"
  )


cat("\nPosibles salidas por tolerancia:\n\n")

print(
  salidas_por_tolerancia,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 10. Candidatos claros de entrada
# ------------------------------------------------------------------------------

candidatos_entrada_100m <- matching_inverso |>
  dplyr::filter(
    distancia_anterior_m > 100
  ) |>
  dplyr::arrange(
    anio_actual,
    dplyr::desc(
      distancia_anterior_m
    )
  )



cat("\nCandidatos de entrada >100 m:\n\n")

candidatos_entrada_100m |>
  tibble::as_tibble() |>
  print(
    n = Inf,
    width = Inf
  )


# ------------------------------------------------------------------------------
# 11. Candidatos claros de salida
# ------------------------------------------------------------------------------

candidatos_salida_100m <- matching_anual |>
  dplyr::filter(
    distancia_siguiente_m > 100
  ) |>
  dplyr::arrange(
    anio_origen,
    dplyr::desc(
      distancia_siguiente_m
    )
  )



cat("\nCandidatos de salida >100 m:\n\n")

candidatos_salida_100m |>
  tibble::as_tibble() |>
  print(
    n = Inf,
    width = Inf
  )


# ------------------------------------------------------------------------------
# 12. Guardar resultados
# ------------------------------------------------------------------------------

readr::write_csv(
  matching_anual,
  fs::path(
    rutas$output_tables,
    "121_radares_matching_anual.csv"
  )
)

readr::write_csv(
  matching_inverso,
  fs::path(
    rutas$output_tables,
    "121_radares_matching_inverso.csv"
  )
)

readr::write_csv(
  resumen_matching_anual,
  fs::path(
    rutas$output_tables,
    "121_radares_resumen_matching.csv"
  )
)

readr::write_csv(
  sensibilidad_matching,
  fs::path(
    rutas$output_tables,
    "121_radares_sensibilidad_tolerancia.csv"
  )
)

readr::write_csv(
  entradas_por_tolerancia,
  fs::path(
    rutas$output_tables,
    "121_radares_entradas_tolerancia.csv"
  )
)

readr::write_csv(
  salidas_por_tolerancia,
  fs::path(
    rutas$output_tables,
    "121_radares_salidas_tolerancia.csv"
  )
)

readr::write_csv(
  candidatos_entrada_100m,
  fs::path(
    rutas$output_tables,
    "121_radares_candidatos_entrada_100m.csv"
  )
)

readr::write_csv(
  candidatos_salida_100m,
  fs::path(
    rutas$output_tables,
    "121_radares_candidatos_salida_100m.csv"
  )
)


message(
  "Validación preliminar de cambios entre inventarios terminada."
)