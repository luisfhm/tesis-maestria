# ==============================================================================
# 300_construir_panel_radares_c5.R
# Construcción del panel mensual de incidentes C5 alrededor de radares candidatos
# ==============================================================================

source("syntax/00_setup/000_setup.R")


# ------------------------------------------------------------------------------
# 1. Leer tratamientos candidatos validados
# ------------------------------------------------------------------------------

radares_candidatos <- readr::read_csv(
  fs::path(
    rutas$output_tables,
    "126_radares_comparacion_lineas_2022.csv"
  ),
  show_col_types = FALSE
) |>
  dplyr::filter(
    tratamiento_250m
  ) |>
  dplyr::mutate(
    id_radar = stringr::str_squish(id_raw)
  )


cat(
  "\nRadares candidatos >250 m:",
  nrow(radares_candidatos),
  "\n"
)

cat(
  "Tratamiento principal >500 m:",
  sum(radares_candidatos$tratamiento_500m),
  "\n"
)

cat(
  "Tratamiento estricto >1000 m:",
  sum(radares_candidatos$tratamiento_1000m),
  "\n"
)


# ------------------------------------------------------------------------------
# 2. Crear objeto espacial de radares
# ------------------------------------------------------------------------------

radares_sf <- radares_candidatos |>
  sf::st_as_sf(
    coords = c(
      "x_utm",
      "y_utm"
    ),
    crs = 32614,
    remove = FALSE
  )


# ------------------------------------------------------------------------------
# 3. Localizar archivos C5
# ------------------------------------------------------------------------------

ruta_c5 <- fs::path(
  rutas$data_raw,
  "01_incidentes_c5"
)


archivos_c5 <- fs::dir_ls(
  ruta_c5,
  regexp = "\\.csv$",
  type = "file"
) |>
  purrr::discard(
    ~ fs::path_file(.x) == "inViales_2022_2023.csv"
  )


cat(
  "\nArchivos C5 utilizados:",
  length(archivos_c5),
  "\n\n"
)

print(
  fs::path_file(archivos_c5)
)


# ------------------------------------------------------------------------------
# 4. Leer muestra para identificar variables
# ------------------------------------------------------------------------------

muestra_c5 <- readr::read_csv(
  archivos_c5[1],
  n_max = 1000,
  show_col_types = FALSE,
  progress = FALSE,
  name_repair = "unique"
)


cat("\nVariables C5:\n\n")

print(
  names(muestra_c5)
)


# ------------------------------------------------------------------------------
# 5. Detectar variables clave
# ------------------------------------------------------------------------------

buscar_variable <- function(
    nombres,
    candidatos
) {

  nombres_lower <- stringr::str_to_lower(
    nombres
  )

  candidatos_lower <- stringr::str_to_lower(
    candidatos
  )

  pos <- match(
    candidatos_lower,
    nombres_lower
  )

  pos <- pos[
    !is.na(pos)
  ]

  if (length(pos) == 0) {
    return(NA_character_)
  }

  nombres[
    pos[1]
  ]
}


variable_fecha <- buscar_variable(
  names(muestra_c5),
  c(
    "fecha_creacion",
    "fecha_evento",
    "fecha",
    "fecha_incidente",
    "fecha_hora",
    "fecha_registro"
  )
)


variable_lat <- buscar_variable(
  names(muestra_c5),
  c(
    "latitud",
    "latitude",
    "lat"
  )
)


variable_lon <- buscar_variable(
  names(muestra_c5),
  c(
    "longitud",
    "longitude",
    "lon",
    "lng"
  )
)


cat(
  "\nVariable fecha detectada:",
  variable_fecha,
  "\n"
)

cat(
  "Variable latitud detectada:",
  variable_lat,
  "\n"
)

cat(
  "Variable longitud detectada:",
  variable_lon,
  "\n"
)


if (
  is.na(variable_fecha) ||
  is.na(variable_lat) ||
  is.na(variable_lon)
) {

  stop(
    paste0(
      "\nNo fue posible detectar automáticamente ",
      "fecha/latitud/longitud en C5.\n",
      "Revisa names(muestra_c5) y ajusta el bloque 5."
    )
  )
}


# ------------------------------------------------------------------------------
# 6. Leer y consolidar archivos C5
# ------------------------------------------------------------------------------

leer_c5 <- function(ruta) {

  readr::read_csv(
    ruta,
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "unique"
  ) |>
    dplyr::mutate(
      archivo_origen =
        fs::path_file(ruta)
    )
}


c5_raw <- purrr::map_dfr(
  archivos_c5,
  leer_c5
)


cat(
  "\nFilas C5 consolidadas:",
  format(
    nrow(c5_raw),
    big.mark = ","
  ),
  "\n"
)


# ------------------------------------------------------------------------------
# 7. Estandarizar fecha y coordenadas
# ------------------------------------------------------------------------------

c5 <- c5_raw |>
  dplyr::mutate(
    fecha_raw =
      .data[[variable_fecha]],

    lat =
      suppressWarnings(
        as.numeric(
          .data[[variable_lat]]
        )
      ),

    lon =
      suppressWarnings(
        as.numeric(
          .data[[variable_lon]]
        )
      )
  )


# ------------------------------------------------------------------------------
# 8. Parsear fecha
# ------------------------------------------------------------------------------

c5 <- c5 |>
  dplyr::mutate(
    fecha = dplyr::coalesce(

      suppressWarnings(
        lubridate::ymd(
          fecha_raw,
          quiet = TRUE
        )
      ),

      suppressWarnings(
        lubridate::dmy(
          fecha_raw,
          quiet = TRUE
        )
      ),

      suppressWarnings(
        as.Date(
          lubridate::ymd_hms(
            fecha_raw,
            quiet = TRUE
          )
        )
      ),

      suppressWarnings(
        as.Date(
          lubridate::dmy_hms(
            fecha_raw,
            quiet = TRUE
          )
        )
      )
    )
  )


cat(
  "\nFechas no interpretadas:",
  sum(is.na(c5$fecha)),
  "\n"
)


# ------------------------------------------------------------------------------
# 9. Filtrar registros utilizables
# ------------------------------------------------------------------------------

c5_valido <- c5 |>
  dplyr::filter(
    !is.na(fecha),

    fecha >= as.Date(
      "2014-01-01"
    ),

    is.finite(lat),
    is.finite(lon),

    dplyr::between(
      lat,
      18,
      21
    ),

    dplyr::between(
      lon,
      -101,
      -97
    )
  )


cat(
  "\nRegistros C5 utilizables:",
  format(
    nrow(c5_valido),
    big.mark = ","
  ),
  "\n"
)


cat(
  "Fecha mínima:",
  as.character(
    min(c5_valido$fecha)
  ),
  "\n"
)

cat(
  "Fecha máxima:",
  as.character(
    max(c5_valido$fecha)
  ),
  "\n"
)


# ------------------------------------------------------------------------------
# 10. Crear geometría C5
# ------------------------------------------------------------------------------

c5_sf <- c5_valido |>
  sf::st_as_sf(
    coords = c(
      "lon",
      "lat"
    ),
    crs = 4326,
    remove = FALSE
  ) |>
  sf::st_transform(
    32614
  )


# ------------------------------------------------------------------------------
# 11. Crear variables temporales
# ------------------------------------------------------------------------------

c5_sf <- c5_sf |>
  dplyr::mutate(
    anio =
      lubridate::year(
        fecha
      ),

    mes =
      lubridate::floor_date(
        fecha,
        unit = "month"
      )
  )


# ------------------------------------------------------------------------------
# 12. Calcular distancias incidente × radar
# ------------------------------------------------------------------------------

cat(
  "\nCalculando distancias C5 × radares...\n"
)


matriz_distancias <- sf::st_distance(
  c5_sf,
  radares_sf
)


# ------------------------------------------------------------------------------
# 13. Identificar pares incidente-radar dentro de 1 km
# ------------------------------------------------------------------------------

indices_1km <- which(
  matriz_distancias <=
    units::set_units(
      1000,
      "m"
    ),
  arr.ind = TRUE
)


cat(
  "\nPares incidente-radar dentro de 1 km:",
  format(
    nrow(indices_1km),
    big.mark = ","
  ),
  "\n"
)


# ------------------------------------------------------------------------------
# 14. Construir tabla incidente × radar
# ------------------------------------------------------------------------------

pares_c5_radar <- tibble::tibble(
  fila_c5 =
    indices_1km[, 1],

  fila_radar =
    indices_1km[, 2],

  distancia_m =
    as.numeric(
      matriz_distancias[
        indices_1km
      ]
    )
) |>
  dplyr::mutate(
    fecha =
      c5_sf$fecha[
        fila_c5
      ],

    mes =
      c5_sf$mes[
        fila_c5
      ],

    anio =
      c5_sf$anio[
        fila_c5
      ],

    id_radar =
      radares_sf$id_radar[
        fila_radar
      ],

    tratamiento_250m =
      radares_sf$tratamiento_250m[
        fila_radar
      ],

    tratamiento_500m =
      radares_sf$tratamiento_500m[
        fila_radar
      ],

    tratamiento_1000m =
      radares_sf$tratamiento_1000m[
        fila_radar
      ],

    dentro_250m =
      distancia_m <= 250,

    dentro_500m =
      distancia_m <= 500,

    dentro_1000m =
      distancia_m <= 1000
  )


# ------------------------------------------------------------------------------
# 15. Contar incidentes mensuales por radar
# ------------------------------------------------------------------------------

conteos_mensuales <- pares_c5_radar |>
  dplyr::group_by(
    id_radar,
    mes
  ) |>
  dplyr::summarise(
    accidentes_250m =
      sum(
        dentro_250m
      ),

    accidentes_500m =
      sum(
        dentro_500m
      ),

    accidentes_1000m =
      sum(
        dentro_1000m
      ),

    .groups = "drop"
  )


# ------------------------------------------------------------------------------
# 16. Crear calendario mensual
# ------------------------------------------------------------------------------

fecha_min_panel <- lubridate::floor_date(
  min(
    c5_sf$fecha
  ),
  unit = "month"
)


fecha_max_panel <- lubridate::floor_date(
  max(
    c5_sf$fecha
  ),
  unit = "month"
)


calendario <- tibble::tibble(
  mes = seq.Date(
    fecha_min_panel,
    fecha_max_panel,
    by = "month"
  )
)


# ------------------------------------------------------------------------------
# 17. Crear panel balanceado radar × mes
# ------------------------------------------------------------------------------

panel_radares_c5 <- tidyr::crossing(
  id_radar =
    radares_sf$id_radar,

  calendario
) |>
  dplyr::left_join(
    conteos_mensuales,
    by = c(
      "id_radar",
      "mes"
    )
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(
        accidentes_250m,
        accidentes_500m,
        accidentes_1000m
      ),
      ~ tidyr::replace_na(
        .x,
        0L
      )
    ),

    anio =
      lubridate::year(
        mes
      ),

    numero_mes =
      lubridate::month(
        mes
      ),

    post_2023 =
      mes >= as.Date(
        "2023-01-01"
      )
  )


# ------------------------------------------------------------------------------
# 18. Incorporar características de radares
# ------------------------------------------------------------------------------

atributos_radares <- radares_candidatos |>
  dplyr::select(
    id_radar,

    calle1_raw,
    calle2_raw,

    distancia_historica_m,
    distancia_fotocivica_2022_m,
    distancia_linea_2022_m,
    distancia_infraestructura_2022_m,

    tratamiento_250m,
    tratamiento_500m,
    tratamiento_1000m
  )


panel_radares_c5 <- panel_radares_c5 |>
  dplyr::left_join(
    atributos_radares,
    by = "id_radar"
  ) |>
  dplyr::arrange(
    id_radar,
    mes
  )


# ------------------------------------------------------------------------------
# 19. Diagnóstico general del panel
# ------------------------------------------------------------------------------

resumen_panel <- panel_radares_c5 |>
  dplyr::summarise(
    radares =
      dplyr::n_distinct(
        id_radar
      ),

    meses =
      dplyr::n_distinct(
        mes
      ),

    filas =
      dplyr::n(),

    inicio =
      min(
        mes
      ),

    fin =
      max(
        mes
      ),

    total_accidentes_250m =
      sum(
        accidentes_250m
      ),

    total_accidentes_500m =
      sum(
        accidentes_500m
      ),

    total_accidentes_1000m =
      sum(
        accidentes_1000m
      )
  )


cat("\nResumen del panel:\n\n")

print(
  resumen_panel,
  width = Inf
)


# ------------------------------------------------------------------------------
# 20. Resumen por radar
# ------------------------------------------------------------------------------

resumen_por_radar <- panel_radares_c5 |>
  dplyr::group_by(
    id_radar,
    tratamiento_500m,
    tratamiento_1000m
  ) |>
  dplyr::summarise(
    total_accidentes_250m =
      sum(
        accidentes_250m
      ),

    total_accidentes_500m =
      sum(
        accidentes_500m
      ),

    total_accidentes_1000m =
      sum(
        accidentes_1000m
      ),

    media_mensual_250m =
      mean(
        accidentes_250m
      ),

    media_mensual_500m =
      mean(
        accidentes_500m
      ),

    media_mensual_1000m =
      mean(
        accidentes_1000m
      ),

    .groups = "drop"
  ) |>
  dplyr::arrange(
    dplyr::desc(
      total_accidentes_500m
    )
  )


cat("\nResumen por radar:\n\n")

print(
  resumen_por_radar,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 21. Serie agregada de tratamientos principales
# ------------------------------------------------------------------------------

serie_agregada <- panel_radares_c5 |>
  dplyr::filter(
    tratamiento_500m
  ) |>
  dplyr::group_by(
    mes
  ) |>
  dplyr::summarise(
    accidentes_250m =
      sum(
        accidentes_250m
      ),

    accidentes_500m =
      sum(
        accidentes_500m
      ),

    accidentes_1000m =
      sum(
        accidentes_1000m
      ),

    .groups = "drop"
  )


cat("\nPrimeros meses de serie agregada:\n\n")

print(
  serie_agregada |>
    dplyr::slice_head(
      n = 20
    ),
  width = Inf
)


# ------------------------------------------------------------------------------
# 22. Resumen anual para diagnóstico
# ------------------------------------------------------------------------------

resumen_anual <- panel_radares_c5 |>
  dplyr::filter(
    tratamiento_500m
  ) |>
  dplyr::group_by(
    anio
  ) |>
  dplyr::summarise(
    accidentes_250m =
      sum(
        accidentes_250m
      ),

    accidentes_500m =
      sum(
        accidentes_500m
      ),

    accidentes_1000m =
      sum(
        accidentes_1000m
      ),

    meses_disponibles =
      dplyr::n_distinct(
        mes
      ),

    .groups = "drop"
  )


cat("\nResumen anual — tratamientos >500 m:\n\n")

print(
  resumen_anual,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 23. Guardar resultados
# ------------------------------------------------------------------------------

readr::write_csv(
  panel_radares_c5,
  fs::path(
    rutas$data_processed,
    "300_panel_radares_c5.csv"
  )
)


readr::write_csv(
  resumen_panel,
  fs::path(
    rutas$output_tables,
    "300_radares_c5_resumen_panel.csv"
  )
)


readr::write_csv(
  resumen_por_radar,
  fs::path(
    rutas$output_tables,
    "300_radares_c5_resumen_por_radar.csv"
  )
)


readr::write_csv(
  serie_agregada,
  fs::path(
    rutas$output_tables,
    "300_radares_c5_serie_agregada.csv"
  )
)


readr::write_csv(
  resumen_anual,
  fs::path(
    rutas$output_tables,
    "300_radares_c5_resumen_anual.csv"
  )
)


message(
  "Panel radar × mes de incidentes C5 construido."
)