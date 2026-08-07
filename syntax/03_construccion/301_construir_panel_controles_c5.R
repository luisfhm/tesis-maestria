# ==============================================================================
# 301_construir_panel_controles_c5.R
# Construcción del panel mensual C5 para controles Fotocívicas preexistentes
# ==============================================================================

source("syntax/00_setup/000_setup.R")


# ------------------------------------------------------------------------------
# 1. Leer tratamientos principales
# ------------------------------------------------------------------------------

tratamientos <- readr::read_csv(
  fs::path(
    rutas$output_tables,
    "126_radares_tratamientos_finales.csv"
  ),
  show_col_types = FALSE
) |>
  dplyr::mutate(
    id_radar = stringr::str_squish(id_raw)
  )


cat(
  "\nTratamientos principales:",
  nrow(tratamientos),
  "\n"
)


tratamientos_sf <- tratamientos |>
  sf::st_as_sf(
    coords = c(
      "x_utm",
      "y_utm"
    ),
    crs = 32614,
    remove = FALSE
  )


# ------------------------------------------------------------------------------
# 2. Leer capa de puntos Fotocívicas de diciembre de 2022
# ------------------------------------------------------------------------------

ruta_zip_puntos <- fs::path(
  rutas$data_raw,
  "07_fotocivicas",
  "ubicacion",
  "fotocivicas-ubicacion-puntos.zip"
)


dir_puntos <- fs::path_temp(
  "fotocivicas_controles_301"
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


controles_raw <- sf::st_read(
  shp_puntos,
  quiet = TRUE
) |>
  sf::st_transform(
    32614
  )


cat(
  "\nPuntos Fotocívicas dic-2022:",
  nrow(controles_raw),
  "\n"
)


# ------------------------------------------------------------------------------
# 3. Crear ID estable para controles
# ------------------------------------------------------------------------------

coords_controles <- sf::st_coordinates(
  controles_raw
)


controles <- controles_raw |>
  dplyr::mutate(
    x_utm = coords_controles[, "X"],
    y_utm = coords_controles[, "Y"],

    id_control = paste0(
      "CTRL_",
      stringr::str_pad(
        dplyr::row_number(),
        width = 3,
        pad = "0"
      )
    )
  )


# ------------------------------------------------------------------------------
# 4. Calcular distancia de cada control al tratamiento más cercano
# ------------------------------------------------------------------------------

distancias_control_tratado <- sf::st_distance(
  controles,
  tratamientos_sf
)


distancia_tratado_min <- apply(
  distancias_control_tratado,
  1,
  min
)


controles <- controles |>
  dplyr::mutate(
    distancia_tratado_m =
      as.numeric(
        distancia_tratado_min
      )
  )


# ------------------------------------------------------------------------------
# 5. Excluir controles potencialmente contaminados
# ------------------------------------------------------------------------------

# Definición conservadora:
# excluir cualquier control situado a <= 1 km de un radar tratado.

controles_elegibles <- controles |>
  dplyr::filter(
    distancia_tratado_m > 1000
  )


cat(
  "\nControles >1 km de cualquier tratamiento:",
  nrow(controles_elegibles),
  "\n"
)


# ------------------------------------------------------------------------------
# 6. Guardar inventario de controles elegibles
# ------------------------------------------------------------------------------

inventario_controles <- controles_elegibles |>
  sf::st_drop_geometry() |>
  dplyr::select(
    id_control,
    ubi,
    no,
    via_princi,
    ubicacion,
    sentido,
    x_utm,
    y_utm,
    distancia_tratado_m
  )


readr::write_csv(
  inventario_controles,
  fs::path(
    rutas$output_tables,
    "301_controles_elegibles.csv"
  )
)


# ------------------------------------------------------------------------------
# 7. Localizar archivos C5
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
  "\n"
)


# ------------------------------------------------------------------------------
# 8. Leer y consolidar C5
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
# 9. Estandarizar variables C5
# ------------------------------------------------------------------------------

c5 <- c5_raw |>
  dplyr::mutate(
    fecha_raw = fecha_creacion,

    lat = suppressWarnings(
      as.numeric(latitud)
    ),

    lon = suppressWarnings(
      as.numeric(longitud)
    ),

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
      )
    )
  )


# ------------------------------------------------------------------------------
# 10. Filtrar C5 utilizable
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


# ------------------------------------------------------------------------------
# 11. Crear geometría C5
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
  ) |>
  dplyr::mutate(
    mes = lubridate::floor_date(
      fecha,
      unit = "month"
    ),

    anio = lubridate::year(
      fecha
    )
  )


# ------------------------------------------------------------------------------
# 12. Identificar incidentes a <=1 km de cada control
# ------------------------------------------------------------------------------

cat(
  "\nBuscando incidentes C5 alrededor de controles...\n"
)


indices_lista <- sf::st_is_within_distance(
  c5_sf,
  controles_elegibles,
  dist = units::set_units(
    1000,
    "m"
  )
)


# ------------------------------------------------------------------------------
# 13. Convertir lista de vecinos en pares incidente-control
# ------------------------------------------------------------------------------

indices_control <- do.call(
  rbind,
  lapply(
    seq_along(indices_lista),
    function(i) {

      if (
        length(
          indices_lista[[i]]
        ) == 0
      ) {
        return(NULL)
      }

      cbind(
        fila_c5 = i,
        fila_control =
          indices_lista[[i]]
      )
    }
  )
)


cat(
  "\nPares incidente-control dentro de 1 km:",
  format(
    nrow(indices_control),
    big.mark = ","
  ),
  "\n"
)


# ------------------------------------------------------------------------------
# 14. Calcular distancia exacta solo para pares cercanos
#     Versión vectorizada por bloques + barra de progreso
# ------------------------------------------------------------------------------

n_pares <- nrow(indices_control)

tamano_bloque <- 100000L

bloques <- split(
  seq_len(n_pares),
  ceiling(
    seq_len(n_pares) / tamano_bloque
  )
)


cat(
  "\nCalculando distancias exactas para",
  format(n_pares, big.mark = ","),
  "pares en",
  length(bloques),
  "bloques...\n\n"
)


cli::cli_progress_bar(
  "Distancias C5 × controles",
  total = length(bloques)
)


distancias_exactas <- numeric(
  n_pares
)


for (b in seq_along(bloques)) {

  idx <- bloques[[b]]

  filas_c5 <- indices_control[
    idx,
    "fila_c5"
  ]

  filas_control <- indices_control[
    idx,
    "fila_control"
  ]


  distancias_exactas[idx] <- as.numeric(
    sf::st_distance(
      c5_sf[
        filas_c5,
      ],

      controles_elegibles[
        filas_control,
      ],

      by_element = TRUE
    )
  )


  cli::cli_progress_update()
}


cli::cli_progress_done()


cat(
  "\nDistancias calculadas:",
  format(
    length(distancias_exactas),
    big.mark = ","
  ),
  "\n"
)

# ------------------------------------------------------------------------------
# 15. Construir tabla incidente-control
# ------------------------------------------------------------------------------

pares_c5_control <- tibble::tibble(
  fila_c5 =
    indices_control[, "fila_c5"],

  fila_control =
    indices_control[, "fila_control"],

  distancia_m =
    distancias_exactas
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

    id_control =
      controles_elegibles$id_control[
        fila_control
      ],

    dentro_250m =
      distancia_m <= 250,

    dentro_500m =
      distancia_m <= 500,

    dentro_1000m =
      distancia_m <= 1000
  )


# ------------------------------------------------------------------------------
# 16. Conteos mensuales por control
# ------------------------------------------------------------------------------

conteos_controles <- pares_c5_control |>
  dplyr::group_by(
    id_control,
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
# 17. Crear calendario mensual
# ------------------------------------------------------------------------------

calendario <- tibble::tibble(
  mes = seq.Date(
    as.Date("2014-01-01"),
    as.Date("2024-02-01"),
    by = "month"
  )
)


# ------------------------------------------------------------------------------
# 18. Crear panel balanceado control × mes
# ------------------------------------------------------------------------------

panel_controles <- tidyr::crossing(
  id_control =
    controles_elegibles$id_control,

  calendario
) |>
  dplyr::left_join(
    conteos_controles,
    by = c(
      "id_control",
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

    tratado = FALSE
  )


# ------------------------------------------------------------------------------
# 19. Incorporar atributos espaciales del control
# ------------------------------------------------------------------------------

atributos_controles <- controles_elegibles |>
  sf::st_drop_geometry() |>
  dplyr::select(
    id_control,
    ubi,
    no,
    via_princi,
    ubicacion,
    sentido,
    x_utm,
    y_utm,
    distancia_tratado_m
  )


panel_controles <- panel_controles |>
  dplyr::left_join(
    atributos_controles,
    by = "id_control"
  ) |>
  dplyr::arrange(
    id_control,
    mes
  )


# ------------------------------------------------------------------------------
# 20. Diagnóstico general
# ------------------------------------------------------------------------------

resumen_panel_controles <- panel_controles |>
  dplyr::summarise(
    controles =
      dplyr::n_distinct(
        id_control
      ),

    meses =
      dplyr::n_distinct(
        mes
      ),

    filas =
      dplyr::n(),

    inicio =
      min(mes),

    fin =
      max(mes),

    total_accidentes_250m =
      sum(accidentes_250m),

    total_accidentes_500m =
      sum(accidentes_500m),

    total_accidentes_1000m =
      sum(accidentes_1000m)
  )


cat("\nResumen panel controles:\n\n")

print(
  resumen_panel_controles,
  width = Inf
)


# ------------------------------------------------------------------------------
# 21. Resumen por control
# ------------------------------------------------------------------------------

resumen_por_control <- panel_controles |>
  dplyr::group_by(
    id_control
  ) |>
  dplyr::summarise(
    total_accidentes_250m =
      sum(accidentes_250m),

    total_accidentes_500m =
      sum(accidentes_500m),

    total_accidentes_1000m =
      sum(accidentes_1000m),

    media_mensual_500m =
      mean(accidentes_500m),

    .groups = "drop"
  ) |>
  dplyr::arrange(
    dplyr::desc(
      total_accidentes_500m
    )
  )


cat("\nResumen por control:\n\n")

print(
  resumen_por_control,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 22. Guardar
# ------------------------------------------------------------------------------

readr::write_csv(
  panel_controles,
  fs::path(
    rutas$data_processed,
    "301_panel_controles_c5.csv"
  )
)


readr::write_csv(
  resumen_panel_controles,
  fs::path(
    rutas$output_tables,
    "301_controles_c5_resumen_panel.csv"
  )
)


readr::write_csv(
  resumen_por_control,
  fs::path(
    rutas$output_tables,
    "301_controles_c5_resumen_por_control.csv"
  )
)


message(
  "Panel control × mes de incidentes C5 construido."
)