# ==============================================================================
# 120_construir_panel_radares.R
# Construcción preliminar del panel histórico de ubicaciones de radares
# ==============================================================================

source("syntax/00_setup/000_setup.R")


# ------------------------------------------------------------------------------
# 1. Leer inventario de radares
# ------------------------------------------------------------------------------

ruta_radares <- fs::path(
  rutas$data_raw,
  "08_radares",
  "radares_fotocivicas.gpkg"
)

radares <- sf::st_read(
  ruta_radares,
  quiet = TRUE
)


# ------------------------------------------------------------------------------
# 2. Conservar geometrías utilizables
# ------------------------------------------------------------------------------

geometrias_vacias <- sf::st_is_empty(radares)

radares_validos <- radares[
  !geometrias_vacias,
] |>
  sf::st_transform(32614)


cat(
  "\nRegistros originales:",
  nrow(radares),
  "\n"
)

cat(
  "Geometrías vacías:",
  sum(geometrias_vacias),
  "\n"
)

cat(
  "Registros utilizables:",
  nrow(radares_validos),
  "\n"
)

# ------------------------------------------------------------------------------
# 3. Extraer coordenadas métricas
# ------------------------------------------------------------------------------

coords <- sf::st_coordinates(
  radares_validos
)

radares_panel <- radares_validos |>
  sf::st_drop_geometry() |>
  dplyr::mutate(
    x_utm = coords[, "X"],
    y_utm = coords[, "Y"]
  )


# ------------------------------------------------------------------------------
# 4. Resumen por inventario anual
# ------------------------------------------------------------------------------

resumen_anual <- radares_panel |>
  dplyr::group_by(
    anio
  ) |>
  dplyr::summarise(
    registros = dplyr::n(),
    ids = dplyr::n_distinct(id),
    tipos = dplyr::n_distinct(tipo),
    .groups = "drop"
  )


cat("\nResumen anual:\n\n")

print(
  resumen_anual,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 5. Crear ubicaciones únicas dentro de cada año
# ------------------------------------------------------------------------------

# Por ahora utilizamos una tolerancia simple mediante coordenadas redondeadas.
# Posteriormente el matching entre años se hará por distancia.

radares_panel <- radares_panel |>
  dplyr::mutate(
    x_round = round(x_utm, 0),
    y_round = round(y_utm, 0),

    id_ubicacion_anual = paste(
      anio,
      x_round,
      y_round,
      sep = "_"
    )
  )


ubicaciones_anuales <- radares_panel |>
  dplyr::group_by(
    anio,
    id_ubicacion_anual
  ) |>
  dplyr::summarise(
    x_utm = mean(x_utm),
    y_utm = mean(y_utm),

    n_registros = dplyr::n(),

    ids = paste(
      sort(unique(id[!is.na(id)])),
      collapse = " | "
    ),

    tipos = paste(
      sort(unique(tipo[!is.na(tipo)])),
      collapse = " | "
    ),

    calle1 = dplyr::first(calle1),
    calle2 = dplyr::first(calle2),

    .groups = "drop"
  )


cat("\nUbicaciones físicas aproximadas por año:\n\n")

print(
  ubicaciones_anuales |>
    dplyr::count(
      anio,
      name = "ubicaciones"
    ),
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 6. Crear objeto espacial de ubicaciones anuales
# ------------------------------------------------------------------------------

ubicaciones_anuales_sf <- sf::st_as_sf(
  ubicaciones_anuales,
  coords = c(
    "x_utm",
    "y_utm"
  ),
  crs = 32614,
  remove = FALSE
)


# ------------------------------------------------------------------------------
# 7. Guardar panel anual preliminar
# ------------------------------------------------------------------------------

readr::write_csv(
  ubicaciones_anuales,
  fs::path(
    rutas$data_processed,
    "120_radares_ubicaciones_anuales.csv"
  )
)

sf::st_write(
  ubicaciones_anuales_sf,
  fs::path(
    rutas$data_processed,
    "120_radares_ubicaciones_anuales.gpkg"
  ),
  delete_dsn = TRUE,
  quiet = TRUE
)


message(
  "Panel anual preliminar de radares construido."
)