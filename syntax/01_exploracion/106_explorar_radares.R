# ==============================================================================
# 106_explorar_radares.R
# Exploración inicial de radares / Fotocívicas
# ==============================================================================

source("syntax/00_setup/000_setup.R")


# ------------------------------------------------------------------------------
# 1. Ruta
# ------------------------------------------------------------------------------

ruta_radares <- fs::path(
  rutas$data_raw,
  "08_radares",
  "radares_fotocivicas.gpkg"
)

cat("\nArchivo:\n")
print(ruta_radares)


# ------------------------------------------------------------------------------
# 2. Revisar capas disponibles
# ------------------------------------------------------------------------------

capas_radares <- sf::st_layers(
  ruta_radares
)

cat("\nCapas disponibles:\n\n")

print(capas_radares)


# ------------------------------------------------------------------------------
# 3. Leer capa
# ------------------------------------------------------------------------------

radares <- sf::st_read(
  ruta_radares,
  quiet = TRUE
)


cat("\n\n============================================================\n")
cat("RADARES / FOTOCÍVICAS\n")
cat("============================================================\n\n")

print(radares)


# ------------------------------------------------------------------------------
# 4. Dimensiones
# ------------------------------------------------------------------------------

cat(
  "\nObservaciones:",
  nrow(radares),
  "\n"
)

cat(
  "Variables:",
  ncol(radares) - 1,
  "\n"
)


# ------------------------------------------------------------------------------
# 5. Variables
# ------------------------------------------------------------------------------

cat("\nVariables disponibles:\n\n")

print(
  names(radares)
)


# ------------------------------------------------------------------------------
# 6. Tipos de variables
# ------------------------------------------------------------------------------

tipos_radares <- tibble::tibble(
  variable = names(
    sf::st_drop_geometry(radares)
  ),

  clase = purrr::map_chr(
    sf::st_drop_geometry(radares),
    ~ paste(class(.x), collapse = "/")
  )
)


cat("\nTipos de variables:\n\n")

print(
  tipos_radares,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 7. Primeras observaciones
# ------------------------------------------------------------------------------

cat("\nPrimeras observaciones sin geometría:\n\n")

print(
  sf::st_drop_geometry(radares) |>
    head(20),
  width = Inf
)


# ------------------------------------------------------------------------------
# 8. Geometría
# ------------------------------------------------------------------------------

cat("\nCRS:\n\n")
print(
  sf::st_crs(radares)
)

cat("\nBounding box:\n\n")
print(
  sf::st_bbox(radares)
)

cat("\nTipos de geometría:\n\n")

print(
  table(
    sf::st_geometry_type(radares)
  )
)


# ------------------------------------------------------------------------------
# 9. Geometrías vacías
# ------------------------------------------------------------------------------

geometrias_vacias <- sf::st_is_empty(
  radares
)

cat(
  "\nGeometrías vacías:",
  sum(geometrias_vacias),
  "\n"
)

cat(
  "Geometrías utilizables:",
  sum(!geometrias_vacias),
  "\n"
)


if (any(geometrias_vacias)) {

  cat("\nRegistros con geometría vacía:\n\n")

  print(
    sf::st_drop_geometry(
      radares[geometrias_vacias, ]
    ),
    n = Inf,
    width = Inf
  )
}


# ------------------------------------------------------------------------------
# 10. Geometrías formalmente válidas
# ------------------------------------------------------------------------------

validas_sf <- sf::st_is_valid(
  radares
)

cat(
  "\nGeometrías inválidas según sf:",
  sum(!validas_sf, na.rm = TRUE),
  "\n"
)


# ------------------------------------------------------------------------------
# 11. Coordenadas
# ------------------------------------------------------------------------------

radares_validos <- radares[
  !geometrias_vacias,
]

coords_radares <- sf::st_coordinates(
  radares_validos
)

validacion_coordenadas <- tibble::tibble(
  observaciones = nrow(radares),

  geometria_valida = nrow(radares_validos),

  geometria_vacia = sum(
    geometrias_vacias
  ),

  coordenadas_no_finitas = sum(
    !is.finite(coords_radares[, "X"]) |
      !is.finite(coords_radares[, "Y"])
  ),

  x_min = min(
    coords_radares[, "X"],
    na.rm = TRUE
  ),

  x_max = max(
    coords_radares[, "X"],
    na.rm = TRUE
  ),

  y_min = min(
    coords_radares[, "Y"],
    na.rm = TRUE
  ),

  y_max = max(
    coords_radares[, "Y"],
    na.rm = TRUE
  )
)


cat("\nValidación de coordenadas:\n\n")

print(
  validacion_coordenadas,
  width = Inf
)


# ------------------------------------------------------------------------------
# 12. Transformar a WGS84 para inspección
# ------------------------------------------------------------------------------

radares_ll <- sf::st_transform(
  radares_validos,
  4326
)

cat("\nBounding box en WGS84:\n\n")

print(
  sf::st_bbox(radares_ll)
)


# ------------------------------------------------------------------------------
# 13. Buscar variables potencialmente importantes
# ------------------------------------------------------------------------------

variables_radares <- names(
  sf::st_drop_geometry(radares)
)

patron_importante <- paste(
  c(
    "id",
    "tipo",
    "radar",
    "fotoc",
    "fecha",
    "inicio",
    "instal",
    "estatus",
    "estado",
    "via",
    "calle",
    "ubic",
    "sentido",
    "lat",
    "lon"
  ),
  collapse = "|"
)


variables_importantes <- tibble::tibble(
  variable = variables_radares
) |>
  dplyr::filter(
    stringr::str_detect(
      stringr::str_to_lower(variable),
      patron_importante
    )
  )


cat("\nVariables potencialmente relevantes:\n\n")

print(
  variables_importantes,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 14. Cardinalidad de variables
# ------------------------------------------------------------------------------

cardinalidad_radares <- purrr::map_dfr(
  variables_radares,
  function(variable_actual) {

    valores <- sf::st_drop_geometry(radares)[[
      variable_actual
    ]]

    tibble::tibble(
      variable = variable_actual,

      observaciones = length(valores),

      valores_unicos = dplyr::n_distinct(
        valores,
        na.rm = TRUE
      ),

      faltantes = sum(
        is.na(valores)
      ),

      porcentaje_faltantes = round(
        mean(is.na(valores)) * 100,
        2
      )
    )
  }
)


cat("\nCardinalidad de variables:\n\n")

print(
  cardinalidad_radares,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 15. Detectar coordenadas duplicadas
# ------------------------------------------------------------------------------

coords_df <- tibble::as_tibble(
  coords_radares
)


duplicados_espaciales <- coords_df |>
  dplyr::count(
    X,
    Y,
    name = "n"
  ) |>
  dplyr::filter(
    n > 1
  ) |>
  dplyr::arrange(
    dplyr::desc(n)
  )


cat("\nCoordenadas compartidas por múltiples registros:\n\n")

cat(
  "Ubicaciones duplicadas:",
  nrow(duplicados_espaciales),
  "\n"
)

print(
  duplicados_espaciales,
  n = 30,
  width = Inf
)


cat(
  "\nNúmero de ubicaciones espaciales únicas:",
  nrow(
    dplyr::distinct(
      coords_df,
      X,
      Y
    )
  ),
  "\n"
)


# ------------------------------------------------------------------------------
# 16. Guardar auditoría
# ------------------------------------------------------------------------------

readr::write_csv(
  tipos_radares,
  fs::path(
    rutas$output_tables,
    "106_radares_tipos_variables.csv"
  )
)

readr::write_csv(
  validacion_coordenadas,
  fs::path(
    rutas$output_tables,
    "106_radares_validacion_coordenadas.csv"
  )
)

readr::write_csv(
  cardinalidad_radares,
  fs::path(
    rutas$output_tables,
    "106_radares_cardinalidad.csv"
  )
)

readr::write_csv(
  duplicados_espaciales,
  fs::path(
    rutas$output_tables,
    "106_radares_duplicados_espaciales.csv"
  )
)


message("Exploración inicial de radares terminada.")

# ------------------------------------------------------------------------------
# 17. Distribución por año
# ------------------------------------------------------------------------------

radares_por_anio <- radares |>
  sf::st_drop_geometry() |>
  dplyr::count(
    anio,
    sort = FALSE,
    name = "registros"
  ) |>
  dplyr::arrange(anio)


cat("\nRegistros por año:\n\n")

print(
  radares_por_anio,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 18. Distribución por tipo
# ------------------------------------------------------------------------------

radares_por_tipo <- radares |>
  sf::st_drop_geometry() |>
  dplyr::count(
    tipo,
    sort = TRUE,
    name = "registros"
  )


cat("\nRegistros por tipo:\n\n")

print(
  radares_por_tipo,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 19. Distribución año × tipo
# ------------------------------------------------------------------------------

radares_anio_tipo <- radares |>
  sf::st_drop_geometry() |>
  dplyr::count(
    anio,
    tipo,
    name = "registros"
  ) |>
  dplyr::arrange(
    anio,
    tipo
  )


cat("\nRegistros por año y tipo:\n\n")

print(
  radares_anio_tipo,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 20. IDs únicos por año
# ------------------------------------------------------------------------------

ids_por_anio <- radares |>
  sf::st_drop_geometry() |>
  dplyr::group_by(anio) |>
  dplyr::summarise(
    registros = dplyr::n(),

    ids_no_na = sum(
      !is.na(id) &
      id != ""
    ),

    ids_unicos = dplyr::n_distinct(
      id[
        !is.na(id) &
        id != ""
      ]
    ),

    .groups = "drop"
  ) |>
  dplyr::arrange(anio)


cat("\nIDs por año:\n\n")

print(
  ids_por_anio,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 21. Años observados por ID
# ------------------------------------------------------------------------------

panel_ids <- radares |>
  sf::st_drop_geometry() |>
  dplyr::filter(
    !is.na(id),
    id != ""
  ) |>
  dplyr::group_by(id) |>
  dplyr::summarise(
    n_registros = dplyr::n(),

    n_anios = dplyr::n_distinct(anio),

    anio_min = min(
      anio,
      na.rm = TRUE
    ),

    anio_max = max(
      anio,
      na.rm = TRUE
    ),

    tipos = paste(
      sort(unique(tipo)),
      collapse = " | "
    ),

    .groups = "drop"
  ) |>
  dplyr::arrange(
    dplyr::desc(n_anios),
    id
  )


cat("\nPanel de IDs:\n\n")

print(
  panel_ids,
  n = 50,
  width = Inf
)


# ------------------------------------------------------------------------------
# 22. Distribución del número de años por ID
# ------------------------------------------------------------------------------

distribucion_anios_id <- panel_ids |>
  dplyr::count(
    n_anios,
    name = "ids"
  ) |>
  dplyr::arrange(n_anios)


cat("\nNúmero de años en que aparece cada ID:\n\n")

print(
  distribucion_anios_id,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 23. Apariciones y desapariciones de IDs
# ------------------------------------------------------------------------------

entradas_ids <- panel_ids |>
  dplyr::count(
    anio_min,
    name = "ids_que_aparecen"
  ) |>
  dplyr::arrange(anio_min)


salidas_ids <- panel_ids |>
  dplyr::count(
    anio_max,
    name = "ids_ultima_aparicion"
  ) |>
  dplyr::arrange(anio_max)


cat("\nPrimera aparición de IDs:\n\n")

print(
  entradas_ids,
  n = Inf,
  width = Inf
)


cat("\nÚltima aparición de IDs:\n\n")

print(
  salidas_ids,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 24. Comprobar unicidad de ID dentro de año
# ------------------------------------------------------------------------------

duplicados_id_anio <- radares |>
  sf::st_drop_geometry() |>
  dplyr::filter(
    !is.na(id),
    id != ""
  ) |>
  dplyr::count(
    anio,
    id,
    name = "n"
  ) |>
  dplyr::filter(
    n > 1
  ) |>
  dplyr::arrange(
    dplyr::desc(n),
    anio,
    id
  )


cat("\nIDs repetidos dentro del mismo año:\n\n")

cat(
  "Combinaciones año-ID repetidas:",
  nrow(duplicados_id_anio),
  "\n"
)

print(
  duplicados_id_anio,
  n = 50,
  width = Inf
)


# ------------------------------------------------------------------------------
# 25. Inspeccionar valores de tipo
# ------------------------------------------------------------------------------

cat("\nValores de tipo:\n\n")

print(
  sort(
    unique(radares$tipo)
  )
)


# ------------------------------------------------------------------------------
# 26. Ejemplos de IDs con mayor cobertura temporal
# ------------------------------------------------------------------------------

ids_largos <- panel_ids |>
  dplyr::filter(
    n_anios == max(n_anios)
  ) |>
  dplyr::slice_head(
    n = 10
  ) |>
  dplyr::pull(id)


ejemplos_panel <- radares |>
  sf::st_drop_geometry() |>
  dplyr::filter(
    id %in% ids_largos
  ) |>
  dplyr::select(
    anio,
    tipo,
    id,
    num,
    num_disp,
    num_disp_2,
    calle1,
    calle2,
    x,
    y
  ) |>
  dplyr::arrange(
    id,
    anio
  )


cat("\nEjemplos de IDs observados durante más años:\n\n")

print(
  ejemplos_panel,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 27. Guardar resultados
# ------------------------------------------------------------------------------

readr::write_csv(
  radares_anio_tipo,
  fs::path(
    rutas$output_tables,
    "106_radares_anio_tipo.csv"
  )
)

readr::write_csv(
  ids_por_anio,
  fs::path(
    rutas$output_tables,
    "106_radares_ids_por_anio.csv"
  )
)

readr::write_csv(
  panel_ids,
  fs::path(
    rutas$output_tables,
    "106_radares_panel_ids.csv"
  )
)

readr::write_csv(
  distribucion_anios_id,
  fs::path(
    rutas$output_tables,
    "106_radares_distribucion_anios_id.csv"
  )
)

readr::write_csv(
  duplicados_id_anio,
  fs::path(
    rutas$output_tables,
    "106_radares_duplicados_id_anio.csv"
  )
)


# ------------------------------------------------------------------------------
# 28. Construir panel espacial de ubicaciones
# ------------------------------------------------------------------------------

# Transformar primero a WGS84
radares_panel_sf <- radares_validos |>
  sf::st_transform(4326)


# Extraer coordenadas
coords_panel <- sf::st_coordinates(
  radares_panel_sf
)


# Construir panel sin geometría
radares_panel_espacial <- radares_panel_sf |>
  sf::st_drop_geometry() |>
  dplyr::mutate(
    lon = coords_panel[, "X"],
    lat = coords_panel[, "Y"],

    # Redondeo para identificar ubicaciones iguales o prácticamente iguales
    lon_round = round(lon, 5),
    lat_round = round(lat, 5)
  )


# Comprobación
cat("\nPanel espacial construido:\n\n")

cat(
  "Registros:",
  nrow(radares_panel_espacial),
  "\n"
)

cat(
  "Coordenadas únicas:",
  dplyr::n_distinct(
    paste(
      radares_panel_espacial$lon_round,
      radares_panel_espacial$lat_round
    )
  ),
  "\n"
)

print(
  head(radares_panel_espacial),
  width = Inf
)


# ------------------------------------------------------------------------------
# 29. Ubicaciones únicas por año
# ------------------------------------------------------------------------------

ubicaciones_por_anio <- radares_panel_espacial |>
  dplyr::group_by(anio) |>
  dplyr::summarise(
    registros = dplyr::n(),

    ubicaciones_unicas = dplyr::n_distinct(
      paste(
        lon_round,
        lat_round
      )
    ),

    .groups = "drop"
  ) |>
  dplyr::arrange(anio)


cat("\nUbicaciones espaciales únicas por año:\n\n")

print(
  ubicaciones_por_anio,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 30. Crear identificador espacial
# ------------------------------------------------------------------------------

radares_panel_espacial <- radares_panel_espacial |>
  dplyr::mutate(
    id_espacial = paste(
      lon_round,
      lat_round,
      sep = "_"
    )
  )


# ------------------------------------------------------------------------------
# 31. Persistencia temporal de ubicaciones
# ------------------------------------------------------------------------------

panel_ubicaciones <- radares_panel_espacial |>
  dplyr::group_by(id_espacial) |>
  dplyr::summarise(
    n_registros = dplyr::n(),

    n_anios = dplyr::n_distinct(anio),

    anio_min = min(anio),

    anio_max = max(anio),

    ids = paste(
      sort(unique(id[!is.na(id)])),
      collapse = " | "
    ),

    n_ids = dplyr::n_distinct(
      id,
      na.rm = TRUE
    ),

    tipos = paste(
      sort(unique(tipo)),
      collapse = " | "
    ),

    lon = dplyr::first(lon),

    lat = dplyr::first(lat),

    .groups = "drop"
  ) |>
  dplyr::arrange(
    dplyr::desc(n_anios)
  )


cat("\nPersistencia de ubicaciones:\n\n")

print(
  panel_ubicaciones,
  n = 50,
  width = Inf
)


# ------------------------------------------------------------------------------
# 32. Distribución de años por ubicación
# ------------------------------------------------------------------------------

distribucion_anios_ubicacion <- panel_ubicaciones |>
  dplyr::count(
    n_anios,
    name = "ubicaciones"
  ) |>
  dplyr::arrange(n_anios)


cat("\nNúmero de años observados por ubicación:\n\n")

print(
  distribucion_anios_ubicacion,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 33. Ubicaciones que cambian de ID
# ------------------------------------------------------------------------------

ubicaciones_cambio_id <- panel_ubicaciones |>
  dplyr::filter(
    n_ids > 1
  ) |>
  dplyr::arrange(
    dplyr::desc(n_ids),
    dplyr::desc(n_anios)
  )


cat("\nUbicaciones asociadas con más de un ID:\n\n")

cat(
  "Total:",
  nrow(ubicaciones_cambio_id),
  "\n"
)

print(
  ubicaciones_cambio_id,
  n = 50,
  width = Inf
)


# ------------------------------------------------------------------------------
# 34. Primera aparición espacial
# ------------------------------------------------------------------------------

entradas_ubicaciones <- panel_ubicaciones |>
  dplyr::count(
    anio_min,
    name = "ubicaciones_que_aparecen"
  ) |>
  dplyr::arrange(anio_min)


cat("\nPrimera aparición de ubicaciones:\n\n")

print(
  entradas_ubicaciones,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 35. Última aparición espacial
# ------------------------------------------------------------------------------

salidas_ubicaciones <- panel_ubicaciones |>
  dplyr::count(
    anio_max,
    name = "ubicaciones_ultima_aparicion"
  ) |>
  dplyr::arrange(anio_max)


cat("\nÚltima aparición de ubicaciones:\n\n")

print(
  salidas_ubicaciones,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 36. Trayectoria año × ubicación
# ------------------------------------------------------------------------------

trayectoria_ubicaciones <- radares_panel_espacial |>
  dplyr::select(
    id_espacial,
    anio,
    tipo,
    id,
    calle1,
    calle2,
    lon,
    lat
  ) |>
  dplyr::arrange(
    id_espacial,
    anio
  )


# ------------------------------------------------------------------------------
# 37. Guardar
# ------------------------------------------------------------------------------

readr::write_csv(
  ubicaciones_por_anio,
  fs::path(
    rutas$output_tables,
    "106_radares_ubicaciones_por_anio.csv"
  )
)

readr::write_csv(
  panel_ubicaciones,
  fs::path(
    rutas$output_tables,
    "106_radares_panel_ubicaciones.csv"
  )
)

readr::write_csv(
  distribucion_anios_ubicacion,
  fs::path(
    rutas$output_tables,
    "106_radares_distribucion_anios_ubicacion.csv"
  )
)

readr::write_csv(
  ubicaciones_cambio_id,
  fs::path(
    rutas$output_tables,
    "106_radares_ubicaciones_cambio_id.csv"
  )
)

readr::write_csv(
  trayectoria_ubicaciones,
  fs::path(
    rutas$output_tables,
    "106_radares_trayectoria_ubicaciones.csv"
  )
)

# ------------------------------------------------------------------------------
# 38. Matching espacial 2024 vs inventario histórico
# ------------------------------------------------------------------------------

radares_hist <- radares_validos |>
  sf::st_transform(32614) |>
  dplyr::filter(anio <= 2023)

radares_2024 <- radares_validos |>
  sf::st_transform(32614) |>
  dplyr::filter(anio == 2024)


# ------------------------------------------------------------------------------
# 39. Distancia de cada radar 2024 al histórico más cercano
# ------------------------------------------------------------------------------

distancias <- sf::st_distance(
  radares_2024,
  radares_hist
)

indice_cercano <- apply(
  distancias,
  1,
  which.min
)

distancia_min <- apply(
  distancias,
  1,
  min
)


matching_2024 <- sf::st_drop_geometry(
  radares_2024
) |>
  dplyr::mutate(
    distancia_min_m = as.numeric(distancia_min),

    id_historico_cercano =
      radares_hist$id[indice_cercano],

    anio_historico_cercano =
      radares_hist$anio[indice_cercano],

    tipo_historico_cercano =
      radares_hist$tipo[indice_cercano],

    calle1_historica =
      radares_hist$calle1[indice_cercano],

    calle2_historica =
      radares_hist$calle2[indice_cercano]
  )


# ------------------------------------------------------------------------------
# 40. Distribución de distancias
# ------------------------------------------------------------------------------

cat("\nDistancia de radares 2024 al inventario 2019–2023:\n\n")

print(
  summary(
    matching_2024$distancia_min_m
  )
)


# ------------------------------------------------------------------------------
# 41. Clasificar coincidencias
# ------------------------------------------------------------------------------

matching_2024 <- matching_2024 |>
  dplyr::mutate(
    coincidencia = dplyr::case_when(
      distancia_min_m <= 10 ~ "≤10 m",
      distancia_min_m <= 25 ~ "10–25 m",
      distancia_min_m <= 50 ~ "25–50 m",
      distancia_min_m <= 100 ~ "50–100 m",
      TRUE ~ ">100 m"
    )
  )


resumen_matching_2024 <- matching_2024 |>
  dplyr::count(
    coincidencia,
    name = "radares"
  ) |>
  dplyr::mutate(
    porcentaje = round(
      radares / sum(radares) * 100,
      2
    )
  )


cat("\nMatching espacial 2024 vs histórico:\n\n")

print(
  resumen_matching_2024,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 42. Posibles ubicaciones nuevas en 2024
# ------------------------------------------------------------------------------

posibles_nuevos_2024 <- matching_2024 |>
  dplyr::filter(
    distancia_min_m > 100
  ) |>
  dplyr::arrange(
    dplyr::desc(distancia_min_m)
  )


cat("\nPosibles ubicaciones nuevas en 2024 (>100 m):\n\n")

print(
  posibles_nuevos_2024,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 43. Guardar resultados
# ------------------------------------------------------------------------------

readr::write_csv(
  matching_2024,
  fs::path(
    rutas$output_tables,
    "106_radares_matching_2024_historico.csv"
  )
)

readr::write_csv(
  resumen_matching_2024,
  fs::path(
    rutas$output_tables,
    "106_radares_matching_2024_resumen.csv"
  )
)

readr::write_csv(
  posibles_nuevos_2024,
  fs::path(
    rutas$output_tables,
    "106_radares_posibles_nuevos_2024.csv"
  )
)

# ------------------------------------------------------------------------------
# 44. Matching directo 2024 vs 2023
# ------------------------------------------------------------------------------

radares_2023 <- radares_validos |>
  dplyr::filter(anio == 2023) |>
  sf::st_transform(32614)

radares_2024 <- radares_validos |>
  dplyr::filter(anio == 2024) |>
  sf::st_transform(32614)


distancias_23_24 <- sf::st_distance(
  radares_2024,
  radares_2023
)

indice_cercano_2023 <- apply(
  distancias_23_24,
  1,
  which.min
)

distancia_2023 <- apply(
  distancias_23_24,
  1,
  min
)


matching_23_24 <- sf::st_drop_geometry(
  radares_2024
) |>
  dplyr::mutate(
    distancia_2023_m = as.numeric(distancia_2023),

    id_2023 =
      radares_2023$id[indice_cercano_2023],

    tipo_2023 =
      radares_2023$tipo[indice_cercano_2023],

    calle1_2023 =
      radares_2023$calle1[indice_cercano_2023],

    calle2_2023 =
      radares_2023$calle2[indice_cercano_2023],

    coincidencia = dplyr::case_when(
      distancia_2023_m <= 10  ~ "≤10 m",
      distancia_2023_m <= 25  ~ "10–25 m",
      distancia_2023_m <= 50  ~ "25–50 m",
      distancia_2023_m <= 100 ~ "50–100 m",
      TRUE                    ~ ">100 m"
    )
  )


resumen_matching_23_24 <- matching_23_24 |>
  dplyr::count(
    coincidencia,
    name = "radares"
  ) |>
  dplyr::mutate(
    porcentaje = round(
      radares / sum(radares) * 100,
      2
    )
  )


cat("\nMatching directo 2024 vs 2023:\n\n")

print(
  resumen_matching_23_24,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 45. Revisar posibles incorporaciones 2024
# ------------------------------------------------------------------------------

nuevos_2024_vs_2023 <- matching_23_24 |>
  dplyr::filter(
    distancia_2023_m > 100
  ) |>
  dplyr::arrange(
    dplyr::desc(distancia_2023_m)
  )


cat("\nPosibles incorporaciones/reubicaciones 2024:\n\n")

print(
  nuevos_2024_vs_2023,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 46. Guardar
# ------------------------------------------------------------------------------

readr::write_csv(
  matching_23_24,
  fs::path(
    rutas$output_tables,
    "106_radares_matching_2023_2024.csv"
  )
)

readr::write_csv(
  resumen_matching_23_24,
  fs::path(
    rutas$output_tables,
    "106_radares_matching_2023_2024_resumen.csv"
  )
)

readr::write_csv(
  nuevos_2024_vs_2023,
  fs::path(
    rutas$output_tables,
    "106_radares_nuevos_2024_vs_2023.csv"
  )
)