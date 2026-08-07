# ==============================================================================
# 124_validar_fecha_radares_2023.R
# Validación de información temporal para radares candidatos de 2023
# ==============================================================================

source("syntax/00_setup/000_setup.R")


# ------------------------------------------------------------------------------
# 1. Leer base raw de radares
# ------------------------------------------------------------------------------

ruta_radares <- fs::path(
  rutas$data_raw,
  "08_radares",
  "radares_fotocivicas.gpkg"
)

radares_raw <- sf::st_read(
  ruta_radares,
  quiet = TRUE
)


cat("\nDimensiones raw:\n\n")

cat(
  "Filas:",
  nrow(radares_raw),
  "\n"
)

cat(
  "Columnas:",
  ncol(radares_raw),
  "\n"
)


# ------------------------------------------------------------------------------
# 2. Revisar todas las variables disponibles
# ------------------------------------------------------------------------------

cat("\nVariables raw:\n\n")

print(
  names(radares_raw)
)


tipos_variables <- tibble::tibble(
  variable = names(
    sf::st_drop_geometry(radares_raw)
  ),

  clase = purrr::map_chr(
    sf::st_drop_geometry(radares_raw),
    ~ paste(
      class(.x),
      collapse = "/"
    )
  )
)


cat("\nTipos de variables:\n\n")

print(
  tipos_variables,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 3. Buscar cualquier variable potencialmente temporal
# ------------------------------------------------------------------------------

patron_temporal <- paste(
  c(
    "fecha",
    "date",
    "time",
    "hora",
    "anio",
    "año",
    "year",
    "mes",
    "inicio",
    "fin",
    "alta",
    "baja",
    "instal",
    "vigencia",
    "activo",
    "estatus",
    "periodo",
    "actualiza",
    "modifica",
    "registro"
  ),
  collapse = "|"
)


variables_temporales <- names(
  sf::st_drop_geometry(radares_raw)
)[
  stringr::str_detect(
    stringr::str_to_lower(
      names(
        sf::st_drop_geometry(radares_raw)
      )
    ),
    patron_temporal
  )
]


cat("\nVariables potencialmente temporales:\n\n")

print(
  variables_temporales
)


# ------------------------------------------------------------------------------
# 4. Revisar cardinalidad y faltantes de todas las variables
# ------------------------------------------------------------------------------

radares_tab <- radares_raw |>
  sf::st_drop_geometry() |>
  tibble::as_tibble()


calidad_variables <- tibble::tibble(
  variable = names(radares_tab),

  valores_unicos = purrr::map_int(
    radares_tab,
    ~ dplyr::n_distinct(
      .x,
      na.rm = TRUE
    )
  ),

  faltantes = purrr::map_int(
    radares_tab,
    ~ sum(
      is.na(.x)
    )
  ),

  porcentaje_faltantes = round(
    faltantes /
      nrow(radares_tab) *
      100,
    2
  )
)


cat("\nCardinalidad y faltantes:\n\n")

print(
  calidad_variables,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 5. Leer tratamientos candidatos definidos en 123
# ------------------------------------------------------------------------------

validacion_2023 <- readr::read_csv(
  fs::path(
    rutas$output_tables,
    "122_radares_validacion_historica_2023.csv"
  ),
  show_col_types = FALSE
) |>
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


cat("\nCandidatos tratamiento >500 m:\n\n")

print(
  validacion_2023 |>
    dplyr::filter(
      tratamiento_500m
    ) |>
    dplyr::select(
      ids,
      tipos,
      calle1,
      calle2,
      distancia_historica_m
    ),
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 6. Extraer IDs individuales de candidatos >500 m
# ------------------------------------------------------------------------------

ids_tratados_500m <- validacion_2023 |>
  dplyr::filter(
    tratamiento_500m
  ) |>
  dplyr::pull(ids) |>
  stringr::str_split(
    "\\s*\\|\\s*"
  ) |>
  unlist() |>
  unique()


cat("\nIDs individuales tratados >500 m:\n\n")

print(
  ids_tratados_500m
)


# ------------------------------------------------------------------------------
# 7. Recuperar todos los registros raw asociados a esos IDs
# ------------------------------------------------------------------------------

radares_tratados_raw <- radares_tab |>
  dplyr::filter(
    id %in% ids_tratados_500m
  ) |>
  dplyr::arrange(
    id,
    anio
  )


cat("\nRegistros raw de candidatos tratados:\n\n")

print(
  radares_tratados_raw,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 8. Historia completa por ID
# ------------------------------------------------------------------------------

historia_ids <- radares_tratados_raw |>
  dplyr::group_by(id) |>
  dplyr::summarise(
    registros = dplyr::n(),

    anios = paste(
      sort(unique(anio)),
      collapse = " | "
    ),

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

    calles1 = paste(
      unique(calle1),
      collapse = " | "
    ),

    calles2 = paste(
      unique(calle2),
      collapse = " | "
    ),

    .groups = "drop"
  )


cat("\nHistoria por ID:\n\n")

print(
  historia_ids,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 9. Revisar posibles IDs con prefijos o patrones administrativos
# ------------------------------------------------------------------------------

patrones_id <- radares_tratados_raw |>
  dplyr::mutate(
    prefijo = stringr::str_extract(
      id,
      "^[A-Za-z0-9]+"
    ),

    estructura_id = stringr::str_replace_all(
      id,
      "[0-9]",
      "#"
    )
  ) |>
  dplyr::count(
    anio,
    estructura_id,
    name = "registros"
  ) |>
  dplyr::arrange(
    anio,
    dplyr::desc(registros)
  )


cat("\nPatrones de IDs:\n\n")

print(
  patrones_id,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 10. Revisar variables temporalmente constantes o cambiantes por ID
# ------------------------------------------------------------------------------

variacion_por_id <- radares_tratados_raw |>
  dplyr::group_by(id) |>
  dplyr::summarise(
    n_anios = dplyr::n_distinct(anio),

    n_tipo = dplyr::n_distinct(tipo),

    n_num = dplyr::n_distinct(
      num,
      na.rm = TRUE
    ),

    n_num_disp = dplyr::n_distinct(
      num_disp,
      na.rm = TRUE
    ),

    n_num_disp_2 = dplyr::n_distinct(
      num_disp_2,
      na.rm = TRUE
    ),

    n_calle1 = dplyr::n_distinct(
      calle1,
      na.rm = TRUE
    ),

    n_calle2 = dplyr::n_distinct(
      calle2,
      na.rm = TRUE
    ),

    .groups = "drop"
  )


cat("\nVariación administrativa por ID:\n\n")

print(
  variacion_por_id,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 11. Revisar si los candidatos aparecen solo en 2023 o también en 2024
# ------------------------------------------------------------------------------

persistencia_2023_2024 <- radares_tratados_raw |>
  dplyr::group_by(id) |>
  dplyr::summarise(
    aparece_2023 = any(
      anio == 2023
    ),

    aparece_2024 = any(
      anio == 2024
    ),

    persiste_2024 =
      aparece_2023 &
      aparece_2024,

    .groups = "drop"
  )


cat("\nPersistencia 2023 → 2024 por ID:\n\n")

print(
  persistencia_2023_2024,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 12. Resumen de evidencia temporal disponible
# ------------------------------------------------------------------------------

resumen_temporal <- tibble::tibble(
  indicador = c(
    "Variables temporales explícitas encontradas",
    "Candidatos >500 m",
    "IDs individuales candidatos",
    "IDs que aparecen en 2023",
    "IDs que persisten en 2024"
  ),

  valor = c(
    length(variables_temporales),

    sum(
      validacion_2023$tratamiento_500m
    ),

    length(
      ids_tratados_500m
    ),

    sum(
      persistencia_2023_2024$aparece_2023
    ),

    sum(
      persistencia_2023_2024$persiste_2024
    )
  )
)


cat("\nResumen temporal:\n\n")

print(
  resumen_temporal,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 13. Guardar resultados
# ------------------------------------------------------------------------------

readr::write_csv(
  tipos_variables,
  fs::path(
    rutas$output_tables,
    "124_radares_tipos_variables.csv"
  )
)

readr::write_csv(
  calidad_variables,
  fs::path(
    rutas$output_tables,
    "124_radares_calidad_variables.csv"
  )
)

readr::write_csv(
  radares_tratados_raw,
  fs::path(
    rutas$output_tables,
    "124_radares_tratados_raw.csv"
  )
)

readr::write_csv(
  historia_ids,
  fs::path(
    rutas$output_tables,
    "124_radares_historia_ids.csv"
  )
)

readr::write_csv(
  variacion_por_id,
  fs::path(
    rutas$output_tables,
    "124_radares_variacion_ids.csv"
  )
)

readr::write_csv(
  persistencia_2023_2024,
  fs::path(
    rutas$output_tables,
    "124_radares_persistencia_2023_2024.csv"
  )
)

readr::write_csv(
  resumen_temporal,
  fs::path(
    rutas$output_tables,
    "124_radares_resumen_temporal.csv"
  )
)


message(
  "Diagnóstico temporal de radares 2023 terminado."
)

# ------------------------------------------------------------------------------
# 14. Identificar candidatos >500 m no recuperados por ID
# ------------------------------------------------------------------------------

ids_recuperados <- unique(
  radares_tratados_raw$id
)

candidatos_no_recuperados <- validacion_2023 |>
  dplyr::filter(
    tratamiento_500m
  ) |>
  dplyr::mutate(
    recuperado_raw = ids %in% ids_recuperados
  ) |>
  dplyr::filter(
    !recuperado_raw
  ) |>
  dplyr::select(
    ids,
    tipos,
    calle1,
    calle2,
    x_utm,
    y_utm,
    distancia_historica_m
  )


cat("\nCandidatos >500 m no recuperados por ID:\n\n")

print(
  candidatos_no_recuperados,
  n = Inf,
  width = Inf
)
# ------------------------------------------------------------------------------
# 15. Recuperar candidatos directamente por coordenadas
# ------------------------------------------------------------------------------

candidatos_500m <- validacion_2023 |>
  dplyr::filter(
    tratamiento_500m
  ) |>
  sf::st_as_sf(
    coords = c(
      "x_utm",
      "y_utm"
    ),
    crs = 32614,
    remove = FALSE
  )


radares_raw_utm <- radares_raw |>
  sf::st_transform(32614)


raw_2023 <- radares_raw_utm |>
  dplyr::filter(
    anio == 2023
  )


distancias_raw <- sf::st_distance(
  candidatos_500m,
  raw_2023
)


indice_raw <- apply(
  distancias_raw,
  1,
  which.min
)


distancia_raw <- apply(
  distancias_raw,
  1,
  min
)


recuperacion_espacial <- validacion_2023 |>
  dplyr::filter(
    tratamiento_500m
  ) |>
  dplyr::mutate(
    distancia_raw_m =
      as.numeric(distancia_raw),

    id_raw =
      raw_2023$id[
        indice_raw
      ],

    num_raw =
      raw_2023$num[
        indice_raw
      ],

    num_disp_raw =
      raw_2023$num_disp[
        indice_raw
      ],

    num_disp_2_raw =
      raw_2023$num_disp_2[
        indice_raw
      ],

    tipo_raw =
      raw_2023$tipo[
        indice_raw
      ],

    calle1_raw =
      raw_2023$calle1[
        indice_raw
      ],

    calle2_raw =
      raw_2023$calle2[
        indice_raw
      ]
  )


cat("\nRecuperación espacial de los 12 tratamientos:\n\n")

print(
  recuperacion_espacial |>
    dplyr::select(
      ids,
      distancia_raw_m,
      id_raw,
      num_raw,
      num_disp_raw,
      num_disp_2_raw,
      tipo_raw,
      calle1_raw,
      calle2_raw
    ),
  n = Inf,
  width = Inf
)


readr::write_csv(
  recuperacion_espacial,
  fs::path(
    rutas$output_tables,
    "124_radares_recuperacion_espacial.csv"
  )
)
# ------------------------------------------------------------------------------
# 16. Tabla final de tratamientos >500 m
# ------------------------------------------------------------------------------

tratamientos_finales_2023 <- recuperacion_espacial |>
  dplyr::select(
    id_raw,
    tipo_raw,
    calle1_raw,
    calle2_raw,
    x_utm,
    y_utm,
    distancia_historica_m,
    distancia_raw_m
  ) |>
  dplyr::arrange(
    dplyr::desc(distancia_historica_m)
  )


cat("\nTratamientos finales 2023 (>500 m):\n\n")

print(
  tratamientos_finales_2023,
  n = Inf,
  width = Inf
)


readr::write_csv(
  tratamientos_finales_2023,
  fs::path(
    rutas$output_tables,
    "124_radares_tratamientos_finales_2023.csv"
  )
)