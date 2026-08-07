# ==============================================================================
# 108_explorar_accidentes_peatones_ciclistas.R
# Exploración de puntos de accidentes de peatones y ciclistas
# ==============================================================================

source("syntax/00_setup/000_setup.R")


# ------------------------------------------------------------------------------
# 1. Ruta
# ------------------------------------------------------------------------------

ruta_accidentes_vulnerables <- fs::path(
  rutas$data_raw,
  "10_accidentes_peatones_ciclistas"
)

archivos_zip <- fs::dir_ls(
  ruta_accidentes_vulnerables,
  regexp = "\\.zip$",
  type = "file"
)


cat("\nArchivos ZIP encontrados:\n\n")

print(
  fs::path_file(archivos_zip)
)


# ------------------------------------------------------------------------------
# 2. Inventario
# ------------------------------------------------------------------------------

inventario_accidentes_vulnerables <- tibble::tibble(
  archivo = fs::path_file(archivos_zip),

  tamano_mb = round(
    as.numeric(
      fs::file_size(archivos_zip)
    ) / 1024^2,
    3
  )
)


cat("\nInventario:\n\n")

print(
  inventario_accidentes_vulnerables,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 3. Inspeccionar contenido de los ZIP
# ------------------------------------------------------------------------------

contenido_zip <- purrr::map_dfr(
  archivos_zip,
  function(ruta) {

    contenido <- utils::unzip(
      ruta,
      list = TRUE
    )

    tibble::as_tibble(contenido) |>
      dplyr::mutate(
        archivo_zip = fs::path_file(ruta),
        .before = 1
      )
  }
)


cat("\nContenido de los ZIP:\n\n")

print(
  contenido_zip,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 4. Identificar formatos geográficos
# ------------------------------------------------------------------------------

formatos_zip <- contenido_zip |>
  dplyr::mutate(
    extension = stringr::str_to_lower(
      fs::path_ext(Name)
    )
  ) |>
  dplyr::count(
    archivo_zip,
    extension,
    name = "archivos"
  ) |>
  dplyr::arrange(
    archivo_zip,
    extension
  )


cat("\nFormatos encontrados:\n\n")

print(
  formatos_zip,
  n = Inf,
  width = Inf
)

# ------------------------------------------------------------------------------
# 5. Crear directorio temporal para extracción
# ------------------------------------------------------------------------------

ruta_temp <- fs::path(
  rutas$output,
  "temp_108_accidentes_vulnerables"
)

if (fs::dir_exists(ruta_temp)) {
  fs::dir_delete(ruta_temp)
}

fs::dir_create(ruta_temp)


# ------------------------------------------------------------------------------
# 6. Extraer ZIP
# ------------------------------------------------------------------------------

purrr::walk(
  archivos_zip,
  function(ruta) {

    utils::unzip(
      ruta,
      exdir = ruta_temp
    )
  }
)


cat("\nArchivos extraídos en:\n")
cat(ruta_temp, "\n")


# ------------------------------------------------------------------------------
# 7. Localizar Shapefiles reales
# ------------------------------------------------------------------------------

archivos_shp <- fs::dir_ls(
  ruta_temp,
  recurse = TRUE,
  regexp = "\\.shp$",
  type = "file"
)

# Excluir archivos basura generados por macOS
archivos_shp <- archivos_shp[
  !stringr::str_detect(
    archivos_shp,
    "__MACOSX|/\\._|\\\\\\._"
  )
]


cat("\nShapefiles encontrados:\n\n")

print(archivos_shp)


# ------------------------------------------------------------------------------
# 8. Identificar ciclistas y peatones
# ------------------------------------------------------------------------------

shp_ciclistas <- archivos_shp[
  stringr::str_detect(
    stringr::str_to_lower(archivos_shp),
    "ciclista"
  )
]

shp_peatones <- archivos_shp[
  stringr::str_detect(
    stringr::str_to_lower(archivos_shp),
    "peaton"
  )
]


cat("\nShapefile ciclistas:\n")
print(shp_ciclistas)

cat("\nShapefile peatones:\n")
print(shp_peatones)


# ------------------------------------------------------------------------------
# 9. Leer Shapefiles
# ------------------------------------------------------------------------------

accidentes_ciclistas <- sf::st_read(
  shp_ciclistas,
  quiet = TRUE
)

accidentes_peatones <- sf::st_read(
  shp_peatones,
  quiet = TRUE
)


# ------------------------------------------------------------------------------
# 10. Dimensiones
# ------------------------------------------------------------------------------

cat("\nDimensiones:\n\n")

cat(
  "Ciclistas:",
  format(nrow(accidentes_ciclistas), big.mark = ","),
  "filas x",
  ncol(accidentes_ciclistas),
  "columnas\n"
)

cat(
  "Peatones:",
  format(nrow(accidentes_peatones), big.mark = ","),
  "filas x",
  ncol(accidentes_peatones),
  "columnas\n"
)


# ------------------------------------------------------------------------------
# 11. Estructura
# ------------------------------------------------------------------------------

cat("\nEstructura — ciclistas:\n\n")

dplyr::glimpse(
  accidentes_ciclistas
)


cat("\nEstructura — peatones:\n\n")

dplyr::glimpse(
  accidentes_peatones
)


# ------------------------------------------------------------------------------
# 12. Nombres de variables
# ------------------------------------------------------------------------------

cat("\nVariables — ciclistas:\n\n")

print(
  names(accidentes_ciclistas)
)


cat("\nVariables — peatones:\n\n")

print(
  names(accidentes_peatones)
)


# ------------------------------------------------------------------------------
# 13. Primeras observaciones sin geometría
# ------------------------------------------------------------------------------

cat("\nPrimeras observaciones — ciclistas:\n\n")

print(
  accidentes_ciclistas |>
    sf::st_drop_geometry() |>
    head(10),
  width = Inf
)


cat("\nPrimeras observaciones — peatones:\n\n")

print(
  accidentes_peatones |>
    sf::st_drop_geometry() |>
    head(10),
  width = Inf
)


# ------------------------------------------------------------------------------
# 14. Información espacial
# ------------------------------------------------------------------------------

cat("\nCRS — ciclistas:\n\n")

print(
  sf::st_crs(accidentes_ciclistas)
)


cat("\nBounding box — ciclistas:\n\n")

print(
  sf::st_bbox(accidentes_ciclistas)
)


cat("\nCRS — peatones:\n\n")

print(
  sf::st_crs(accidentes_peatones)
)


cat("\nBounding box — peatones:\n\n")

print(
  sf::st_bbox(accidentes_peatones)
)


# ------------------------------------------------------------------------------
# 15. Tipo y calidad de geometrías
# ------------------------------------------------------------------------------

resumen_geometria <- tibble::tibble(
  fuente = c(
    "ciclistas",
    "peatones"
  ),

  observaciones = c(
    nrow(accidentes_ciclistas),
    nrow(accidentes_peatones)
  ),

  vacias = c(
    sum(sf::st_is_empty(accidentes_ciclistas)),
    sum(sf::st_is_empty(accidentes_peatones))
  ),

  validas = c(
    sum(sf::st_is_valid(accidentes_ciclistas)),
    sum(sf::st_is_valid(accidentes_peatones))
  )
)


cat("\nCalidad de geometrías:\n\n")

print(
  resumen_geometria,
  width = Inf
)


cat("\nTipos de geometría — ciclistas:\n\n")

print(
  table(
    sf::st_geometry_type(accidentes_ciclistas)
  )
)


cat("\nTipos de geometría — peatones:\n\n")

print(
  table(
    sf::st_geometry_type(accidentes_peatones)
  )
)


# ------------------------------------------------------------------------------
# 16. Revisar variables y posibles fechas
# ------------------------------------------------------------------------------

cat("\nVariables — ciclistas:\n\n")

print(
  names(accidentes_ciclistas)
)


cat("\nVariables — peatones:\n\n")

print(
  names(accidentes_peatones)
)


# ------------------------------------------------------------------------------
# 17. Primeras observaciones completas sin geometría
# ------------------------------------------------------------------------------

cat("\nPrimeras observaciones — ciclistas:\n\n")

print(
  accidentes_ciclistas |>
    sf::st_drop_geometry() |>
    head(20),
  width = Inf
)


cat("\nPrimeras observaciones — peatones:\n\n")

print(
  accidentes_peatones |>
    sf::st_drop_geometry() |>
    head(20),
  width = Inf
)


# ------------------------------------------------------------------------------
# 18. Buscar variables potencialmente relevantes
# ------------------------------------------------------------------------------

patron_relevante <- paste(
  c(
    "fecha",
    "date",
    "anio",
    "año",
    "mes",
    "hora",
    "folio",
    "id",
    "lesion",
    "falle",
    "muert",
    "sexo",
    "edad",
    "tipo",
    "accidente",
    "evento",
    "alcal",
    "deleg",
    "colonia",
    "calle",
    "ubic"
  ),
  collapse = "|"
)


variables_relevantes_ciclistas <- tibble::tibble(
  variable = names(
    sf::st_drop_geometry(accidentes_ciclistas)
  )
) |>
  dplyr::filter(
    stringr::str_detect(
      stringr::str_to_lower(variable),
      patron_relevante
    )
  )


variables_relevantes_peatones <- tibble::tibble(
  variable = names(
    sf::st_drop_geometry(accidentes_peatones)
  )
) |>
  dplyr::filter(
    stringr::str_detect(
      stringr::str_to_lower(variable),
      patron_relevante
    )
  )


cat("\nVariables relevantes — ciclistas:\n\n")

print(
  variables_relevantes_ciclistas,
  n = Inf
)


cat("\nVariables relevantes — peatones:\n\n")

print(
  variables_relevantes_peatones,
  n = Inf
)

# ------------------------------------------------------------------------------
# 19. Inspeccionar observaciones como tibble
# ------------------------------------------------------------------------------

ciclistas_tab <- accidentes_ciclistas |>
  sf::st_drop_geometry() |>
  tibble::as_tibble()

peatones_tab <- accidentes_peatones |>
  sf::st_drop_geometry() |>
  tibble::as_tibble()


cat("\nPrimeras observaciones — ciclistas:\n\n")

print(
  ciclistas_tab |>
    dplyr::slice_head(n = 10),
  width = Inf
)


cat("\nPrimeras observaciones — peatones:\n\n")

print(
  peatones_tab |>
    dplyr::slice_head(n = 10),
  width = Inf
)


# ------------------------------------------------------------------------------
# 20. Revisar clases de variables temporales
# ------------------------------------------------------------------------------

variables_temporales <- c(
  "fecha_even",
  "ano_evento",
  "mes",
  "dia",
  "hora",
  "hora2"
)


tipos_temporales <- dplyr::bind_rows(

  tibble::tibble(
    fuente = "ciclistas",
    variable = variables_temporales,
    clase = purrr::map_chr(
      ciclistas_tab[variables_temporales],
      ~ paste(class(.x), collapse = "/")
    )
  ),

  tibble::tibble(
    fuente = "peatones",
    variable = variables_temporales,
    clase = purrr::map_chr(
      peatones_tab[variables_temporales],
      ~ paste(class(.x), collapse = "/")
    )
  )
)


cat("\nTipos de variables temporales:\n\n")

print(
  tipos_temporales,
  n = Inf
)


# ------------------------------------------------------------------------------
# 21. Cobertura según año declarado
# ------------------------------------------------------------------------------

cobertura_anual <- dplyr::bind_rows(

  ciclistas_tab |>
    dplyr::count(
      ano_evento,
      name = "eventos"
    ) |>
    dplyr::mutate(
      fuente = "ciclistas",
      .before = 1
    ),

  peatones_tab |>
    dplyr::count(
      ano_evento,
      name = "eventos"
    ) |>
    dplyr::mutate(
      fuente = "peatones",
      .before = 1
    )
) |>
  dplyr::arrange(
    fuente,
    ano_evento
  )


cat("\nCobertura anual:\n\n")

print(
  cobertura_anual,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 22. Cobertura mensual
# ------------------------------------------------------------------------------

cobertura_mensual <- dplyr::bind_rows(

  ciclistas_tab |>
    dplyr::count(
      ano_evento,
      mes,
      name = "eventos"
    ) |>
    dplyr::mutate(
      fuente = "ciclistas",
      .before = 1
    ),

  peatones_tab |>
    dplyr::count(
      ano_evento,
      mes,
      name = "eventos"
    ) |>
    dplyr::mutate(
      fuente = "peatones",
      .before = 1
    )
) |>
  dplyr::arrange(
    fuente,
    ano_evento,
    mes
  )


cat("\nCobertura mensual:\n\n")

print(
  cobertura_mensual,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 23. Rango de fecha original
# ------------------------------------------------------------------------------

cat("\nRango fecha_even — ciclistas:\n\n")

print(
  range(
    ciclistas_tab$fecha_even,
    na.rm = TRUE
  )
)


cat("\nRango fecha_even — peatones:\n\n")

print(
  range(
    peatones_tab$fecha_even,
    na.rm = TRUE
  )
)


# ------------------------------------------------------------------------------
# 24. Revisar folios
# ------------------------------------------------------------------------------

resumen_folios <- dplyr::bind_rows(

  ciclistas_tab |>
    dplyr::summarise(
      fuente = "ciclistas",
      filas = dplyr::n(),
      folios_unicos = dplyr::n_distinct(
        no_folio,
        na.rm = TRUE
      ),
      folios_na = sum(is.na(no_folio))
    ),

  peatones_tab |>
    dplyr::summarise(
      fuente = "peatones",
      filas = dplyr::n(),
      folios_unicos = dplyr::n_distinct(
        no_folio,
        na.rm = TRUE
      ),
      folios_na = sum(is.na(no_folio))
    )
)


cat("\nResumen de folios:\n\n")

print(
  resumen_folios,
  width = Inf
)


# ------------------------------------------------------------------------------
# 25. Folios repetidos
# ------------------------------------------------------------------------------

folios_repetidos_ciclistas <- ciclistas_tab |>
  dplyr::filter(
    !is.na(no_folio)
  ) |>
  dplyr::count(
    no_folio,
    name = "filas"
  ) |>
  dplyr::filter(
    filas > 1
  ) |>
  dplyr::arrange(
    dplyr::desc(filas)
  )


folios_repetidos_peatones <- peatones_tab |>
  dplyr::filter(
    !is.na(no_folio)
  ) |>
  dplyr::count(
    no_folio,
    name = "filas"
  ) |>
  dplyr::filter(
    filas > 1
  ) |>
  dplyr::arrange(
    dplyr::desc(filas)
  )


cat("\nFolios repetidos — ciclistas:\n\n")

print(
  folios_repetidos_ciclistas,
  n = 30,
  width = Inf
)


cat("\nFolios repetidos — peatones:\n\n")

print(
  folios_repetidos_peatones,
  n = 30,
  width = Inf
)


# ------------------------------------------------------------------------------
# 26. Severidad
# ------------------------------------------------------------------------------

resumen_severidad <- dplyr::bind_rows(

  ciclistas_tab |>
    dplyr::summarise(
      fuente = "ciclistas",
      eventos = dplyr::n(),
      total_occisos = sum(
        total_occi,
        na.rm = TRUE
      ),
      total_lesionados = sum(
        total_lesi,
        na.rm = TRUE
      )
    ),

  peatones_tab |>
    dplyr::summarise(
      fuente = "peatones",
      eventos = dplyr::n(),
      total_occisos = sum(
        total_occi,
        na.rm = TRUE
      ),
      total_lesionados = sum(
        total_lesi,
        na.rm = TRUE
      )
    )
)


cat("\nResumen de severidad:\n\n")

print(
  resumen_severidad,
  width = Inf
)


# ------------------------------------------------------------------------------
# 27. Distribución de condición y tipo de evento
# ------------------------------------------------------------------------------

condicion_eventos <- dplyr::bind_rows(

  ciclistas_tab |>
    dplyr::count(
      condicion,
      tipo_de_ev,
      name = "eventos"
    ) |>
    dplyr::mutate(
      fuente = "ciclistas",
      .before = 1
    ),

  peatones_tab |>
    dplyr::count(
      condicion,
      tipo_de_ev,
      name = "eventos"
    ) |>
    dplyr::mutate(
      fuente = "peatones",
      .before = 1
    )
) |>
  dplyr::arrange(
    fuente,
    dplyr::desc(eventos)
  )


cat("\nCondición y tipo de evento:\n\n")

print(
  condicion_eventos,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 28. Cobertura por alcaldía
# ------------------------------------------------------------------------------

eventos_alcaldia <- dplyr::bind_rows(

  ciclistas_tab |>
    dplyr::count(
      alcaldia,
      name = "eventos"
    ) |>
    dplyr::mutate(
      fuente = "ciclistas",
      .before = 1
    ),

  peatones_tab |>
    dplyr::count(
      alcaldia,
      name = "eventos"
    ) |>
    dplyr::mutate(
      fuente = "peatones",
      .before = 1
    )
) |>
  dplyr::arrange(
    fuente,
    dplyr::desc(eventos)
  )


cat("\nEventos por alcaldía:\n\n")

print(
  eventos_alcaldia,
  n = Inf,
  width = Inf
)

# ------------------------------------------------------------------------------
# 29. Inspeccionar folio repetido en peatones
# ------------------------------------------------------------------------------

cat("\nFolio repetido — peatones:\n\n")

peatones_tab |>
  dplyr::filter(
    no_folio == "991661"
  ) |>
  print(
    width = Inf
  )


# ------------------------------------------------------------------------------
# 30. Comprobar filas completamente duplicadas
# ------------------------------------------------------------------------------

cat(
  "\nFilas completamente duplicadas — ciclistas:",
  sum(duplicated(ciclistas_tab)),
  "\n"
)

cat(
  "Filas completamente duplicadas — peatones:",
  sum(duplicated(peatones_tab)),
  "\n"
)


# ------------------------------------------------------------------------------
# 31. Consistencia entre condición y severidad
# ------------------------------------------------------------------------------

consistencia_severidad <- dplyr::bind_rows(

  ciclistas_tab |>
    dplyr::mutate(
      fuente = "ciclistas"
    ),

  peatones_tab |>
    dplyr::mutate(
      fuente = "peatones"
    )

) |>
  dplyr::group_by(
    fuente,
    condicion
  ) |>
  dplyr::summarise(
    eventos = dplyr::n(),

    eventos_con_occisos = sum(
      total_occi > 0,
      na.rm = TRUE
    ),

    eventos_con_lesionados = sum(
      total_lesi > 0,
      na.rm = TRUE
    ),

    total_occisos = sum(
      total_occi,
      na.rm = TRUE
    ),

    total_lesionados = sum(
      total_lesi,
      na.rm = TRUE
    ),

    .groups = "drop"
  )


cat("\nConsistencia de severidad:\n\n")

print(
  consistencia_severidad,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 32. Extraer coordenadas de la geometría
# ------------------------------------------------------------------------------

coords_ciclistas <- sf::st_coordinates(
  accidentes_ciclistas
)

coords_peatones <- sf::st_coordinates(
  accidentes_peatones
)


comparacion_coordenadas <- dplyr::bind_rows(

  tibble::tibble(
    fuente = "ciclistas",

    lon_atributo = ciclistas_tab$coordenada,
    lat_atributo = ciclistas_tab$coordena_1,

    lon_geometria = coords_ciclistas[, "X"],
    lat_geometria = coords_ciclistas[, "Y"]
  ),

  tibble::tibble(
    fuente = "peatones",

    lon_atributo = peatones_tab$coordenada,
    lat_atributo = peatones_tab$coordena_1,

    lon_geometria = coords_peatones[, "X"],
    lat_geometria = coords_peatones[, "Y"]
  )

) |>
  dplyr::mutate(
    diferencia_lon = abs(
      lon_atributo - lon_geometria
    ),

    diferencia_lat = abs(
      lat_atributo - lat_geometria
    )
  )


resumen_coordenadas <- comparacion_coordenadas |>
  dplyr::group_by(
    fuente
  ) |>
  dplyr::summarise(
    observaciones = dplyr::n(),

    coordenadas_atributo_na = sum(
      is.na(lon_atributo) |
        is.na(lat_atributo)
    ),

    diferencia_lon_max = max(
      diferencia_lon,
      na.rm = TRUE
    ),

    diferencia_lat_max = max(
      diferencia_lat,
      na.rm = TRUE
    ),

    .groups = "drop"
  )


cat("\nComparación coordenadas atributo vs geometría:\n\n")

print(
  resumen_coordenadas,
  width = Inf
)


# ------------------------------------------------------------------------------
# 33. Revisar rango espacial en WGS84
# ------------------------------------------------------------------------------

ciclistas_ll <- accidentes_ciclistas |>
  sf::st_transform(4326)

peatones_ll <- accidentes_peatones |>
  sf::st_transform(4326)


cat("\nBounding box ciclistas — WGS84:\n\n")

print(
  sf::st_bbox(ciclistas_ll)
)


cat("\nBounding box peatones — WGS84:\n\n")

print(
  sf::st_bbox(peatones_ll)
)