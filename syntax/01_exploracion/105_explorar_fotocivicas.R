# ==============================================================================
# 105_explorar_fotocivicas.R
# Exploración inicial de datos de Fotocívicas
# ==============================================================================

source("syntax/00_setup/000_setup.R")


# ------------------------------------------------------------------------------
# 1. Rutas
# ------------------------------------------------------------------------------

ruta_fotocivicas <- fs::path(
  rutas$data_raw,
  "07_fotocivicas"
)

ruta_cumplimiento <- fs::path(
  ruta_fotocivicas,
  "cumplimiento"
)

ruta_ubicacion <- fs::path(
  ruta_fotocivicas,
  "ubicacion"
)


# ------------------------------------------------------------------------------
# 2. Inventario
# ------------------------------------------------------------------------------

archivos_fotocivicas <- fs::dir_ls(
  ruta_fotocivicas,
  recurse = TRUE,
  type = "file"
)

inventario_fotocivicas <- tibble::tibble(
  ruta = archivos_fotocivicas,
  archivo = fs::path_file(archivos_fotocivicas),
  extension = fs::path_ext(archivos_fotocivicas),
  tamano_mb = round(
    as.numeric(fs::file_size(archivos_fotocivicas)) / 1024^2,
    3
  )
)

cat("\nInventario Fotocívicas:\n\n")

print(
  inventario_fotocivicas,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 3. Archivos CSV
# ------------------------------------------------------------------------------

archivos_csv <- archivos_fotocivicas[
  stringr::str_to_lower(
    fs::path_ext(archivos_fotocivicas)
  ) == "csv"
]

cat("\nArchivos CSV encontrados:", length(archivos_csv), "\n")


# ------------------------------------------------------------------------------
# 4. Inspeccionar estructura de cada CSV
# ------------------------------------------------------------------------------

inspeccionar_csv <- function(ruta) {

  archivo <- fs::path_file(ruta)

  cat(
    "\n\n============================================================\n",
    archivo,
    "\n============================================================\n"
  )

  # Algunos archivos que descargamos aparecían con tamaño 0.
  tamano <- as.numeric(
    fs::file_size(ruta)
  )

  if (tamano == 0) {

    cat("ARCHIVO VACÍO\n")

    return(
      tibble::tibble(
        archivo = archivo,
        tamano_bytes = tamano,
        filas_muestra = NA_integer_,
        columnas = NA_integer_,
        estado = "vacio"
      )
    )
  }


  datos <- tryCatch(

    readr::read_csv(
      ruta,
      n_max = 1000,
      show_col_types = FALSE,
      progress = FALSE,
      name_repair = "unique"
    ),

    error = function(e) {

      cat(
        "ERROR DE LECTURA:",
        conditionMessage(e),
        "\n"
      )

      return(NULL)
    }
  )


  if (is.null(datos)) {

    return(
      tibble::tibble(
        archivo = archivo,
        tamano_bytes = tamano,
        filas_muestra = NA_integer_,
        columnas = NA_integer_,
        estado = "error_lectura"
      )
    )
  }


  cat("\nDimensiones de muestra:\n")
  print(dim(datos))

  cat("\nVariables:\n")
  print(names(datos))

  cat("\nPrimeras observaciones:\n")

  print(
    head(datos, 5),
    width = Inf
  )

  cat("\nTipos de variables:\n")

  print(
    tibble::tibble(
      variable = names(datos),
      clase = purrr::map_chr(
        datos,
        ~ paste(class(.x), collapse = "/")
      )
    ),
    n = Inf
  )


  tibble::tibble(
    archivo = archivo,
    tamano_bytes = tamano,
    filas_muestra = nrow(datos),
    columnas = ncol(datos),
    estado = "ok"
  )
}


resumen_csv_fotocivicas <- purrr::map_dfr(
  archivos_csv,
  inspeccionar_csv
)


# ------------------------------------------------------------------------------
# 5. Resumen
# ------------------------------------------------------------------------------

cat("\n\nResumen de archivos CSV:\n\n")

print(
  resumen_csv_fotocivicas,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 6. Inspeccionar ZIP
# ------------------------------------------------------------------------------

archivos_zip <- archivos_fotocivicas[
  stringr::str_to_lower(
    fs::path_ext(archivos_fotocivicas)
  ) == "zip"
]


contenido_zip <- purrr::map_dfr(
  archivos_zip,
  function(ruta) {

    archivo <- fs::path_file(ruta)

    cat(
      "\nContenido de:",
      archivo,
      "\n"
    )

    contenido <- utils::unzip(
      ruta,
      list = TRUE
    )

    print(contenido)

    tibble::as_tibble(contenido) |>
      dplyr::mutate(
        archivo_zip = archivo,
        .before = 1
      )
  }
)


cat("\n\nContenido de archivos ZIP:\n\n")

print(
  contenido_zip,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 7. Guardar inventario
# ------------------------------------------------------------------------------

readr::write_csv(
  inventario_fotocivicas,
  fs::path(
    rutas$output_tables,
    "105_fotocivicas_inventario.csv"
  )
)

readr::write_csv(
  resumen_csv_fotocivicas,
  fs::path(
    rutas$output_tables,
    "105_fotocivicas_resumen_csv.csv"
  )
)

readr::write_csv(
  contenido_zip,
  fs::path(
    rutas$output_tables,
    "105_fotocivicas_contenido_zip.csv"
  )
)

message("Exploración inicial de Fotocívicas terminada.")


# ------------------------------------------------------------------------------
# 8. Leer completas las bases de cumplimiento
# ------------------------------------------------------------------------------

archivo_online <- fs::path(
  ruta_cumplimiento,
  "cursos_en_linea_fotocivicas_07_22.csv"
)

archivo_presencial <- fs::path(
  ruta_cumplimiento,
  "cursos_presenciales_fotocivicas__07_22.csv"
)

fotocivicas_online <- readr::read_csv(
  archivo_online,
  show_col_types = FALSE,
  progress = FALSE
)

fotocivicas_presencial <- readr::read_csv(
  archivo_presencial,
  show_col_types = FALSE,
  progress = FALSE
)


cat("\nDimensiones bases completas:\n\n")

cat(
  "Cursos en línea:",
  format(nrow(fotocivicas_online), big.mark = ","),
  "filas\n"
)

cat(
  "Cursos presenciales:",
  format(nrow(fotocivicas_presencial), big.mark = ","),
  "filas\n"
)


# ------------------------------------------------------------------------------
# 9. Cobertura temporal
# ------------------------------------------------------------------------------

cobertura_cumplimiento <- dplyr::bind_rows(

  fotocivicas_online |>
    dplyr::summarise(
      fuente = "Curso en línea",
      fecha_min = min(fecha, na.rm = TRUE),
      fecha_max = max(fecha, na.rm = TRUE),
      observaciones = dplyr::n(),
      usuarios = dplyr::n_distinct(id_user),
      placas = dplyr::n_distinct(placa)
    ),

  fotocivicas_presencial |>
    dplyr::summarise(
      fuente = "Presencial",
      fecha_min = min(fecha, na.rm = TRUE),
      fecha_max = max(fecha, na.rm = TRUE),
      observaciones = dplyr::n(),
      usuarios = dplyr::n_distinct(id_user),
      placas = dplyr::n_distinct(placa)
    )
)


cat("\nCobertura temporal y unidades observadas:\n\n")

print(
  cobertura_cumplimiento,
  width = Inf
)


# ------------------------------------------------------------------------------
# 10. Distribución anual
# ------------------------------------------------------------------------------

cumplimiento_anual <- dplyr::bind_rows(

  fotocivicas_online |>
    dplyr::transmute(
      fuente = "Curso en línea",
      fecha
    ),

  fotocivicas_presencial |>
    dplyr::transmute(
      fuente = "Presencial",
      fecha
    )

) |>
  dplyr::filter(
    !is.na(fecha)
  ) |>
  dplyr::mutate(
    anio = lubridate::year(fecha)
  ) |>
  dplyr::count(
    fuente,
    anio,
    name = "observaciones"
  )


cat("\nCumplimiento por año:\n\n")

print(
  cumplimiento_anual,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 11. Distribución mensual
# ------------------------------------------------------------------------------

cumplimiento_mensual <- dplyr::bind_rows(

  fotocivicas_online |>
    dplyr::transmute(
      fuente = "Curso en línea",
      fecha
    ),

  fotocivicas_presencial |>
    dplyr::transmute(
      fuente = "Presencial",
      fecha
    )

) |>
  dplyr::filter(
    !is.na(fecha)
  ) |>
  dplyr::mutate(
    mes = lubridate::floor_date(
      fecha,
      unit = "month"
    )
  ) |>
  dplyr::count(
    fuente,
    mes,
    name = "observaciones"
  )


# ------------------------------------------------------------------------------
# 12. Tipos de cumplimiento presencial
# ------------------------------------------------------------------------------

tipos_presenciales <- fotocivicas_presencial |>
  dplyr::count(
    tipo,
    subtipo,
    status,
    sort = TRUE,
    name = "observaciones"
  ) |>
  dplyr::mutate(
    porcentaje = round(
      observaciones / sum(observaciones) * 100,
      2
    )
  )


cat("\nTipos de cumplimiento presencial:\n\n")

print(
  tipos_presenciales,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 13. Cursos en línea: tipo y resultado
# ------------------------------------------------------------------------------

tipos_online <- fotocivicas_online |>
  dplyr::count(
    nombre_cita,
    status,
    sort = TRUE,
    name = "observaciones"
  ) |>
  dplyr::mutate(
    porcentaje = round(
      observaciones / sum(observaciones) * 100,
      2
    )
  )


cat("\nCursos en línea y resultado:\n\n")

print(
  tipos_online,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 14. Leer ubicaciones
# ------------------------------------------------------------------------------

fotocivicas_ubicacion <- readr::read_csv(
  fs::path(
    ruta_ubicacion,
    "fotocivicas-ubicacion.csv"
  ),
  show_col_types = FALSE
)


cat("\nNúmero de ubicaciones:", nrow(fotocivicas_ubicacion), "\n")

cat(
  "IDs únicos:",
  dplyr::n_distinct(fotocivicas_ubicacion$id),
  "\n"
)

cat(
  "Vías principales:",
  dplyr::n_distinct(fotocivicas_ubicacion$via_princi),
  "\n"
)


# ------------------------------------------------------------------------------
# 15. Ubicaciones por vialidad
# ------------------------------------------------------------------------------

ubicaciones_vialidad <- fotocivicas_ubicacion |>
  dplyr::count(
    via_princi,
    sort = TRUE,
    name = "ubicaciones"
  )


cat("\nUbicaciones por vialidad:\n\n")

print(
  ubicaciones_vialidad,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 16. Validar coordenadas
# ------------------------------------------------------------------------------

validacion_coordenadas <- fotocivicas_ubicacion |>
  dplyr::summarise(

    observaciones = dplyr::n(),

    latitud_na = sum(
      is.na(latitud)
    ),

    longitud_na = sum(
      is.na(longitud)
    ),

    coordenadas_duplicadas = sum(
      duplicated(
        paste(latitud, longitud)
      )
    ),

    latitud_min = min(
      latitud,
      na.rm = TRUE
    ),

    latitud_max = max(
      latitud,
      na.rm = TRUE
    ),

    longitud_min = min(
      longitud,
      na.rm = TRUE
    ),

    longitud_max = max(
      longitud,
      na.rm = TRUE
    )
  )


cat("\nValidación de coordenadas:\n\n")

print(
  validacion_coordenadas,
  width = Inf
)


# ------------------------------------------------------------------------------
# 17. Guardar resultados
# ------------------------------------------------------------------------------

readr::write_csv(
  cobertura_cumplimiento,
  fs::path(
    rutas$output_tables,
    "105_fotocivicas_cobertura.csv"
  )
)

readr::write_csv(
  cumplimiento_anual,
  fs::path(
    rutas$output_tables,
    "105_fotocivicas_cumplimiento_anual.csv"
  )
)

readr::write_csv(
  cumplimiento_mensual,
  fs::path(
    rutas$output_tables,
    "105_fotocivicas_cumplimiento_mensual.csv"
  )
)

readr::write_csv(
  ubicaciones_vialidad,
  fs::path(
    rutas$output_tables,
    "105_fotocivicas_ubicaciones_vialidad.csv"
  )
)

# ------------------------------------------------------------------------------
# 18. Leer shapefile de puntos
# ------------------------------------------------------------------------------

zip_puntos <- fs::path(
  ruta_ubicacion,
  "fotocivicas-ubicacion-puntos.zip"
)

ruta_shp_puntos <- paste0(
  "/vsizip/",
  normalizePath(
    zip_puntos,
    winslash = "/"
  ),
  "/fotocivicas-ubicacion-puntos/fotocivicas-ubicacion-puntos.shp"
)

fotocivicas_puntos <- sf::st_read(
  ruta_shp_puntos,
  quiet = TRUE
)


cat("\n\n============================================================\n")
cat("SHAPEFILE DE PUNTOS\n")
cat("============================================================\n\n")

print(fotocivicas_puntos)

cat("\nNúmero de puntos:", nrow(fotocivicas_puntos), "\n")

cat("\nVariables:\n")
print(names(fotocivicas_puntos))

cat("\nCRS:\n")
print(sf::st_crs(fotocivicas_puntos))

cat("\nBounding box:\n")
print(sf::st_bbox(fotocivicas_puntos))

cat("\nPrimeras observaciones sin geometría:\n")

print(
  sf::st_drop_geometry(fotocivicas_puntos) |>
    head(20),
  width = Inf
)


# ------------------------------------------------------------------------------
# 19. Leer shapefile de líneas
# ------------------------------------------------------------------------------

zip_lineas <- fs::path(
  ruta_ubicacion,
  "fotocivicas-ubicacion-lineas.zip"
)

ruta_shp_lineas <- paste0(
  "/vsizip/",
  normalizePath(
    zip_lineas,
    winslash = "/"
  ),
  "/fotocivicas-ubicacion-lineas/fotocivicas-ubicacion-lineas.shp"
)

fotocivicas_lineas <- sf::st_read(
  ruta_shp_lineas,
  quiet = TRUE
)


cat("\n\n============================================================\n")
cat("SHAPEFILE DE LÍNEAS\n")
cat("============================================================\n\n")

print(fotocivicas_lineas)

cat("\nNúmero de líneas:", nrow(fotocivicas_lineas), "\n")

cat("\nVariables:\n")
print(names(fotocivicas_lineas))

cat("\nCRS:\n")
print(sf::st_crs(fotocivicas_lineas))

cat("\nBounding box:\n")
print(sf::st_bbox(fotocivicas_lineas))

cat("\nPrimeras observaciones sin geometría:\n")

print(
  sf::st_drop_geometry(fotocivicas_lineas) |>
    head(20),
  width = Inf
)


# ------------------------------------------------------------------------------
# 20. Resumen comparativo de capas espaciales
# ------------------------------------------------------------------------------

resumen_capas_fotocivicas <- tibble::tibble(

  capa = c(
    "puntos",
    "lineas"
  ),

  observaciones = c(
    nrow(fotocivicas_puntos),
    nrow(fotocivicas_lineas)
  ),

  tipo_geometria = c(
    paste(
      unique(
        as.character(
          sf::st_geometry_type(fotocivicas_puntos)
        )
      ),
      collapse = ", "
    ),

    paste(
      unique(
        as.character(
          sf::st_geometry_type(fotocivicas_lineas)
        )
      ),
      collapse = ", "
    )
  ),

  crs = c(
    sf::st_crs(fotocivicas_puntos)$input,
    sf::st_crs(fotocivicas_lineas)$input
  ),

  variables = c(
    paste(
      setdiff(
        names(fotocivicas_puntos),
        attr(fotocivicas_puntos, "sf_column")
      ),
      collapse = " | "
    ),

    paste(
      setdiff(
        names(fotocivicas_lineas),
        attr(fotocivicas_lineas, "sf_column")
      ),
      collapse = " | "
    )
  )
)


cat("\nResumen de capas espaciales:\n\n")

print(
  resumen_capas_fotocivicas,
  width = Inf
)


# ------------------------------------------------------------------------------
# 21. Comparar puntos con CSV de ubicaciones
# ------------------------------------------------------------------------------

cat("\nComparación CSV vs shapefile de puntos:\n\n")

cat(
  "Filas CSV:",
  nrow(fotocivicas_ubicacion),
  "\n"
)

cat(
  "Filas shapefile:",
  nrow(fotocivicas_puntos),
  "\n"
)

if (
  "id" %in% names(fotocivicas_ubicacion) &&
  "id" %in% names(fotocivicas_puntos)
) {

  cat(
    "IDs únicos CSV:",
    dplyr::n_distinct(fotocivicas_ubicacion$id),
    "\n"
  )

  cat(
    "IDs únicos shapefile:",
    dplyr::n_distinct(fotocivicas_puntos$id),
    "\n"
  )

  cat(
    "IDs CSV presentes en shapefile:",
    sum(
      unique(fotocivicas_ubicacion$id) %in%
        unique(fotocivicas_puntos$id)
    ),
    "\n"
  )
}


# ------------------------------------------------------------------------------
# 22. Guardar resumen
# ------------------------------------------------------------------------------

readr::write_csv(
  resumen_capas_fotocivicas,
  fs::path(
    rutas$output_tables,
    "105_fotocivicas_capas_espaciales.csv"
  )
)