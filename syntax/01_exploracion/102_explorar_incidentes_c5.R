# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 102_explorar_incidentes_c5.R
# Objetivo:
#   Explorar la estructura de los archivos de incidentes viales C5.
#
# IMPORTANTE:
#   Este script NO modifica los datos originales.
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------------------------

source(
  here::here(
    "syntax",
    "00_setup",
    "000_setup.R"
  ),
  encoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 1. Localizar archivos
# ------------------------------------------------------------------------------

ruta_c5 <- fs::path(
  rutas$data_raw,
  "01_incidentes_c5"
)

archivos_c5 <- fs::dir_ls(
  path = ruta_c5,
  regexp = "\\.csv$",
  type = "file"
)

cat("\nArchivos encontrados:", length(archivos_c5), "\n\n")

print(
  fs::path_file(archivos_c5)
)


# ------------------------------------------------------------------------------
# 2. Leer una muestra de cada archivo
# ------------------------------------------------------------------------------

# Solo se leen las primeras 1,000 filas para revisar estructura y columnas.
# Todavía no se cargan completos los archivos.

muestras <- purrr::map(
  archivos_c5,
  ~ readr::read_csv(
    file = .x,
    n_max = 1000,
    show_col_types = FALSE,
    progress = FALSE
  )
)

names(muestras) <- fs::path_file(archivos_c5)


# ------------------------------------------------------------------------------
# 3. Dimensiones de las muestras
# ------------------------------------------------------------------------------

resumen_dimensiones <- purrr::imap_dfr(
  muestras,
  ~ tibble::tibble(
    archivo = .y,
    filas_muestra = nrow(.x),
    columnas = ncol(.x)
  )
)

cat("\nDimensiones de las muestras:\n\n")

print(
  resumen_dimensiones,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 4. Comparar nombres de variables entre archivos
# ------------------------------------------------------------------------------

columnas_por_archivo <- purrr::imap_dfr(
  muestras,
  ~ tibble::tibble(
    archivo = .y,
    variable = names(.x)
  )
)

cat("\nVariables por archivo:\n\n")

print(
  columnas_por_archivo,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 5. Identificar variables comunes
# ------------------------------------------------------------------------------

columnas_comunes <- Reduce(
  intersect,
  purrr::map(muestras, names)
)

cat("\nVariables presentes en TODOS los archivos:\n\n")

print(columnas_comunes)


# ------------------------------------------------------------------------------
# 6. Identificar variables que cambian entre archivos
# ------------------------------------------------------------------------------

todas_columnas <- sort(
  unique(
    unlist(
      purrr::map(muestras, names)
    )
  )
)

presencia_columnas <- purrr::map_dfr(
  todas_columnas,
  function(variable_actual) {

    tibble::tibble(
      variable = variable_actual,
      archivos_con_variable = sum(
        purrr::map_lgl(
          muestras,
          ~ variable_actual %in% names(.x)
        )
      ),
      total_archivos = length(muestras)
    )
  }
)

columnas_no_comunes <- presencia_columnas |>
  dplyr::filter(
    archivos_con_variable < total_archivos
  )

cat("\nVariables que NO aparecen en todos los archivos:\n\n")

print(
  columnas_no_comunes,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 7. Revisar tipos de variables
# ------------------------------------------------------------------------------

tipos_variables <- purrr::imap_dfr(
  muestras,
  function(datos, nombre_archivo) {

    tibble::tibble(
      archivo = nombre_archivo,
      variable = names(datos),
      clase = purrr::map_chr(
        datos,
        ~ paste(class(.x), collapse = "/")
      )
    )
  }
)

cat("\nTipos de variables por archivo:\n\n")

print(
  tipos_variables,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 8. Identificar archivos con posibles solapamientos por nombre
# ------------------------------------------------------------------------------

cat("\nNombres completos de archivos:\n\n")

tibble::tibble(
  archivo = fs::path_file(archivos_c5)
) |>
  print(
    n = Inf,
    width = Inf
  )


# ------------------------------------------------------------------------------
# 9. Mostrar primeras filas del archivo más reciente
# ------------------------------------------------------------------------------

# Se toma el último archivo según el orden devuelto por dir_ls().
# Revisar que efectivamente sea el archivo 2022–2024.

nombre_ultimo_archivo <- names(muestras)[length(muestras)]

cat(
  "\nPrimeras filas del archivo seleccionado:\n",
  nombre_ultimo_archivo,
  "\n\n"
)

print(
  dplyr::slice_head(
    muestras[[nombre_ultimo_archivo]],
    n = 10
  ),
  width = Inf
)


# ------------------------------------------------------------------------------
# 10. Guardar resúmenes
# ------------------------------------------------------------------------------

readr::write_csv(
  resumen_dimensiones,
  fs::path(
    rutas$output_tables,
    "102_c5_dimensiones_muestras.csv"
  )
)

readr::write_csv(
  columnas_por_archivo,
  fs::path(
    rutas$output_tables,
    "102_c5_columnas_por_archivo.csv"
  )
)

readr::write_csv(
  presencia_columnas,
  fs::path(
    rutas$output_tables,
    "102_c5_presencia_columnas.csv"
  )
)

readr::write_csv(
  tipos_variables,
  fs::path(
    rutas$output_tables,
    "102_c5_tipos_variables.csv"
  )
)

# ------------------------------------------------------------------------------
# 11. Auditoría temporal completa
# ------------------------------------------------------------------------------

cat("\nAuditoría temporal completa:\n\n")

auditoria_temporal <- purrr::map_dfr(
  archivos_c5,
  function(archivo_actual) {

    datos_fecha <- readr::read_csv(
      archivo_actual,
      col_select = c(
        folio,
        fecha_creacion,
        fecha_cierre
      ),
      show_col_types = FALSE,
      progress = FALSE
    )

    tibble::tibble(
      archivo = fs::path_file(archivo_actual),

      observaciones = nrow(datos_fecha),

      fecha_creacion_min = min(
        datos_fecha$fecha_creacion,
        na.rm = TRUE
      ),

      fecha_creacion_max = max(
        datos_fecha$fecha_creacion,
        na.rm = TRUE
      ),

      fecha_cierre_min = min(
        datos_fecha$fecha_cierre,
        na.rm = TRUE
      ),

      fecha_cierre_max = max(
        datos_fecha$fecha_cierre,
        na.rm = TRUE
      ),

      folios_unicos = dplyr::n_distinct(
        datos_fecha$folio
      ),

      folios_duplicados =
        nrow(datos_fecha) -
        dplyr::n_distinct(datos_fecha$folio)
    )
  }
)

print(
  auditoria_temporal,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 12. Cobertura por año
# ------------------------------------------------------------------------------

cobertura_anual <- purrr::map_dfr(
  archivos_c5,
  function(archivo_actual) {

    readr::read_csv(
      archivo_actual,
      col_select = fecha_creacion,
      show_col_types = FALSE,
      progress = FALSE
    ) |>
      dplyr::mutate(
        anio = lubridate::year(fecha_creacion)
      ) |>
      dplyr::count(anio, name = "observaciones") |>
      dplyr::mutate(
        archivo = fs::path_file(archivo_actual)
      ) |>
      dplyr::select(
        archivo,
        anio,
        observaciones
      )
  }
)

cat("\nCobertura por año y archivo:\n\n")

print(
  cobertura_anual,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 13. Guardar auditoría temporal
# ------------------------------------------------------------------------------

readr::write_csv(
  auditoria_temporal,
  fs::path(
    rutas$output_tables,
    "102_c5_auditoria_temporal.csv"
  )
)

readr::write_csv(
  cobertura_anual,
  fs::path(
    rutas$output_tables,
    "102_c5_cobertura_anual.csv"
  )
)

# ------------------------------------------------------------------------------
# 14. Comprobar solapamiento 2022-2023 vs 2022-2024
# ------------------------------------------------------------------------------

archivo_viejo <- fs::path(
  ruta_c5,
  "inViales_2022_2023.csv"
)

archivo_nuevo <- fs::path(
  ruta_c5,
  "inViales_2022_2024.csv"
)

folios_viejo <- readr::read_csv(
  archivo_viejo,
  col_select = c(folio, fecha_creacion),
  show_col_types = FALSE
)

folios_nuevo <- readr::read_csv(
  archivo_nuevo,
  col_select = c(folio, fecha_creacion),
  show_col_types = FALSE
)

# Comparar por año
comparacion_solapamiento <- folios_viejo |>
  dplyr::mutate(
    anio = lubridate::year(fecha_creacion)
  ) |>
  dplyr::filter(anio %in% c(2022, 2023)) |>
  dplyr::group_by(anio) |>
  dplyr::summarise(
    folios_archivo_viejo = dplyr::n_distinct(folio),

    folios_en_archivo_nuevo = sum(
      unique(folio) %in% folios_nuevo$folio
    ),

    porcentaje_en_nuevo =
      folios_en_archivo_nuevo /
      folios_archivo_viejo * 100,

    .groups = "drop"
  )

print(comparacion_solapamiento)


folios_nuevo |>
  dplyr::filter(
    lubridate::year(fecha_creacion) == 2024
  ) |>
  dplyr::mutate(
    mes = lubridate::floor_date(fecha_creacion, "month")
  ) |>
  dplyr::count(mes) |>
  print(n = Inf)