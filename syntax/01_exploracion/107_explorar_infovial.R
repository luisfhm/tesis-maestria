# ==============================================================================
# 107_explorar_infovial.R
# Exploración inicial de INFOVIAL
# ==============================================================================

source("syntax/00_setup/000_setup.R")


# ------------------------------------------------------------------------------
# 1. Ruta
# ------------------------------------------------------------------------------

ruta_infovial <- fs::path(
  rutas$data_raw,
  "09_infovial"
)

archivos_infovial <- fs::dir_ls(
  ruta_infovial,
  regexp = "\\.csv$",
  type = "file"
)

cat("\nArchivos INFOVIAL encontrados:\n\n")

print(
  fs::path_file(archivos_infovial)
)


# ------------------------------------------------------------------------------
# 2. Inventario
# ------------------------------------------------------------------------------

inventario_infovial <- tibble::tibble(
  ruta = archivos_infovial,
  archivo = fs::path_file(archivos_infovial),
  tamano_mb = round(
    as.numeric(
      fs::file_size(archivos_infovial)
    ) / 1024^2,
    3
  )
)


cat("\nInventario INFOVIAL:\n\n")

print(
  inventario_infovial,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 3. Leer muestra de cada archivo
# ------------------------------------------------------------------------------

muestras_infovial <- purrr::map(
  archivos_infovial,
  function(ruta) {

    readr::read_csv(
      ruta,
      n_max = 2000,
      show_col_types = FALSE,
      progress = FALSE,
      name_repair = "unique"
    )
  }
)

names(muestras_infovial) <- fs::path_file(
  archivos_infovial
)


# ------------------------------------------------------------------------------
# 4. Dimensiones
# ------------------------------------------------------------------------------

dimensiones_infovial <- purrr::imap_dfr(
  muestras_infovial,
  function(datos, archivo) {

    tibble::tibble(
      archivo = archivo,
      filas_muestra = nrow(datos),
      columnas = ncol(datos)
    )
  }
)


cat("\nDimensiones de las muestras:\n\n")

print(
  dimensiones_infovial,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 5. Variables
# ------------------------------------------------------------------------------

columnas_infovial <- purrr::imap_dfr(
  muestras_infovial,
  function(datos, archivo) {

    tibble::tibble(
      archivo = archivo,
      posicion = seq_along(names(datos)),
      variable = names(datos)
    )
  }
)


cat("\nVariables por archivo:\n\n")

print(
  columnas_infovial,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 6. Comparar estructuras
# ------------------------------------------------------------------------------

estructuras_infovial <- purrr::imap_dfr(
  muestras_infovial,
  function(datos, archivo) {

    tibble::tibble(
      archivo = archivo,
      n_variables = ncol(datos),

      estructura = paste(
        names(datos),
        collapse = " | "
      )
    )
  }
) |>
  dplyr::mutate(
    id_estructura = dplyr::dense_rank(
      estructura
    )
  )


cat("\nEstructuras detectadas:\n\n")

print(
  estructuras_infovial |>
    dplyr::select(
      archivo,
      n_variables,
      id_estructura
    ),
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 7. Variables presentes en todos los archivos
# ------------------------------------------------------------------------------

variables_comunes <- Reduce(
  intersect,
  purrr::map(
    muestras_infovial,
    names
  )
)


cat("\nVariables presentes en todos los archivos:\n\n")

print(
  variables_comunes
)


# ------------------------------------------------------------------------------
# 8. Tipos de variables
# ------------------------------------------------------------------------------

tipos_infovial <- purrr::imap_dfr(
  muestras_infovial,
  function(datos, archivo) {

    tibble::tibble(
      archivo = archivo,
      variable = names(datos),

      clase = purrr::map_chr(
        datos,
        ~ paste(
          class(.x),
          collapse = "/"
        )
      )
    )
  }
)


cat("\nTipos de variables:\n\n")

print(
  tipos_infovial,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 9. Buscar variables potencialmente relevantes
# ------------------------------------------------------------------------------

patron_infovial <- paste(
  c(
    "fecha",
    "hora",
    "date",
    "time",
    "vel",
    "speed",
    "flujo",
    "aforo",
    "volumen",
    "ocup",
    "intens",
    "sensor",
    "estacion",
    "punto",
    "via",
    "vial",
    "direccion",
    "sentido",
    "lat",
    "lon",
    "x",
    "y"
  ),
  collapse = "|"
)


variables_relevantes_infovial <- columnas_infovial |>
  dplyr::filter(
    stringr::str_detect(
      stringr::str_to_lower(variable),
      patron_infovial
    )
  )


cat("\nVariables potencialmente relevantes:\n\n")

print(
  variables_relevantes_infovial,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 10. Primeras observaciones
# ------------------------------------------------------------------------------

purrr::iwalk(
  muestras_infovial,
  function(datos, archivo) {

    cat(
      "\n\n============================================================\n",
      archivo,
      "\n============================================================\n\n"
    )

    print(
      dplyr::slice_head(
        datos,
        n = 10
      ),
      width = Inf
    )
  }
)


# ------------------------------------------------------------------------------
# 11. Cardinalidad y missing en muestras
# ------------------------------------------------------------------------------

calidad_muestras_infovial <- purrr::imap_dfr(
  muestras_infovial,
  function(datos, archivo) {

    tibble::tibble(
      archivo = archivo,
      variable = names(datos),

      valores_unicos = purrr::map_int(
        datos,
        ~ dplyr::n_distinct(
          .x,
          na.rm = TRUE
        )
      ),

      faltantes = purrr::map_int(
        datos,
        ~ sum(
          is.na(.x)
        )
      ),

      porcentaje_faltantes = round(
        faltantes / nrow(datos) * 100,
        2
      )
    )
  }
)


cat("\nCardinalidad y faltantes en muestras:\n\n")

print(
  calidad_muestras_infovial,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 12. Guardar auditoría inicial
# ------------------------------------------------------------------------------

readr::write_csv(
  inventario_infovial,
  fs::path(
    rutas$output_tables,
    "107_infovial_inventario.csv"
  )
)

readr::write_csv(
  columnas_infovial,
  fs::path(
    rutas$output_tables,
    "107_infovial_columnas.csv"
  )
)

readr::write_csv(
  tipos_infovial,
  fs::path(
    rutas$output_tables,
    "107_infovial_tipos_variables.csv"
  )
)

readr::write_csv(
  calidad_muestras_infovial,
  fs::path(
    rutas$output_tables,
    "107_infovial_calidad_muestras.csv"
  )
)


message("Exploración estructural inicial de INFOVIAL terminada.")

# ------------------------------------------------------------------------------
# 13. Leer bases completas
# ------------------------------------------------------------------------------

archivo_velocidad <- fs::path(
  ruta_infovial,
  "201601_velocidad_limpio.csv"
)

archivo_clasificacion <- fs::path(
  ruta_infovial,
  "201701_clasificacion_limpio.csv"
)


cat("\nLeyendo base completa de velocidad...\n")

infovial_velocidad <- readr::read_csv(
  archivo_velocidad,
  show_col_types = FALSE,
  progress = TRUE
)


cat("\nLeyendo base completa de clasificación vehicular...\n")

infovial_clasificacion <- readr::read_csv(
  archivo_clasificacion,
  show_col_types = FALSE,
  progress = TRUE
)


cat("\nDimensiones completas:\n\n")

cat(
  "Velocidad:",
  format(nrow(infovial_velocidad), big.mark = ","),
  "filas x",
  ncol(infovial_velocidad),
  "columnas\n"
)

cat(
  "Clasificación:",
  format(nrow(infovial_clasificacion), big.mark = ","),
  "filas x",
  ncol(infovial_clasificacion),
  "columnas\n"
)


# ------------------------------------------------------------------------------
# 14. Cobertura temporal
# ------------------------------------------------------------------------------

cobertura_velocidad <- infovial_velocidad |>
  dplyr::summarise(
    fecha_min = min(fecha, na.rm = TRUE),
    fecha_max = max(fecha, na.rm = TRUE),
    observaciones = dplyr::n(),
    dias = dplyr::n_distinct(as.Date(fecha)),
    ids = dplyr::n_distinct(id),
    vialidades = dplyr::n_distinct(vialidad),
    ubicaciones = dplyr::n_distinct(ubicacion)
  )


cobertura_clasificacion <- infovial_clasificacion |>
  dplyr::summarise(
    fecha_min = min(fecha, na.rm = TRUE),
    fecha_max = max(fecha, na.rm = TRUE),
    observaciones = dplyr::n(),
    dias = dplyr::n_distinct(fecha),
    ids = dplyr::n_distinct(id),
    vialidades = dplyr::n_distinct(vialidad),
    tipos_vehiculo = dplyr::n_distinct(tipo_vehiculo)
  )


cat("\nCobertura base de velocidad:\n\n")

print(
  cobertura_velocidad,
  width = Inf
)


cat("\nCobertura base de clasificación vehicular:\n\n")

print(
  cobertura_clasificacion,
  width = Inf
)


# ------------------------------------------------------------------------------
# 15. Cobertura por año
# ------------------------------------------------------------------------------

velocidad_anual <- infovial_velocidad |>
  dplyr::mutate(
    anio = lubridate::year(fecha)
  ) |>
  dplyr::group_by(anio) |>
  dplyr::summarise(
    observaciones = dplyr::n(),
    dias = dplyr::n_distinct(as.Date(fecha)),
    ids = dplyr::n_distinct(id),
    vialidades = dplyr::n_distinct(vialidad),
    ubicaciones = dplyr::n_distinct(ubicacion),
    .groups = "drop"
  )


clasificacion_anual <- infovial_clasificacion |>
  dplyr::mutate(
    anio = lubridate::year(fecha)
  ) |>
  dplyr::group_by(anio) |>
  dplyr::summarise(
    observaciones = dplyr::n(),
    dias = dplyr::n_distinct(fecha),
    ids = dplyr::n_distinct(id),
    vialidades = dplyr::n_distinct(vialidad),
    .groups = "drop"
  )


cat("\nVelocidad por año:\n\n")

print(
  velocidad_anual,
  n = Inf,
  width = Inf
)


cat("\nClasificación vehicular por año:\n\n")

print(
  clasificacion_anual,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 16. Cobertura por mes
# ------------------------------------------------------------------------------

velocidad_mensual <- infovial_velocidad |>
  dplyr::mutate(
    mes = lubridate::floor_date(
      as.Date(fecha),
      "month"
    )
  ) |>
  dplyr::group_by(mes) |>
  dplyr::summarise(
    observaciones = dplyr::n(),
    dias = dplyr::n_distinct(as.Date(fecha)),
    ids = dplyr::n_distinct(id),
    ubicaciones = dplyr::n_distinct(ubicacion),
    velocidad_media = mean(
      velocidad,
      na.rm = TRUE
    ),
    velocidad_mediana = median(
      velocidad,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


clasificacion_mensual <- infovial_clasificacion |>
  dplyr::mutate(
    mes = lubridate::floor_date(
      fecha,
      "month"
    )
  ) |>
  dplyr::group_by(mes) |>
  dplyr::summarise(
    observaciones = dplyr::n(),
    dias = dplyr::n_distinct(fecha),
    ids = dplyr::n_distinct(id),

    vehiculos = sum(
      cantidad,
      na.rm = TRUE
    ),

    .groups = "drop"
  )


cat("\nCobertura mensual velocidad:\n\n")

print(
  velocidad_mensual,
  n = Inf,
  width = Inf
)


cat("\nCobertura mensual clasificación:\n\n")

print(
  clasificacion_mensual,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 17. Tipos de vehículo
# ------------------------------------------------------------------------------

tipos_vehiculo <- infovial_clasificacion |>
  dplyr::group_by(tipo_vehiculo) |>
  dplyr::summarise(
    observaciones = dplyr::n(),

    cantidad_total = sum(
      cantidad,
      na.rm = TRUE
    ),

    .groups = "drop"
  ) |>
  dplyr::arrange(
    dplyr::desc(cantidad_total)
  )


cat("\nTipos de vehículo:\n\n")

print(
  tipos_vehiculo,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 18. Estadísticos de velocidad
# ------------------------------------------------------------------------------

resumen_velocidad <- infovial_velocidad |>
  dplyr::summarise(
    n = dplyr::n(),

    minimo = min(
      velocidad,
      na.rm = TRUE
    ),

    p01 = quantile(
      velocidad,
      0.01,
      na.rm = TRUE
    ),

    p05 = quantile(
      velocidad,
      0.05,
      na.rm = TRUE
    ),

    mediana = median(
      velocidad,
      na.rm = TRUE
    ),

    media = mean(
      velocidad,
      na.rm = TRUE
    ),

    p95 = quantile(
      velocidad,
      0.95,
      na.rm = TRUE
    ),

    p99 = quantile(
      velocidad,
      0.99,
      na.rm = TRUE
    ),

    maximo = max(
      velocidad,
      na.rm = TRUE
    )
  )


cat("\nDistribución de velocidad:\n\n")

print(
  resumen_velocidad,
  width = Inf
)


# ------------------------------------------------------------------------------
# 19. Revisar identificadores y vialidades
# ------------------------------------------------------------------------------

ids_velocidad <- infovial_velocidad |>
  dplyr::count(
    id,
    vialidad,
    sentido,
    ubicacion,
    sort = TRUE,
    name = "observaciones"
  )


ids_clasificacion <- infovial_clasificacion |>
  dplyr::count(
    id,
    vialidad,
    sentido,
    sort = TRUE,
    name = "observaciones"
  )


cat("\nEjemplos de identificadores - velocidad:\n\n")

print(
  ids_velocidad,
  n = 30,
  width = Inf
)


cat("\nEjemplos de identificadores - clasificación:\n\n")

print(
  ids_clasificacion,
  n = 30,
  width = Inf
)


# ------------------------------------------------------------------------------
# 20. Guardar resultados
# ------------------------------------------------------------------------------

readr::write_csv(
  cobertura_velocidad,
  fs::path(
    rutas$output_tables,
    "107_infovial_cobertura_velocidad.csv"
  )
)

readr::write_csv(
  cobertura_clasificacion,
  fs::path(
    rutas$output_tables,
    "107_infovial_cobertura_clasificacion.csv"
  )
)

readr::write_csv(
  velocidad_anual,
  fs::path(
    rutas$output_tables,
    "107_infovial_velocidad_anual.csv"
  )
)

readr::write_csv(
  clasificacion_anual,
  fs::path(
    rutas$output_tables,
    "107_infovial_clasificacion_anual.csv"
  )
)

readr::write_csv(
  velocidad_mensual,
  fs::path(
    rutas$output_tables,
    "107_infovial_velocidad_mensual.csv"
  )
)

readr::write_csv(
  clasificacion_mensual,
  fs::path(
    rutas$output_tables,
    "107_infovial_clasificacion_mensual.csv"
  )
)

readr::write_csv(
  tipos_vehiculo,
  fs::path(
    rutas$output_tables,
    "107_infovial_tipos_vehiculo.csv"
  )
)

readr::write_csv(
  resumen_velocidad,
  fs::path(
    rutas$output_tables,
    "107_infovial_resumen_velocidad.csv"
  )
)


message("Exploración temporal de INFOVIAL terminada.")

# ------------------------------------------------------------------------------
# 21. Revisar estructura horaria de velocidad
# ------------------------------------------------------------------------------

velocidad_horas <- infovial_velocidad |>
  dplyr::mutate(
    hora = lubridate::hour(fecha)
  ) |>
  dplyr::count(
    hora,
    name = "observaciones"
  ) |>
  dplyr::arrange(hora)


cat("\nObservaciones de velocidad por hora:\n\n")

print(
  velocidad_horas,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 22. Observaciones por ubicación - velocidad
# ------------------------------------------------------------------------------

ubicaciones_velocidad <- infovial_velocidad |>
  dplyr::count(
    vialidad,
    sentido,
    ubicacion,
    name = "observaciones"
  ) |>
  dplyr::arrange(
    dplyr::desc(observaciones)
  )


cat("\nUbicaciones de velocidad:\n\n")

print(
  ubicaciones_velocidad,
  n = 50,
  width = Inf
)


# ------------------------------------------------------------------------------
# 23. Cobertura temporal por ubicación
# ------------------------------------------------------------------------------

cobertura_ubicaciones_velocidad <- infovial_velocidad |>
  dplyr::group_by(
    vialidad,
    sentido,
    ubicacion
  ) |>
  dplyr::summarise(
    fecha_min = min(fecha),
    fecha_max = max(fecha),
    observaciones = dplyr::n(),
    dias = dplyr::n_distinct(as.Date(fecha)),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    dplyr::desc(observaciones)
  )


cat("\nCobertura por ubicación de velocidad:\n\n")

print(
  cobertura_ubicaciones_velocidad,
  n = 50,
  width = Inf
)


# ------------------------------------------------------------------------------
# 24. Calidad de velocidad
# ------------------------------------------------------------------------------

calidad_velocidad <- infovial_velocidad |>
  dplyr::summarise(
    observaciones = dplyr::n(),

    velocidad_cero = sum(
      velocidad == 0,
      na.rm = TRUE
    ),

    velocidad_negativa = sum(
      velocidad < 0,
      na.rm = TRUE
    ),

    velocidad_mayor_100 = sum(
      velocidad > 100,
      na.rm = TRUE
    ),

    velocidad_mayor_150 = sum(
      velocidad > 150,
      na.rm = TRUE
    ),

    velocidad_mayor_200 = sum(
      velocidad > 200,
      na.rm = TRUE
    ),

    porcentaje_cero = round(
      mean(velocidad == 0, na.rm = TRUE) * 100,
      2
    ),

    porcentaje_mayor_150 = round(
      mean(velocidad > 150, na.rm = TRUE) * 100,
      4
    )
  )


cat("\nCalidad de la variable velocidad:\n\n")

print(
  calidad_velocidad,
  width = Inf
)


# ------------------------------------------------------------------------------
# 25. Revisar unidad de observación en clasificación
# ------------------------------------------------------------------------------

unidad_clasificacion <- infovial_clasificacion |>
  dplyr::count(
    fecha,
    hora,
    id,
    vialidad,
    sentido,
    name = "filas"
  )


cat("\nDistribución de filas por observación base:\n\n")

print(
  unidad_clasificacion |>
    dplyr::count(
      filas,
      name = "observaciones_base"
    ) |>
    dplyr::arrange(filas),
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 26. Verificar presencia de las seis categorías
# ------------------------------------------------------------------------------

categorias_por_observacion <- infovial_clasificacion |>
  dplyr::group_by(
    fecha,
    hora,
    id,
    vialidad,
    sentido
  ) |>
  dplyr::summarise(
    categorias = dplyr::n_distinct(tipo_vehiculo),
    .groups = "drop"
  ) |>
  dplyr::count(
    categorias,
    name = "observaciones_base"
  )


cat("\nCategorías vehiculares por observación base:\n\n")

print(
  categorias_por_observacion,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 27. Relación ID - vialidad en clasificación
# ------------------------------------------------------------------------------

relacion_id_vialidad <- infovial_clasificacion |>
  dplyr::distinct(
    id,
    vialidad,
    sentido
  ) |>
  dplyr::arrange(
    id,
    vialidad,
    sentido
  )


resumen_id_clasificacion <- relacion_id_vialidad |>
  dplyr::group_by(id) |>
  dplyr::summarise(
    vialidades = dplyr::n_distinct(vialidad),
    combinaciones_vialidad_sentido = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    dplyr::desc(vialidades)
  )


cat("\nRelación ID - vialidad:\n\n")

print(
  resumen_id_clasificacion,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 28. Conteo vehicular por hora
# ------------------------------------------------------------------------------

trafico_horario <- infovial_clasificacion |>
  dplyr::mutate(
    hora_num = lubridate::hour(hora)
  ) |>
  dplyr::group_by(
    hora_num
  ) |>
  dplyr::summarise(
    vehiculos = sum(
      cantidad,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


cat("\nConteo vehicular por hora:\n\n")

print(
  trafico_horario,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 29. Guardar diagnóstico final
# ------------------------------------------------------------------------------

readr::write_csv(
  ubicaciones_velocidad,
  fs::path(
    rutas$output_tables,
    "107_infovial_ubicaciones_velocidad.csv"
  )
)

readr::write_csv(
  cobertura_ubicaciones_velocidad,
  fs::path(
    rutas$output_tables,
    "107_infovial_cobertura_ubicaciones_velocidad.csv"
  )
)

readr::write_csv(
  calidad_velocidad,
  fs::path(
    rutas$output_tables,
    "107_infovial_calidad_velocidad.csv"
  )
)

readr::write_csv(
  categorias_por_observacion,
  fs::path(
    rutas$output_tables,
    "107_infovial_categorias_por_observacion.csv"
  )
)

readr::write_csv(
  resumen_id_clasificacion,
  fs::path(
    rutas$output_tables,
    "107_infovial_resumen_id_clasificacion.csv"
  )
)

readr::write_csv(
  trafico_horario,
  fs::path(
    rutas$output_tables,
    "107_infovial_trafico_horario.csv"
  )
)


message("Diagnóstico de INFOVIAL terminado.")