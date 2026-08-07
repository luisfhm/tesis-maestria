# ==============================================================================
# 104_explorar_infracciones.R
# Exploración inicial de infracciones generales de tránsito
# ==============================================================================

source("syntax/00_setup/000_setup.R")


# ------------------------------------------------------------------------------
# 1. Rutas
# ------------------------------------------------------------------------------

ruta_infracciones <- fs::path(
  rutas$data_raw,
  "05_infracciones",
  "generales"
)

cat("\nRuta de infracciones:\n")
print(ruta_infracciones)


# ------------------------------------------------------------------------------
# 2. Inventario de archivos
# ------------------------------------------------------------------------------

archivos_infracciones <- fs::dir_ls(
  ruta_infracciones,
  regexp = "\\.csv$",
  type = "file"
)

inventario_infracciones <- tibble::tibble(
  ruta = archivos_infracciones,
  archivo = fs::path_file(archivos_infracciones),
  tamano_mb = round(
    fs::file_size(archivos_infracciones) / 1024^2,
    2
  )
) |>
  dplyr::mutate(
    anio = stringr::str_extract(
      archivo,
      "20\\d{2}"
    ),
    bimestre = stringr::str_extract(
      archivo,
      "b[1-6]"
    )
  ) |>
  dplyr::arrange(
    anio,
    bimestre
  )

cat("\nInventario de infracciones generales:\n\n")

print(
  inventario_infracciones,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 3. Leer solamente una muestra de cada archivo
# ------------------------------------------------------------------------------

leer_muestra_infracciones <- function(ruta, n = 1000) {

  readr::read_csv(
    ruta,
    n_max = n,
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "unique"
  )
}


muestras_infracciones <- purrr::map(
  archivos_infracciones,
  leer_muestra_infracciones
)

names(muestras_infracciones) <- fs::path_file(
  archivos_infracciones
)


# ------------------------------------------------------------------------------
# 4. Dimensiones de las muestras
# ------------------------------------------------------------------------------

dimensiones_muestras <- purrr::imap_dfr(
  muestras_infracciones,
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
  dimensiones_muestras,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 5. Comparar nombres de columnas
# ------------------------------------------------------------------------------

columnas_por_archivo <- purrr::imap_dfr(
  muestras_infracciones,
  function(datos, archivo) {

    tibble::tibble(
      archivo = archivo,
      posicion = seq_along(names(datos)),
      variable = names(datos)
    )
  }
)

cat("\nNúmero de variables distintas entre todos los archivos:\n")

print(
  dplyr::n_distinct(
    columnas_por_archivo$variable
  )
)


# ------------------------------------------------------------------------------
# 6. Variables presentes en todos los archivos
# ------------------------------------------------------------------------------

n_archivos <- length(muestras_infracciones)

presencia_variables <- columnas_por_archivo |>
  dplyr::distinct(
    archivo,
    variable
  ) |>
  dplyr::count(
    variable,
    name = "archivos_presente"
  ) |>
  dplyr::mutate(
    porcentaje_archivos = round(
      archivos_presente / n_archivos * 100,
      1
    )
  ) |>
  dplyr::arrange(
    dplyr::desc(archivos_presente),
    variable
  )

cat("\nPresencia de variables entre archivos:\n\n")

print(
  presencia_variables,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 7. Detectar estructuras distintas
# ------------------------------------------------------------------------------

estructura_archivos <- purrr::imap_dfr(
  muestras_infracciones,
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
    id_estructura = dplyr::dense_rank(estructura)
  )

cat("\nEstructuras detectadas:\n\n")

estructura_archivos |>
  dplyr::select(
    archivo,
    n_variables,
    id_estructura
  ) |>
  print(
    n = Inf,
    width = Inf
  )


cat("\nNúmero de estructuras distintas:\n")

print(
  dplyr::n_distinct(
    estructura_archivos$id_estructura
  )
)


# ------------------------------------------------------------------------------
# 8. Variables potencialmente relevantes
# ------------------------------------------------------------------------------

patron_relevantes <- paste(
  c(
    "fecha",
    "folio",
    "infrac",
    "articulo",
    "motivo",
    "tipo",
    "lat",
    "lon",
    "long",
    "alcal",
    "deleg",
    "colonia",
    "calle",
    "ubic",
    "placa",
    "vehic",
    "agente",
    "policia",
    "unidad"
  ),
  collapse = "|"
)

variables_relevantes <- presencia_variables |>
  dplyr::filter(
    stringr::str_detect(
      stringr::str_to_lower(variable),
      patron_relevantes
    )
  )

cat("\nVariables potencialmente relevantes:\n\n")

print(
  variables_relevantes,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 9. Tipos de datos inferidos
# ------------------------------------------------------------------------------

tipos_variables <- purrr::imap_dfr(
  muestras_infracciones,
  function(datos, archivo) {

    tibble::tibble(
      archivo = archivo,
      variable = names(datos),
      clase = purrr::map_chr(
        datos,
        ~ paste(class(.x), collapse = "/")
      )
    )
  }
)

cat("\nTipos inferidos por archivo:\n\n")

print(
  tipos_variables,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 10. Primeras observaciones de cada archivo
# ------------------------------------------------------------------------------

cat("\nEjemplos de registros por archivo:\n")

purrr::iwalk(
  muestras_infracciones,
  function(datos, archivo) {

    cat(
      "\n\n============================================\n",
      archivo,
      "\n============================================\n"
    )

    print(
      dplyr::slice_head(
        datos,
        n = 3
      ),
      width = Inf
    )
  }
)


# ------------------------------------------------------------------------------
# 11. Guardar auditoría estructural
# ------------------------------------------------------------------------------

readr::write_csv(
  inventario_infracciones,
  fs::path(
    rutas$output_tables,
    "104_infracciones_inventario.csv"
  )
)

readr::write_csv(
  presencia_variables,
  fs::path(
    rutas$output_tables,
    "104_infracciones_presencia_variables.csv"
  )
)

readr::write_csv(
  estructura_archivos |>
    dplyr::select(
      archivo,
      n_variables,
      id_estructura
    ),
  fs::path(
    rutas$output_tables,
    "104_infracciones_estructuras.csv"
  )
)

readr::write_csv(
  tipos_variables,
  fs::path(
    rutas$output_tables,
    "104_infracciones_tipos_variables.csv"
  )
)

cat("\nExploración estructural inicial de infracciones terminada.\n")

# ------------------------------------------------------------------------------
# 12. Auditoría completa archivo por archivo
# ------------------------------------------------------------------------------

auditar_archivo_infracciones <- function(ruta) {

  archivo <- fs::path_file(ruta)

  cat("\nProcesando:", archivo, "\n")

  datos <- readr::read_csv(
    ruta,
    show_col_types = FALSE,
    progress = TRUE,
    name_repair = "unique",
    col_types = readr::cols(
      .default = readr::col_character()
    )
  )

  # Fecha
  fecha <- suppressWarnings(
    lubridate::ymd(datos$fecha_infraccion)
  )

  # Coordenadas
  latitud <- suppressWarnings(
    readr::parse_number(datos$latitud)
  )

  longitud <- suppressWarnings(
    readr::parse_number(datos$longitud)
  )

  # Código postal
  cp <- stringr::str_trim(
    datos$codigo_postal
  )

  cp[cp == ""] <- NA_character_

  # Identificador según estructura
  variable_id <- dplyr::case_when(
    "id_infraccion" %in% names(datos) ~ "id_infraccion",
    "id_folio" %in% names(datos) ~ "id_folio",
    TRUE ~ NA_character_
  )

  if (!is.na(variable_id)) {

    ids <- datos[[variable_id]]

    ids_validos <- ids[
      !is.na(ids) &
      ids != ""
    ]

    n_ids_unicos <- dplyr::n_distinct(ids_validos)

    ids_duplicados <- length(ids_validos) - n_ids_unicos

  } else {

    n_ids_unicos <- NA_integer_
    ids_duplicados <- NA_integer_

  }


  tibble::tibble(
    archivo = archivo,

    observaciones = nrow(datos),

    fecha_min = if (all(is.na(fecha))) {
      as.Date(NA)
    } else {
      min(fecha, na.rm = TRUE)
    },

    fecha_max = if (all(is.na(fecha))) {
      as.Date(NA)
    } else {
      max(fecha, na.rm = TRUE)
    },

    fechas_invalidas = sum(
      is.na(fecha) &
      !is.na(datos$fecha_infraccion) &
      datos$fecha_infraccion != ""
    ),

    variable_id = variable_id,

    ids_unicos = n_ids_unicos,

    ids_duplicados = ids_duplicados,

    codigo_postal_no_na = sum(
      !is.na(cp)
    ),

    porcentaje_cp = round(
      mean(!is.na(cp)) * 100,
      2
    ),

    latitud_no_na = sum(
      !is.na(latitud)
    ),

    longitud_no_na = sum(
      !is.na(longitud)
    ),

    coordenadas_completas = sum(
      !is.na(latitud) &
      !is.na(longitud)
    ),

    porcentaje_coordenadas = round(
      mean(
        !is.na(latitud) &
        !is.na(longitud)
      ) * 100,
      2
    ),

    alcaldia_no_na = sum(
      !is.na(datos$alcaldia) &
      datos$alcaldia != ""
    ),

    porcentaje_alcaldia = round(
      mean(
        !is.na(datos$alcaldia) &
        datos$alcaldia != ""
      ) * 100,
      2
    ),

    colonia_no_na = sum(
      !is.na(datos$colonia) &
      datos$colonia != ""
    ),

    porcentaje_colonia = round(
      mean(
        !is.na(datos$colonia) &
        datos$colonia != ""
      ) * 100,
      2
    ),

    articulo_no_na = sum(
      !is.na(datos$articulo) &
      datos$articulo != ""
    ),

    porcentaje_articulo = round(
      mean(
        !is.na(datos$articulo) &
        datos$articulo != ""
      ) * 100,
      2
    )
  )
}


auditoria_completa_infracciones <- purrr::map_dfr(
  archivos_infracciones,
  auditar_archivo_infracciones
)


cat("\nAuditoría completa de infracciones:\n\n")

print(
  auditoria_completa_infracciones,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 13. Agregar año y bimestre
# ------------------------------------------------------------------------------

auditoria_completa_infracciones <- auditoria_completa_infracciones |>
  dplyr::mutate(
    anio_archivo = stringr::str_extract(
      archivo,
      "20\\d{2}"
    ) |>
      as.integer(),

    bimestre_archivo = stringr::str_extract(
      archivo,
      "b[1-6]"
    )
  ) |>
  dplyr::arrange(
    anio_archivo,
    bimestre_archivo
  )


# ------------------------------------------------------------------------------
# 14. Resumen por año
# ------------------------------------------------------------------------------

resumen_anual_infracciones <- auditoria_completa_infracciones |>
  dplyr::group_by(
    anio_archivo
  ) |>
  dplyr::summarise(
    archivos = dplyr::n(),

    observaciones = sum(observaciones),

    fecha_min = min(
      fecha_min,
      na.rm = TRUE
    ),

    fecha_max = max(
      fecha_max,
      na.rm = TRUE
    ),

    porcentaje_cp = round(
      sum(codigo_postal_no_na) /
        sum(observaciones) * 100,
      2
    ),

    porcentaje_coordenadas = round(
      sum(coordenadas_completas) /
        sum(observaciones) * 100,
      2
    ),

    porcentaje_alcaldia = round(
      sum(alcaldia_no_na) /
        sum(observaciones) * 100,
      2
    ),

    .groups = "drop"
  )


cat("\nResumen anual de infracciones:\n\n")

print(
  resumen_anual_infracciones,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 15. Guardar resultados
# ------------------------------------------------------------------------------

readr::write_csv(
  auditoria_completa_infracciones,
  fs::path(
    rutas$output_tables,
    "104_infracciones_auditoria_completa.csv"
  )
)

readr::write_csv(
  resumen_anual_infracciones,
  fs::path(
    rutas$output_tables,
    "104_infracciones_resumen_anual.csv"
  )
)

cat("\nAuditoría completa terminada.\n")

# ------------------------------------------------------------------------------
# 16. Cobertura mensual exacta
# ------------------------------------------------------------------------------

auditar_meses_infracciones <- function(ruta) {

  archivo <- fs::path_file(ruta)

  cat("\nProcesando cobertura mensual:", archivo, "\n")

  datos <- readr::read_csv(
    ruta,
    col_select = fecha_infraccion,
    col_types = readr::cols(
      fecha_infraccion = readr::col_character()
    ),
    show_col_types = FALSE,
    progress = FALSE
  )

  datos |>
    dplyr::mutate(
      fecha = suppressWarnings(
        lubridate::ymd(fecha_infraccion)
      ),
      mes = lubridate::floor_date(
        fecha,
        unit = "month"
      )
    ) |>
    dplyr::filter(
      !is.na(mes)
    ) |>
    dplyr::count(
      mes,
      name = "infracciones"
    ) |>
    dplyr::mutate(
      archivo = archivo,
      .before = 1
    )
}


cobertura_mensual_archivos <- purrr::map_dfr(
  archivos_infracciones,
  auditar_meses_infracciones
)


cat("\nCobertura mensual por archivo:\n\n")

print(
  cobertura_mensual_archivos,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 17. Cobertura mensual total
# ------------------------------------------------------------------------------

cobertura_mensual_total <- cobertura_mensual_archivos |>
  dplyr::group_by(
    mes
  ) |>
  dplyr::summarise(
    infracciones = sum(infracciones),
    archivos = dplyr::n_distinct(archivo),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    mes
  )


cat("\nCobertura mensual total:\n\n")

print(
  cobertura_mensual_total,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 18. Detectar meses faltantes
# ------------------------------------------------------------------------------

rango_meses <- tibble::tibble(
  mes = seq.Date(
    min(cobertura_mensual_total$mes),
    max(cobertura_mensual_total$mes),
    by = "month"
  )
)


meses_faltantes <- rango_meses |>
  dplyr::anti_join(
    cobertura_mensual_total,
    by = "mes"
  )


cat("\nMeses completamente ausentes:\n\n")

print(
  meses_faltantes,
  n = Inf
)


# ------------------------------------------------------------------------------
# 19. Detectar observaciones fuera del periodo esperado del archivo
# ------------------------------------------------------------------------------

extraer_periodo_archivo <- function(archivo) {

  anio <- as.integer(
    stringr::str_extract(
      archivo,
      "20\\d{2}"
    )
  )

  bimestre <- as.integer(
    stringr::str_extract(
      archivo,
      "(?<=_b)[1-6]"
    )
  )

  mes_inicio <- (bimestre - 1) * 2 + 1

  fecha_inicio <- as.Date(
    sprintf(
      "%04d-%02d-01",
      anio,
      mes_inicio
    )
  )

  fecha_fin <- fecha_inicio %m+%
    lubridate::months(2) -
    lubridate::days(1)

  list(
    fecha_inicio = fecha_inicio,
    fecha_fin = fecha_fin
  )
}


auditar_fechas_fuera_periodo <- function(ruta) {

  archivo <- fs::path_file(ruta)

  periodo <- extraer_periodo_archivo(
    archivo
  )

  datos <- readr::read_csv(
    ruta,
    col_select = fecha_infraccion,
    col_types = readr::cols(
      fecha_infraccion = readr::col_character()
    ),
    show_col_types = FALSE,
    progress = FALSE
  ) |>
    dplyr::mutate(
      fecha = suppressWarnings(
        lubridate::ymd(fecha_infraccion)
      )
    )

  fuera <- datos |>
    dplyr::filter(
      !is.na(fecha),
      fecha < periodo$fecha_inicio |
        fecha > periodo$fecha_fin
    )

  tibble::tibble(
    archivo = archivo,

    fecha_inicio_esperada =
      periodo$fecha_inicio,

    fecha_fin_esperada =
      periodo$fecha_fin,

    observaciones_fuera =
      nrow(fuera),

    fecha_min_fuera =
      if (nrow(fuera) == 0) {
        as.Date(NA)
      } else {
        min(fuera$fecha)
      },

    fecha_max_fuera =
      if (nrow(fuera) == 0) {
        as.Date(NA)
      } else {
        max(fuera$fecha)
      }
  )
}


# Ejecutar auditoría
fechas_fuera_periodo <- purrr::map_dfr(
  archivos_infracciones,
  auditar_fechas_fuera_periodo
)


cat("\nObservaciones fuera del periodo esperado:\n\n")

print(
  fechas_fuera_periodo |>
    dplyr::filter(
      observaciones_fuera > 0
    ),
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 20. Guardar resultados
# ------------------------------------------------------------------------------

readr::write_csv(
  cobertura_mensual_total,
  fs::path(
    rutas$output_tables,
    "104_infracciones_cobertura_mensual.csv"
  )
)

readr::write_csv(
  fechas_fuera_periodo,
  fs::path(
    rutas$output_tables,
    "104_infracciones_fechas_fuera_periodo.csv"
  )
)

message("Auditoría temporal de infracciones terminada.")

# ------------------------------------------------------------------------------
# 21. Investigar registros de enero 2023 presentes en más de un archivo
# ------------------------------------------------------------------------------

archivos_enero_2023 <- cobertura_mensual_archivos |>
  dplyr::filter(
    mes == as.Date("2023-01-01")
  ) |>
  dplyr::select(
    archivo,
    infracciones
  )

cat("\nArchivos que contienen observaciones de enero 2023:\n\n")

print(
  archivos_enero_2023,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 22. Extraer registros de enero 2023
# ------------------------------------------------------------------------------

extraer_enero_2023 <- function(ruta) {

  archivo <- fs::path_file(ruta)

  if (!archivo %in% archivos_enero_2023$archivo) {
    return(NULL)
  }

  datos <- readr::read_csv(
    ruta,
    show_col_types = FALSE,
    progress = FALSE,
    col_types = readr::cols(
      .default = readr::col_character()
    )
  ) |>
    dplyr::mutate(
      fecha = suppressWarnings(
        lubridate::ymd(fecha_infraccion)
      )
    ) |>
    dplyr::filter(
      fecha >= as.Date("2023-01-01"),
      fecha < as.Date("2023-02-01")
    ) |>
    dplyr::mutate(
      archivo_origen = archivo,
      .before = 1
    )

  datos
}


registros_enero_2023 <- purrr::map_dfr(
  archivos_infracciones,
  extraer_enero_2023
)


# ------------------------------------------------------------------------------
# 23. Resumen por archivo
# ------------------------------------------------------------------------------

cat("\nRegistros de enero 2023 por archivo:\n\n")

registros_enero_2023 |>
  dplyr::count(
    archivo_origen,
    fecha,
    name = "infracciones"
  ) |>
  dplyr::arrange(
    archivo_origen,
    fecha
  ) |>
  print(
    n = Inf,
    width = Inf
  )

  # ------------------------------------------------------------------------------
# 24. Revisar si los 56 registros de 2022_b1 están duplicados en 2023_b1
# ------------------------------------------------------------------------------

archivo_2022_b1 <- archivos_infracciones[
  stringr::str_detect(
    fs::path_file(archivos_infracciones),
    "2022_b1"
  )
]

archivo_2023_b1 <- archivos_infracciones[
  stringr::str_detect(
    fs::path_file(archivos_infracciones),
    "2023_b1"
  )
]


datos_2022_b1 <- readr::read_csv(
  archivo_2022_b1,
  show_col_types = FALSE,
  progress = FALSE,
  col_types = readr::cols(
    .default = readr::col_character()
  )
) |>
  dplyr::mutate(
    fecha = suppressWarnings(
      lubridate::ymd(fecha_infraccion)
    )
  ) |>
  dplyr::filter(
    fecha == as.Date("2023-01-01")
  )


datos_2023_b1 <- readr::read_csv(
  archivo_2023_b1,
  show_col_types = FALSE,
  progress = FALSE,
  col_types = readr::cols(
    .default = readr::col_character()
  )
) |>
  dplyr::mutate(
    fecha = suppressWarnings(
      lubridate::ymd(fecha_infraccion)
    )
  ) |>
  dplyr::filter(
    fecha == as.Date("2023-01-01")
  )


# Identificador común
id_2022 <- if ("id_infraccion" %in% names(datos_2022_b1)) {
  datos_2022_b1$id_infraccion
} else {
  datos_2022_b1$id_folio
}

id_2023 <- if ("id_infraccion" %in% names(datos_2023_b1)) {
  datos_2023_b1$id_infraccion
} else {
  datos_2023_b1$id_folio
}


cat(
  "\nRegistros 2023-01-01 en 2022_b1:",
  length(id_2022),
  "\n"
)

cat(
  "Registros 2023-01-01 en 2023_b1:",
  length(id_2023),
  "\n"
)

cat(
  "IDs del 2022_b1 presentes también en 2023_b1:",
  sum(id_2022 %in% id_2023, na.rm = TRUE),
  "\n"
)

# ------------------------------------------------------------------------------
# 25. Explorar clasificación de las infracciones
# ------------------------------------------------------------------------------

extraer_clasificacion_infracciones <- function(ruta) {

  archivo <- fs::path_file(ruta)

  cat("\nProcesando clasificación:", archivo, "\n")

  datos <- readr::read_csv(
    ruta,
    show_col_types = FALSE,
    progress = FALSE,
    col_types = readr::cols(
      .default = readr::col_character()
    )
  )

  tibble::tibble(
    archivo = archivo,

    fecha = suppressWarnings(
      lubridate::ymd(datos$fecha_infraccion)
    ),

    articulo = datos$articulo,

    fraccion = datos$fraccion,

    inciso = datos$inciso,

    parrafo = datos$parrafo,

    motivacion = if ("motivacion" %in% names(datos)) {
      datos$motivacion
    } else {
      NA_character_
    },

    categoria = if ("categoria" %in% names(datos)) {
      datos$categoria
    } else {
      NA_character_
    }
  )
}


clasificacion_infracciones <- purrr::map_dfr(
  archivos_infracciones,
  extraer_clasificacion_infracciones
)


# ------------------------------------------------------------------------------
# 26. Disponibilidad de variables de clasificación
# ------------------------------------------------------------------------------

disponibilidad_clasificacion <- clasificacion_infracciones |>
  dplyr::mutate(
    mes = lubridate::floor_date(
      fecha,
      "month"
    )
  ) |>
  dplyr::group_by(
    mes
  ) |>
  dplyr::summarise(
    infracciones = dplyr::n(),

    pct_articulo = round(
      mean(!is.na(articulo) & articulo != "") * 100,
      2
    ),

    pct_fraccion = round(
      mean(!is.na(fraccion) & fraccion != "") * 100,
      2
    ),

    pct_inciso = round(
      mean(!is.na(inciso) & inciso != "") * 100,
      2
    ),

    pct_parrafo = round(
      mean(!is.na(parrafo) & parrafo != "") * 100,
      2
    ),

    pct_motivacion = round(
      mean(!is.na(motivacion) & motivacion != "") * 100,
      2
    ),

    pct_categoria = round(
      mean(!is.na(categoria) & categoria != "") * 100,
      2
    ),

    .groups = "drop"
  )


cat("\nDisponibilidad mensual de clasificación:\n\n")

print(
  disponibilidad_clasificacion,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 27. Artículos más frecuentes
# ------------------------------------------------------------------------------

articulos_frecuentes <- clasificacion_infracciones |>
  dplyr::filter(
    !is.na(articulo),
    articulo != ""
  ) |>
  dplyr::count(
    articulo,
    sort = TRUE,
    name = "infracciones"
  ) |>
  dplyr::mutate(
    porcentaje = round(
      infracciones / sum(infracciones) * 100,
      2
    ),
    porcentaje_acumulado = round(
      cumsum(infracciones) / sum(infracciones) * 100,
      2
    )
  )


cat("\nArtículos más frecuentes:\n\n")

print(
  articulos_frecuentes,
  n = 30,
  width = Inf
)


# ------------------------------------------------------------------------------
# 28. Motivaciones más frecuentes — esquema antiguo
# ------------------------------------------------------------------------------

motivaciones_frecuentes <- clasificacion_infracciones |>
  dplyr::filter(
    !is.na(motivacion),
    motivacion != ""
  ) |>
  dplyr::count(
    motivacion,
    sort = TRUE,
    name = "infracciones"
  ) |>
  dplyr::mutate(
    porcentaje = round(
      infracciones / sum(infracciones) * 100,
      2
    )
  )


cat("\nMotivaciones más frecuentes:\n\n")

print(
  motivaciones_frecuentes,
  n = 30,
  width = Inf
)


# ------------------------------------------------------------------------------
# 29. Categorías más frecuentes — esquema nuevo
# ------------------------------------------------------------------------------

categorias_frecuentes <- clasificacion_infracciones |>
  dplyr::filter(
    !is.na(categoria),
    categoria != ""
  ) |>
  dplyr::count(
    categoria,
    sort = TRUE,
    name = "infracciones"
  ) |>
  dplyr::mutate(
    porcentaje = round(
      infracciones / sum(infracciones) * 100,
      2
    )
  )


cat("\nCategorías más frecuentes:\n\n")

print(
  categorias_frecuentes,
  n = 30,
  width = Inf
)


# ------------------------------------------------------------------------------
# 30. Combinaciones artículo + fracción más frecuentes
# ------------------------------------------------------------------------------

articulo_fraccion <- clasificacion_infracciones |>
  dplyr::filter(
    !is.na(articulo),
    articulo != ""
  ) |>
  dplyr::count(
    articulo,
    fraccion,
    sort = TRUE,
    name = "infracciones"
  ) |>
  dplyr::mutate(
    porcentaje = round(
      infracciones / sum(infracciones) * 100,
      2
    )
  )


cat("\nCombinaciones artículo + fracción más frecuentes:\n\n")

print(
  articulo_fraccion,
  n = 50,
  width = Inf
)


# ------------------------------------------------------------------------------
# 31. Guardar resultados
# ------------------------------------------------------------------------------

readr::write_csv(
  disponibilidad_clasificacion,
  fs::path(
    rutas$output_tables,
    "104_infracciones_disponibilidad_clasificacion.csv"
  )
)

readr::write_csv(
  articulos_frecuentes,
  fs::path(
    rutas$output_tables,
    "104_infracciones_articulos.csv"
  )
)

readr::write_csv(
  motivaciones_frecuentes,
  fs::path(
    rutas$output_tables,
    "104_infracciones_motivaciones.csv"
  )
)

readr::write_csv(
  categorias_frecuentes,
  fs::path(
    rutas$output_tables,
    "104_infracciones_categorias.csv"
  )
)

readr::write_csv(
  articulo_fraccion,
  fs::path(
    rutas$output_tables,
    "104_infracciones_articulo_fraccion.csv"
  )
)

# ------------------------------------------------------------------------------
# 32. Construir clasificación preliminar de infracciones
# ------------------------------------------------------------------------------

infracciones_tipo_mes <- clasificacion_infracciones |>
  dplyr::filter(
    !is.na(fecha)
  ) |>
  dplyr::mutate(
    mes = lubridate::floor_date(
      fecha,
      unit = "month"
    ),

    tipo_infraccion = dplyr::case_when(

      articulo == "9" ~
        "Velocidad",

      TRUE ~
        "Otras infracciones"
    )
  ) |>
  dplyr::count(
    mes,
    tipo_infraccion,
    name = "infracciones"
  )


# ------------------------------------------------------------------------------
# 33. Totales mensuales por tipo
# ------------------------------------------------------------------------------

cat("\nInfracciones mensuales por tipo:\n\n")

print(
  infracciones_tipo_mes,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 34. Participación de velocidad
# ------------------------------------------------------------------------------

participacion_velocidad <- infracciones_tipo_mes |>
  tidyr::pivot_wider(
    names_from = tipo_infraccion,
    values_from = infracciones,
    values_fill = 0
  ) |>
  dplyr::mutate(
    total = Velocidad + `Otras infracciones`,

    pct_velocidad = round(
      Velocidad / total * 100,
      2
    )
  ) |>
  dplyr::arrange(
    mes
  )


cat("\nParticipación mensual de infracciones de velocidad:\n\n")

print(
  participacion_velocidad,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 35. Gráfica: infracciones mensuales
# ------------------------------------------------------------------------------

grafica_infracciones_tipo <- ggplot2::ggplot(
  infracciones_tipo_mes,
  ggplot2::aes(
    x = mes,
    y = infracciones,
    linetype = tipo_infraccion
  )
) +
  ggplot2::geom_line(
    linewidth = 0.9
  ) +
  ggplot2::geom_vline(
    xintercept = as.Date("2022-04-01"),
    linetype = "dashed"
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::label_number(
      big.mark = ","
    )
  ) +
  ggplot2::labs(
    title = "Infracciones de tránsito en la Ciudad de México",
    subtitle = "Línea vertical: abril de 2022",
    x = NULL,
    y = "Número de infracciones",
    linetype = "Tipo de infracción"
  ) +
  ggplot2::theme_minimal()


print(
  grafica_infracciones_tipo
)


# ------------------------------------------------------------------------------
# 36. Gráfica: porcentaje correspondiente a velocidad
# ------------------------------------------------------------------------------

grafica_pct_velocidad <- ggplot2::ggplot(
  participacion_velocidad,
  ggplot2::aes(
    x = mes,
    y = pct_velocidad
  )
) +
  ggplot2::geom_line(
    linewidth = 0.9
  ) +
  ggplot2::geom_vline(
    xintercept = as.Date("2022-04-01"),
    linetype = "dashed"
  ) +
  ggplot2::labs(
    title = "Participación de infracciones por exceso de velocidad",
    subtitle = "Línea vertical: abril de 2022",
    x = NULL,
    y = "% de infracciones"
  ) +
  ggplot2::theme_minimal()


print(
  grafica_pct_velocidad
)


# ------------------------------------------------------------------------------
# 37. Guardar resultados
# ------------------------------------------------------------------------------

readr::write_csv(
  infracciones_tipo_mes,
  fs::path(
    rutas$output_tables,
    "104_infracciones_tipo_mes.csv"
  )
)

readr::write_csv(
  participacion_velocidad,
  fs::path(
    rutas$output_tables,
    "104_infracciones_participacion_velocidad.csv"
  )
)

ggplot2::ggsave(
  fs::path(
    rutas$output_figures,
    "104_infracciones_tipo_mes.png"
  ),
  grafica_infracciones_tipo,
  width = 10,
  height = 6,
  dpi = 300
)

ggplot2::ggsave(
  fs::path(
    rutas$output_figures,
    "104_infracciones_pct_velocidad.png"
  ),
  grafica_pct_velocidad,
  width = 10,
  height = 6,
  dpi = 300
)