# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 103_explorar_hechos_ssc.R
#
# Objetivo:
#   Explorar la estructura del conjunto ampliado de hechos de tránsito SSC:
#   - Hechos de tránsito
#   - Personas lesionadas o fallecidas por sexo
#   - Personas lesionadas o fallecidas por edad
#   - Personas lesionadas o fallecidas por tipo de persona
#   - Vehículos involucrados
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

ruta_ssc <- fs::path(
  rutas$data_raw,
  "02_hechos_transito_ssc"
)

archivos_ssc <- fs::dir_ls(
  path = ruta_ssc,
  regexp = "\\.csv$",
  type = "file"
)

cat("\nArchivos encontrados:", length(archivos_ssc), "\n\n")

print(
  fs::path_file(archivos_ssc)
)


# ------------------------------------------------------------------------------
# 2. Información física de los archivos
# ------------------------------------------------------------------------------

info_archivos <- fs::file_info(archivos_ssc) |>
  dplyr::transmute(
    archivo = fs::path_file(path),
    tamano_mb = round(as.numeric(size) / 1024^2, 3),
    fecha_modificacion = modification_time
  )

cat("\nInformación de archivos:\n\n")

print(
  info_archivos,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 3. Leer muestra de cada archivo
# ------------------------------------------------------------------------------

# Se leen hasta 2,000 filas por archivo para conocer la estructura.
# No se cargan todavía los archivos completos.

muestras_ssc <- purrr::map(
  archivos_ssc,
  function(archivo_actual) {

    readr::read_csv(
      archivo_actual,
      n_max = 2000,
      show_col_types = FALSE,
      progress = FALSE,
      name_repair = "unique"
    )
  }
)

names(muestras_ssc) <- fs::path_file(archivos_ssc)


# ------------------------------------------------------------------------------
# 4. Dimensiones de las muestras
# ------------------------------------------------------------------------------

dimensiones_muestras <- purrr::imap_dfr(
  muestras_ssc,
  function(datos, nombre_archivo) {

    tibble::tibble(
      archivo = nombre_archivo,
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
# 5. Variables por archivo
# ------------------------------------------------------------------------------

columnas_por_archivo <- purrr::imap_dfr(
  muestras_ssc,
  function(datos, nombre_archivo) {

    tibble::tibble(
      archivo = nombre_archivo,
      posicion = seq_along(names(datos)),
      variable = names(datos)
    )
  }
)

cat("\nVariables por archivo:\n\n")

print(
  columnas_por_archivo,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 6. Tipos de variables
# ------------------------------------------------------------------------------

tipos_variables <- purrr::imap_dfr(
  muestras_ssc,
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

cat("\nTipos de variables:\n\n")

print(
  tipos_variables,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 7. Buscar variables posiblemente identificadoras
# ------------------------------------------------------------------------------

patron_id <- paste(
  c(
    "id",
    "folio",
    "evento",
    "hecho",
    "expediente",
    "registro",
    "consecutivo",
    "llave"
  ),
  collapse = "|"
)

variables_id <- columnas_por_archivo |>
  dplyr::filter(
    stringr::str_detect(
      stringr::str_to_lower(variable),
      patron_id
    )
  )

cat("\nVariables candidatas a identificador o llave:\n\n")

print(
  variables_id,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 8. Identificar columnas compartidas entre archivos
# ------------------------------------------------------------------------------

todas_columnas <- sort(
  unique(
    unlist(
      purrr::map(muestras_ssc, names)
    )
  )
)

presencia_columnas <- purrr::map_dfr(
  todas_columnas,
  function(variable_actual) {

    archivos_con_variable <- names(muestras_ssc)[
      purrr::map_lgl(
        muestras_ssc,
        ~ variable_actual %in% names(.x)
      )
    ]

    tibble::tibble(
      variable = variable_actual,
      numero_archivos = length(archivos_con_variable),
      archivos = paste(
        archivos_con_variable,
        collapse = " | "
      )
    )
  }
) |>
  dplyr::arrange(
    dplyr::desc(numero_archivos),
    variable
  )

cat("\nPresencia de variables entre archivos:\n\n")

print(
  presencia_columnas,
  n = Inf,
  width = Inf
)


# Posibles llaves compartidas por al menos dos archivos

llaves_compartidas <- presencia_columnas |>
  dplyr::filter(
    numero_archivos >= 2,
    stringr::str_detect(
      stringr::str_to_lower(variable),
      patron_id
    )
  )

cat("\nPosibles llaves compartidas:\n\n")

print(
  llaves_compartidas,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 9. Clasificación preliminar de archivos según sus columnas
# ------------------------------------------------------------------------------

clasificar_archivo <- function(datos) {

  columnas <- stringr::str_to_lower(names(datos))
  texto_columnas <- paste(columnas, collapse = " ")

  dplyr::case_when(

    stringr::str_detect(
      texto_columnas,
      "vehiculo|vehículo|tipo_veh|marca|modelo"
    ) ~ "Vehículos involucrados",

    stringr::str_detect(
      texto_columnas,
      "tipo_persona|usuario_via|peaton|peatón|ciclista|motociclista"
    ) ~ "Personas por tipo de persona",

    stringr::str_detect(
      texto_columnas,
      "sexo|genero|género"
    ) ~ "Personas por sexo",

    stringr::str_detect(
      texto_columnas,
      "edad|rango_edad"
    ) ~ "Personas por edad",

    stringr::str_detect(
      texto_columnas,
      "latitud|longitud|fecha.*hecho|tipo.*hecho|lesionados|fallecidos"
    ) ~ "Hechos de tránsito",

    TRUE ~ "No identificado"
  )
}

clasificacion_archivos <- purrr::imap_dfr(
  muestras_ssc,
  function(datos, nombre_archivo) {

    tibble::tibble(
      archivo = nombre_archivo,
      clasificacion_preliminar = clasificar_archivo(datos)
    )
  }
)

cat("\nClasificación preliminar de archivos:\n\n")

print(
  clasificacion_archivos,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 10. Primeras filas de cada archivo
# ------------------------------------------------------------------------------

cat("\nPrimeras cinco filas de cada archivo:\n")

purrr::iwalk(
  muestras_ssc,
  function(datos, nombre_archivo) {

    cat("\n")
    cat("==============================================================\n")
    cat(nombre_archivo, "\n")
    cat("==============================================================\n")

    print(
      dplyr::slice_head(datos, n = 5),
      width = Inf
    )
  }
)


# ------------------------------------------------------------------------------
# 11. Buscar variables de fecha
# ------------------------------------------------------------------------------

patron_fecha <- "fecha|date|anio|año|mes"

variables_fecha <- columnas_por_archivo |>
  dplyr::filter(
    stringr::str_detect(
      stringr::str_to_lower(variable),
      patron_fecha
    )
  )

cat("\nVariables candidatas de fecha:\n\n")

print(
  variables_fecha,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 12. Valores faltantes en las muestras
# ------------------------------------------------------------------------------

faltantes_muestras <- purrr::imap_dfr(
  muestras_ssc,
  function(datos, nombre_archivo) {

    tibble::tibble(
      archivo = nombre_archivo,
      variable = names(datos),
      observaciones_muestra = nrow(datos),
      faltantes = purrr::map_int(datos, ~ sum(is.na(.x))),
      porcentaje_faltantes = round(
        faltantes / observaciones_muestra * 100,
        2
      )
    )
  }
)

cat("\nFaltantes en las muestras:\n\n")

print(
  faltantes_muestras,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 13. Cardinalidad de variables candidatas a llave
# ------------------------------------------------------------------------------

cardinalidad_ids <- purrr::imap_dfr(
  muestras_ssc,
  function(datos, nombre_archivo) {

    candidatos <- names(datos)[
      stringr::str_detect(
        stringr::str_to_lower(names(datos)),
        patron_id
      )
    ]

    if (length(candidatos) == 0) {
      return(
        tibble::tibble(
          archivo = nombre_archivo,
          variable = NA_character_,
          filas_muestra = nrow(datos),
          valores_unicos = NA_integer_,
          faltantes = NA_integer_,
          duplicados_aprox = NA_integer_
        )
      )
    }

    purrr::map_dfr(
      candidatos,
      function(variable_actual) {

        valores <- datos[[variable_actual]]

        tibble::tibble(
          archivo = nombre_archivo,
          variable = variable_actual,
          filas_muestra = nrow(datos),
          valores_unicos = dplyr::n_distinct(
            valores,
            na.rm = TRUE
          ),
          faltantes = sum(is.na(valores)),
          duplicados_aprox =
            sum(!is.na(valores)) -
            dplyr::n_distinct(valores, na.rm = TRUE)
        )
      }
    )
  }
)

cat("\nCardinalidad de posibles identificadores:\n\n")

print(
  cardinalidad_ids,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 14. Guardar resultados de la inspección
# ------------------------------------------------------------------------------

readr::write_csv(
  info_archivos,
  fs::path(
    rutas$output_tables,
    "103_ssc_info_archivos.csv"
  )
)

readr::write_csv(
  dimensiones_muestras,
  fs::path(
    rutas$output_tables,
    "103_ssc_dimensiones_muestras.csv"
  )
)

readr::write_csv(
  columnas_por_archivo,
  fs::path(
    rutas$output_tables,
    "103_ssc_columnas_por_archivo.csv"
  )
)

readr::write_csv(
  tipos_variables,
  fs::path(
    rutas$output_tables,
    "103_ssc_tipos_variables.csv"
  )
)

readr::write_csv(
  presencia_columnas,
  fs::path(
    rutas$output_tables,
    "103_ssc_presencia_columnas.csv"
  )
)

readr::write_csv(
  clasificacion_archivos,
  fs::path(
    rutas$output_tables,
    "103_ssc_clasificacion_archivos.csv"
  )
)

readr::write_csv(
  variables_fecha,
  fs::path(
    rutas$output_tables,
    "103_ssc_variables_fecha.csv"
  )
)

readr::write_csv(
  faltantes_muestras,
  fs::path(
    rutas$output_tables,
    "103_ssc_faltantes_muestras.csv"
  )
)

readr::write_csv(
  cardinalidad_ids,
  fs::path(
    rutas$output_tables,
    "103_ssc_cardinalidad_ids.csv"
  )
)




# ------------------------------------------------------------------------------
# 14. Identificar archivos principales del conjunto ampliado
# ------------------------------------------------------------------------------

nombre_hechos <- names(muestras_ssc)[
  stringr::str_detect(
    names(muestras_ssc),
    "^nuevo_acumulado_hechos_de_transito_"
  )
]

nombre_edad <- names(muestras_ssc)[
  stringr::str_detect(
    names(muestras_ssc),
    "personas_lesionadas_o_fallecidas_por_edad"
  )
]

nombre_sexo <- names(muestras_ssc)[
  stringr::str_detect(
    names(muestras_ssc),
    "personas_lesionadas_o_fallecidas_por_sexo"
  )
]

nombre_tipo_persona <- names(muestras_ssc)[
  stringr::str_detect(
    names(muestras_ssc),
    "personas_lesionadas_o_fallecidas_por_tipo_persona"
  )
]

nombre_vehiculos <- names(muestras_ssc)[
  stringr::str_detect(
    names(muestras_ssc),
    "vehiculos_incolucrados|vehiculos_involucrados"
  )
]

archivos_identificados <- tibble::tibble(
  tabla = c(
    "hechos",
    "personas_edad",
    "personas_sexo",
    "personas_tipo",
    "vehiculos"
  ),
  archivo = c(
    nombre_hechos,
    nombre_edad,
    nombre_sexo,
    nombre_tipo_persona,
    nombre_vehiculos
  )
)

cat("\nArchivos identificados para auditoría completa:\n\n")

print(
  archivos_identificados,
  n = Inf,
  width = Inf
)


# Validar que se encontró exactamente un archivo por tabla

if (any(lengths(list(
  nombre_hechos,
  nombre_edad,
  nombre_sexo,
  nombre_tipo_persona,
  nombre_vehiculos
)) != 1)) {

  stop(
    "No se pudo identificar exactamente un archivo para cada tabla. ",
    "Revisa los nombres de los archivos.",
    call. = FALSE
  )
}


# ------------------------------------------------------------------------------
# 15. Leer tablas completas
# ------------------------------------------------------------------------------

leer_ssc_completo <- function(nombre_archivo) {

  ruta_archivo <- fs::path(
    ruta_ssc,
    nombre_archivo
  )

  datos <- readr::read_csv(
    ruta_archivo,
    show_col_types = FALSE,
    progress = TRUE,
    name_repair = "unique"
  )

  problemas <- readr::problems(datos)

  if (nrow(problemas) > 0) {
    warning(
      "Se detectaron ",
      nrow(problemas),
      " problemas de parsing en: ",
      nombre_archivo
    )
  }

  list(
    datos = datos,
    problemas = problemas
  )
}


lectura_hechos <- leer_ssc_completo(nombre_hechos)
lectura_edad <- leer_ssc_completo(nombre_edad)
lectura_sexo <- leer_ssc_completo(nombre_sexo)
lectura_tipo <- leer_ssc_completo(nombre_tipo_persona)
lectura_vehiculos <- leer_ssc_completo(nombre_vehiculos)


hechos_ssc <- lectura_hechos$datos
personas_edad_ssc <- lectura_edad$datos
personas_sexo_ssc <- lectura_sexo$datos
personas_tipo_ssc <- lectura_tipo$datos
vehiculos_ssc <- lectura_vehiculos$datos


# ------------------------------------------------------------------------------
# 16. Resumen general de tablas completas
# ------------------------------------------------------------------------------

tablas_ssc <- list(
  hechos = hechos_ssc,
  personas_edad = personas_edad_ssc,
  personas_sexo = personas_sexo_ssc,
  personas_tipo = personas_tipo_ssc,
  vehiculos = vehiculos_ssc
)

resumen_tablas_completas <- purrr::imap_dfr(
  tablas_ssc,
  function(datos, nombre_tabla) {

    tibble::tibble(
      tabla = nombre_tabla,
      observaciones = nrow(datos),
      columnas = ncol(datos),
      folios_unicos = dplyr::n_distinct(
        datos$folio,
        na.rm = TRUE
      ),
      folios_faltantes = sum(is.na(datos$folio)),
      filas_duplicadas_por_folio =
        sum(!is.na(datos$folio)) -
        dplyr::n_distinct(datos$folio, na.rm = TRUE)
    )
  }
)

cat("\nResumen de tablas completas:\n\n")

print(
  resumen_tablas_completas,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 17. Cobertura temporal de cada tabla
# ------------------------------------------------------------------------------

cobertura_temporal_ssc <- purrr::imap_dfr(
  tablas_ssc,
  function(datos, nombre_tabla) {

    tibble::tibble(
      tabla = nombre_tabla,
      fecha_min = min(
        datos$fecha_evento,
        na.rm = TRUE
      ),
      fecha_max = max(
        datos$fecha_evento,
        na.rm = TRUE
      ),
      anio_min = lubridate::year(fecha_min),
      anio_max = lubridate::year(fecha_max)
    )
  }
)

cat("\nCobertura temporal por tabla:\n\n")

print(
  cobertura_temporal_ssc,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 18. Cobertura anual por tabla
# ------------------------------------------------------------------------------

cobertura_anual_ssc <- purrr::imap_dfr(
  tablas_ssc,
  function(datos, nombre_tabla) {

    datos |>
      dplyr::mutate(
        anio = lubridate::year(fecha_evento)
      ) |>
      dplyr::count(
        anio,
        name = "observaciones"
      ) |>
      dplyr::mutate(
        tabla = nombre_tabla
      ) |>
      dplyr::select(
        tabla,
        anio,
        observaciones
      )
  }
)

cat("\nCobertura anual por tabla:\n\n")

print(
  cobertura_anual_ssc,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 19. Verificar unicidad de folio en tabla principal
# ------------------------------------------------------------------------------

duplicados_folio_hechos <- hechos_ssc |>
  dplyr::filter(
    !is.na(folio)
  ) |>
  dplyr::count(
    folio,
    name = "n"
  ) |>
  dplyr::filter(
    n > 1
  ) |>
  dplyr::arrange(
    dplyr::desc(n)
  )

cat("\nFolios repetidos en la tabla principal de hechos:\n\n")

cat(
  "Número de folios repetidos:",
  nrow(duplicados_folio_hechos),
  "\n"
)

print(
  dplyr::slice_head(
    duplicados_folio_hechos,
    n = 20
  ),
  n = 20,
  width = Inf
)


# ------------------------------------------------------------------------------
# 20. Comparar folios de tablas auxiliares con tabla principal
# ------------------------------------------------------------------------------

folios_hechos <- unique(
  stats::na.omit(
    hechos_ssc$folio
  )
)

comparacion_folios <- purrr::imap_dfr(
  tablas_ssc[names(tablas_ssc) != "hechos"],
  function(datos, nombre_tabla) {

    folios_tabla <- unique(
      stats::na.omit(
        datos$folio
      )
    )

    tibble::tibble(
      tabla = nombre_tabla,
      folios_unicos_tabla = length(folios_tabla),
      folios_presentes_en_hechos = sum(
        folios_tabla %in% folios_hechos
      ),
      folios_ausentes_en_hechos = sum(
        !folios_tabla %in% folios_hechos
      ),
      porcentaje_presentes = round(
        mean(folios_tabla %in% folios_hechos) * 100,
        2
      )
    )
  }
)

cat("\nCobertura de folios auxiliares en tabla principal:\n\n")

print(
  comparacion_folios,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 21. Revisar multiplicidad por folio en tablas auxiliares
# ------------------------------------------------------------------------------

multiplicidad_folios <- purrr::imap_dfr(
  tablas_ssc[names(tablas_ssc) != "hechos"],
  function(datos, nombre_tabla) {

    resumen_folio <- datos |>
      dplyr::filter(
        !is.na(folio)
      ) |>
      dplyr::count(
        folio,
        name = "filas_por_folio"
      )

    tibble::tibble(
      tabla = nombre_tabla,
      folios = nrow(resumen_folio),
      promedio_filas_por_folio = mean(
        resumen_folio$filas_por_folio
      ),
      mediana_filas_por_folio = stats::median(
        resumen_folio$filas_por_folio
      ),
      maximo_filas_por_folio = max(
        resumen_folio$filas_por_folio
      ),
      folios_con_multiples_filas = sum(
        resumen_folio$filas_por_folio > 1
      ),
      porcentaje_folios_multiples = round(
        mean(resumen_folio$filas_por_folio > 1) * 100,
        2
      )
    )
  }
)

cat("\nMultiplicidad de filas por folio:\n\n")

print(
  multiplicidad_folios,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 22. Distribuciones básicas de variables relevantes
# ------------------------------------------------------------------------------

cat("\nTipos de evento en tabla principal:\n\n")

hechos_ssc |>
  dplyr::count(
    tipo_evento,
    sort = TRUE
  ) |>
  print(
    n = Inf,
    width = Inf
  )


cat("\nTipos de persona:\n\n")

personas_tipo_ssc |>
  dplyr::count(
    tipo_persona,
    condicion_persona,
    wt = total,
    name = "personas",
    sort = TRUE
  ) |>
  print(
    n = Inf,
    width = Inf
  )


cat("\nTipos de vehículo:\n\n")

vehiculos_ssc |>
  dplyr::count(
    tipo_vehiculo,
    sort = TRUE
  ) |>
  print(
    n = Inf,
    width = Inf
  )


# ------------------------------------------------------------------------------
# 23. Resumen de problemas de parsing
# ------------------------------------------------------------------------------

problemas_parsing <- list(
  hechos = lectura_hechos$problemas,
  personas_edad = lectura_edad$problemas,
  personas_sexo = lectura_sexo$problemas,
  personas_tipo = lectura_tipo$problemas,
  vehiculos = lectura_vehiculos$problemas
) |>
  purrr::imap_dfr(
    function(problemas, nombre_tabla) {

      if (nrow(problemas) == 0) {
        return(
          tibble::tibble(
            tabla = nombre_tabla,
            problemas = 0L
          )
        )
      }

      tibble::tibble(
        tabla = nombre_tabla,
        problemas = nrow(problemas)
      )
    }
  )

cat("\nResumen de problemas de parsing:\n\n")

print(
  problemas_parsing,
  n = Inf,
  width = Inf
)


# Guardar detalle de problemas, cuando existan

purrr::iwalk(
  list(
    hechos = lectura_hechos$problemas,
    personas_edad = lectura_edad$problemas,
    personas_sexo = lectura_sexo$problemas,
    personas_tipo = lectura_tipo$problemas,
    vehiculos = lectura_vehiculos$problemas
  ),
  function(problemas, nombre_tabla) {

    if (nrow(problemas) > 0) {

      readr::write_csv(
        problemas,
        fs::path(
          rutas$output_tables,
          paste0(
            "103_ssc_problemas_parsing_",
            nombre_tabla,
            ".csv"
          )
        )
      )
    }
  }
)


# ------------------------------------------------------------------------------
# 24. Guardar resultados de auditoría completa
# ------------------------------------------------------------------------------

readr::write_csv(
  resumen_tablas_completas,
  fs::path(
    rutas$output_tables,
    "103_ssc_resumen_tablas_completas.csv"
  )
)

readr::write_csv(
  cobertura_temporal_ssc,
  fs::path(
    rutas$output_tables,
    "103_ssc_cobertura_temporal.csv"
  )
)

readr::write_csv(
  cobertura_anual_ssc,
  fs::path(
    rutas$output_tables,
    "103_ssc_cobertura_anual.csv"
  )
)

readr::write_csv(
  duplicados_folio_hechos,
  fs::path(
    rutas$output_tables,
    "103_ssc_duplicados_folio_hechos.csv"
  )
)

readr::write_csv(
  comparacion_folios,
  fs::path(
    rutas$output_tables,
    "103_ssc_comparacion_folios.csv"
  )
)

readr::write_csv(
  multiplicidad_folios,
  fs::path(
    rutas$output_tables,
    "103_ssc_multiplicidad_folios.csv"
  )
)

readr::write_csv(
  problemas_parsing,
  fs::path(
    rutas$output_tables,
    "103_ssc_resumen_problemas_parsing.csv"
  )
)

# ------------------------------------------------------------------------------
# 25. Investigar folios repetidos reales
# ------------------------------------------------------------------------------

folios_genericos <- c(
  "SD",
  "SIN FOLIO",
  "PM",
  "",
  NA
)

duplicados_reales <- hechos_ssc |>
  dplyr::filter(
    !folio %in% folios_genericos,
    !is.na(folio)
  ) |>
  dplyr::add_count(
    folio,
    name = "n_folio"
  ) |>
  dplyr::filter(
    n_folio > 1
  ) |>
  dplyr::arrange(
    dplyr::desc(n_folio),
    folio
  )

cat("\nFolios reales repetidos:\n\n")

duplicados_reales |>
  dplyr::select(
    folio,
    fecha_evento,
    hora_evento,
    tipo_evento,
    latitud,
    longitud,
    alcaldia,
    dplyr::everything()
  ) |>
  print(
    n = 50,
    width = Inf
  )


  # ------------------------------------------------------------------------------
# 26. Evaluar llaves compuestas para identificar hechos
# ------------------------------------------------------------------------------

evaluar_llave <- function(datos, variables, nombre_tabla) {

  resumen <- datos |>
    dplyr::count(
      dplyr::across(
        dplyr::all_of(variables)
      ),
      name = "n"
    )

  tibble::tibble(
    tabla = nombre_tabla,
    llave = paste(variables, collapse = " + "),
    filas = nrow(datos),
    llaves_unicas = nrow(resumen),
    llaves_repetidas = sum(resumen$n > 1),
    filas_excedentes = sum(resumen$n - 1)
  )
}


llaves_candidatas <- list(
  c("folio"),
  c("folio", "fecha_evento"),
  c("folio", "fecha_evento", "hora_evento")
)


evaluacion_llaves_hechos <- purrr::map_dfr(
  llaves_candidatas,
  ~ evaluar_llave(
    hechos_ssc,
    .x,
    "hechos"
  )
)

cat("\nEvaluación de llaves en tabla principal:\n\n")

print(
  evaluacion_llaves_hechos,
  n = Inf,
  width = Inf
)

# ------------------------------------------------------------------------------
# 27. Inspeccionar casos no únicos con llave folio + fecha + hora
# ------------------------------------------------------------------------------

conflictos_llave <- hechos_ssc |>
  dplyr::add_count(
    folio,
    fecha_evento,
    hora_evento,
    name = "n_llave"
  ) |>
  dplyr::filter(
    n_llave > 1
  ) |>
  dplyr::arrange(
    folio,
    fecha_evento,
    hora_evento
  )

cat("\nCasos con llave folio + fecha + hora repetida:\n\n")

conflictos_llave |>
  dplyr::select(
    folio,
    fecha_evento,
    hora_evento,
    tipo_evento,
    latitud,
    longitud,
    alcaldia,
    colonia,
    personas_lesionadas,
    personas_fallecidas,
    n_llave,
    dplyr::everything()
  ) |>
  print(
    n = Inf,
    width = Inf
  )


  # ------------------------------------------------------------------------------
# 28. Evaluar llave espacial
# ------------------------------------------------------------------------------

evaluacion_llave_espacial <- evaluar_llave(
  hechos_ssc,
  c(
    "folio",
    "fecha_evento",
    "hora_evento",
    "latitud",
    "longitud"
  ),
  "hechos"
)

cat("\nEvaluación de llave espacial:\n\n")

print(
  evaluacion_llave_espacial,
  n = Inf,
  width = Inf
)

# ------------------------------------------------------------------------------
# 29. Inspeccionar único conflicto de llave espacial
# ------------------------------------------------------------------------------

conflicto_espacial <- hechos_ssc |>
  dplyr::add_count(
    folio,
    fecha_evento,
    hora_evento,
    latitud,
    longitud,
    name = "n_llave"
  ) |>
  dplyr::filter(
    n_llave > 1
  )

print(
  conflicto_espacial,
  n = Inf,
  width = Inf
)

# ¿Son filas completamente idénticas?
cat(
  "\nFilas completamente duplicadas en toda la base:",
  sum(duplicated(hechos_ssc)),
  "\n"
)

# ------------------------------------------------------------------------------
# 30. Identificar diferencias en el único conflicto espacial
# ------------------------------------------------------------------------------

conflicto_espacial <- hechos_ssc |>
  dplyr::add_count(
    folio,
    fecha_evento,
    hora_evento,
    latitud,
    longitud,
    name = "n_llave"
  ) |>
  dplyr::filter(
    n_llave > 1
  ) |>
  dplyr::select(-n_llave)


# Comparar las dos filas columna por columna
comparacion_conflicto <- tibble::tibble(
  variable = names(conflicto_espacial),
  valor_fila_1 = purrr::map_chr(
    conflicto_espacial,
    ~ as.character(.x[1])
  ),
  valor_fila_2 = purrr::map_chr(
    conflicto_espacial,
    ~ as.character(.x[2])
  )
) |>
  dplyr::filter(
    dplyr::coalesce(valor_fila_1, "<NA>") !=
      dplyr::coalesce(valor_fila_2, "<NA>")
  )

print(
  comparacion_conflicto,
  n = Inf,
  width = Inf
)