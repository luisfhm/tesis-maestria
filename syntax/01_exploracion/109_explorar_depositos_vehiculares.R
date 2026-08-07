# ==============================================================================
# 109_explorar_depositos_vehiculares.R
# Exploración inicial de depósitos vehiculares
# ==============================================================================

source("syntax/00_setup/000_setup.R")


# ------------------------------------------------------------------------------
# 1. Ruta
# ------------------------------------------------------------------------------

ruta_depositos <- fs::path(
  rutas$data_raw,
  "11_depositos_vehiculares"
)

archivos_depositos <- fs::dir_ls(
  ruta_depositos,
  regexp = "\\.csv$",
  type = "file"
)


cat("\nArchivos encontrados:\n\n")

print(
  fs::path_file(archivos_depositos)
)


# ------------------------------------------------------------------------------
# 2. Inventario
# ------------------------------------------------------------------------------

inventario_depositos <- tibble::tibble(
  ruta = archivos_depositos,
  archivo = fs::path_file(archivos_depositos),

  tamano_mb = round(
    as.numeric(
      fs::file_size(archivos_depositos)
    ) / 1024^2,
    3
  )
)


cat("\nInventario:\n\n")

print(
  inventario_depositos,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 3. Leer muestra inicial
# ------------------------------------------------------------------------------

muestra_depositos <- readr::read_csv(
  archivos_depositos[1],
  n_max = 2000,
  show_col_types = FALSE,
  progress = FALSE,
  name_repair = "unique"
)


cat("\nDimensiones de la muestra:\n\n")

cat(
  "Filas:",
  nrow(muestra_depositos),
  "\n"
)

cat(
  "Columnas:",
  ncol(muestra_depositos),
  "\n"
)


# ------------------------------------------------------------------------------
# 4. Variables
# ------------------------------------------------------------------------------

cat("\nVariables disponibles:\n\n")

print(
  names(muestra_depositos)
)


# ------------------------------------------------------------------------------
# 5. Tipos de variables
# ------------------------------------------------------------------------------

tipos_depositos <- tibble::tibble(
  variable = names(muestra_depositos),

  clase = purrr::map_chr(
    muestra_depositos,
    ~ paste(
      class(.x),
      collapse = "/"
    )
  )
)


cat("\nTipos de variables:\n\n")

print(
  tipos_depositos,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 6. Primeras observaciones
# ------------------------------------------------------------------------------

cat("\nPrimeras observaciones:\n\n")

print(
  muestra_depositos |>
    dplyr::slice_head(
      n = 20
    ),
  width = Inf
)


# ------------------------------------------------------------------------------
# 7. Buscar variables potencialmente relevantes
# ------------------------------------------------------------------------------

patron_relevante <- paste(
  c(
    "fecha",
    "hora",
    "folio",
    "id",
    "placa",
    "vehic",
    "marca",
    "modelo",
    "tipo",
    "motivo",
    "causa",
    "infrac",
    "articulo",
    "deposit",
    "corral",
    "alcal",
    "deleg",
    "colonia",
    "calle",
    "ubic",
    "lat",
    "lon",
    "arrastre",
    "remision",
    "remisión"
  ),
  collapse = "|"
)


variables_relevantes_depositos <- tibble::tibble(
  variable = names(muestra_depositos)
) |>
  dplyr::filter(
    stringr::str_detect(
      stringr::str_to_lower(variable),
      patron_relevante
    )
  )


cat("\nVariables potencialmente relevantes:\n\n")

print(
  variables_relevantes_depositos,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 8. Cardinalidad y faltantes
# ------------------------------------------------------------------------------

calidad_muestra_depositos <- tibble::tibble(
  variable = names(muestra_depositos),

  valores_unicos = purrr::map_int(
    muestra_depositos,
    ~ dplyr::n_distinct(
      .x,
      na.rm = TRUE
    )
  ),

  faltantes = purrr::map_int(
    muestra_depositos,
    ~ sum(
      is.na(.x)
    )
  ),

  porcentaje_faltantes = round(
    faltantes /
      nrow(muestra_depositos) *
      100,
    2
  )
)


cat("\nCardinalidad y faltantes en la muestra:\n\n")

print(
  calidad_muestra_depositos,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 9. Buscar posibles variables temporales
# ------------------------------------------------------------------------------

variables_temporales_depositos <- names(
  muestra_depositos
)[
  stringr::str_detect(
    stringr::str_to_lower(
      names(muestra_depositos)
    ),
    "fecha|hora|anio|año|mes|dia|día"
  )
]


cat("\nVariables temporales candidatas:\n\n")

print(
  variables_temporales_depositos
)


# ------------------------------------------------------------------------------
# 10. Buscar posibles variables geográficas
# ------------------------------------------------------------------------------

variables_geo_depositos <- names(
  muestra_depositos
)[
  stringr::str_detect(
    stringr::str_to_lower(
      names(muestra_depositos)
    ),
    "alcal|deleg|colonia|calle|ubic|lat|lon|coord|deposit"
  )
]


cat("\nVariables geográficas candidatas:\n\n")

print(
  variables_geo_depositos
)


# ------------------------------------------------------------------------------
# 11. Guardar auditoría inicial
# ------------------------------------------------------------------------------

readr::write_csv(
  inventario_depositos,
  fs::path(
    rutas$output_tables,
    "109_depositos_inventario.csv"
  )
)

readr::write_csv(
  tipos_depositos,
  fs::path(
    rutas$output_tables,
    "109_depositos_tipos_variables.csv"
  )
)

readr::write_csv(
  calidad_muestra_depositos,
  fs::path(
    rutas$output_tables,
    "109_depositos_calidad_muestra.csv"
  )
)


message("Exploración estructural inicial de depósitos terminada.")

# ------------------------------------------------------------------------------
# 12. Leer base completa
# ------------------------------------------------------------------------------

depositos <- readr::read_csv(
  archivos_depositos[1],
  show_col_types = FALSE,
  progress = TRUE,
  name_repair = "unique"
)


cat("\nDimensiones completas:\n\n")

cat(
  "Filas:",
  format(nrow(depositos), big.mark = ","),
  "\n"
)

cat(
  "Columnas:",
  ncol(depositos),
  "\n"
)


# ------------------------------------------------------------------------------
# 13. Unidad de observación
# ------------------------------------------------------------------------------

resumen_unidad <- depositos |>
  dplyr::summarise(
    filas = dplyr::n(),

    placas_unicas = dplyr::n_distinct(
      placa,
      na.rm = TRUE
    ),

    fechas_busqueda = dplyr::n_distinct(
      fecha_busqueda,
      na.rm = TRUE
    ),

    combinaciones_placa_fecha = dplyr::n_distinct(
      paste(
        placa,
        fecha_busqueda,
        sep = "_"
      )
    ),

    fecha_busqueda_min = min(
      fecha_busqueda,
      na.rm = TRUE
    ),

    fecha_busqueda_max = max(
      fecha_busqueda,
      na.rm = TRUE
    )
  )


cat("\nUnidad de observación:\n\n")

print(
  resumen_unidad,
  width = Inf
)


# ------------------------------------------------------------------------------
# 14. Número de consultas por placa
# ------------------------------------------------------------------------------

consultas_por_placa <- depositos |>
  dplyr::count(
    placa,
    name = "consultas"
  ) |>
  dplyr::count(
    consultas,
    name = "placas"
  ) |>
  dplyr::arrange(
    consultas
  )


cat("\nNúmero de consultas por placa:\n\n")

print(
  consultas_por_placa,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 15. Consultas por fecha
# ------------------------------------------------------------------------------

busquedas_fecha <- depositos |>
  dplyr::count(
    fecha_busqueda,
    name = "consultas"
  ) |>
  dplyr::arrange(
    fecha_busqueda
  )


cat("\nConsultas por fecha:\n\n")

print(
  busquedas_fecha,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 16. Placas CDMX
# ------------------------------------------------------------------------------

resumen_placa_cdmx <- depositos |>
  dplyr::count(
    placa_cdmx,
    name = "vehiculos"
  ) |>
  dplyr::mutate(
    porcentaje = round(
      vehiculos / sum(vehiculos) * 100,
      2
    )
  )


cat("\nPlacas CDMX:\n\n")

print(
  resumen_placa_cdmx,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 17. Estatus del padrón
# ------------------------------------------------------------------------------

resumen_padron <- depositos |>
  dplyr::count(
    placa_cdmx,
    estatus_padron,
    name = "vehiculos"
  ) |>
  dplyr::arrange(
    placa_cdmx,
    dplyr::desc(vehiculos)
  )


cat("\nEstatus del padrón:\n\n")

print(
  resumen_padron,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 18. Relación entre padrón y disponibilidad de datos vehiculares
# ------------------------------------------------------------------------------

disponibilidad_padron <- depositos |>
  dplyr::mutate(
    datos_vehiculo_disponibles =
      !is.na(marca) |
      !is.na(linea) |
      !is.na(modelo) |
      !is.na(fecha_alta)
  ) |>
  dplyr::count(
    placa_cdmx,
    estatus_padron,
    datos_vehiculo_disponibles,
    name = "vehiculos"
  ) |>
  dplyr::arrange(
    placa_cdmx,
    estatus_padron,
    datos_vehiculo_disponibles
  )


cat("\nDisponibilidad de datos del vehículo:\n\n")

print(
  disponibilidad_padron,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 19. Estatus administrativos
# ------------------------------------------------------------------------------

estatus_tarjeta <- depositos |>
  dplyr::count(
    estatus_tarjeta_circulacion,
    name = "vehiculos",
    sort = TRUE
  )


estatus_seguro <- depositos |>
  dplyr::count(
    estatus_seguro,
    name = "vehiculos",
    sort = TRUE
  )


estatus_infracciones_tab <- depositos |>
  dplyr::count(
    estatus_infracciones,
    name = "vehiculos",
    sort = TRUE
  )


estatus_tenencia <- depositos |>
  dplyr::count(
    adeudos_tenencia,
    name = "vehiculos",
    sort = TRUE
  )


cat("\nEstatus tarjeta de circulación:\n\n")
print(estatus_tarjeta, n = Inf, width = Inf)

cat("\nEstatus seguro:\n\n")
print(estatus_seguro, n = Inf, width = Inf)

cat("\nEstatus infracciones:\n\n")
print(estatus_infracciones_tab, n = Inf, width = Inf)

cat("\nAdeudos de tenencia:\n\n")
print(estatus_tenencia, n = Inf, width = Inf)


# ------------------------------------------------------------------------------
# 20. Distribución de infracciones y sanciones
# ------------------------------------------------------------------------------

distribucion_infracciones <- depositos |>
  dplyr::count(
    infracciones,
    name = "vehiculos"
  ) |>
  dplyr::arrange(
    infracciones
  )


distribucion_sanciones <- depositos |>
  dplyr::count(
    sanciones,
    name = "vehiculos"
  ) |>
  dplyr::arrange(
    sanciones
  )


cat("\nNúmero de infracciones:\n\n")

print(
  distribucion_infracciones,
  n = Inf,
  width = Inf
)


cat("\nNúmero de sanciones:\n\n")

print(
  distribucion_sanciones,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 21. Tipos de vehículo
# ------------------------------------------------------------------------------

tipos_vehiculo_depositos <- depositos |>
  dplyr::filter(
    !is.na(tipo_vehiculo)
  ) |>
  dplyr::count(
    clase_vehiculo,
    tipo_vehiculo,
    name = "vehiculos",
    sort = TRUE
  )


cat("\nTipos de vehículo:\n\n")

print(
  tipos_vehiculo_depositos,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 22. Modelos
# ------------------------------------------------------------------------------

modelos_vehiculos <- depositos |>
  dplyr::filter(
    !is.na(modelo)
  ) |>
  dplyr::count(
    modelo,
    name = "vehiculos"
  ) |>
  dplyr::arrange(
    modelo
  )


cat("\nVehículos por modelo:\n\n")

print(
  modelos_vehiculos,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 23. Guardar resultados
# ------------------------------------------------------------------------------

readr::write_csv(
  resumen_unidad,
  fs::path(
    rutas$output_tables,
    "109_depositos_resumen_unidad.csv"
  )
)

readr::write_csv(
  busquedas_fecha,
  fs::path(
    rutas$output_tables,
    "109_depositos_busquedas_fecha.csv"
  )
)

readr::write_csv(
  resumen_padron,
  fs::path(
    rutas$output_tables,
    "109_depositos_resumen_padron.csv"
  )
)

readr::write_csv(
  disponibilidad_padron,
  fs::path(
    rutas$output_tables,
    "109_depositos_disponibilidad_padron.csv"
  )
)

readr::write_csv(
  tipos_vehiculo_depositos,
  fs::path(
    rutas$output_tables,
    "109_depositos_tipos_vehiculo.csv"
  )
)


message("Segundo bloque de exploración de depósitos terminado.")

# ------------------------------------------------------------------------------
# 24. Resumen general de estatus administrativos
# ------------------------------------------------------------------------------

resumen_estatus <- dplyr::bind_rows(

  depositos |>
    dplyr::count(
      estatus_tarjeta_circulacion,
      name = "vehiculos"
    ) |>
    dplyr::mutate(
      variable = "tarjeta_circulacion",
      valor = estatus_tarjeta_circulacion
    ) |>
    dplyr::select(
      variable,
      valor,
      vehiculos
    ),

  depositos |>
    dplyr::count(
      estatus_seguro,
      name = "vehiculos"
    ) |>
    dplyr::mutate(
      variable = "seguro",
      valor = estatus_seguro
    ) |>
    dplyr::select(
      variable,
      valor,
      vehiculos
    ),

  depositos |>
    dplyr::count(
      adeudos_tenencia,
      name = "vehiculos"
    ) |>
    dplyr::mutate(
      variable = "adeudos_tenencia",
      valor = adeudos_tenencia
    ) |>
    dplyr::select(
      variable,
      valor,
      vehiculos
    ),

  depositos |>
    dplyr::count(
      estatus_infracciones,
      name = "vehiculos"
    ) |>
    dplyr::mutate(
      variable = "infracciones",
      valor = estatus_infracciones
    ) |>
    dplyr::select(
      variable,
      valor,
      vehiculos
    )

) |>
  dplyr::group_by(variable) |>
  dplyr::mutate(
    porcentaje = round(
      vehiculos / sum(vehiculos) * 100,
      2
    )
  ) |>
  dplyr::ungroup()


cat("\nEstatus administrativos:\n\n")

print(
  resumen_estatus,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 25. Distribución del número de infracciones
# ------------------------------------------------------------------------------

resumen_infracciones <- depositos |>
  dplyr::summarise(
    vehiculos = dplyr::n(),

    sin_infracciones = sum(
      infracciones == 0,
      na.rm = TRUE
    ),

    con_infracciones = sum(
      infracciones > 0,
      na.rm = TRUE
    ),

    infracciones_totales = sum(
      infracciones,
      na.rm = TRUE
    ),

    media = mean(
      infracciones,
      na.rm = TRUE
    ),

    mediana = median(
      infracciones,
      na.rm = TRUE
    ),

    p90 = quantile(
      infracciones,
      0.90,
      na.rm = TRUE
    ),

    p95 = quantile(
      infracciones,
      0.95,
      na.rm = TRUE
    ),

    maximo = max(
      infracciones,
      na.rm = TRUE
    )
  )


cat("\nResumen de infracciones:\n\n")

print(
  resumen_infracciones,
  width = Inf
)


# ------------------------------------------------------------------------------
# 26. Distribución del número de sanciones
# ------------------------------------------------------------------------------

resumen_sanciones <- depositos |>
  dplyr::summarise(
    vehiculos = dplyr::n(),

    sin_sanciones = sum(
      sanciones == 0,
      na.rm = TRUE
    ),

    con_sanciones = sum(
      sanciones > 0,
      na.rm = TRUE
    ),

    sanciones_totales = sum(
      sanciones,
      na.rm = TRUE
    ),

    media = mean(
      sanciones,
      na.rm = TRUE
    ),

    mediana = median(
      sanciones,
      na.rm = TRUE
    ),

    p90 = quantile(
      sanciones,
      0.90,
      na.rm = TRUE
    ),

    p95 = quantile(
      sanciones,
      0.95,
      na.rm = TRUE
    ),

    maximo = max(
      sanciones,
      na.rm = TRUE
    )
  )


cat("\nResumen de sanciones:\n\n")

print(
  resumen_sanciones,
  width = Inf
)


# ------------------------------------------------------------------------------
# 27. Motocicletas
# ------------------------------------------------------------------------------

depositos <- depositos |>
  dplyr::mutate(
    motocicleta = dplyr::case_when(
      clase_vehiculo == "MOTOCICLETA" ~ TRUE,
      tipo_vehiculo %in% c(
        "MOTOCICLETA",
        "MOTONETA",
        "CUATRIMOTO",
        "TRIMOTO"
      ) ~ TRUE,
      TRUE ~ FALSE
    )
  )


resumen_motocicletas <- depositos |>
  dplyr::filter(
    !is.na(clase_vehiculo)
  ) |>
  dplyr::count(
    motocicleta,
    name = "vehiculos"
  ) |>
  dplyr::mutate(
    porcentaje = round(
      vehiculos / sum(vehiculos) * 100,
      2
    )
  )


cat("\nParticipación de motocicletas entre vehículos clasificados:\n\n")

print(
  resumen_motocicletas,
  width = Inf
)


# ------------------------------------------------------------------------------
# 28. Infracciones: motocicletas vs otros vehículos
# ------------------------------------------------------------------------------

comparacion_motocicletas <- depositos |>
  dplyr::filter(
    !is.na(clase_vehiculo)
  ) |>
  dplyr::group_by(
    motocicleta
  ) |>
  dplyr::summarise(
    vehiculos = dplyr::n(),

    infracciones_totales = sum(
      infracciones,
      na.rm = TRUE
    ),

    infracciones_media = mean(
      infracciones,
      na.rm = TRUE
    ),

    vehiculos_con_infracciones = sum(
      infracciones > 0,
      na.rm = TRUE
    ),

    porcentaje_con_infracciones = round(
      mean(
        infracciones > 0,
        na.rm = TRUE
      ) * 100,
      2
    ),

    sanciones_totales = sum(
      sanciones,
      na.rm = TRUE
    ),

    porcentaje_con_sanciones = round(
      mean(
        sanciones > 0,
        na.rm = TRUE
      ) * 100,
      2
    ),

    .groups = "drop"
  )


cat("\nMotocicletas vs otros vehículos:\n\n")

print(
  comparacion_motocicletas,
  width = Inf
)


# ------------------------------------------------------------------------------
# 29. Guardar diagnóstico final
# ------------------------------------------------------------------------------

readr::write_csv(
  resumen_estatus,
  fs::path(
    rutas$output_tables,
    "109_depositos_resumen_estatus.csv"
  )
)

readr::write_csv(
  resumen_infracciones,
  fs::path(
    rutas$output_tables,
    "109_depositos_resumen_infracciones.csv"
  )
)

readr::write_csv(
  resumen_sanciones,
  fs::path(
    rutas$output_tables,
    "109_depositos_resumen_sanciones.csv"
  )
)

readr::write_csv(
  resumen_motocicletas,
  fs::path(
    rutas$output_tables,
    "109_depositos_resumen_motocicletas.csv"
  )
)

readr::write_csv(
  comparacion_motocicletas,
  fs::path(
    rutas$output_tables,
    "109_depositos_comparacion_motocicletas.csv"
  )
)


message("Diagnóstico final de depósitos vehiculares terminado.")