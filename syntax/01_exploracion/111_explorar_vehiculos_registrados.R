# ==============================================================================
# 111_explorar_vehiculos_registrados.R
# Exploración inicial de vehículos registrados en circulación
# ==============================================================================

source("syntax/00_setup/000_setup.R")


# ------------------------------------------------------------------------------
# 1. Ruta
# ------------------------------------------------------------------------------

ruta_vehiculos <- fs::path(
  rutas$data_raw,
  "13_vehiculos_registrados"
)

archivos_vehiculos <- fs::dir_ls(
  ruta_vehiculos,
  regexp = "\\.csv$",
  type = "file"
)

cat("\nArchivos encontrados:\n\n")

print(
  fs::path_file(archivos_vehiculos)
)


# ------------------------------------------------------------------------------
# 2. Inventario
# ------------------------------------------------------------------------------

inventario_vehiculos <- tibble::tibble(
  ruta = archivos_vehiculos,
  archivo = fs::path_file(archivos_vehiculos),

  tamano_bytes = as.numeric(
    fs::file_size(archivos_vehiculos)
  ),

  tamano_kb = round(
    as.numeric(
      fs::file_size(archivos_vehiculos)
    ) / 1024,
    2
  )
)


cat("\nInventario:\n\n")

print(
  inventario_vehiculos,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 3. Leer base
# ------------------------------------------------------------------------------

vehiculos <- readr::read_csv(
  archivos_vehiculos[1],
  show_col_types = FALSE,
  progress = FALSE,
  name_repair = "unique"
)


cat("\nDimensiones:\n\n")

cat(
  "Filas:",
  nrow(vehiculos),
  "\n"
)

cat(
  "Columnas:",
  ncol(vehiculos),
  "\n"
)


# ------------------------------------------------------------------------------
# 4. Variables
# ------------------------------------------------------------------------------

cat("\nVariables disponibles:\n\n")

print(
  names(vehiculos)
)


# ------------------------------------------------------------------------------
# 5. Tipos de variables
# ------------------------------------------------------------------------------

tipos_vehiculos <- tibble::tibble(
  variable = names(vehiculos),

  clase = purrr::map_chr(
    vehiculos,
    ~ paste(
      class(.x),
      collapse = "/"
    )
  )
)


cat("\nTipos de variables:\n\n")

print(
  tipos_vehiculos,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 6. Base completa
# ------------------------------------------------------------------------------

cat("\nContenido de la base:\n\n")

print(
  vehiculos,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 7. Cardinalidad y faltantes
# ------------------------------------------------------------------------------

calidad_vehiculos <- tibble::tibble(
  variable = names(vehiculos),

  valores_unicos = purrr::map_int(
    vehiculos,
    ~ dplyr::n_distinct(
      .x,
      na.rm = TRUE
    )
  ),

  faltantes = purrr::map_int(
    vehiculos,
    ~ sum(
      is.na(.x)
    )
  ),

  porcentaje_faltantes = round(
    faltantes /
      nrow(vehiculos) *
      100,
    2
  )
)


cat("\nCardinalidad y faltantes:\n\n")

print(
  calidad_vehiculos,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 8. Buscar variables temporales
# ------------------------------------------------------------------------------

variables_temporales_vehiculos <- names(
  vehiculos
)[
  stringr::str_detect(
    stringr::str_to_lower(
      names(vehiculos)
    ),
    "anio|año|year|fecha|periodo"
  )
]


cat("\nVariables temporales candidatas:\n\n")

print(
  variables_temporales_vehiculos
)


# ------------------------------------------------------------------------------
# 9. Buscar variables de tipo de vehículo
# ------------------------------------------------------------------------------

variables_tipo_vehiculo <- names(
  vehiculos
)[
  stringr::str_detect(
    stringr::str_to_lower(
      names(vehiculos)
    ),
    "tipo|vehic|auto|moto|camion|camión|carga|pasaj"
  )
]


cat("\nVariables de tipo de vehículo candidatas:\n\n")

print(
  variables_tipo_vehiculo
)


# ------------------------------------------------------------------------------
# 10. Buscar variables geográficas
# ------------------------------------------------------------------------------

variables_geo_vehiculos <- names(
  vehiculos
)[
  stringr::str_detect(
    stringr::str_to_lower(
      names(vehiculos)
    ),
    "alcal|deleg|municip|entidad|cdmx"
  )
]


cat("\nVariables geográficas candidatas:\n\n")

print(
  variables_geo_vehiculos
)


# ------------------------------------------------------------------------------
# 11. Buscar variables numéricas
# ------------------------------------------------------------------------------

variables_numericas <- names(
  vehiculos
)[
  purrr::map_lgl(
    vehiculos,
    is.numeric
  )
]


cat("\nVariables numéricas:\n\n")

print(
  variables_numericas
)


# ------------------------------------------------------------------------------
# 12. Guardar auditoría inicial
# ------------------------------------------------------------------------------

readr::write_csv(
  inventario_vehiculos,
  fs::path(
    rutas$output_tables,
    "111_vehiculos_inventario.csv"
  )
)

readr::write_csv(
  tipos_vehiculos,
  fs::path(
    rutas$output_tables,
    "111_vehiculos_tipos_variables.csv"
  )
)

readr::write_csv(
  calidad_vehiculos,
  fs::path(
    rutas$output_tables,
    "111_vehiculos_calidad.csv"
  )
)


message(
  "Exploración estructural inicial de vehículos registrados terminada."
)

# ------------------------------------------------------------------------------
# 13. Revisar cobertura temporal
# ------------------------------------------------------------------------------

resumen_anios <- vehiculos |>
  dplyr::count(
    Año,
    name = "observaciones"
  ) |>
  dplyr::arrange(
    Año
  )


cat("\nCobertura por año:\n\n")

print(
  resumen_anios,
  n = Inf,
  width = Inf
)


cat(
  "\nAño mínimo:",
  min(vehiculos$Año, na.rm = TRUE),
  "\n"
)

cat(
  "Año máximo:",
  max(vehiculos$Año, na.rm = TRUE),
  "\n"
)

cat(
  "Número de años distintos:",
  dplyr::n_distinct(vehiculos$Año),
  "\n"
)


# ------------------------------------------------------------------------------
# 14. Revisar tipos de vehículo
# ------------------------------------------------------------------------------

tipos_vehiculo <- vehiculos |>
  dplyr::count(
    `Tipo de vehiculo`,
    name = "observaciones"
  ) |>
  dplyr::arrange(
    `Tipo de vehiculo`
  )


cat("\nTipos de vehículo:\n\n")

print(
  tipos_vehiculo,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 15. Revisar contenido de Numero de registro
# ------------------------------------------------------------------------------

cat("\nValores de Numero de registro:\n\n")

print(
  unique(
    vehiculos$`Numero de registro`
  )
)


# Intentar conversión numérica
vehiculos <- vehiculos |>
  dplyr::mutate(
    numero_registro_num = readr::parse_number(
      `Numero de registro`,
      locale = readr::locale(
        grouping_mark = ","
      )
    )
  )


cat("\nProblemas de conversión numérica:\n\n")

problemas_conversion <- vehiculos |>
  dplyr::filter(
    !is.na(`Numero de registro`) &
      is.na(numero_registro_num)
  )


print(
  problemas_conversion,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 16. Revisar estructura año × tipo de vehículo
# ------------------------------------------------------------------------------

estructura_panel <- vehiculos |>
  dplyr::count(
    Año,
    `Tipo de vehiculo`,
    name = "filas"
  ) |>
  dplyr::arrange(
    Año,
    `Tipo de vehiculo`
  )


cat("\nEstructura año × tipo de vehículo:\n\n")

print(
  estructura_panel,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 17. Detectar combinaciones repetidas
# ------------------------------------------------------------------------------

duplicados_panel <- estructura_panel |>
  dplyr::filter(
    filas > 1
  )


cat("\nCombinaciones año × tipo repetidas:\n\n")

print(
  duplicados_panel,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 18. Número de categorías por año
# ------------------------------------------------------------------------------

categorias_por_anio <- vehiculos |>
  dplyr::group_by(
    Año
  ) |>
  dplyr::summarise(
    categorias = dplyr::n_distinct(
      `Tipo de vehiculo`
    ),

    registros = dplyr::n(),

    .groups = "drop"
  ) |>
  dplyr::arrange(
    Año
  )


cat("\nCategorías disponibles por año:\n\n")

print(
  categorias_por_anio,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 19. Serie completa ya convertida
# ------------------------------------------------------------------------------

serie_vehiculos <- vehiculos |>
  dplyr::select(
    Año,
    `Tipo de vehiculo`,
    numero_registro_num
  ) |>
  dplyr::arrange(
    Año,
    `Tipo de vehiculo`
  )


cat("\nSerie completa:\n\n")

print(
  serie_vehiculos,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 20. Guardar diagnóstico
# ------------------------------------------------------------------------------

readr::write_csv(
  resumen_anios,
  fs::path(
    rutas$output_tables,
    "111_vehiculos_cobertura_anual.csv"
  )
)

readr::write_csv(
  tipos_vehiculo,
  fs::path(
    rutas$output_tables,
    "111_vehiculos_tipos.csv"
  )
)

readr::write_csv(
  categorias_por_anio,
  fs::path(
    rutas$output_tables,
    "111_vehiculos_categorias_por_anio.csv"
  )
)

readr::write_csv(
  serie_vehiculos,
  fs::path(
    rutas$output_tables,
    "111_vehiculos_serie.csv"
  )
)


message(
  "Diagnóstico temporal de vehículos registrados terminado."
)

# ------------------------------------------------------------------------------
# 21. Revisar faltantes después de conversión numérica
# ------------------------------------------------------------------------------

faltantes_registro <- vehiculos |>
  dplyr::filter(
    is.na(numero_registro_num)
  ) |>
  dplyr::select(
    Año,
    `Tipo de vehiculo`,
    `Numero de registro`,
    numero_registro_num
  )


cat("\nRegistros sin valor numérico:\n\n")

print(
  faltantes_registro,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 22. Comprobar unicidad año × tipo
# ------------------------------------------------------------------------------

resumen_panel <- vehiculos |>
  dplyr::summarise(
    filas = dplyr::n(),

    anios = dplyr::n_distinct(Año),

    tipos = dplyr::n_distinct(
      `Tipo de vehiculo`
    ),

    combinaciones = dplyr::n_distinct(
      paste(
        Año,
        `Tipo de vehiculo`,
        sep = "_"
      )
    )
  )


cat("\nEstructura del panel:\n\n")

print(
  resumen_panel,
  width = Inf
)


# ------------------------------------------------------------------------------
# 23. Pasar a formato ancho
# ------------------------------------------------------------------------------

vehiculos_wide <- vehiculos |>
  dplyr::select(
    Año,
    `Tipo de vehiculo`,
    numero_registro_num
  ) |>
  tidyr::pivot_wider(
    names_from = `Tipo de vehiculo`,
    values_from = numero_registro_num
  )


cat("\nSerie en formato ancho:\n\n")

print(
  vehiculos_wide,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 24. Construir total del parque vehicular
# ------------------------------------------------------------------------------

vehiculos_wide <- vehiculos_wide |>
  dplyr::mutate(
    total_vehiculos = rowSums(
      dplyr::across(
        c(
          `Automóviles`,
          `Camiones para pasajeros`,
          `Camiones y camionetas para carga`,
          `Motocicletas`
        )
      ),
      na.rm = FALSE
    )
  )


# ------------------------------------------------------------------------------
# 25. Participación de motocicletas
# ------------------------------------------------------------------------------

vehiculos_wide <- vehiculos_wide |>
  dplyr::mutate(
    participacion_motos = 100 *
      Motocicletas /
      total_vehiculos
  )


cat("\nParticipación de motocicletas en el parque vehicular:\n\n")

print(
  vehiculos_wide |>
    dplyr::select(
      Año,
      Motocicletas,
      total_vehiculos,
      participacion_motos
    ),
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 26. Crecimiento anual de motocicletas
# ------------------------------------------------------------------------------

serie_motocicletas <- vehiculos_wide |>
  dplyr::select(
    Año,
    Motocicletas,
    total_vehiculos,
    participacion_motos
  ) |>
  dplyr::arrange(
    Año
  ) |>
  dplyr::mutate(
    crecimiento_motos = 100 * (
      Motocicletas /
        dplyr::lag(Motocicletas) -
        1
    ),

    crecimiento_total = 100 * (
      total_vehiculos /
        dplyr::lag(total_vehiculos) -
        1
    )
  )


cat("\nSerie de motocicletas:\n\n")

print(
  serie_motocicletas,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 27. Resumen del periodo reciente
# ------------------------------------------------------------------------------

resumen_motos_reciente <- serie_motocicletas |>
  dplyr::filter(
    Año >= 2012
  )


cat("\nEvolución reciente de motocicletas:\n\n")

print(
  resumen_motos_reciente,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 28. Crecimiento acumulado 2012-2020
# ------------------------------------------------------------------------------

motos_2012 <- serie_motocicletas |>
  dplyr::filter(
    Año == 2012
  ) |>
  dplyr::pull(
    Motocicletas
  )


motos_2020 <- serie_motocicletas |>
  dplyr::filter(
    Año == 2020
  ) |>
  dplyr::pull(
    Motocicletas
  )


crecimiento_motos_2012_2020 <- (
  motos_2020 / motos_2012 - 1
) * 100


cat(
  "\nMotocicletas 2012:",
  format(motos_2012, big.mark = ","),
  "\n"
)

cat(
  "Motocicletas 2020:",
  format(motos_2020, big.mark = ","),
  "\n"
)

cat(
  "Crecimiento acumulado 2012-2020:",
  round(crecimiento_motos_2012_2020, 2),
  "%\n"
)


# ------------------------------------------------------------------------------
# 29. Gráfica de motocicletas registradas
# ------------------------------------------------------------------------------

grafica_motocicletas <- serie_motocicletas |>
  dplyr::filter(
    Año >= 2000
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = Año,
      y = Motocicletas
    )
  ) +
  ggplot2::geom_line(
    linewidth = 0.8
  ) +
  ggplot2::geom_point(
    size = 1.8
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::label_number(
      big.mark = ","
    )
  ) +
  ggplot2::labs(
    title = "Motocicletas registradas en circulación",
    subtitle = "Ciudad de México",
    x = NULL,
    y = "Motocicletas registradas"
  ) +
  ggplot2::theme_minimal()


print(
  grafica_motocicletas
)


# ------------------------------------------------------------------------------
# 30. Gráfica de participación de motocicletas
# ------------------------------------------------------------------------------

grafica_participacion_motos <- serie_motocicletas |>
  dplyr::filter(
    Año >= 2000
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = Año,
      y = participacion_motos
    )
  ) +
  ggplot2::geom_line(
    linewidth = 0.8
  ) +
  ggplot2::geom_point(
    size = 1.8
  ) +
  ggplot2::labs(
    title = "Participación de motocicletas en el parque vehicular",
    subtitle = "Ciudad de México",
    x = NULL,
    y = "Porcentaje del parque vehicular"
  ) +
  ggplot2::theme_minimal()


print(
  grafica_participacion_motos
)


# ------------------------------------------------------------------------------
# 31. Guardar resultados
# ------------------------------------------------------------------------------

readr::write_csv(
  vehiculos_wide,
  fs::path(
    rutas$output_tables,
    "111_vehiculos_panel_anual.csv"
  )
)

readr::write_csv(
  serie_motocicletas,
  fs::path(
    rutas$output_tables,
    "111_vehiculos_serie_motocicletas.csv"
  )
)


ggplot2::ggsave(
  fs::path(
    rutas$output_figures,
    "111_motocicletas_registradas.png"
  ),
  grafica_motocicletas,
  width = 9,
  height = 5,
  dpi = 300
)


ggplot2::ggsave(
  fs::path(
    rutas$output_figures,
    "111_participacion_motocicletas.png"
  ),
  grafica_participacion_motos,
  width = 9,
  height = 5,
  dpi = 300
)


message(
  "Exploración de vehículos registrados terminada."
)