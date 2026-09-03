# ==============================================================================
# 502_event_study_binned.R
#
# Objetivo:
#   Estimar un event study agrupado (binned) para reducir dimensionalidad
#   y evaluar tendencias pretratamiento de forma más parsimoniosa.
#
# Especificación:
#   - Tratamientos: 8 radares validados
#   - Controles: matching 1:5
#   - Outcome: incidentes C5 mensuales dentro de 500 m
#   - Referencia temporal: diciembre de 2022
#   - FE ubicación
#   - FE mes-año
#   - Pesos de matching
#   - SE agrupados por ubicación
#
# IMPORTANTE:
#   Enero de 2023 es una referencia temporal del inventario, no una fecha
#   confirmada de instalación.
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. Configuración
# ------------------------------------------------------------------------------

source(
  "syntax/00_setup/000_setup.R"
)


# ------------------------------------------------------------------------------
# 2. Parámetros
# ------------------------------------------------------------------------------

fecha_evento <- as.Date(
  "2023-01-01"
)

fecha_inicio_ventana <- as.Date(
  "2021-01-01"
)

fecha_fin_ventana <- as.Date(
  "2024-02-01"
)

k_matching <- 5L


# ------------------------------------------------------------------------------
# 3. Archivos
# ------------------------------------------------------------------------------

archivo_tratados <- fs::path(
  rutas$data_processed,
  "300_panel_radares_c5.csv"
)

archivo_controles <- fs::path(
  rutas$data_processed,
  "301_panel_controles_c5.csv"
)

archivo_matching <- fs::path(
  rutas$data_processed,
  "302_matching_controles_seleccionados.csv"
)


stopifnot(
  file.exists(archivo_tratados),
  file.exists(archivo_controles),
  file.exists(archivo_matching)
)


# ------------------------------------------------------------------------------
# 4. Leer datos
# ------------------------------------------------------------------------------

panel_tratados <- readr::read_csv(
  archivo_tratados,
  show_col_types = FALSE
)

panel_controles <- readr::read_csv(
  archivo_controles,
  show_col_types = FALSE
)

matching <- readr::read_csv(
  archivo_matching,
  show_col_types = FALSE
)


cat("\nMatching utilizado:\n")

cat(
  "Tratamientos:",
  dplyr::n_distinct(
    matching$id_tratamiento
  ),
  "\n"
)

cat(
  "Controles únicos:",
  dplyr::n_distinct(
    matching$id_control
  ),
  "\n"
)

cat(
  "Pares:",
  nrow(matching),
  "\n"
)


# ------------------------------------------------------------------------------
# 5. Pesos del matching
# ------------------------------------------------------------------------------

pesos_controles <- matching |>
  dplyr::count(
    id_control,
    name = "veces_seleccionado"
  ) |>
  dplyr::mutate(
    peso_matching =
      veces_seleccionado /
      k_matching
  )


cat(
  "\nPeso total controles:",
  sum(
    pesos_controles$peso_matching
  ),
  "\n"
)


# ------------------------------------------------------------------------------
# 6. Preparar tratamientos
# ------------------------------------------------------------------------------

tratados <- panel_tratados |>
  dplyr::filter(
    tratamiento_500m
  ) |>
  dplyr::transmute(
    id_ubicacion =
      id_radar,

    mes =
      as.Date(mes),

    accidentes_500m,

    tratado = 1L,

    peso_matching = 1
  )


# ------------------------------------------------------------------------------
# 7. Preparar controles
# ------------------------------------------------------------------------------

controles <- panel_controles |>
  dplyr::inner_join(
    pesos_controles,
    by = "id_control"
  ) |>
  dplyr::transmute(
    id_ubicacion =
      id_control,

    mes =
      as.Date(mes),

    accidentes_500m,

    tratado = 0L,

    peso_matching
  )


# ------------------------------------------------------------------------------
# 8. Combinar muestra
# ------------------------------------------------------------------------------

panel_event <- dplyr::bind_rows(
  tratados,
  controles
) |>
  dplyr::mutate(

    tiempo_evento =
      (
        lubridate::year(mes) -
          lubridate::year(fecha_evento)
      ) * 12 +
      (
        lubridate::month(mes) -
          lubridate::month(fecha_evento)
      )

  ) |>
  dplyr::filter(
    mes >= fecha_inicio_ventana,
    mes <= fecha_fin_ventana
  )


# ------------------------------------------------------------------------------
# 9. Crear bloques temporales
# ------------------------------------------------------------------------------

panel_event <- panel_event |>
  dplyr::mutate(

    bloque_evento = dplyr::case_when(

      tiempo_evento <= -19 ~
        "pre_19_mas",

      tiempo_evento >= -18 &
        tiempo_evento <= -13 ~
        "pre_18_13",

      tiempo_evento >= -12 &
        tiempo_evento <= -7 ~
        "pre_12_7",

      tiempo_evento >= -6 &
        tiempo_evento <= -2 ~
        "pre_6_2",

      tiempo_evento == -1 ~
        "referencia",

      tiempo_evento >= 0 &
        tiempo_evento <= 2 ~
        "post_0_2",

      tiempo_evento >= 3 &
        tiempo_evento <= 5 ~
        "post_3_5",

      tiempo_evento >= 6 &
        tiempo_evento <= 8 ~
        "post_6_8",

      tiempo_evento >= 9 &
        tiempo_evento <= 11 ~
        "post_9_11",

      tiempo_evento >= 12 ~
        "post_12_mas",

      TRUE ~
        NA_character_
    ),

    bloque_evento = factor(
      bloque_evento,
      levels = c(
        "pre_19_mas",
        "pre_18_13",
        "pre_12_7",
        "pre_6_2",
        "referencia",
        "post_0_2",
        "post_3_5",
        "post_6_8",
        "post_9_11",
        "post_12_mas"
      )
    )
  )


# ------------------------------------------------------------------------------
# 10. Diagnóstico de bloques
# ------------------------------------------------------------------------------

resumen_bloques <- panel_event |>
  dplyr::count(
    bloque_evento,
    tratado,
    name = "observaciones"
  )


cat(
  "\nObservaciones por bloque:\n\n"
)

print(
  resumen_bloques,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 11. Modelo event study binned
#
# La categoría "referencia" (diciembre 2022) queda omitida.
# ------------------------------------------------------------------------------

modelo_binned <- fixest::feols(
  accidentes_500m ~
    i(
      bloque_evento,
      tratado,
      ref = "referencia"
    ) |
    id_ubicacion +
    mes,

  data = panel_event,

  weights =
    ~ peso_matching,

  cluster =
    ~ id_ubicacion
)


cat(
  "\n==============================================================\n"
)

cat(
  "EVENT STUDY BINNED — RADIO 500 m\n"
)

cat(
  "Referencia: diciembre 2022\n"
)

cat(
  "==============================================================\n\n"
)


print(
  summary(
    modelo_binned
  )
)


# ------------------------------------------------------------------------------
# 12. Extraer coeficientes
# ------------------------------------------------------------------------------

coef_binned <- broom::tidy(
  modelo_binned,
  conf.int = TRUE
) |>
  dplyr::filter(
    stringr::str_detect(
      term,
      "bloque_evento::"
    )
  )


cat(
  "\nCoeficientes binned:\n\n"
)

print(
  coef_binned,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 13. Extraer nombre del bloque
# ------------------------------------------------------------------------------

coef_binned <- coef_binned |>
  dplyr::mutate(

    bloque =
      stringr::str_match(
        term,
        "bloque_evento::([^:]+)"
      )[, 2],

    orden = dplyr::case_when(
      bloque == "pre_19_mas" ~ 1,
      bloque == "pre_18_13" ~ 2,
      bloque == "pre_12_7" ~ 3,
      bloque == "pre_6_2" ~ 4,
      bloque == "post_0_2" ~ 6,
      bloque == "post_3_5" ~ 7,
      bloque == "post_6_8" ~ 8,
      bloque == "post_9_11" ~ 9,
      bloque == "post_12_mas" ~ 10,
      TRUE ~ NA_real_
    ),

    etiqueta = dplyr::recode(
      bloque,

      pre_19_mas =
        "≤ -19",

      pre_18_13 =
        "-18 a -13",

      pre_12_7 =
        "-12 a -7",

      pre_6_2 =
        "-6 a -2",

      post_0_2 =
        "0 a 2",

      post_3_5 =
        "3 a 5",

      post_6_8 =
        "6 a 8",

      post_9_11 =
        "9 a 11",

      post_12_mas =
        "12+"
    )
  )


# ------------------------------------------------------------------------------
# 14. Agregar periodo de referencia para gráfica
# ------------------------------------------------------------------------------

coef_referencia <- tibble::tibble(
  term = "referencia",
  estimate = 0,
  std.error = NA_real_,
  statistic = NA_real_,
  p.value = NA_real_,
  conf.low = 0,
  conf.high = 0,
  bloque = "referencia",
  orden = 5,
  etiqueta = "-1"
)


coef_grafica <- dplyr::bind_rows(
  coef_binned,
  coef_referencia
) |>
  dplyr::arrange(
    orden
  ) |>
  dplyr::mutate(
    etiqueta = factor(
      etiqueta,
      levels = etiqueta
    )
  )


# ------------------------------------------------------------------------------
# 15. Gráfica principal
# ------------------------------------------------------------------------------

grafica_binned <- ggplot2::ggplot(
  coef_grafica,
  ggplot2::aes(
    x = etiqueta,
    y = estimate,
    group = 1
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::geom_vline(
    xintercept = 4.5,
    linetype = "dotted"
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    width = 0.15,
    na.rm = TRUE
  ) +
  ggplot2::geom_point(
    size = 2.5
  ) +
  ggplot2::geom_line(
    linewidth = 0.6
  ) +
  ggplot2::labs(
    title =
      "Event study agrupado de incidentes C5",

    subtitle =
      paste0(
        "Radio de 500 m · referencia: diciembre de 2022\n",
        "Bloques de meses relativos a enero de 2023"
      ),

    x =
      "Meses respecto de enero de 2023",

    y =
      "Efecto diferencial estimado\n(incidentes mensuales)"
  ) +
  ggplot2::theme_minimal()


ggplot2::ggsave(
  fs::path(
    rutas$output_figures,
    "502_event_study_binned_500m.png"
  ),
  plot = grafica_binned,
  width = 10,
  height = 6,
  dpi = 300
)


# ------------------------------------------------------------------------------
# 16. Test conjunto de bloques PRE
#
# H0:
# los cuatro bloques pretratamiento son conjuntamente cero.
# ------------------------------------------------------------------------------

cat(
  "\n==============================================================\n"
)

cat(
  "TEST CONJUNTO DE BLOQUES PRE\n"
)

cat(
  "==============================================================\n\n"
)


test_pre_binned <- tryCatch(

  fixest::wald(
    modelo_binned,
    keep = "bloque_evento::pre_"
  ),

  error = function(e) {

    message(
      "No fue posible ejecutar el test conjunto:"
    )

    message(
      e$message
    )

    NULL
  }
)


if (!is.null(test_pre_binned)) {

  print(
    test_pre_binned
  )

}


# ------------------------------------------------------------------------------
# 17. Tabla separada PRE y POST
# ------------------------------------------------------------------------------

tabla_binned <- coef_binned |>
  dplyr::mutate(
    periodo = dplyr::if_else(
      stringr::str_starts(
        bloque,
        "pre"
      ),
      "Pre",
      "Post"
    )
  ) |>
  dplyr::select(
    periodo,
    bloque,
    etiqueta,
    estimate,
    std.error,
    statistic,
    p.value,
    conf.low,
    conf.high
  ) |>
  dplyr::arrange(
    periodo,
    bloque
  )


cat(
  "\nCoeficientes agrupados:\n\n"
)

print(
  tabla_binned,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 18. Resumen promedio PRE/POST
# ------------------------------------------------------------------------------

resumen_binned <- tabla_binned |>
  dplyr::group_by(
    periodo
  ) |>
  dplyr::summarise(
    coef_promedio =
      mean(
        estimate,
        na.rm = TRUE
      ),

    coef_mediano =
      stats::median(
        estimate,
        na.rm = TRUE
      ),

    minimo =
      min(
        estimate,
        na.rm = TRUE
      ),

    maximo =
      max(
        estimate,
        na.rm = TRUE
      ),

    .groups = "drop"
  )


cat(
  "\nResumen PRE/POST:\n\n"
)

print(
  resumen_binned,
  width = Inf
)


# ------------------------------------------------------------------------------
# 19. Guardar resultados
# ------------------------------------------------------------------------------

readr::write_csv(
  tabla_binned,
  fs::path(
    rutas$output_tables,
    "502_coeficientes_event_study_binned.csv"
  )
)


readr::write_csv(
  resumen_binned,
  fs::path(
    rutas$output_tables,
    "502_resumen_event_study_binned.csv"
  )
)


readr::write_csv(
  resumen_bloques,
  fs::path(
    rutas$output_tables,
    "502_soporte_bloques.csv"
  )
)


# ------------------------------------------------------------------------------
# 20. Guardar modelo
# ------------------------------------------------------------------------------

saveRDS(
  modelo_binned,
  fs::path(
    rutas$data_processed,
    "502_modelo_event_study_binned.rds"
  )
)


# ------------------------------------------------------------------------------
# 21. Resumen final
# ------------------------------------------------------------------------------

cat(
  "\n==============================================================\n"
)

cat(
  "EVENT STUDY BINNED TERMINADO\n"
)

cat(
  "==============================================================\n"
)

cat(
  "Outcome: incidentes C5 dentro de 500 m\n"
)

cat(
  "Referencia: diciembre 2022\n"
)

cat(
  "Bloques PRE: 4\n"
)

cat(
  "Bloques POST: 5\n"
)

cat(
  "FE ubicación: sí\n"
)

cat(
  "FE mes-año: sí\n"
)

cat(
  "Pesos matching: sí\n"
)

cat(
  "Clustering: ubicación\n"
)

cat(
  "==============================================================\n"
)