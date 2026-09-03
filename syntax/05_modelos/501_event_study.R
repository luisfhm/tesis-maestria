# ==============================================================================
# 501_event_study.R
#
# Objetivo:
#   Evaluar la dinámica temporal de los incidentes C5 alrededor de las
#   ubicaciones tratadas respecto de sus controles emparejados.
#
# Especificación:
#   - Tratamientos: 8 radares validados
#   - Controles: matching 1:5 del script 302
#   - Outcome: incidentes C5 mensuales dentro de 500 m
#   - Evento de referencia: enero de 2023
#   - Periodo base: diciembre de 2022 (k = -1)
#   - Ventana principal: enero 2021 – febrero 2024
#                      k = -24,...,+13
#   - FE ubicación
#   - FE mes-año
#   - Pesos de matching
#   - Errores estándar agrupados por ubicación
#
# IMPORTANTE:
#   Enero de 2023 funciona como referencia temporal del inventario.
#   No constituye una fecha confirmada de instalación de cada radar.
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

periodo_referencia <- -1L

k_matching <- 5L


# ------------------------------------------------------------------------------
# 3. Localizar archivos
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


cat(
  "\nMatching utilizado:\n"
)

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
  "Pares tratamiento-control:",
  nrow(matching),
  "\n"
)


# ------------------------------------------------------------------------------
# 5. Construir pesos de matching
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
# 8. Construir panel analítico
# ------------------------------------------------------------------------------

panel_event <- dplyr::bind_rows(
  tratados,
  controles
) |>
  dplyr::mutate(

    # Diferencia mensual respecto de enero de 2023
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
  ) |>
  dplyr::arrange(
    id_ubicacion,
    mes
  )


# ------------------------------------------------------------------------------
# 9. Diagnóstico de ventana
# ------------------------------------------------------------------------------

resumen_event <- panel_event |>
  dplyr::summarise(
    ubicaciones =
      dplyr::n_distinct(
        id_ubicacion
      ),

    tratados =
      dplyr::n_distinct(
        id_ubicacion[
          tratado == 1
        ]
      ),

    controles =
      dplyr::n_distinct(
        id_ubicacion[
          tratado == 0
        ]
      ),

    meses =
      dplyr::n_distinct(
        mes
      ),

    filas =
      dplyr::n(),

    k_min =
      min(
        tiempo_evento
      ),

    k_max =
      max(
        tiempo_evento
      )
  )


cat(
  "\nResumen muestra event study:\n\n"
)

print(
  resumen_event,
  width = Inf
)


# ------------------------------------------------------------------------------
# 10. Verificar soporte temporal
# ------------------------------------------------------------------------------

soporte_evento <- panel_event |>
  dplyr::group_by(
    tiempo_evento,
    tratado
  ) |>
  dplyr::summarise(
    ubicaciones =
      dplyr::n_distinct(
        id_ubicacion
      ),

    observaciones =
      dplyr::n(),

    .groups = "drop"
  )


cat(
  "\nSoporte temporal:\n\n"
)

print(
  soporte_evento,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 11. Estimar event study mensual
#
# diciembre de 2022 (k = -1) se omite como categoría de referencia.
#
# Los efectos fijos de mes absorben toda la evolución temporal común.
# Los coeficientes i(tiempo_evento, tratado) representan la diferencia
# tratado-control respecto de la brecha existente en diciembre de 2022.
# ------------------------------------------------------------------------------

modelo_event <- fixest::feols(
  accidentes_500m ~
    i(
      tiempo_evento,
      tratado,
      ref = periodo_referencia
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
  "EVENT STUDY — RADIO 500 m\n"
)

cat(
  "Referencia: diciembre de 2022 (k = -1)\n"
)

cat(
  "==============================================================\n\n"
)


print(
  summary(
    modelo_event
  )
)


# ------------------------------------------------------------------------------
# 12. Extraer coeficientes
# ------------------------------------------------------------------------------

coef_event <- broom::tidy(
  modelo_event,
  conf.int = TRUE
) |>
  dplyr::filter(
    stringr::str_detect(
      term,
      "tiempo_evento::"
    )
  )


cat(
  "\nNombres de coeficientes event study:\n\n"
)

print(
  coef_event$term
)


# ------------------------------------------------------------------------------
# 13. Extraer tiempo relativo desde el nombre del coeficiente
# ------------------------------------------------------------------------------

coef_event <- coef_event |>
  dplyr::mutate(

    tiempo_evento =
      stringr::str_match(
        term,
        "tiempo_evento::(-?[0-9]+)"
      )[, 2] |>
      as.integer(),

    periodo =
      dplyr::case_when(
        tiempo_evento < 0 ~
          "Pre",

        tiempo_evento >= 0 ~
          "Post"
      )
  ) |>
  dplyr::arrange(
    tiempo_evento
  )


# ------------------------------------------------------------------------------
# 14. Incorporar periodo de referencia
#
# Para la gráfica agregamos k = -1 manualmente:
# coeficiente = 0 por definición.
# ------------------------------------------------------------------------------

coef_referencia <- tibble::tibble(
  term =
    "Referencia",

  estimate =
    0,

  std.error =
    NA_real_,

  statistic =
    NA_real_,

  p.value =
    NA_real_,

  conf.low =
    0,

  conf.high =
    0,

  tiempo_evento =
    periodo_referencia,

  periodo =
    "Pre"
)


coef_event_grafica <- dplyr::bind_rows(
  coef_event,
  coef_referencia
) |>
  dplyr::arrange(
    tiempo_evento
  )


# ------------------------------------------------------------------------------
# 15. Gráfica principal del event study
# ------------------------------------------------------------------------------

grafica_event <- ggplot2::ggplot(
  coef_event_grafica,
  ggplot2::aes(
    x = tiempo_evento,
    y = estimate
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::geom_vline(
    xintercept = -0.5,
    linetype = "dotted"
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    width = 0.2,
    na.rm = TRUE
  ) +
  ggplot2::geom_point(
    size = 2
  ) +
  ggplot2::geom_line(
    linewidth = 0.55
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(
      -24,
      12,
      by = 3
    )
  ) +
  ggplot2::labs(
    title =
      "Event study de incidentes C5 alrededor de radares",

    subtitle =
      paste0(
        "Radio de 500 m · referencia: diciembre de 2022\n",
        "Enero de 2023 es una referencia temporal, ",
        "no una fecha confirmada de instalación"
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
    "501_event_study_500m.png"
  ),
  plot = grafica_event,
  width = 10,
  height = 6,
  dpi = 300
)


# ------------------------------------------------------------------------------
# 16. Tabla de coeficientes pretratamiento
# ------------------------------------------------------------------------------

coef_pre <- coef_event |>
  dplyr::filter(
    tiempo_evento <= -2
  )


cat(
  "\nCoeficientes PRE (-24 a -2):\n\n"
)

print(
  coef_pre |>
    dplyr::select(
      tiempo_evento,
      estimate,
      std.error,
      p.value,
      conf.low,
      conf.high
    ),
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 17. Test conjunto de coeficientes pretratamiento
#
# H0:
# todos los coeficientes PRE son conjuntamente iguales a cero.
#
# IMPORTANTE:
# con 8 tratamientos este test debe interpretarse con cautela.
# ------------------------------------------------------------------------------

cat(
  "\n==============================================================\n"
)

cat(
  "TEST CONJUNTO DE PRETENDENCIAS\n"
)

cat(
  "==============================================================\n\n"
)


test_pre <- tryCatch(

  fixest::wald(
    modelo_event,
    keep = "tiempo_evento::-"
  ),

  error = function(e) {

    message(
      "No fue posible ejecutar automáticamente el test conjunto:"
    )

    message(
      e$message
    )

    NULL
  }
)


if (!is.null(test_pre)) {

  print(
    test_pre
  )

}


# ------------------------------------------------------------------------------
# 18. Resumen promedio PRE y POST
#
# Esto NO sustituye el DiD del 500.
# Solo resume la dinámica de los coeficientes del event study.
# ------------------------------------------------------------------------------

resumen_event_coef <- coef_event |>
  dplyr::group_by(
    periodo
  ) |>
  dplyr::summarise(
    coeficiente_promedio =
      mean(
        estimate,
        na.rm = TRUE
      ),

    coeficiente_mediano =
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
  "\nResumen de coeficientes PRE/POST:\n\n"
)

print(
  resumen_event_coef,
  width = Inf
)


# ------------------------------------------------------------------------------
# 19. Serie promedio observada para complementar la interpretación
# ------------------------------------------------------------------------------

serie_observada <- panel_event |>
  dplyr::mutate(
    grupo =
      dplyr::if_else(
        tratado == 1,
        "Tratamientos",
        "Controles emparejados"
      )
  ) |>
  dplyr::group_by(
    grupo,
    mes
  ) |>
  dplyr::summarise(
    accidentes =
      weighted.mean(
        accidentes_500m,
        w = peso_matching,
        na.rm = TRUE
      ),

    .groups = "drop"
  )


grafica_observada <- ggplot2::ggplot(
  serie_observada,
  ggplot2::aes(
    x = mes,
    y = accidentes,
    linetype = grupo
  )
) +
  ggplot2::geom_vline(
    xintercept =
      as.numeric(
        fecha_evento
      ),
    linetype = "dotted"
  ) +
  ggplot2::geom_line(
    linewidth = 0.8
  ) +
  ggplot2::labs(
    title =
      "Incidentes observados alrededor de enero de 2023",

    subtitle =
      "Tratamientos y controles emparejados · radio de 500 m",

    x = NULL,

    y =
      "Incidentes mensuales promedio",

    linetype = NULL
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position =
      "bottom"
  )


ggplot2::ggsave(
  fs::path(
    rutas$output_figures,
    "501_series_observadas_500m.png"
  ),
  plot = grafica_observada,
  width = 10,
  height = 5.5,
  dpi = 300
)


# ------------------------------------------------------------------------------
# 20. Guardar resultados
# ------------------------------------------------------------------------------

readr::write_csv(
  coef_event,
  fs::path(
    rutas$output_tables,
    "501_coeficientes_event_study.csv"
  )
)


readr::write_csv(
  resumen_event_coef,
  fs::path(
    rutas$output_tables,
    "501_resumen_event_study.csv"
  )
)


readr::write_csv(
  serie_observada,
  fs::path(
    rutas$output_tables,
    "501_series_observadas.csv"
  )
)


# ------------------------------------------------------------------------------
# 21. Guardar modelo
# ------------------------------------------------------------------------------

saveRDS(
  modelo_event,
  fs::path(
    rutas$data_processed,
    "501_modelo_event_study.rds"
  )
)


# ------------------------------------------------------------------------------
# 22. Resumen final
# ------------------------------------------------------------------------------

cat(
  "\n==============================================================\n"
)

cat(
  "EVENT STUDY TERMINADO\n"
)

cat(
  "==============================================================\n"
)

cat(
  "Outcome: incidentes C5 dentro de 500 m\n"
)

cat(
  "Ventana:",
  format(
    fecha_inicio_ventana,
    "%Y-%m"
  ),
  "→",
  format(
    fecha_fin_ventana,
    "%Y-%m"
  ),
  "\n"
)

cat(
  "Periodo referencia: diciembre 2022 (k = -1)\n"
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