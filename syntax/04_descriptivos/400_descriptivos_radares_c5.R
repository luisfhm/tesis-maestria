# ==============================================================================
# 400_descriptivos_radares_c5.R
# Descriptivos temporales del panel C5 alrededor de radares candidatos
# ==============================================================================

source("syntax/00_setup/000_setup.R")


# ------------------------------------------------------------------------------
# 1. Leer panel
# ------------------------------------------------------------------------------

panel <- readr::read_csv(
  fs::path(
    rutas$data_processed,
    "300_panel_radares_c5.csv"
  ),
  show_col_types = FALSE
) |>
  dplyr::mutate(
    mes = as.Date(mes)
  )


cat(
  "\nFilas panel:",
  nrow(panel),
  "\n"
)

cat(
  "Radares:",
  dplyr::n_distinct(panel$id_radar),
  "\n"
)


# ------------------------------------------------------------------------------
# 2. Conservar tratamiento principal >500 m
# ------------------------------------------------------------------------------

panel_principal <- panel |>
  dplyr::filter(
    tratamiento_500m
  )


cat(
  "\nRadares tratamiento principal:",
  dplyr::n_distinct(panel_principal$id_radar),
  "\n"
)


# ------------------------------------------------------------------------------
# 3. Serie mensual agregada
# ------------------------------------------------------------------------------

serie_mensual <- panel_principal |>
  dplyr::group_by(
    mes
  ) |>
  dplyr::summarise(
    accidentes_250m =
      sum(accidentes_250m),

    accidentes_500m =
      sum(accidentes_500m),

    accidentes_1000m =
      sum(accidentes_1000m),

    .groups = "drop"
  )


# ------------------------------------------------------------------------------
# 4. Promedio móvil de 12 meses
# ------------------------------------------------------------------------------

serie_mensual <- serie_mensual |>
  dplyr::arrange(
    mes
  ) |>
  dplyr::mutate(
    ma12_250m =
      slider::slide_dbl(
        accidentes_250m,
        mean,
        .before = 11,
        .complete = TRUE
      ),

    ma12_500m =
      slider::slide_dbl(
        accidentes_500m,
        mean,
        .before = 11,
        .complete = TRUE
      ),

    ma12_1000m =
      slider::slide_dbl(
        accidentes_1000m,
        mean,
        .before = 11,
        .complete = TRUE
      )
  )


# ------------------------------------------------------------------------------
# 5. Gráfica mensual — 500 m
# ------------------------------------------------------------------------------

grafica_mensual_500m <- ggplot2::ggplot(
  serie_mensual,
  ggplot2::aes(
    x = mes,
    y = accidentes_500m
  )
) +
  ggplot2::geom_line(
    linewidth = 0.45,
    alpha = 0.6
  ) +
  ggplot2::geom_line(
    data = serie_mensual |>
      dplyr::filter(
        !is.na(ma12_500m)
      ),
    ggplot2::aes(
      x = mes,
      y = ma12_500m
    ),
    linewidth = 1
  ) +
  ggplot2::geom_vline(
    xintercept = as.numeric(
      as.Date("2019-04-01")
    ),
    linetype = "dashed"
  ) +
  ggplot2::geom_vline(
    xintercept = as.numeric(
      as.Date("2023-01-01")
    ),
    linetype = "dotted"
  ) +
  ggplot2::labs(
    title = "Incidentes C5 alrededor de radares candidatos",
    subtitle = "8 ubicaciones principales · radio de 500 m",
    x = NULL,
    y = "Incidentes mensuales",
    caption = paste0(
      "Línea continua gruesa: promedio móvil 12 meses. ",
      "Las líneas verticales son referencias temporales, ",
      "no fechas confirmadas de instalación."
    )
  ) +
  ggplot2::theme_minimal()
  


# ------------------------------------------------------------------------------
# 6. Serie anual comparable
# ------------------------------------------------------------------------------

serie_anual <- panel_principal |>
  dplyr::filter(
    anio <= 2023
  ) |>
  dplyr::group_by(
    anio
  ) |>
  dplyr::summarise(
    accidentes_250m =
      sum(accidentes_250m),

    accidentes_500m =
      sum(accidentes_500m),

    accidentes_1000m =
      sum(accidentes_1000m),

    .groups = "drop"
  )


cat("\nSerie anual:\n\n")

print(
  serie_anual,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 7. Cambio anual
# ------------------------------------------------------------------------------

cambios_anuales <- serie_anual |>
  dplyr::arrange(
    anio
  ) |>
  dplyr::mutate(
    cambio_250m = 100 * (
      accidentes_250m /
        dplyr::lag(accidentes_250m) -
        1
    ),

    cambio_500m = 100 * (
      accidentes_500m /
        dplyr::lag(accidentes_500m) -
        1
    ),

    cambio_1000m = 100 * (
      accidentes_1000m /
        dplyr::lag(accidentes_1000m) -
        1
    )
  )


cat("\nCambios anuales (%):\n\n")

print(
  cambios_anuales,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 8. Comparación 2022 vs 2023 por radar
# ------------------------------------------------------------------------------

comparacion_2022_2023 <- panel_principal |>
  dplyr::filter(
    anio %in% c(
      2022,
      2023
    )
  ) |>
  dplyr::group_by(
    id_radar,
    anio
  ) |>
  dplyr::summarise(
    accidentes_250m =
      sum(accidentes_250m),

    accidentes_500m =
      sum(accidentes_500m),

    accidentes_1000m =
      sum(accidentes_1000m),

    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = anio,
    values_from = c(
      accidentes_250m,
      accidentes_500m,
      accidentes_1000m
    )
  ) |>
  dplyr::mutate(
    cambio_250m = 100 * (
      accidentes_250m_2023 /
        accidentes_250m_2022 -
        1
    ),

    cambio_500m = 100 * (
      accidentes_500m_2023 /
        accidentes_500m_2022 -
        1
    ),

    cambio_1000m = 100 * (
      accidentes_1000m_2023 /
        accidentes_1000m_2022 -
        1
    )
  ) |>
  dplyr::arrange(
    cambio_500m
  )


cat("\nComparación 2022 vs 2023 por radar:\n\n")

print(
  comparacion_2022_2023,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 9. Gráfica de cambio 2022-2023 por radar
# ------------------------------------------------------------------------------

grafica_cambio_radar <- comparacion_2022_2023 |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = reorder(
        id_radar,
        cambio_500m
      ),
      y = cambio_500m
    )
  ) +
  ggplot2::geom_col() +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Cambio de incidentes C5 entre 2022 y 2023",
    subtitle = "Radio de 500 m alrededor de cada radar candidato",
    x = NULL,
    y = "Cambio porcentual"
  ) +
  ggplot2::theme_minimal()


print(
  grafica_cambio_radar
)


# ------------------------------------------------------------------------------
# 10. Serie mensual por radar
# ------------------------------------------------------------------------------

grafica_por_radar <- panel_principal |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = mes,
      y = accidentes_500m
    )
  ) +
  ggplot2::geom_line(
    linewidth = 0.4
  ) +
  ggplot2::geom_vline(
    xintercept = as.Date("2023-01-01"),
    linetype = "dashed"
  ) +
  ggplot2::facet_wrap(
    ~ id_radar,
    scales = "free_y",
    ncol = 2
  ) +
  ggplot2::labs(
    title = "Incidentes C5 por ubicación candidata",
    subtitle = "Radio de 500 m",
    x = NULL,
    y = "Incidentes mensuales"
  ) +
  ggplot2::theme_minimal()


print(
  grafica_por_radar
)


# ------------------------------------------------------------------------------
# 11. Comparar radios espaciales
# ------------------------------------------------------------------------------

serie_radios <- serie_mensual |>
  dplyr::select(
    mes,
    accidentes_250m,
    accidentes_500m,
    accidentes_1000m
  ) |>
  tidyr::pivot_longer(
    cols = -mes,
    names_to = "radio",
    values_to = "accidentes"
  ) |>
  dplyr::mutate(
    radio = dplyr::recode(
      radio,
      accidentes_250m = "250 m",
      accidentes_500m = "500 m",
      accidentes_1000m = "1000 m"
    )
  )


grafica_radios <- ggplot2::ggplot(
  serie_radios,
  ggplot2::aes(
    x = mes,
    y = accidentes
  )
) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(
    ~ radio,
    scales = "free_y",
    ncol = 1
  ) +
  ggplot2::geom_vline(
    xintercept = as.Date("2023-01-01"),
    linetype = "dashed"
  ) +
  ggplot2::labs(
    title = "Incidentes C5 según distancia al radar",
    subtitle = "8 ubicaciones principales",
    x = NULL,
    y = "Incidentes"
  ) +
  ggplot2::theme_minimal()


print(
  grafica_radios
)


# ------------------------------------------------------------------------------
# 12. Comparación mensual 2022 vs 2023
# ------------------------------------------------------------------------------

comparacion_mensual <- panel_principal |>
  dplyr::filter(
    anio %in% c(
      2022,
      2023
    )
  ) |>
  dplyr::group_by(
    anio,
    numero_mes
  ) |>
  dplyr::summarise(
    accidentes_500m =
      sum(accidentes_500m),

    .groups = "drop"
  )


grafica_2022_2023 <- ggplot2::ggplot(
  comparacion_mensual,
  ggplot2::aes(
    x = numero_mes,
    y = accidentes_500m,
    group = factor(anio),
    linetype = factor(anio)
  )
) +
  ggplot2::geom_line(
    linewidth = 0.8
  ) +
  ggplot2::geom_point() +
  ggplot2::scale_x_continuous(
    breaks = 1:12
  ) +
  ggplot2::labs(
    title = "Comparación mensual de incidentes: 2022 vs 2023",
    subtitle = "8 ubicaciones principales · radio de 500 m",
    x = "Mes",
    y = "Incidentes",
    linetype = "Año"
  ) +
  ggplot2::theme_minimal()


print(
  grafica_2022_2023
)


# ------------------------------------------------------------------------------
# 13. Guardar tablas
# ------------------------------------------------------------------------------

readr::write_csv(
  serie_mensual,
  fs::path(
    rutas$output_tables,
    "400_radares_c5_serie_mensual.csv"
  )
)


readr::write_csv(
  cambios_anuales,
  fs::path(
    rutas$output_tables,
    "400_radares_c5_cambios_anuales.csv"
  )
)


readr::write_csv(
  comparacion_2022_2023,
  fs::path(
    rutas$output_tables,
    "400_radares_c5_comparacion_2022_2023.csv"
  )
)


# ------------------------------------------------------------------------------
# 14. Guardar figuras
# ------------------------------------------------------------------------------

ggplot2::ggsave(
  fs::path(
    rutas$output_figures,
    "400_radares_c5_serie_mensual_500m.png"
  ),
  grafica_mensual_500m,
  width = 10,
  height = 5.5,
  dpi = 300
)


ggplot2::ggsave(
  fs::path(
    rutas$output_figures,
    "400_radares_c5_cambio_2022_2023_por_radar.png"
  ),
  grafica_cambio_radar,
  width = 9,
  height = 6,
  dpi = 300
)


ggplot2::ggsave(
  fs::path(
    rutas$output_figures,
    "400_radares_c5_series_por_radar.png"
  ),
  grafica_por_radar,
  width = 12,
  height = 14,
  dpi = 300
)


ggplot2::ggsave(
  fs::path(
    rutas$output_figures,
    "400_radares_c5_radios.png"
  ),
  grafica_radios,
  width = 10,
  height = 9,
  dpi = 300
)


ggplot2::ggsave(
  fs::path(
    rutas$output_figures,
    "400_radares_c5_2022_vs_2023.png"
  ),
  grafica_2022_2023,
  width = 9,
  height = 5.5,
  dpi = 300
)


message(
  "Descriptivos de radares × C5 terminados."
)