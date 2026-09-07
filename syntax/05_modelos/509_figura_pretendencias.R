# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 509_figura_pretendencias.R
# Objetivo:
#   - Figura resumen de pre-tendencias: incidentes C5 vs. víctimas de tránsito,
#     para los tres eventos, con el p-value de la prueba conjunta anotado.
#   - Se construye a partir de las tablas que genera 507 (no reestima).
#
# Entradas:
#   output/tables/507_event_study_coeficientes.csv
#   output/tables/507_event_study_resumen.csv
#
# Salida:
#   output/figures/509_pretendencias_c5_vs_victimas.png
# ==============================================================================

source(
  here::here("syntax", "00_setup", "000_setup.R"),
  encoding = "UTF-8"
)

BUFFER <- 500

coefs <- readr::read_csv(
  fs::path(rutas$output_tables, "507_event_study_coeficientes.csv"),
  show_col_types = FALSE
) |>
  dplyr::filter(buffer_m == BUFFER)

resumen <- readr::read_csv(
  fs::path(rutas$output_tables, "507_event_study_resumen.csv"),
  show_col_types = FALSE
) |>
  dplyr::filter(buffer_m == BUFFER)

etiqueta_evento <- c(
  "2021-02" = "Feb 2021 — facultades de infracción",
  "2022-04" = "Abr 2022 — Policía de Tránsito",
  "2023-09" = "Sep 2023 — reforma motociclistas"
)
etiqueta_outcome <- c(
  "n_incidentes" = "Incidentes C5",
  "n_victimas"   = "Víctimas de tránsito (FGJ)"
)

coefs <- coefs |>
  dplyr::mutate(
    evento_lab  = factor(etiqueta_evento[evento], levels = etiqueta_evento),
    outcome_lab = factor(etiqueta_outcome[outcome], levels = etiqueta_outcome)
  )

anotacion <- resumen |>
  dplyr::mutate(
    evento_lab  = factor(etiqueta_evento[evento], levels = etiqueta_evento),
    outcome_lab = factor(etiqueta_outcome[outcome], levels = etiqueta_outcome),
    txt = paste0(
      "prueba conjunta pre: p = ",
      formatC(wald_pre_p, format = "g", digits = 2)
    )
  )

g <- ggplot2::ggplot(coefs, ggplot2::aes(tiempo_evento, estimate)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
  ggplot2::geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey40") +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "grey30"
  ) +
  ggplot2::geom_point(size = 1.7) +
  ggplot2::geom_text(
    data = anotacion,
    ggplot2::aes(x = -Inf, y = Inf, label = txt),
    hjust = -0.05, vjust = 1.4, size = 3.2, color = "grey20", inherit.aes = FALSE
  ) +
  ggplot2::facet_grid(
    outcome_lab ~ evento_lab, scales = "free",
    switch = "y"
  ) +
  ggplot2::labs(
    title = "Pre-tendencias: incidentes C5 vs. víctimas de tránsito",
    subtitle = paste0("Tratamiento: exposición a infraestructura de fiscalización, buffer ", BUFFER, " m"),
    x = "Mes relativo al evento",
    y = "Efecto estimado (colonias tratadas vs. control)"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    strip.placement = "outside",
    panel.spacing = ggplot2::unit(1, "lines")
  )

ggplot2::ggsave(
  fs::path(rutas$output_figures, "509_pretendencias_c5_vs_victimas.png"),
  g, width = 13, height = 7, dpi = 300
)

message("Figura guardada.")
