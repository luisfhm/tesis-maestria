# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 506_event_study_2023.R
# Objetivo:
#   - Estimar un estudio de evento alrededor de septiembre de 2023
#   - Comparar colonias con y sin infraestructura preexistente
#   - Evaluar pre-tendencias y dinámica posterior al evento
#
# Entrada:
#   data/final/panel_evento_2023.parquet
#
# Salidas:
#   output/figures/506_event_study_2023.png
#   output/tables/506_event_study_2023_coeficientes.csv
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
# 1. Leer panel
# ------------------------------------------------------------------------------

archivo_panel <- fs::path(
  rutas$data_final,
  "panel_evento_2023.parquet"
)

validar_archivo(archivo_panel)

panel <- arrow::read_parquet(
  archivo_panel
) |>
  tibble::as_tibble()


# ------------------------------------------------------------------------------
# 2. Definir ventana del estudio de evento
# ------------------------------------------------------------------------------

# Evento:
#   septiembre 2023 = 0
#
# Referencia:
#   agosto 2023 = -1

ventana_pre <- 12
ventana_post <- 5

panel_evento <- panel |>
  dplyr::filter(
    tiempo_evento >= -ventana_pre,
    tiempo_evento <= ventana_post
  )


# ------------------------------------------------------------------------------
# 3. Diagnóstico de muestra
# ------------------------------------------------------------------------------

message("")
message("==============================================================")
message("MUESTRA EVENT STUDY 2023")
message("==============================================================")
message(
  "Colonias:             ",
  dplyr::n_distinct(panel_evento$id_colonia)
)
message(
  "Tratadas:             ",
  dplyr::n_distinct(
    panel_evento$id_colonia[
      panel_evento$tratado == 1
    ]
  )
)
message(
  "Controles:            ",
  dplyr::n_distinct(
    panel_evento$id_colonia[
      panel_evento$tratado == 0
    ]
  )
)
message(
  "Mes inicial:          ",
  min(panel_evento$mes)
)
message(
  "Mes final:            ",
  max(panel_evento$mes)
)
message(
  "Observaciones:        ",
  nrow(panel_evento)
)
message("==============================================================")


# ------------------------------------------------------------------------------
# 4. Estimar estudio de evento
# ------------------------------------------------------------------------------

modelo_evento <- fixest::feols(
  n_incidentes ~
    fixest::i(
      tiempo_evento,
      tratado,
      ref = -1
    ) |
    id_colonia + mes,
  cluster = ~ id_colonia,
  data = panel_evento
)


# ------------------------------------------------------------------------------
# 5. Mostrar resultados
# ------------------------------------------------------------------------------

summary(modelo_evento)


# ------------------------------------------------------------------------------
# 6. Prueba conjunta de pre-tendencias
# ------------------------------------------------------------------------------

# Probamos si todos los coeficientes anteriores al periodo de referencia
# son conjuntamente iguales a cero.

pre_terms <- names(stats::coef(modelo_evento))[
  stringr::str_detect(
    names(stats::coef(modelo_evento)),
    "tiempo_evento::-[0-9]+:tratado"
  )
]

if (length(pre_terms) > 0) {

  prueba_pre <- fixest::wald(
    modelo_evento,
    keep = "tiempo_evento::-[0-9]+:tratado"
  )

  print(prueba_pre)

} else {

  warning(
    "No se identificaron coeficientes pretratamiento para la prueba conjunta."
  )
}


# ------------------------------------------------------------------------------
# 7. Extraer coeficientes
# ------------------------------------------------------------------------------

coeficientes <- broom::tidy(
  modelo_evento,
  conf.int = TRUE
) |>
  dplyr::filter(
    stringr::str_detect(
      term,
      "tiempo_evento"
    )
  ) |>
  dplyr::mutate(
    tiempo_evento = stringr::str_extract(
      term,
      "-?\\d+"
    ) |>
      as.integer()
  ) |>
  dplyr::arrange(
    tiempo_evento
  )


# ------------------------------------------------------------------------------
# 8. Agregar periodo de referencia
# ------------------------------------------------------------------------------

coeficientes_plot <- coeficientes |>
  dplyr::select(
    tiempo_evento,
    estimate,
    std.error,
    conf.low,
    conf.high
  ) |>
  dplyr::bind_rows(
    tibble::tibble(
      tiempo_evento = -1L,
      estimate = 0,
      std.error = NA_real_,
      conf.low = 0,
      conf.high = 0
    )
  ) |>
  dplyr::arrange(
    tiempo_evento
  )


# ------------------------------------------------------------------------------
# 9. Graficar estudio de evento
# ------------------------------------------------------------------------------

grafica_evento <- ggplot2::ggplot(
  coeficientes_plot,
  ggplot2::aes(
    x = tiempo_evento,
    y = estimate
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dotted"
  ) +
  ggplot2::geom_vline(
    xintercept = -0.5,
    linetype = "dashed"
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    width = 0.15
  ) +
  ggplot2::geom_point(
    size = 2
  ) +
  ggplot2::labs(
    title = "Estudio de evento: reforma de septiembre de 2023",
    subtitle = "Colonias con infraestructura de Fotocívicas vs. colonias sin infraestructura",
    x = "Mes relativo al evento",
    y = "Diferencia en incidentes C5"
  ) +
  ggplot2::theme_minimal()


grafica_evento


# ------------------------------------------------------------------------------
# 10. Guardar gráfica
# ------------------------------------------------------------------------------

archivo_grafica <- fs::path(
  rutas$output_figures,
  "506_event_study_2023.png"
)

ggplot2::ggsave(
  filename = archivo_grafica,
  plot = grafica_evento,
  width = 10,
  height = 6,
  dpi = 300
)


# ------------------------------------------------------------------------------
# 11. Guardar coeficientes
# ------------------------------------------------------------------------------

archivo_coeficientes <- fs::path(
  rutas$output_tables,
  "506_event_study_2023_coeficientes.csv"
)

readr::write_csv(
  coeficientes_plot,
  archivo_coeficientes
)


# ------------------------------------------------------------------------------
# 12. Resumen final
# ------------------------------------------------------------------------------

message("")
message("==============================================================")
message("EVENT STUDY 2023 COMPLETADO")
message("==============================================================")
message(
  "Ventana:          ",
  -ventana_pre,
  " a +",
  ventana_post,
  " meses"
)
message("Referencia:       mes -1 (agosto 2023)")
message("Efectos fijos:    colonia + mes")
message("Clustering:       colonia")
message("Gráfica:          ", archivo_grafica)
message("Coeficientes:     ", archivo_coeficientes)
message("==============================================================")