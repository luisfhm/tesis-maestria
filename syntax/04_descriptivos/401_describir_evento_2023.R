# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 401_describir_evento_2023.R
# Objetivo:
#   - Describir la evolución mensual de incidentes C5
#   - Comparar colonias tratadas y de control
#   - Visualizar el comportamiento previo y posterior al evento de septiembre 2023
#
# Entrada:
#   data/final/panel_evento_2023.parquet
#
# Salidas:
#   output/figures/401_tendencias_evento_2023.png
#   output/figures/401_tendencias_normalizadas_2023.png
#   output/tables/401_resumen_mensual_evento_2023.csv
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
# 2. Definir fecha del evento
# ------------------------------------------------------------------------------

fecha_evento <- as.Date("2023-09-01")


# ------------------------------------------------------------------------------
# 3. Resumen mensual por grupo
# ------------------------------------------------------------------------------

resumen_mensual <- panel |>
  dplyr::group_by(
    mes,
    tratado
  ) |>
  dplyr::summarise(
    n_colonias = dplyr::n_distinct(id_colonia),
    media_incidentes = mean(n_incidentes),
    mediana_incidentes = median(n_incidentes),
    sd_incidentes = sd(n_incidentes),
    total_incidentes = sum(n_incidentes),
    .groups = "drop"
  )


# ------------------------------------------------------------------------------
# 4. Etiquetar grupos
# ------------------------------------------------------------------------------

resumen_mensual <- resumen_mensual |>
  dplyr::mutate(
    grupo = dplyr::case_when(
      tratado == 1 ~ "Con infraestructura",
      tratado == 0 ~ "Sin infraestructura"
    )
  )


# ------------------------------------------------------------------------------
# 5. Guardar resumen mensual
# ------------------------------------------------------------------------------

archivo_resumen <- fs::path(
  rutas$output_tables,
  "401_resumen_mensual_evento_2023.csv"
)

readr::write_csv(
  resumen_mensual,
  archivo_resumen
)


# ------------------------------------------------------------------------------
# 6. Gráfica de tendencias en niveles
# ------------------------------------------------------------------------------

grafica_niveles <- ggplot2::ggplot(
  resumen_mensual,
  ggplot2::aes(
    x = mes,
    y = media_incidentes,
    linetype = grupo
  )
) +
  ggplot2::geom_line(
    linewidth = 0.8
  ) +
  ggplot2::geom_vline(
    xintercept = fecha_evento,
    linetype = "dashed"
  ) +
  ggplot2::labs(
    title = "Incidentes C5 por colonia y mes",
    subtitle = "Colonias con y sin infraestructura de Fotocívicas",
    x = NULL,
    y = "Incidentes promedio por colonia",
    linetype = NULL
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom"
  )

grafica_niveles


# ------------------------------------------------------------------------------
# 7. Guardar gráfica en niveles
# ------------------------------------------------------------------------------

ggplot2::ggsave(
  filename = fs::path(
    rutas$output_figures,
    "401_tendencias_evento_2023.png"
  ),
  plot = grafica_niveles,
  width = 10,
  height = 6,
  dpi = 300
)


# ------------------------------------------------------------------------------
# 8. Normalizar tendencias
# ------------------------------------------------------------------------------

# Para comparar movimientos relativos, normalizamos cada grupo respecto
# al promedio de un periodo previo al evento.
#
# Aquí usamos enero-agosto de 2023 como referencia descriptiva.

promedio_base <- resumen_mensual |>
  dplyr::filter(
    mes >= as.Date("2023-01-01"),
    mes <= as.Date("2023-08-01")
  ) |>
  dplyr::group_by(
    tratado
  ) |>
  dplyr::summarise(
    promedio_pre = mean(media_incidentes),
    .groups = "drop"
  )


resumen_normalizado <- resumen_mensual |>
  dplyr::left_join(
    promedio_base,
    by = "tratado"
  ) |>
  dplyr::mutate(
    indice_incidentes = 100 * media_incidentes / promedio_pre
  )


# ------------------------------------------------------------------------------
# 9. Gráfica de tendencias normalizadas
# ------------------------------------------------------------------------------

grafica_normalizada <- ggplot2::ggplot(
  resumen_normalizado,
  ggplot2::aes(
    x = mes,
    y = indice_incidentes,
    linetype = grupo
  )
) +
  ggplot2::geom_line(
    linewidth = 0.8
  ) +
  ggplot2::geom_hline(
    yintercept = 100,
    linetype = "dotted"
  ) +
  ggplot2::geom_vline(
    xintercept = fecha_evento,
    linetype = "dashed"
  ) +
  ggplot2::labs(
    title = "Evolución normalizada de incidentes C5",
    subtitle = "Promedio enero-agosto 2023 = 100",
    x = NULL,
    y = "Índice de incidentes",
    linetype = NULL
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom"
  )

grafica_normalizada


# ------------------------------------------------------------------------------
# 10. Guardar gráfica normalizada
# ------------------------------------------------------------------------------

ggplot2::ggsave(
  filename = fs::path(
    rutas$output_figures,
    "401_tendencias_normalizadas_2023.png"
  ),
  plot = grafica_normalizada,
  width = 10,
  height = 6,
  dpi = 300
)


# ------------------------------------------------------------------------------
# 11. Resumen pre y post
# ------------------------------------------------------------------------------

resumen_pre_post <- panel |>
  dplyr::mutate(
    periodo = dplyr::if_else(
      mes < fecha_evento,
      "Pre",
      "Post"
    )
  ) |>
  dplyr::group_by(
    tratado,
    periodo
  ) |>
  dplyr::summarise(
    colonias = dplyr::n_distinct(id_colonia),
    observaciones = dplyr::n(),
    media_incidentes = mean(n_incidentes),
    mediana_incidentes = median(n_incidentes),
    sd_incidentes = sd(n_incidentes),
    .groups = "drop"
  )

print(resumen_pre_post)


# ------------------------------------------------------------------------------
# 12. Diagnóstico de ventana cercana al evento
# ------------------------------------------------------------------------------

ventana_evento <- resumen_mensual |>
  dplyr::filter(
    mes >= as.Date("2022-09-01"),
    mes <= as.Date("2024-02-01")
  )

print(
  ventana_evento,
  n = Inf
)


# ------------------------------------------------------------------------------
# 13. Guardar resumen pre/post
# ------------------------------------------------------------------------------

readr::write_csv(
  resumen_pre_post,
  fs::path(
    rutas$output_tables,
    "401_resumen_pre_post_2023.csv"
  )
)


# ------------------------------------------------------------------------------
# 14. Resumen final
# ------------------------------------------------------------------------------

message("")
message("==============================================================")
message("DESCRIPTIVOS EVENTO 2023")
message("==============================================================")
message(
  "Colonias tratadas: ",
  dplyr::n_distinct(
    panel$id_colonia[
      panel$tratado == 1
    ]
  )
)
message(
  "Colonias control:  ",
  dplyr::n_distinct(
    panel$id_colonia[
      panel$tratado == 0
    ]
  )
)
message(
  "Periodo:           ",
  min(panel$mes),
  " a ",
  max(panel$mes)
)
message("==============================================================")