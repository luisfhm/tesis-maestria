# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 508_primera_etapa_enforcement.R
# Objetivo:
#   - Primera etapa de la estrategia de doble diferencia sobre enforcement:
#     ¿las infracciones (multas) aumentan diferencialmente cerca de la
#     infraestructura de fiscalización después de un cambio de política?
#   - Eventos con cobertura de multas: feb-2021 y abr-2022.
#   - Outcomes: total de infracciones e infracciones de velocidad (art. 9).
#
# Entradas:
#   data/final/panel_analisis_colonia_mes.parquet
#   data/processed/exposicion_radares_colonia_anual.parquet
#
# Salidas:
#   output/tables/508_primera_etapa_coeficientes.csv
#   output/tables/508_primera_etapa_resumen.csv
#   output/figures/508_primera_etapa.png
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------------------------

source(
  here::here("syntax", "00_setup", "000_setup.R"),
  encoding = "UTF-8"
)

panel <- arrow::read_parquet(
  fs::path(rutas$data_final, "panel_analisis_colonia_mes.parquet")
) |>
  tibble::as_tibble()

exposicion <- arrow::read_parquet(
  fs::path(rutas$data_processed, "exposicion_radares_colonia_anual.parquet")
) |>
  tibble::as_tibble()


# ------------------------------------------------------------------------------
# 1. Función de estimación
# ------------------------------------------------------------------------------

estimar <- function(evento_fecha, anio_inventario, buffer_m, outcome,
                    ventana_pre, ventana_post, etiqueta) {

  evento_fecha <- as.Date(evento_fecha)

  trato <- exposicion |>
    dplyr::filter(anio == anio_inventario, buffer_m == !!buffer_m) |>
    dplyr::transmute(id_colonia, tratada = as.integer(n_dispositivos > 0))

  datos <- panel |>
    dplyr::inner_join(trato, by = "id_colonia") |>
    dplyr::mutate(
      tiempo_evento =
        (lubridate::year(mes) - lubridate::year(evento_fecha)) * 12 +
        (lubridate::month(mes) - lubridate::month(evento_fecha)),
      y = .data[[outcome]]
    ) |>
    dplyr::filter(
      !is.na(y),
      tiempo_evento >= -ventana_pre,
      tiempo_evento <= ventana_post
    )

  modelo <- fixest::feols(
    y ~ fixest::i(tiempo_evento, tratada, ref = -1) | id_colonia + mes,
    cluster = ~ id_colonia,
    data = datos
  )

  wald_pre <- tryCatch(
    fixest::wald(modelo, keep = "tiempo_evento::-[0-9]+:tratada", print = FALSE),
    error = function(e) list(stat = NA_real_, p = NA_real_)
  )

  # Efecto post promedio (agregado)
  modelo_post <- fixest::feols(
    y ~ I(tratada * (tiempo_evento >= 0)) | id_colonia + mes,
    cluster = ~ id_colonia,
    data = datos
  )
  b_post <- stats::coef(modelo_post)[1]
  se_post <- sqrt(stats::vcov(modelo_post)[1, 1])

  coefs <- broom::tidy(modelo, conf.int = TRUE) |>
    dplyr::filter(stringr::str_detect(term, "tiempo_evento")) |>
    dplyr::mutate(tiempo_evento = as.integer(stringr::str_extract(term, "-?\\d+"))) |>
    dplyr::bind_rows(tibble::tibble(
      tiempo_evento = -1L, estimate = 0, std.error = NA, conf.low = 0, conf.high = 0
    )) |>
    dplyr::arrange(tiempo_evento) |>
    dplyr::mutate(escenario = etiqueta, evento = format(evento_fecha, "%Y-%m"),
                  buffer_m = buffer_m, outcome = outcome)

  media_control_pre <- datos |>
    dplyr::filter(tratada == 0, tiempo_evento < 0) |>
    dplyr::summarise(m = mean(y)) |> dplyr::pull(m)
  media_trat_pre <- datos |>
    dplyr::filter(tratada == 1, tiempo_evento < 0) |>
    dplyr::summarise(m = mean(y)) |> dplyr::pull(m)

  resumen <- tibble::tibble(
    escenario = etiqueta, evento = format(evento_fecha, "%Y-%m"),
    buffer_m = buffer_m, outcome = outcome,
    media_control_pre = round(media_control_pre, 2),
    media_tratada_pre = round(media_trat_pre, 2),
    efecto_post = round(b_post, 3),
    ee_post = round(se_post, 3),
    efecto_post_pct = round(100 * b_post / media_trat_pre, 1),
    wald_pre_p = wald_pre$p
  )

  list(coeficientes = coefs, resumen = resumen)
}


# ------------------------------------------------------------------------------
# 2. Escenarios (solo eventos con cobertura de multas)
# ------------------------------------------------------------------------------

escenarios <- tidyr::expand_grid(
  tibble::tribble(
    ~evento_fecha, ~anio_inventario, ~ventana_pre, ~ventana_post,
    "2021-02-01",  2020,             11,           12,
    "2022-04-01",  2021,             12,           12
  ),
  buffer_m = c(250, 500, 1000),
  outcome = c("n_infracciones", "n_infracciones_velocidad")
) |>
  dplyr::mutate(
    etiqueta = paste0(evento_fecha, " | ", outcome, " | ", buffer_m, "m")
  )

resultados <- purrr::pmap(
  escenarios,
  function(evento_fecha, anio_inventario, ventana_pre, ventana_post, buffer_m, outcome, etiqueta) {
    message("Estimando: ", etiqueta)
    estimar(evento_fecha, anio_inventario, buffer_m, outcome,
            ventana_pre, ventana_post, etiqueta)
  }
)

coeficientes <- purrr::map_dfr(resultados, "coeficientes")
resumen <- purrr::map_dfr(resultados, "resumen")

message("")
message("==============================================================")
message("PRIMERA ETAPA — ENFORCEMENT DIFERENCIAL")
message("==============================================================")
print(as.data.frame(resumen), row.names = FALSE)


# ------------------------------------------------------------------------------
# 3. Guardar
# ------------------------------------------------------------------------------

readr::write_csv(coeficientes, fs::path(rutas$output_tables, "508_primera_etapa_coeficientes.csv"))
readr::write_csv(resumen, fs::path(rutas$output_tables, "508_primera_etapa_resumen.csv"))

g <- coeficientes |>
  ggplot2::ggplot(ggplot2::aes(tiempo_evento, estimate)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
  ggplot2::geom_vline(xintercept = -0.5, linetype = "dashed") +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  ggplot2::geom_point(size = 1.5) +
  ggplot2::facet_grid(outcome ~ evento + buffer_m, scales = "free_y",
                      labeller = ggplot2::label_both) +
  ggplot2::labs(
    title = "Primera etapa: infracciones cerca de la infraestructura alrededor de cada evento",
    x = "Mes relativo al evento", y = "Efecto estimado (multas)"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  fs::path(rutas$output_figures, "508_primera_etapa.png"),
  g, width = 14, height = 7, dpi = 300
)

message("")
message("Listo.")
