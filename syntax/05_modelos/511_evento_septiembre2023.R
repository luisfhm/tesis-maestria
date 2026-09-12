# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 511_evento_septiembre2023.R
# Objetivo:
#   Repetir el diseño de 510 (abril de 2022) para el evento de septiembre de
#   2023 (reforma para motociclistas), como robustez pedida por el asesor:
#   si el resultado nulo se repite en un segundo evento, la conclusión no
#   depende de uno solo.
#
#   Diferencia con 510: no se estima el mecanismo (infracciones). La fuente de
#   infracciones termina en abril de 2023, antes del evento (sep-2023), por lo
#   que no hay un solo mes posterior con datos de multas — ni siquiera el mes
#   de referencia (k=-1 = agosto 2023) tiene cobertura. No es estimable.
#
# Entradas:
#   data/final/panel_analisis_colonia_mes.parquet
#   data/processed/exposicion_radares_colonia_anual.parquet
#
# Salidas:
#   output/tables/511_event_study_coeficientes.csv
#   output/tables/511_did_agregado.csv
#   output/figures/511_victimas.png
#   output/figures/511_incidentes_c5.png
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------------------------

source(
  here::here("syntax", "00_setup", "000_setup.R"),
  encoding = "UTF-8"
)

EVENTO          <- as.Date("2023-09-01")
ANIO_INVENTARIO <- 2022
VENTANA         <- 10
BUFFERS         <- c(250, 500, 1000)

OUTCOMES <- tibble::tribble(
  ~outcome,       ~rol,        ~etiqueta,
  "n_victimas",   "principal", "Víctimas de tránsito (FGJ)",
  "n_fallecidos", "principal", "Víctimas fallecidas",
  "n_incidentes", "secundario","Incidentes C5"
)


# ------------------------------------------------------------------------------
# 1. Datos
# ------------------------------------------------------------------------------

panel <- arrow::read_parquet(
  fs::path(rutas$data_final, "panel_analisis_colonia_mes.parquet")
) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    tiempo_evento =
      (lubridate::year(mes) - lubridate::year(EVENTO)) * 12 +
      (lubridate::month(mes) - lubridate::month(EVENTO))
  )

exposicion <- arrow::read_parquet(
  fs::path(rutas$data_processed, "exposicion_radares_colonia_anual.parquet")
) |>
  tibble::as_tibble() |>
  dplyr::filter(anio == ANIO_INVENTARIO)


# ------------------------------------------------------------------------------
# 2. Función: un outcome, un buffer (idéntica a 510)
# ------------------------------------------------------------------------------

estimar <- function(outcome, buffer_m, etiqueta, rol) {

  trato <- exposicion |>
    dplyr::filter(buffer_m == !!buffer_m) |>
    dplyr::transmute(id_colonia, tratada = as.integer(n_dispositivos > 0))

  datos <- panel |>
    dplyr::inner_join(trato, by = "id_colonia") |>
    dplyr::mutate(y = .data[[outcome]]) |>
    dplyr::filter(
      !is.na(y),
      tiempo_evento >= -VENTANA,
      tiempo_evento <= VENTANA
    )

  m_es <- fixest::feols(
    y ~ fixest::i(tiempo_evento, tratada, ref = -1) | id_colonia + mes,
    cluster = ~ id_colonia, data = datos
  )

  wald_pre <- tryCatch(
    fixest::wald(m_es, keep = "tiempo_evento::-[0-9]+:tratada", print = FALSE),
    error = function(e) list(stat = NA_real_, p = NA_real_)
  )

  coefs <- broom::tidy(m_es, conf.int = TRUE) |>
    dplyr::filter(stringr::str_detect(term, "tiempo_evento")) |>
    dplyr::mutate(tiempo_evento = as.integer(stringr::str_extract(term, "-?\\d+"))) |>
    dplyr::bind_rows(tibble::tibble(
      tiempo_evento = -1L, estimate = 0, std.error = NA, conf.low = 0, conf.high = 0
    )) |>
    dplyr::arrange(tiempo_evento) |>
    dplyr::mutate(outcome = outcome, etiqueta = etiqueta, rol = rol, buffer_m = buffer_m)

  m_did <- fixest::feols(
    y ~ I(tratada * (tiempo_evento >= 0)) | id_colonia + mes,
    cluster = ~ id_colonia, data = datos
  )
  b <- stats::coef(m_did)[1]
  se <- sqrt(stats::vcov(m_did)[1, 1])

  media_trat_pre <- datos |>
    dplyr::filter(tratada == 1, tiempo_evento < 0) |>
    dplyr::summarise(m = mean(y)) |> dplyr::pull(m)

  did <- tibble::tibble(
    outcome = outcome, etiqueta = etiqueta, rol = rol, buffer_m = buffer_m,
    n_tratadas = dplyr::n_distinct(datos$id_colonia[datos$tratada == 1]),
    n_obs = nrow(datos),
    media_tratada_pre = round(media_trat_pre, 3),
    att = round(b, 4),
    ee = round(se, 4),
    att_pct = round(100 * b / media_trat_pre, 1),
    p_att = round(2 * stats::pnorm(-abs(b / se)), 4),
    wald_pre_p = signif(wald_pre$p, 3)
  )

  list(coeficientes = coefs, did = did)
}


# ------------------------------------------------------------------------------
# 3. Correr todo
# ------------------------------------------------------------------------------

grid <- tidyr::expand_grid(OUTCOMES, buffer_m = BUFFERS)

res <- purrr::pmap(
  grid,
  function(outcome, rol, etiqueta, buffer_m) {
    message("  ", etiqueta, " | ", buffer_m, "m")
    estimar(outcome, buffer_m, etiqueta, rol)
  }
)

coeficientes <- purrr::map_dfr(res, "coeficientes")
did <- purrr::map_dfr(res, "did")

message("")
message("==============================================================")
message("EFECTO PROMEDIO POSTERIOR — SEPTIEMBRE 2023 (ventana -10/+10)")
message("==============================================================")
print(as.data.frame(did), row.names = FALSE)


# ------------------------------------------------------------------------------
# 4. Guardar tablas
# ------------------------------------------------------------------------------

readr::write_csv(coeficientes, fs::path(rutas$output_tables, "511_event_study_coeficientes.csv"))
readr::write_csv(did, fs::path(rutas$output_tables, "511_did_agregado.csv"))


# ------------------------------------------------------------------------------
# 5. Figuras
# ------------------------------------------------------------------------------

graf_es <- function(df, titulo, subtitulo) {
  ggplot2::ggplot(df, ggplot2::aes(tiempo_evento, estimate)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
    ggplot2::geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey40") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = conf.low, ymax = conf.high),
                           width = 0.2, color = "grey30") +
    ggplot2::geom_point(size = 1.7) +
    ggplot2::facet_grid(etiqueta ~ buffer_m, scales = "free_y",
                        labeller = ggplot2::labeller(buffer_m = function(x) paste0(x, " m"))) +
    ggplot2::labs(title = titulo, subtitle = subtitulo,
                  x = "Mes relativo a septiembre de 2023",
                  y = "Efecto (colonias tratadas vs. control)") +
    ggplot2::theme_minimal(base_size = 12)
}

ggplot2::ggsave(
  fs::path(rutas$output_figures, "511_victimas.png"),
  graf_es(dplyr::filter(coeficientes, outcome %in% c("n_victimas", "n_fallecidos")),
          "Efecto de la reforma para motociclistas (septiembre 2023)",
          "Outcome principal: víctimas de hechos de tránsito"),
  width = 12, height = 7, dpi = 300
)

ggplot2::ggsave(
  fs::path(rutas$output_figures, "511_incidentes_c5.png"),
  graf_es(dplyr::filter(coeficientes, outcome == "n_incidentes"),
          "Outcome secundario: incidentes C5 (septiembre 2023)",
          "Ventana post truncada a +5 por cobertura de C5; interpretar con cautela"),
  width = 12, height = 5, dpi = 300
)

message("")
message("Listo. Tablas y figuras en output/.")
