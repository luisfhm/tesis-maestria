# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 507_event_study_framework.R
# Objetivo:
#   - Estudio de evento mes a mes, generalizado a:
#       * varios eventos candidatos (feb-2021, abr-2022, sep-2023)
#       * varias definiciones espaciales de tratamiento (buffer 250/500/1000 m)
#       * varios outcomes (incidentes C5, víctimas de tránsito)
#   - Implementa la sugerencia del asesor: T_i = exposición a infraestructura
#     preexistente, interactuada con el tiempo relativo a cada evento.
#
# Entradas:
#   data/final/panel_analisis_colonia_mes.parquet
#   data/processed/exposicion_radares_colonia_anual.parquet
#
# Salidas:
#   output/tables/507_event_study_coeficientes.csv
#   output/tables/507_event_study_resumen.csv
#   output/figures/507_event_study_sep2023.png
#   output/figures/507_event_study_multievento.png
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------------------------

source(
  here::here("syntax", "00_setup", "000_setup.R"),
  encoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 1. Datos
# ------------------------------------------------------------------------------

panel <- arrow::read_parquet(
  fs::path(rutas$data_final, "panel_analisis_colonia_mes.parquet")
) |>
  tibble::as_tibble()

exposicion <- arrow::read_parquet(
  fs::path(rutas$data_processed, "exposicion_radares_colonia_anual.parquet")
) |>
  tibble::as_tibble()


# ------------------------------------------------------------------------------
# 2. Función principal
# ------------------------------------------------------------------------------

run_event_study <- function(evento_fecha,
                            anio_inventario,
                            buffer_m,
                            outcome,
                            ventana_pre = 12,
                            ventana_post = 12,
                            etiqueta = NULL) {

  evento_fecha <- as.Date(evento_fecha)

  # --- Definición de tratamiento: exposición al inventario preexistente -------
  trato <- exposicion |>
    dplyr::filter(anio == anio_inventario, buffer_m == !!buffer_m) |>
    dplyr::transmute(
      id_colonia,
      tratada = as.integer(n_dispositivos > 0),
      n_dispositivos,
      share_area_expuesta
    )

  # --- Muestra --------------------------------------------------------------
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

  if (nrow(datos) == 0 || dplyr::n_distinct(datos$tiempo_evento) < 3) {
    warning("Muestra insuficiente para ", etiqueta)
    return(NULL)
  }

  # --- Estimación ---------------------------------------------------------
  modelo <- fixest::feols(
    y ~ fixest::i(tiempo_evento, tratada, ref = -1) | id_colonia + mes,
    cluster = ~ id_colonia,
    data = datos
  )

  # --- Prueba conjunta de pre-tendencias --------------------------------
  wald_pre <- tryCatch(
    fixest::wald(modelo, keep = "tiempo_evento::-[0-9]+:tratada", print = FALSE),
    error = function(e) list(stat = NA_real_, p = NA_real_)
  )

  # --- Coeficientes -----------------------------------------------------
  coefs <- broom::tidy(modelo, conf.int = TRUE) |>
    dplyr::filter(stringr::str_detect(term, "tiempo_evento")) |>
    dplyr::mutate(
      tiempo_evento = as.integer(stringr::str_extract(term, "-?\\d+"))
    ) |>
    dplyr::bind_rows(
      tibble::tibble(tiempo_evento = -1L, estimate = 0,
                     std.error = NA_real_, conf.low = 0, conf.high = 0)
    ) |>
    dplyr::arrange(tiempo_evento) |>
    dplyr::mutate(
      escenario = etiqueta,
      evento = format(evento_fecha, "%Y-%m"),
      buffer_m = buffer_m,
      outcome = outcome
    )

  media_control <- datos |>
    dplyr::filter(tratada == 0, tiempo_evento < 0) |>
    dplyr::summarise(m = mean(y)) |>
    dplyr::pull(m)

  resumen <- tibble::tibble(
    escenario = etiqueta,
    evento = format(evento_fecha, "%Y-%m"),
    anio_inventario = anio_inventario,
    buffer_m = buffer_m,
    outcome = outcome,
    ventana = paste0("-", ventana_pre, " a +", ventana_post),
    n_obs = nrow(datos),
    n_colonias = dplyr::n_distinct(datos$id_colonia),
    n_tratadas = dplyr::n_distinct(datos$id_colonia[datos$tratada == 1]),
    media_control_pre = round(media_control, 3),
    wald_pre_stat = unname(wald_pre$stat),
    wald_pre_p = wald_pre$p
  )

  list(coeficientes = coefs, resumen = resumen)
}


# ------------------------------------------------------------------------------
# 3. Grid de escenarios
# ------------------------------------------------------------------------------

eventos <- tibble::tribble(
  ~evento_fecha, ~anio_inventario, ~post_c5, ~post_vic,
  "2021-02-01",  2020,             12,       12,
  "2022-04-01",  2021,             12,       12,
  "2023-09-01",  2022,              5,       10
)

buffers <- c(250, 500, 1000)

escenarios <- tidyr::expand_grid(eventos, buffer_m = buffers) |>
  tidyr::pivot_longer(
    c(post_c5, post_vic),
    names_to = "cual", values_to = "ventana_post"
  ) |>
  dplyr::mutate(
    outcome = dplyr::if_else(cual == "post_c5", "n_incidentes", "n_victimas"),
    etiqueta = paste0(evento_fecha, " | ", outcome, " | ", buffer_m, "m")
  ) |>
  dplyr::select(-cual)


# ------------------------------------------------------------------------------
# 4. Estimar todos los escenarios
# ------------------------------------------------------------------------------

resultados <- purrr::pmap(
  escenarios,
  function(evento_fecha, anio_inventario, buffer_m, ventana_post, outcome, etiqueta) {
    message("Estimando: ", etiqueta)
    run_event_study(
      evento_fecha = evento_fecha,
      anio_inventario = anio_inventario,
      buffer_m = buffer_m,
      outcome = outcome,
      ventana_pre = 12,
      ventana_post = ventana_post,
      etiqueta = etiqueta
    )
  }
)

resultados <- purrr::compact(resultados)

coeficientes <- purrr::map_dfr(resultados, "coeficientes")
resumen <- purrr::map_dfr(resultados, "resumen")

message("")
message("==============================================================")
message("RESUMEN DE ESCENARIOS")
message("==============================================================")
print(as.data.frame(resumen), row.names = FALSE)


# ------------------------------------------------------------------------------
# 5. Guardar tablas
# ------------------------------------------------------------------------------

readr::write_csv(
  coeficientes,
  fs::path(rutas$output_tables, "507_event_study_coeficientes.csv")
)
readr::write_csv(
  resumen,
  fs::path(rutas$output_tables, "507_event_study_resumen.csv")
)


# ------------------------------------------------------------------------------
# 6. Gráfica — evento de septiembre de 2023
# ------------------------------------------------------------------------------

graficar <- function(df, titulo) {
  ggplot2::ggplot(df, ggplot2::aes(tiempo_evento, estimate)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
    ggplot2::geom_vline(xintercept = -0.5, linetype = "dashed") +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.2
    ) +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::facet_grid(outcome ~ buffer_m, scales = "free_y",
                        labeller = ggplot2::label_both) +
    ggplot2::labs(
      title = titulo,
      x = "Mes relativo al evento",
      y = "Efecto estimado (tratadas vs. control)"
    ) +
    ggplot2::theme_minimal()
}

g_sep2023 <- coeficientes |>
  dplyr::filter(evento == "2023-09") |>
  graficar("Estudio de evento — reforma de septiembre de 2023")

ggplot2::ggsave(
  fs::path(rutas$output_figures, "507_event_study_sep2023.png"),
  g_sep2023, width = 12, height = 7, dpi = 300
)


# ------------------------------------------------------------------------------
# 7. Gráfica — comparación multi-evento (incidentes, buffer 500 m)
# ------------------------------------------------------------------------------

g_multi <- coeficientes |>
  dplyr::filter(outcome == "n_incidentes", buffer_m == 500) |>
  ggplot2::ggplot(ggplot2::aes(tiempo_evento, estimate)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
  ggplot2::geom_vline(xintercept = -0.5, linetype = "dashed") +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = conf.low, ymax = conf.high), width = 0.2
  ) +
  ggplot2::geom_point(size = 1.6) +
  ggplot2::facet_wrap(~ evento, scales = "free") +
  ggplot2::labs(
    title = "Estudio de evento por evento candidato — incidentes C5, buffer 500 m",
    x = "Mes relativo al evento",
    y = "Efecto estimado"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(
  fs::path(rutas$output_figures, "507_event_study_multievento.png"),
  g_multi, width = 12, height = 5, dpi = 300
)

message("")
message("Listo. Tablas y figuras en output/.")
