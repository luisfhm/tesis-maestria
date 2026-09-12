# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 604_robustez_septiembre2023.R
# Objetivo:
#   Robustez del evento de septiembre de 2023 (511), en paralelo a 603:
#     A. Modelo de conteo (Poisson efectos fijos)
#     B. Tratamiento continuo (área expuesta y nº de dispositivos)
#     C. Sensibilidad a la ventana pretratamiento (-8, -10, -12)
#   No incluye infracciones: la fuente no tiene cobertura alrededor de este
#   evento (termina en abril de 2023, antes de septiembre de 2023).
#
# Entradas:
#   data/final/panel_analisis_colonia_mes.parquet
#   data/processed/exposicion_radares_colonia_anual.parquet
#
# Salidas:
#   output/tables/604_robustez_att.csv
#   output/figures/604_robustez_att.png
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------------------------

source(here::here("syntax", "00_setup", "000_setup.R"), encoding = "UTF-8")

EVENTO          <- as.Date("2023-09-01")
ANIO_INVENTARIO <- 2022
BUFFER_MAIN     <- 500

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
# 1. Base con tratamiento (binario y continuo) para un buffer
# ------------------------------------------------------------------------------

base_buffer <- function(buffer_m) {
  trato <- exposicion |>
    dplyr::filter(buffer_m == !!buffer_m) |>
    dplyr::transmute(
      id_colonia,
      trat_bin  = as.integer(n_dispositivos > 0),
      trat_area = share_area_expuesta,
      trat_disp = asinh(n_dispositivos)
    )
  dplyr::inner_join(panel, trato, by = "id_colonia")
}


# ------------------------------------------------------------------------------
# 2. ATT agrupado bajo distintas variantes (idéntico a 603)
# ------------------------------------------------------------------------------

att_variante <- function(datos, outcome, trato_var, ventana_pre,
                         ref = -1, familia = "ols", etiqueta) {

  d <- datos |>
    dplyr::mutate(y = .data[[outcome]], D = .data[[trato_var]]) |>
    dplyr::filter(!is.na(y), tiempo_evento >= -ventana_pre, tiempo_evento <= 10)

  form <- y ~ I(D * (tiempo_evento >= 0)) | id_colonia + mes

  modelo <- tryCatch(
    if (familia == "poisson") {
      fixest::fepois(form, data = d, cluster = ~ id_colonia)
    } else {
      fixest::feols(form, data = d, cluster = ~ id_colonia)
    },
    error = function(e) NULL
  )
  if (is.null(modelo)) return(NULL)

  b  <- stats::coef(modelo)[1]
  se <- sqrt(stats::vcov(modelo)[1, 1])

  media_pre <- d |>
    dplyr::filter(tiempo_evento < 0, D > 0) |>
    dplyr::summarise(m = mean(y)) |> dplyr::pull(m)

  tibble::tibble(
    variante = etiqueta,
    outcome = outcome,
    familia = familia,
    trato = trato_var,
    ventana_pre = ventana_pre,
    ref = ref,
    att = b,
    ee = se,
    efecto_pct = if (familia == "poisson") round(100 * (exp(b) - 1), 1)
                 else round(100 * b / media_pre, 1),
    p = round(2 * stats::pnorm(-abs(b / se)), 4)
  )
}


# ------------------------------------------------------------------------------
# 3. Grid de robustez (sin infracciones)
# ------------------------------------------------------------------------------

d500 <- base_buffer(500)
d250 <- base_buffer(250)

outcomes <- c("n_victimas", "n_fallecidos")

resultados <- dplyr::bind_rows(
  purrr::map_dfr(outcomes, ~ att_variante(d500, .x, "trat_bin", 10, familia = "ols",
                                          etiqueta = "OLS · binario · -10 · 500m")),
  purrr::map_dfr(outcomes, ~ att_variante(d500, .x, "trat_bin", 10, familia = "poisson",
                                          etiqueta = "Poisson · binario · -10 · 500m")),
  purrr::map_dfr(outcomes, ~ att_variante(d500, .x, "trat_area", 10, familia = "ols",
                                          etiqueta = "OLS · área expuesta · -10 · 500m")),
  purrr::map_dfr(outcomes, ~ att_variante(d500, .x, "trat_disp", 10, familia = "ols",
                                          etiqueta = "OLS · asinh(nº disp.) · -10 · 500m")),
  purrr::map_dfr(outcomes, ~ att_variante(d500, .x, "trat_bin", 8, familia = "ols",
                                          etiqueta = "OLS · binario · -8 · 500m")),
  purrr::map_dfr(outcomes, ~ att_variante(d500, .x, "trat_bin", 12, familia = "ols",
                                          etiqueta = "OLS · binario · -12 · 500m")),
  purrr::map_dfr(outcomes, ~ att_variante(d250, .x, "trat_bin", 10, familia = "ols",
                                          etiqueta = "OLS · binario · -10 · 250m")),
  purrr::map_dfr(outcomes, ~ att_variante(d250, .x, "trat_bin", 10, familia = "poisson",
                                          etiqueta = "Poisson · binario · -10 · 250m"))
)

resultados <- resultados |>
  dplyr::mutate(dplyr::across(c(att, ee), ~ round(.x, 4)))

message("")
message("==============================================================")
message("ROBUSTEZ — ATT SEPTIEMBRE 2023")
message("==============================================================")
print(as.data.frame(resultados), row.names = FALSE)

readr::write_csv(resultados, fs::path(rutas$output_tables, "604_robustez_att.csv"))


# ------------------------------------------------------------------------------
# 4. Figura
# ------------------------------------------------------------------------------

g <- resultados |>
  dplyr::mutate(
    outcome = dplyr::recode(outcome, n_victimas = "Víctimas", n_fallecidos = "Fallecidos")
  ) |>
  ggplot2::ggplot(ggplot2::aes(efecto_pct, variante)) +
  ggplot2::geom_vline(xintercept = 0, linetype = "dotted") +
  ggplot2::geom_point(ggplot2::aes(color = p < 0.05), size = 2.5) +
  ggplot2::scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "grey60"),
                              name = "p < 0.05") +
  ggplot2::facet_wrap(~ outcome, scales = "free_x") +
  ggplot2::labs(
    title = "Robustez del ATT — septiembre de 2023",
    subtitle = "Efecto porcentual sobre el grupo tratado; negro = p < 0.05",
    x = "Efecto estimado (%)", y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11)

ggplot2::ggsave(
  fs::path(rutas$output_figures, "604_robustez_att.png"),
  g, width = 11, height = 6, dpi = 300
)

message("")
message("Listo.")
