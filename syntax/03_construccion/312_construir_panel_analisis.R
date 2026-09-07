# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 312_construir_panel_analisis.R
# Objetivo:
#   - Construir el panel colonia-mes con todos los outcomes disponibles
#     (incidentes C5, víctimas de tránsito FGJ, infracciones)
#   - Cada fuente con su indicador de cobertura temporal
#
# Entradas:
#   data/processed/colonias_limpias.gpkg
#   data/final/panel_colonia_mes.parquet                (incidentes C5)
#   data/processed/victimas_transito_colonia.parquet
#   data/processed/infracciones_colonia.parquet
#
# Salida:
#   data/final/panel_analisis_colonia_mes.parquet
#
# Regla:
#   Este script NO define tratamiento ni eventos (eso va en 507).
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------------------------

source(
  here::here("syntax", "00_setup", "000_setup.R"),
  encoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 1. Catálogo de colonias y esqueleto temporal
# ------------------------------------------------------------------------------

catalogo <- sf::st_read(
  fs::path(rutas$data_processed, "colonias_limpias.gpkg"),
  quiet = TRUE
) |>
  sf::st_drop_geometry() |>
  dplyr::select(id_colonia, colonia, id_alcaldia, alcaldia, area_km2)

meses <- seq.Date(
  from = as.Date("2016-01-01"),
  to   = as.Date("2024-07-01"),
  by   = "month"
)

panel <- tidyr::crossing(
  catalogo,
  tibble::tibble(mes = meses)
)

message("Esqueleto: ", nrow(panel), " filas (",
        dplyr::n_distinct(panel$id_colonia), " colonias x ",
        length(meses), " meses)")


# ------------------------------------------------------------------------------
# 2. Outcome — incidentes C5
# ------------------------------------------------------------------------------

c5 <- arrow::read_parquet(
  fs::path(rutas$data_final, "panel_colonia_mes.parquet")
) |>
  dplyr::select(id_colonia, mes, n_incidentes)

cobertura_c5 <- range(c5$mes)

panel <- panel |>
  dplyr::left_join(c5, by = c("id_colonia", "mes")) |>
  dplyr::mutate(
    c5_cobertura = mes >= cobertura_c5[1] & mes <= cobertura_c5[2],
    n_incidentes = dplyr::if_else(c5_cobertura, tidyr::replace_na(n_incidentes, 0), NA_real_)
  )


# ------------------------------------------------------------------------------
# 3. Outcome — víctimas de tránsito FGJ
# ------------------------------------------------------------------------------

victimas <- arrow::read_parquet(
  fs::path(rutas$data_processed, "victimas_transito_colonia.parquet")
) |>
  dplyr::filter(!is.na(id_colonia))

cobertura_vic <- range(victimas$mes)

victimas_mes <- victimas |>
  dplyr::group_by(id_colonia, mes) |>
  dplyr::summarise(
    n_victimas = dplyr::n(),
    n_fallecidos = sum(gravedad == "fallecido"),
    n_lesionados = sum(gravedad == "lesionado"),
    n_atropellados = sum(tipo_siniestro == "atropellado"),
    .groups = "drop"
  )

panel <- panel |>
  dplyr::left_join(victimas_mes, by = c("id_colonia", "mes")) |>
  dplyr::mutate(
    victimas_cobertura = mes >= cobertura_vic[1] & mes <= cobertura_vic[2],
    dplyr::across(
      c(n_victimas, n_fallecidos, n_lesionados, n_atropellados),
      ~ dplyr::if_else(victimas_cobertura, tidyr::replace_na(.x, 0), NA_real_)
    )
  )


# ------------------------------------------------------------------------------
# 4. Medida de enforcement — infracciones
# ------------------------------------------------------------------------------

infracciones <- arrow::read_parquet(
  fs::path(rutas$data_processed, "infracciones_colonia.parquet")
) |>
  dplyr::filter(!is.na(id_colonia))

# Meses con problemas de fuente que no deben tratarse como cobertura válida:
#   - abr-2021: ausente por completo en la fuente
#   - may/jun-2021 (bimestre 2021_b3): ~67% de registros con colonia = DESCONOCIDO
meses_excluidos <- as.Date(c("2021-04-01", "2021-05-01", "2021-06-01"))

meses_infr <- sort(setdiff(unique(infracciones$mes), meses_excluidos)) |>
  as.Date(origin = "1970-01-01")

infracciones_mes <- infracciones |>
  dplyr::group_by(id_colonia, mes) |>
  dplyr::summarise(
    n_infracciones = dplyr::n(),
    n_infracciones_velocidad = sum(tipo_infraccion == "velocidad"),
    .groups = "drop"
  )

panel <- panel |>
  dplyr::left_join(infracciones_mes, by = c("id_colonia", "mes")) |>
  dplyr::mutate(
    infracciones_cobertura = mes %in% meses_infr,
    dplyr::across(
      c(n_infracciones, n_infracciones_velocidad),
      ~ dplyr::if_else(infracciones_cobertura, tidyr::replace_na(.x, 0), NA_real_)
    )
  )


# ------------------------------------------------------------------------------
# 5. Variables temporales
# ------------------------------------------------------------------------------

panel <- panel |>
  dplyr::mutate(
    anio = lubridate::year(mes),
    numero_mes = lubridate::month(mes)
  ) |>
  dplyr::arrange(id_colonia, mes)


# ------------------------------------------------------------------------------
# 6. Diagnóstico
# ------------------------------------------------------------------------------

message("")
message("==============================================================")
message("PANEL DE ANÁLISIS COLONIA-MES")
message("==============================================================")
message("Filas:              ", nrow(panel))
message("Colonias:           ", dplyr::n_distinct(panel$id_colonia))
message("Periodo:            ", min(panel$mes), " a ", max(panel$mes))
message("C5 cobertura:       ", cobertura_c5[1], " a ", cobertura_c5[2])
message("Víctimas cobertura: ", cobertura_vic[1], " a ", cobertura_vic[2])
message("Infracc. cobertura: ", min(meses_infr), " a ", max(meses_infr))
message("==============================================================")

resumen_outcomes <- panel |>
  dplyr::summarise(
    incidentes_media = round(mean(n_incidentes, na.rm = TRUE), 2),
    victimas_media   = round(mean(n_victimas, na.rm = TRUE), 3),
    victimas_pct_cero = round(100 * mean(n_victimas == 0, na.rm = TRUE), 1),
    fallecidos_total = sum(n_fallecidos, na.rm = TRUE)
  )
print(as.data.frame(resumen_outcomes))


# ------------------------------------------------------------------------------
# 7. Guardar
# ------------------------------------------------------------------------------

archivo_salida <- fs::path(
  rutas$data_final,
  "panel_analisis_colonia_mes.parquet"
)

arrow::write_parquet(panel, archivo_salida)

message("")
message("Panel guardado en: ", archivo_salida)
