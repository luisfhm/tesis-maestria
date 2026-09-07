# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 309_construir_panel_infracciones.R
# Objetivo:
#   - Agregar las infracciones asignadas a nivel colonia-mes
#   - Distinguir infracciones de velocidad (art. 9) del resto
#   - Incorporarlas al panel colonia-mes con un indicador de cobertura
#
# Entradas:
#   data/final/panel_colonia_mes.parquet
#   data/processed/infracciones_colonia.parquet
#
# Salida:
#   data/final/panel_colonia_mes_infracciones.parquet
#
# Nota sobre cobertura:
#   La fuente de infracciones solo cubre ene-2020 a abr-2023 (falta abr-2021).
#   Fuera de ese rango los conteos se dejan como NA, no como cero.
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
# 1. Leer bases
# ------------------------------------------------------------------------------

archivo_panel <- fs::path(
  rutas$data_final,
  "panel_colonia_mes.parquet"
)

archivo_infracciones <- fs::path(
  rutas$data_processed,
  "infracciones_colonia.parquet"
)

validar_archivo(archivo_panel)
validar_archivo(archivo_infracciones)

panel <- arrow::read_parquet(archivo_panel) |>
  tibble::as_tibble()

infracciones <- arrow::read_parquet(archivo_infracciones) |>
  tibble::as_tibble()


# ------------------------------------------------------------------------------
# 2. Determinar cobertura temporal de la fuente
# ------------------------------------------------------------------------------

meses_con_datos <- infracciones |>
  dplyr::filter(!is.na(id_colonia)) |>
  dplyr::distinct(mes) |>
  dplyr::arrange(mes) |>
  dplyr::pull(mes)

message(
  "Cobertura infracciones: ",
  min(meses_con_datos), " a ", max(meses_con_datos),
  " (", length(meses_con_datos), " meses)"
)


# ------------------------------------------------------------------------------
# 3. Agregar a colonia-mes
# ------------------------------------------------------------------------------

infracciones_mes <- infracciones |>
  dplyr::filter(!is.na(id_colonia)) |>
  dplyr::group_by(id_colonia, mes) |>
  dplyr::summarise(
    n_infracciones = dplyr::n(),
    n_infracciones_velocidad = sum(tipo_infraccion == "velocidad"),
    n_infracciones_otra = sum(tipo_infraccion == "otra"),
    .groups = "drop"
  )


# ------------------------------------------------------------------------------
# 4. Incorporar al panel
# ------------------------------------------------------------------------------

panel_infracciones <- panel |>
  dplyr::left_join(
    infracciones_mes,
    by = c("id_colonia", "mes")
  ) |>
  dplyr::mutate(
    infracciones_cobertura = mes %in% meses_con_datos,

    dplyr::across(
      c(n_infracciones, n_infracciones_velocidad, n_infracciones_otra),
      ~ dplyr::if_else(infracciones_cobertura, tidyr::replace_na(.x, 0), NA_real_)
    )
  )


# ------------------------------------------------------------------------------
# 5. Diagnóstico
# ------------------------------------------------------------------------------

resumen <- panel_infracciones |>
  dplyr::filter(infracciones_cobertura) |>
  dplyr::summarise(
    obs = dplyr::n(),
    total_infracciones = sum(n_infracciones),
    media_colonia_mes = round(mean(n_infracciones), 2),
    mediana_colonia_mes = median(n_infracciones),
    max_colonia_mes = max(n_infracciones),
    pct_cero = round(100 * mean(n_infracciones == 0), 1),
    pct_velocidad = round(100 * sum(n_infracciones_velocidad) / sum(n_infracciones), 1)
  )

message("")
message("==============================================================")
message("PANEL COLONIA-MES CON INFRACCIONES (meses con cobertura)")
message("==============================================================")
print(as.data.frame(resumen))
message("==============================================================")


# ------------------------------------------------------------------------------
# 6. Guardar
# ------------------------------------------------------------------------------

archivo_salida <- fs::path(
  rutas$data_final,
  "panel_colonia_mes_infracciones.parquet"
)

arrow::write_parquet(panel_infracciones, archivo_salida)

message("")
message("Panel guardado en: ", archivo_salida)
