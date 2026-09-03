# ==============================================================================
# 303_diagnosticar_tendencias_pre.R
#
# Objetivo:
#   Evaluar si los radares tratados y sus controles seleccionados presentan
#   trayectorias comparables antes de 2023.
#
# Diagnósticos:
#   1. Serie promedio mensual tratados vs controles
#   2. Serie normalizada (base 2019 = 100)
#   3. Diferencia mensual tratados - controles
#   4. Tendencias lineales pretratamiento
#   5. Tendencias recientes 2019-2022
#
# IMPORTANTE:
#   - Solo se utiliza información hasta diciembre de 2022.
#   - Se conserva la estructura del matching 1:k.
#   - Un control puede aparecer para más de un tratamiento.
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. Configuración
# ------------------------------------------------------------------------------

source(
  file.path(
    "syntax",
    "00_setup",
    "000_setup.R"
  )
)


# ------------------------------------------------------------------------------
# 2. Parámetros
# ------------------------------------------------------------------------------

fecha_inicio_pre <- as.Date("2014-01-01")
fecha_inicio_reciente <- as.Date("2019-01-01")
fecha_fin_pre <- as.Date("2022-12-01")


# ------------------------------------------------------------------------------
# 3. Archivos
# ------------------------------------------------------------------------------

archivo_tratados <- fs::path(
  rutas$data_processed,
  "300_panel_radares_c5.csv"
)

archivo_controles <- fs::path(
  rutas$data_processed,
  "301_panel_controles_c5.csv"
)

archivo_matching <- fs::path(
  rutas$data_processed,
  "302_matching_controles_seleccionados.csv"
)


stopifnot(
  file.exists(archivo_tratados),
  file.exists(archivo_controles),
  file.exists(archivo_matching)
)


# ------------------------------------------------------------------------------
# 4. Leer datos
# ------------------------------------------------------------------------------

panel_tratados <- readr::read_csv(
  archivo_tratados,
  show_col_types = FALSE
)

panel_controles <- readr::read_csv(
  archivo_controles,
  show_col_types = FALSE
)

matching <- readr::read_csv(
  archivo_matching,
  show_col_types = FALSE
)


cat("\nMatching cargado:\n")

cat(
  "Tratamientos:",
  dplyr::n_distinct(matching$id_tratamiento),
  "\n"
)

cat(
  "Controles únicos:",
  dplyr::n_distinct(matching$id_control),
  "\n"
)

cat(
  "Pares tratamiento-control:",
  nrow(matching),
  "\n"
)


# ------------------------------------------------------------------------------
# 5. Preparar panel de tratamientos
# ------------------------------------------------------------------------------

tratados <- panel_tratados |>
  dplyr::filter(
    tratamiento_500m
  ) |>
  dplyr::transmute(
    id_tratamiento = id_radar,
    mes = as.Date(mes),
    accidentes_tratado = accidentes_500m
  ) |>
  dplyr::filter(
    mes >= fecha_inicio_pre,
    mes <= fecha_fin_pre
  )


# ------------------------------------------------------------------------------
# 6. Preparar panel de controles
# ------------------------------------------------------------------------------

controles <- panel_controles |>
  dplyr::transmute(
    id_control,
    mes = as.Date(mes),
    accidentes_control = accidentes_500m
  ) |>
  dplyr::filter(
    mes >= fecha_inicio_pre,
    mes <= fecha_fin_pre
  )


# ------------------------------------------------------------------------------
# 7. Construir panel matched pair × mes
#
# Cada tratamiento se compara únicamente con sus 5 controles seleccionados.
# Si un control fue seleccionado para dos tratamientos, aparece en ambos grupos.
# ------------------------------------------------------------------------------

panel_matched <- matching |>
  dplyr::select(
    id_tratamiento,
    id_control,
    ranking_control,
    distancia_matching
  ) |>
  dplyr::left_join(
    tratados,
    by = "id_tratamiento",
    relationship = "many-to-many"
  ) |>
  dplyr::left_join(
    controles,
    by = c(
      "id_control",
      "mes"
    )
  )


cat(
  "\nFilas panel matched:",
  nrow(panel_matched),
  "\n"
)


cat(
  "Valores faltantes tratados:",
  sum(is.na(panel_matched$accidentes_tratado)),
  "\n"
)

cat(
  "Valores faltantes controles:",
  sum(is.na(panel_matched$accidentes_control)),
  "\n"
)


# ------------------------------------------------------------------------------
# 8. Promedio de los controles para cada tratamiento y mes
#
# Esto evita que un tratamiento tenga más peso simplemente porque algún
# control fue reutilizado por otro tratamiento.
# ------------------------------------------------------------------------------

panel_tratamiento_mes <- panel_matched |>
  dplyr::group_by(
    id_tratamiento,
    mes
  ) |>
  dplyr::summarise(
    accidentes_tratado =
      dplyr::first(
        accidentes_tratado
      ),

    accidentes_control =
      mean(
        accidentes_control,
        na.rm = TRUE
      ),

    .groups = "drop"
  ) |>
  dplyr::mutate(
    diferencia =
      accidentes_tratado -
      accidentes_control
  )


# ------------------------------------------------------------------------------
# 9. Serie agregada
# ------------------------------------------------------------------------------

serie_agregada <- panel_tratamiento_mes |>
  dplyr::group_by(
    mes
  ) |>
  dplyr::summarise(
    tratados =
      mean(
        accidentes_tratado,
        na.rm = TRUE
      ),

    controles =
      mean(
        accidentes_control,
        na.rm = TRUE
      ),

    diferencia =
      mean(
        diferencia,
        na.rm = TRUE
      ),

    .groups = "drop"
  )


# ------------------------------------------------------------------------------
# 10. Formato largo para gráficas
# ------------------------------------------------------------------------------

serie_larga <- serie_agregada |>
  dplyr::select(
    mes,
    tratados,
    controles
  ) |>
  tidyr::pivot_longer(
    cols = c(
      tratados,
      controles
    ),
    names_to = "grupo",
    values_to = "accidentes"
  ) |>
  dplyr::mutate(
    grupo = dplyr::recode(
      grupo,
      tratados = "Tratamientos",
      controles = "Controles emparejados"
    )
  )


# ------------------------------------------------------------------------------
# 11. Gráfica de niveles
# ------------------------------------------------------------------------------

grafica_niveles <- ggplot2::ggplot(
  serie_larga,
  ggplot2::aes(
    x = mes,
    y = accidentes,
    linetype = grupo
  )
) +
  ggplot2::geom_line(
    linewidth = 0.75
  ) +
  ggplot2::geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 0.8
  ) +
  ggplot2::labs(
    title = "Tendencias pretratamiento: tratados vs controles",
    subtitle = "Incidentes C5 mensuales dentro de 500 m · 2014–2022",
    x = NULL,
    y = "Incidentes mensuales promedio",
    linetype = NULL
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom"
  )


ggplot2::ggsave(
  fs::path(
    rutas$output_figures,
    "303_tendencias_pre_niveles.png"
  ),
  plot = grafica_niveles,
  width = 10,
  height = 5.5,
  dpi = 300
)


# ------------------------------------------------------------------------------
# 12. Normalizar series usando promedio 2019 = 100
#
# Normalizamos con un año completo anterior a COVID y relativamente próximo
# al periodo de interés.
# ------------------------------------------------------------------------------

base_2019 <- serie_larga |>
  dplyr::filter(
    lubridate::year(mes) == 2019
  ) |>
  dplyr::group_by(
    grupo
  ) |>
  dplyr::summarise(
    base =
      mean(
        accidentes,
        na.rm = TRUE
      ),
    .groups = "drop"
  )


serie_normalizada <- serie_larga |>
  dplyr::left_join(
    base_2019,
    by = "grupo"
  ) |>
  dplyr::mutate(
    indice_2019 =
      100 * accidentes / base
  )


grafica_normalizada <- ggplot2::ggplot(
  serie_normalizada,
  ggplot2::aes(
    x = mes,
    y = indice_2019,
    linetype = grupo
  )
) +
  ggplot2::geom_hline(
    yintercept = 100,
    linetype = "dotted"
  ) +
  ggplot2::geom_line(
    linewidth = 0.75
  ) +
  ggplot2::labs(
    title = "Trayectorias relativas antes del tratamiento",
    subtitle = "Índice: promedio mensual de 2019 = 100",
    x = NULL,
    y = "Índice (2019 = 100)",
    linetype = NULL
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "bottom"
  )


ggplot2::ggsave(
  fs::path(
    rutas$output_figures,
    "303_tendencias_pre_indice_2019.png"
  ),
  plot = grafica_normalizada,
  width = 10,
  height = 5.5,
  dpi = 300
)


# ------------------------------------------------------------------------------
# 13. Diferencia mensual tratado - control
#
# Si las tendencias son comparables, no debería observarse una deriva
# sistemática marcada de esta diferencia antes de 2023.
# ------------------------------------------------------------------------------

grafica_diferencia <- ggplot2::ggplot(
  serie_agregada,
  ggplot2::aes(
    x = mes,
    y = diferencia
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::geom_line(
    linewidth = 0.7
  ) +
  ggplot2::geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  ggplot2::labs(
    title = "Brecha pretratamiento",
    subtitle = "Tratamientos − controles emparejados · radio de 500 m",
    x = NULL,
    y = "Diferencia mensual promedio"
  ) +
  ggplot2::theme_minimal()


ggplot2::ggsave(
  fs::path(
    rutas$output_figures,
    "303_brecha_pretratamiento.png"
  ),
  plot = grafica_diferencia,
  width = 10,
  height = 5.5,
  dpi = 300
)


# ------------------------------------------------------------------------------
# 14. Variable temporal para pruebas de tendencia
# ------------------------------------------------------------------------------

panel_modelo <- panel_tratamiento_mes |>
  dplyr::mutate(
    t =
      as.numeric(
        mes - min(mes)
      ) / 30.4375,

    reciente =
      mes >= fecha_inicio_reciente
  )


# ------------------------------------------------------------------------------
# 15. Tendencia agregada completa 2014-2022
#
# diferencia_it = alpha_i + beta*t + error_it
#
# beta indica si la brecha tratado-control presenta una tendencia sistemática.
# ------------------------------------------------------------------------------

modelo_tendencia_completa <- fixest::feols(
  diferencia ~ t | id_tratamiento,
  data = panel_modelo,
  cluster = ~ id_tratamiento
)


cat(
  "\n==============================================================\n"
)

cat(
  "TENDENCIA DE LA BRECHA — 2014-2022\n"
)

cat(
  "==============================================================\n\n"
)

print(
  summary(
    modelo_tendencia_completa
  )
)


# ------------------------------------------------------------------------------
# 16. Tendencia reciente 2019-2022
#
# Esta prueba es especialmente importante porque compara el periodo más
# próximo a la intervención.
# ------------------------------------------------------------------------------

panel_modelo_reciente <- panel_modelo |>
  dplyr::filter(
    reciente
  ) |>
  dplyr::group_by(
    id_tratamiento
  ) |>
  dplyr::mutate(
    t_reciente =
      dplyr::row_number()
  ) |>
  dplyr::ungroup()


modelo_tendencia_reciente <- fixest::feols(
  diferencia ~ t_reciente | id_tratamiento,
  data = panel_modelo_reciente,
  cluster = ~ id_tratamiento
)


cat(
  "\n==============================================================\n"
)

cat(
  "TENDENCIA DE LA BRECHA — 2019-2022\n"
)

cat(
  "==============================================================\n\n"
)

print(
  summary(
    modelo_tendencia_reciente
  )
)


# ------------------------------------------------------------------------------
# 17. Tendencia individual por radar
# ------------------------------------------------------------------------------

tendencias_individuales <- panel_tratamiento_mes |>
  dplyr::filter(
    mes >= fecha_inicio_reciente
  ) |>
  dplyr::group_by(
    id_tratamiento
  ) |>
  dplyr::arrange(
    mes,
    .by_group = TRUE
  ) |>
  dplyr::mutate(
    t = dplyr::row_number()
  ) |>
  dplyr::group_modify(
    ~ {

      modelo <- stats::lm(
        diferencia ~ t,
        data = .x
      )

      tibble::tibble(
        tendencia_diferencia =
          unname(
            stats::coef(
              modelo
            )[["t"]]
          ),

        diferencia_media =
          mean(
            .x$diferencia,
            na.rm = TRUE
          )
      )
    }
  ) |>
  dplyr::ungroup()


cat(
  "\nTendencia de la brecha por radar — 2019-2022:\n\n"
)

print(
  tendencias_individuales,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 18. Comparación anual
# ------------------------------------------------------------------------------

comparacion_anual <- panel_tratamiento_mes |>
  dplyr::mutate(
    anio =
      lubridate::year(
        mes
      )
  ) |>
  dplyr::group_by(
    anio
  ) |>
  dplyr::summarise(
    tratados =
      mean(
        accidentes_tratado,
        na.rm = TRUE
      ),

    controles =
      mean(
        accidentes_control,
        na.rm = TRUE
      ),

    diferencia =
      mean(
        diferencia,
        na.rm = TRUE
      ),

    .groups = "drop"
  )


cat(
  "\nComparación anual pretratamiento:\n\n"
)

print(
  comparacion_anual,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 19. Guardar tablas
# ------------------------------------------------------------------------------

readr::write_csv(
  panel_tratamiento_mes,
  fs::path(
    rutas$data_processed,
    "303_panel_matched_pretratamiento.csv"
  )
)


readr::write_csv(
  serie_agregada,
  fs::path(
    rutas$output_tables,
    "303_serie_pretratamiento.csv"
  )
)


readr::write_csv(
  comparacion_anual,
  fs::path(
    rutas$output_tables,
    "303_comparacion_anual_pretratamiento.csv"
  )
)


readr::write_csv(
  tendencias_individuales,
  fs::path(
    rutas$output_tables,
    "303_tendencias_individuales_pretratamiento.csv"
  )
)


# ------------------------------------------------------------------------------
# 20. Extraer coeficientes de diagnóstico
# ------------------------------------------------------------------------------

coef_completa <- broom::tidy(
  modelo_tendencia_completa,
  conf.int = TRUE
) |>
  dplyr::filter(
    term == "t"
  ) |>
  dplyr::mutate(
    periodo = "2014-2022"
  )


coef_reciente <- broom::tidy(
  modelo_tendencia_reciente,
  conf.int = TRUE
) |>
  dplyr::filter(
    term == "t_reciente"
  ) |>
  dplyr::mutate(
    periodo = "2019-2022"
  )


diagnostico_tendencias <- dplyr::bind_rows(
  coef_completa,
  coef_reciente
) |>
  dplyr::select(
    periodo,
    term,
    estimate,
    std.error,
    statistic,
    p.value,
    conf.low,
    conf.high
  )


readr::write_csv(
  diagnostico_tendencias,
  fs::path(
    rutas$output_tables,
    "303_diagnostico_tendencias.csv"
  )
)


cat(
  "\n==============================================================\n"
)

cat(
  "DIAGNÓSTICO DE TENDENCIAS TERMINADO\n"
)

cat(
  "==============================================================\n\n"
)

print(
  diagnostico_tendencias,
  width = Inf
)

cat(
  "\nFiguras guardadas en:\n",
  rutas$output_figures,
  "\n"
)

cat(
  "\nInterpretación preliminar:\n",
  "- Revisar especialmente el coeficiente 2019-2022.\n",
  "- Un coeficiente cercano a cero favorece tendencias comparables.\n",
  "- La significancia estadística no debe utilizarse como único criterio.\n",
  "- Revisar conjuntamente gráficas, magnitud y tendencias individuales.\n"
)