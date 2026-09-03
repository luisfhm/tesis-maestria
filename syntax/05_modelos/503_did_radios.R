# ==============================================================================
# 503_did_radios.R
#
# Objetivo:
#   Evaluar la robustez espacial del efecto estimado comparando incidentes C5
#   dentro de radios de 250 m, 500 m y 1000 m alrededor de las ubicaciones.
#
# Especificación:
#   - Tratamientos: 8 radares validados (>500 m de infraestructura 2022)
#   - Controles: matching 1:5 del script 302
#   - Fecha post de referencia: enero de 2023
#   - FE ubicación
#   - FE mes-año
#   - Pesos de matching
#   - Errores estándar agrupados por ubicación
#
# Outcomes:
#   - accidentes_250m
#   - accidentes_500m  <- especificación principal
#   - accidentes_1000m
#
# IMPORTANTE:
#   Enero de 2023 continúa siendo una referencia temporal del inventario,
#   no una fecha confirmada de instalación.
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. Configuración
# ------------------------------------------------------------------------------

source(
  "syntax/00_setup/000_setup.R"
)


# ------------------------------------------------------------------------------
# 2. Parámetros
# ------------------------------------------------------------------------------

fecha_post <- as.Date(
  "2023-01-01"
)

k_matching <- 5L


# ------------------------------------------------------------------------------
# 3. Localizar archivos
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


cat("\nMatching utilizado:\n")

cat(
  "Tratamientos:",
  dplyr::n_distinct(
    matching$id_tratamiento
  ),
  "\n"
)

cat(
  "Controles únicos:",
  dplyr::n_distinct(
    matching$id_control
  ),
  "\n"
)

cat(
  "Pares tratamiento-control:",
  nrow(matching),
  "\n"
)


# ------------------------------------------------------------------------------
# 5. Construir pesos de matching
# ------------------------------------------------------------------------------

pesos_controles <- matching |>
  dplyr::count(
    id_control,
    name = "veces_seleccionado"
  ) |>
  dplyr::mutate(
    peso_matching =
      veces_seleccionado /
      k_matching
  )


cat(
  "\nPeso total controles:",
  sum(
    pesos_controles$peso_matching
  ),
  "\n"
)


# ------------------------------------------------------------------------------
# 6. Preparar tratamientos
# ------------------------------------------------------------------------------

tratados <- panel_tratados |>
  dplyr::filter(
    tratamiento_500m
  ) |>
  dplyr::transmute(
    id_ubicacion =
      id_radar,

    mes =
      as.Date(mes),

    accidentes_250m,
    accidentes_500m,
    accidentes_1000m,

    tratado = 1L,

    peso_matching = 1
  )


# ------------------------------------------------------------------------------
# 7. Preparar controles
# ------------------------------------------------------------------------------

controles <- panel_controles |>
  dplyr::inner_join(
    pesos_controles,
    by = "id_control"
  ) |>
  dplyr::transmute(
    id_ubicacion =
      id_control,

    mes =
      as.Date(mes),

    accidentes_250m,
    accidentes_500m,
    accidentes_1000m,

    tratado = 0L,

    peso_matching
  )


# ------------------------------------------------------------------------------
# 8. Construir panel DiD
# ------------------------------------------------------------------------------

panel_did <- dplyr::bind_rows(
  tratados,
  controles
) |>
  dplyr::mutate(
    post =
      as.integer(
        mes >= fecha_post
      ),

    did =
      tratado * post,

    anio =
      lubridate::year(
        mes
      ),

    numero_mes =
      lubridate::month(
        mes
      )
  ) |>
  dplyr::arrange(
    id_ubicacion,
    mes
  )


# ------------------------------------------------------------------------------
# 9. Diagnóstico de muestra
# ------------------------------------------------------------------------------

resumen_muestra <- panel_did |>
  dplyr::summarise(
    ubicaciones =
      dplyr::n_distinct(
        id_ubicacion
      ),

    tratados =
      dplyr::n_distinct(
        id_ubicacion[
          tratado == 1
        ]
      ),

    controles =
      dplyr::n_distinct(
        id_ubicacion[
          tratado == 0
        ]
      ),

    meses =
      dplyr::n_distinct(
        mes
      ),

    filas =
      dplyr::n(),

    inicio =
      min(mes),

    fin =
      max(mes)
  )


cat(
  "\nResumen muestra:\n\n"
)

print(
  resumen_muestra,
  width = Inf
)


# ------------------------------------------------------------------------------
# 10. Medias pretratamiento por radio
# ------------------------------------------------------------------------------

medias_pre <- panel_did |>
  dplyr::filter(
    tratado == 1,
    post == 0
  ) |>
  dplyr::summarise(
    media_pre_250m =
      mean(
        accidentes_250m,
        na.rm = TRUE
      ),

    media_pre_500m =
      mean(
        accidentes_500m,
        na.rm = TRUE
      ),

    media_pre_1000m =
      mean(
        accidentes_1000m,
        na.rm = TRUE
      )
  )


cat(
  "\nMedias mensuales pretratamiento — tratados:\n\n"
)

print(
  medias_pre,
  width = Inf
)


# ------------------------------------------------------------------------------
# 11. DiD — radio 250 m
# ------------------------------------------------------------------------------

modelo_250m <- fixest::feols(
  accidentes_250m ~ did |
    id_ubicacion + mes,

  data = panel_did,

  weights =
    ~ peso_matching,

  cluster =
    ~ id_ubicacion
)


# ------------------------------------------------------------------------------
# 12. DiD — radio 500 m
#
# Especificación espacial principal.
# ------------------------------------------------------------------------------

modelo_500m <- fixest::feols(
  accidentes_500m ~ did |
    id_ubicacion + mes,

  data = panel_did,

  weights =
    ~ peso_matching,

  cluster =
    ~ id_ubicacion
)


# ------------------------------------------------------------------------------
# 13. DiD — radio 1000 m
# ------------------------------------------------------------------------------

modelo_1000m <- fixest::feols(
  accidentes_1000m ~ did |
    id_ubicacion + mes,

  data = panel_did,

  weights =
    ~ peso_matching,

  cluster =
    ~ id_ubicacion
)


# ------------------------------------------------------------------------------
# 14. Mostrar modelos conjuntamente
# ------------------------------------------------------------------------------

cat(
  "\n==============================================================\n"
)

cat(
  "DID SEGÚN RADIO ESPACIAL\n"
)

cat(
  "==============================================================\n\n"
)


fixest::etable(
  modelo_250m,
  modelo_500m,
  modelo_1000m,

  headers = c(
    "250 m",
    "500 m",
    "1000 m"
  ),

  dict = c(
    did =
      "Tratado × Post"
  ),

  fitstat =
    ~ n + r2 + wr2
)


# ------------------------------------------------------------------------------
# 15. Función para extraer resultados
# ------------------------------------------------------------------------------

extraer_resultado <- function(
    modelo,
    radio,
    media_pre
) {

  broom::tidy(
    modelo,
    conf.int = TRUE
  ) |>
    dplyr::filter(
      term == "did"
    ) |>
    dplyr::mutate(
      radio =
        radio,

      media_pre_tratados =
        media_pre,

      efecto_porcentual_aprox =
        100 *
        estimate /
        media_pre
    ) |>
    dplyr::select(
      radio,
      term,
      estimate,
      std.error,
      statistic,
      p.value,
      conf.low,
      conf.high,
      media_pre_tratados,
      efecto_porcentual_aprox
    )
}


# ------------------------------------------------------------------------------
# 16. Construir tabla comparativa
# ------------------------------------------------------------------------------

resultados_radios <- dplyr::bind_rows(

  extraer_resultado(
    modelo_250m,
    "250 m",
    medias_pre$media_pre_250m
  ),

  extraer_resultado(
    modelo_500m,
    "500 m",
    medias_pre$media_pre_500m
  ),

  extraer_resultado(
    modelo_1000m,
    "1000 m",
    medias_pre$media_pre_1000m
  )

)


cat(
  "\nResultados DiD según radio:\n\n"
)

print(
  resultados_radios,
  width = Inf
)


# ------------------------------------------------------------------------------
# 17. Gráfica de coeficientes
# ------------------------------------------------------------------------------

resultados_grafica <- resultados_radios |>
  dplyr::mutate(
    radio = factor(
      radio,
      levels = c(
        "250 m",
        "500 m",
        "1000 m"
      )
    )
  )


grafica_radios <- ggplot2::ggplot(
  resultados_grafica,
  ggplot2::aes(
    x = radio,
    y = estimate
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
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
    size = 2.8
  ) +
  ggplot2::labs(
    title =
      "Efecto DiD según radio alrededor del radar",

    subtitle =
      "Incidentes C5 · tratamiento principal >500 m",

    x =
      "Radio del outcome",

    y =
      "Efecto estimado\n(incidentes mensuales)"
  ) +
  ggplot2::theme_minimal()


ggplot2::ggsave(
  fs::path(
    rutas$output_figures,
    "503_did_radios_coeficientes.png"
  ),
  plot = grafica_radios,
  width = 8,
  height = 5.5,
  dpi = 300
)


# ------------------------------------------------------------------------------
# 18. Gráfica en términos porcentuales
# ------------------------------------------------------------------------------

grafica_radios_pct <- ggplot2::ggplot(
  resultados_grafica,
  ggplot2::aes(
    x = radio,
    y = efecto_porcentual_aprox
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::geom_point(
    size = 2.8
  ) +
  ggplot2::labs(
    title =
      "Efecto aproximado respecto del nivel pretratamiento",

    subtitle =
      "Estimación DiD según radio espacial",

    x =
      "Radio del outcome",

    y =
      "Efecto aproximado (%)"
  ) +
  ggplot2::theme_minimal()


ggplot2::ggsave(
  fs::path(
    rutas$output_figures,
    "503_did_radios_porcentaje.png"
  ),
  plot = grafica_radios_pct,
  width = 8,
  height = 5.5,
  dpi = 300
)


# ------------------------------------------------------------------------------
# 19. Descriptivos pre/post por radio y grupo
# ------------------------------------------------------------------------------

descriptivos_radios <- panel_did |>
  dplyr::mutate(
    grupo =
      dplyr::if_else(
        tratado == 1,
        "Tratamientos",
        "Controles"
      ),

    periodo =
      dplyr::if_else(
        post == 1,
        "Post",
        "Pre"
      )
  ) |>
  dplyr::group_by(
    grupo,
    periodo
  ) |>
  dplyr::summarise(
    media_250m =
      weighted.mean(
        accidentes_250m,
        w = peso_matching,
        na.rm = TRUE
      ),

    media_500m =
      weighted.mean(
        accidentes_500m,
        w = peso_matching,
        na.rm = TRUE
      ),

    media_1000m =
      weighted.mean(
        accidentes_1000m,
        w = peso_matching,
        na.rm = TRUE
      ),

    .groups = "drop"
  )


cat(
  "\nDescriptivos pre/post por radio:\n\n"
)

print(
  descriptivos_radios,
  width = Inf
)


# ------------------------------------------------------------------------------
# 20. Guardar resultados
# ------------------------------------------------------------------------------

readr::write_csv(
  resultados_radios,
  fs::path(
    rutas$output_tables,
    "503_resultados_did_radios.csv"
  )
)


readr::write_csv(
  descriptivos_radios,
  fs::path(
    rutas$output_tables,
    "503_descriptivos_did_radios.csv"
  )
)


# ------------------------------------------------------------------------------
# 21. Guardar tabla conjunta
# ------------------------------------------------------------------------------

fixest::etable(
  modelo_250m,
  modelo_500m,
  modelo_1000m,

  headers = c(
    "250 m",
    "500 m",
    "1000 m"
  ),

  dict = c(
    did =
      "Tratado × Post"
  ),

  fitstat =
    ~ n + r2 + wr2,

  tex = FALSE,

  file = fs::path(
    rutas$output_tables,
    "503_tabla_did_radios.html"
  )
)


# ------------------------------------------------------------------------------
# 22. Guardar modelos
# ------------------------------------------------------------------------------

saveRDS(
  list(
    modelo_250m =
      modelo_250m,

    modelo_500m =
      modelo_500m,

    modelo_1000m =
      modelo_1000m
  ),
  fs::path(
    rutas$data_processed,
    "503_modelos_did_radios.rds"
  )
)


# ------------------------------------------------------------------------------
# 23. Resumen final
# ------------------------------------------------------------------------------

cat(
  "\n==============================================================\n"
)

cat(
  "DID POR RADIOS TERMINADO\n"
)

cat(
  "==============================================================\n"
)

cat(
  "Tratamientos: 8\n"
)

cat(
  "Outcome principal: 500 m\n"
)

cat(
  "Radios evaluados: 250 / 500 / 1000 m\n"
)

cat(
  "Post: enero 2023\n"
)

cat(
  "FE ubicación: sí\n"
)

cat(
  "FE mes-año: sí\n"
)

cat(
  "Pesos matching: sí\n"
)

cat(
  "Clustering: ubicación\n"
)

cat(
  "==============================================================\n"
)