# ==============================================================================
# 601_inferencia_clusters.R
#
# Objetivo:
#   Evaluar la inferencia del DiD principal utilizando wild cluster bootstrap.
#
# Motivación:
#   - 8 ubicaciones tratadas
#   - 29 controles únicos
#   - 37 clusters de ubicación
#   - Los errores estándar cluster convencionales pueden ser imprecisos
#     con pocos clusters efectivos.
#
# Especificación:
#   - Tratamiento principal >500 m
#   - Outcome: incidentes C5 dentro de 500 m
#   - Post: enero 2023
#   - FE ubicación
#   - FE mes-año
#   - Pesos del matching 1:5
#
# Inferencia:
#   1. Cluster convencional
#   2. Wild cluster bootstrap — Rademacher
#   3. Wild cluster bootstrap — Webb
#
# IMPORTANTE:
#   El bootstrap modifica la inferencia, no el coeficiente puntual.
# ==============================================================================


# ------------------------------------------------------------------------------
# 1. Configuración
# ------------------------------------------------------------------------------

source(
  "syntax/00_setup/000_setup.R"
)


# ------------------------------------------------------------------------------
# 2. Comprobar paquete
# ------------------------------------------------------------------------------

if (
  !requireNamespace(
    "fwildclusterboot",
    quietly = TRUE
  )
) {

  stop(
    paste0(
      "\nFalta el paquete 'fwildclusterboot'.\n",
      "Instálalo una vez con:\n\n",
      "install.packages(\"fwildclusterboot\")\n"
    )
  )
}


# ------------------------------------------------------------------------------
# 3. Parámetros
# ------------------------------------------------------------------------------

fecha_post <- as.Date(
  "2023-01-01"
)

k_matching <- 5L

B_boot <- 9999L

semilla <- 20260807L


# ------------------------------------------------------------------------------
# 4. Archivos
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
# 5. Leer datos
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


cat(
  "\nTratamientos:",
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
  "Pares matching:",
  nrow(matching),
  "\n"
)


# ------------------------------------------------------------------------------
# 6. Pesos del matching
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
# 7. Tratamientos
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

    accidentes_500m,

    tratado =
      1L,

    peso_matching =
      1
  )


# ------------------------------------------------------------------------------
# 8. Controles
# ------------------------------------------------------------------------------

controles <- panel_controles |>
  dplyr::inner_join(
    pesos_controles,
    by =
      "id_control"
  ) |>
  dplyr::transmute(
    id_ubicacion =
      id_control,

    mes =
      as.Date(mes),

    accidentes_500m,

    tratado =
      0L,

    peso_matching
  )


# ------------------------------------------------------------------------------
# 9. Construir panel analítico
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

    # Claves para fwildclusterboot
    id_ubicacion_fe =
      factor(
        id_ubicacion
      ),

    mes_fe =
      factor(
        format(
          mes,
          "%Y-%m"
        )
      )
  ) |>
  dplyr::arrange(
    id_ubicacion,
    mes
  )


# ------------------------------------------------------------------------------
# 10. Diagnóstico de clusters
# ------------------------------------------------------------------------------

resumen_clusters <- panel_did |>
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
      dplyr::n()
  )


cat(
  "\nResumen de muestra:\n\n"
)

print(
  resumen_clusters,
  width = Inf
)


# ------------------------------------------------------------------------------
# 11. Modelo principal
# ------------------------------------------------------------------------------

modelo_principal <- fixest::feols(
  accidentes_500m ~ did |
    id_ubicacion_fe +
    mes_fe,

  data =
    panel_did,

  weights =
    ~ peso_matching,

  cluster =
    ~ id_ubicacion_fe
)

cat(
  "\n==============================================================\n"
)

cat(
  "INFERENCIA CLUSTER CONVENCIONAL\n"
)

cat(
  "==============================================================\n\n"
)


print(
  summary(
    modelo_principal
  )
)


# ------------------------------------------------------------------------------
# 12. Resultado cluster convencional
# ------------------------------------------------------------------------------

resultado_convencional <- broom::tidy(
  modelo_principal,
  conf.int = TRUE
) |>
  dplyr::filter(
    term ==
      "did"
  ) |>
  dplyr::mutate(
    metodo =
      "Cluster convencional"
  ) |>
  dplyr::select(
    metodo,
    estimate,
    std.error,
    statistic,
    p.value,
    conf.low,
    conf.high
  )


# ------------------------------------------------------------------------------
# 13. Wild cluster bootstrap — Rademacher
#
# fwildclusterboot usa un generador específico para estas ponderaciones.
# Fijamos ambas semillas para reproducibilidad.
# ------------------------------------------------------------------------------

set.seed(
  semilla
)

if (
  requireNamespace(
    "dqrng",
    quietly = TRUE
  )
) {

  dqrng::dqset.seed(
    semilla
  )

}


cat(
  "\n==============================================================\n"
)

cat(
  "WILD CLUSTER BOOTSTRAP — RADEMACHER\n"
)

cat(
  "==============================================================\n\n"
)


bootstrap_rademacher <- fwildclusterboot::boottest(
  modelo_principal,

  param =
    "did",

  B =
    B_boot,

  clustid =
    "id_ubicacion",

  type =
    "rademacher",

  impose_null =
    TRUE,

  conf_int =
    TRUE,

  sign_level =
    0.05
)


print(
  bootstrap_rademacher
)


# ------------------------------------------------------------------------------
# 14. Wild cluster bootstrap — Webb
#
# Webb utiliza una distribución de pesos con más puntos de soporte.
# La usamos como sensibilidad inferencial.
# ------------------------------------------------------------------------------

set.seed(
  semilla
)

if (
  requireNamespace(
    "dqrng",
    quietly = TRUE
  )
) {

  dqrng::dqset.seed(
    semilla
  )

}


cat(
  "\n==============================================================\n"
)

cat(
  "WILD CLUSTER BOOTSTRAP — WEBB\n"
)

cat(
  "==============================================================\n\n"
)


bootstrap_webb <- fwildclusterboot::boottest(
  modelo_principal,

  param =
    "did",

  B =
    B_boot,

  clustid =
    "id_ubicacion",

  type =
    "webb",

  impose_null =
    TRUE,

  conf_int =
    TRUE,

  sign_level =
    0.05
)


print(
  bootstrap_webb
)


# ------------------------------------------------------------------------------
# 15. Inspeccionar estructura de objetos bootstrap
#
# Esto ayuda si cambia ligeramente la estructura entre versiones del paquete.
# ------------------------------------------------------------------------------

cat(
  "\nElementos disponibles — Rademacher:\n"
)

print(
  names(
    bootstrap_rademacher
  )
)


cat(
  "\nElementos disponibles — Webb:\n"
)

print(
  names(
    bootstrap_webb
  )
)


# ------------------------------------------------------------------------------
# 16. Función robusta para extraer resultados bootstrap
# ------------------------------------------------------------------------------

extraer_bootstrap <- function(
    objeto,
    metodo,
    estimate_original
) {

  # p-value

  p_boot <- tryCatch(
    as.numeric(
      objeto$p_val
    ),
    error = function(e)
      NA_real_
  )


  if (
    length(p_boot) == 0 ||
      is.na(p_boot)
  ) {

    p_boot <- tryCatch(
      as.numeric(
        objeto$p.value
      ),
      error = function(e)
        NA_real_
    )

  }


  # estadístico

  stat_boot <- tryCatch(
    as.numeric(
      objeto$t_stat
    ),
    error = function(e)
      NA_real_
  )


  # intervalo

  ci_boot <- tryCatch(
    as.numeric(
      objeto$conf_int
    ),
    error = function(e)
      c(
        NA_real_,
        NA_real_
      )
  )


  if (
    length(ci_boot) < 2
  ) {

    ci_boot <- c(
      NA_real_,
      NA_real_
    )

  }


  tibble::tibble(
    metodo =
      metodo,

    estimate =
      estimate_original,

    std.error =
      NA_real_,

    statistic =
      stat_boot,

    p.value =
      p_boot,

    conf.low =
      ci_boot[1],

    conf.high =
      ci_boot[2]
  )
}


# ------------------------------------------------------------------------------
# 17. Tabla conjunta
# ------------------------------------------------------------------------------

coef_principal <- stats::coef(
  modelo_principal
)[["did"]]


resultado_rademacher <- extraer_bootstrap(
  bootstrap_rademacher,
  "Wild cluster bootstrap — Rademacher",
  coef_principal
)


resultado_webb <- extraer_bootstrap(
  bootstrap_webb,
  "Wild cluster bootstrap — Webb",
  coef_principal
)


resultados_inferencia <- dplyr::bind_rows(
  resultado_convencional,
  resultado_rademacher,
  resultado_webb
)


cat(
  "\n==============================================================\n"
)

cat(
  "COMPARACIÓN DE MÉTODOS DE INFERENCIA\n"
)

cat(
  "==============================================================\n\n"
)


print(
  resultados_inferencia,
  width = Inf
)


# ------------------------------------------------------------------------------
# 18. Media pretratamiento para interpretación
# ------------------------------------------------------------------------------

media_pre <- panel_did |>
  dplyr::filter(
    tratado ==
      1,

    post ==
      0
  ) |>
  dplyr::summarise(
    media =
      mean(
        accidentes_500m,
        na.rm = TRUE
      )
  ) |>
  dplyr::pull(
    media
  )


resultados_inferencia <- resultados_inferencia |>
  dplyr::mutate(
    media_pre_tratados =
      media_pre,

    efecto_porcentual_aprox =
      100 *
      estimate /
      media_pre
  )


# ------------------------------------------------------------------------------
# 19. Guardar tabla
# ------------------------------------------------------------------------------

readr::write_csv(
  resultados_inferencia,

  fs::path(
    rutas$output_tables,
    "601_inferencia_clusters.csv"
  )
)


# ------------------------------------------------------------------------------
# 20. Guardar objetos bootstrap
# ------------------------------------------------------------------------------

saveRDS(
  list(
    modelo_principal =
      modelo_principal,

    bootstrap_rademacher =
      bootstrap_rademacher,

    bootstrap_webb =
      bootstrap_webb
  ),

  fs::path(
    rutas$data_processed,
    "601_inferencia_clusters.rds"
  )
)


# ------------------------------------------------------------------------------
# 21. Gráfica de intervalos
#
# Solo se grafican métodos para los que el paquete pudo devolver IC.
# ------------------------------------------------------------------------------

grafica_inferencia <- resultados_inferencia |>
  dplyr::filter(
    !is.na(
      conf.low
    ),
    !is.na(
      conf.high
    )
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x =
        metodo,

      y =
        estimate
    )
  ) +
  ggplot2::geom_hline(
    yintercept =
      0,

    linetype =
      "dashed"
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin =
        conf.low,

      ymax =
        conf.high
    ),

    width =
      0.12
  ) +
  ggplot2::geom_point(
    size =
      2.7
  ) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title =
      "Inferencia del efecto DiD con pocos clusters",

    subtitle =
      paste0(
        "Coeficiente puntual idéntico; ",
        "cambia el método de inferencia"
      ),

    x =
      NULL,

    y =
      "Efecto DiD estimado"
  ) +
  ggplot2::theme_minimal()


ggplot2::ggsave(
  fs::path(
    rutas$output_figures,
    "601_inferencia_clusters.png"
  ),

  plot =
    grafica_inferencia,

  width =
    9,

  height =
    5.5,

  dpi =
    300
)


# ------------------------------------------------------------------------------
# 22. Resumen final
# ------------------------------------------------------------------------------

cat(
  "\n==============================================================\n"
)

cat(
  "INFERENCIA CON POCOS CLUSTERS TERMINADA\n"
)

cat(
  "==============================================================\n"
)

cat(
  "Tratamientos: 8\n"
)

cat(
  "Clusters totales:",
  dplyr::n_distinct(
    panel_did$id_ubicacion
  ),
  "\n"
)

cat(
  "Bootstrap repeticiones:",
  B_boot,
  "\n"
)

cat(
  "Métodos: cluster convencional / Rademacher / Webb\n"
)

cat(
  "Outcome: accidentes C5 dentro de 500 m\n"
)

cat(
  "==============================================================\n"
)