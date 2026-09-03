# ==============================================================================
# 600_leave_one_out.R
#
# Objetivo:
#   Evaluar si el resultado DiD principal depende de una ubicación tratada
#   específica.
#
# Estrategia:
#   - Especificación principal >500 m
#   - 8 tratamientos
#   - Outcome: incidentes C5 dentro de 500 m
#   - Post: enero 2023
#   - Matching 1:5 del script 302
#   - Se elimina un radar tratado por vez
#   - Se eliminan también sus pares del matching
#   - Se recalculan los pesos de los controles restantes
#   - FE ubicación + FE mes-año
#   - SE agrupados por ubicación
#
# Interpretación:
#   Si el coeficiente cambia drásticamente al eliminar una ubicación,
#   el resultado principal podría depender excesivamente de ese radar.
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

matching_original <- readr::read_csv(
  archivo_matching,
  show_col_types = FALSE
)


ids_tratamientos <- sort(
  unique(
    matching_original$id_tratamiento
  )
)


cat(
  "\nTratamientos principales:",
  length(ids_tratamientos),
  "\n"
)

print(
  ids_tratamientos
)


# ------------------------------------------------------------------------------
# 5. Función para construir muestra y estimar DiD
# ------------------------------------------------------------------------------

estimar_did <- function(
    id_excluido = NA_character_
) {

  # --------------------------------------------------------------------------
  # 5.1. Matching correspondiente
  # --------------------------------------------------------------------------

  if (
    is.na(id_excluido)
  ) {

    matching_actual <- matching_original

  } else {

    matching_actual <- matching_original |>
      dplyr::filter(
        id_tratamiento !=
          id_excluido
      )

  }


  # --------------------------------------------------------------------------
  # 5.2. Tratamientos incluidos
  # --------------------------------------------------------------------------

  ids_tratamientos_actuales <- unique(
    matching_actual$id_tratamiento
  )


  n_tratamientos <- length(
    ids_tratamientos_actuales
  )


  # --------------------------------------------------------------------------
  # 5.3. Recalcular pesos de controles
  #
  # Cada aparición de un control en el matching vale 1/k.
  # En el modelo completo:
  #   40 pares / 5 = peso total 8
  #
  # En cada leave-one-out:
  #   35 pares / 5 = peso total 7
  # --------------------------------------------------------------------------

  pesos_controles <- matching_actual |>
    dplyr::count(
      id_control,
      name =
        "veces_seleccionado"
    ) |>
    dplyr::mutate(
      peso_matching =
        veces_seleccionado /
        k_matching
    )


  # --------------------------------------------------------------------------
  # 5.4. Panel tratados
  # --------------------------------------------------------------------------

  tratados <- panel_tratados |>
    dplyr::filter(
      id_radar %in%
        ids_tratamientos_actuales
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


  # --------------------------------------------------------------------------
  # 5.5. Panel controles
  # --------------------------------------------------------------------------

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


  # --------------------------------------------------------------------------
  # 5.6. Muestra analítica
  # --------------------------------------------------------------------------

  panel <- dplyr::bind_rows(
    tratados,
    controles
  ) |>
    dplyr::mutate(
      post =
        as.integer(
          mes >=
            fecha_post
        ),

      did =
        tratado *
        post
    )


  # --------------------------------------------------------------------------
  # 5.7. Modelo
  # --------------------------------------------------------------------------

  modelo <- fixest::feols(
    accidentes_500m ~ did |
      id_ubicacion +
      mes,

    data =
      panel,

    weights =
      ~ peso_matching,

    cluster =
      ~ id_ubicacion
  )


  # --------------------------------------------------------------------------
  # 5.8. Media pretratamiento
  # --------------------------------------------------------------------------

  media_pre <- panel |>
    dplyr::filter(
      tratado == 1,
      post == 0
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


  # --------------------------------------------------------------------------
  # 5.9. Resultado
  # --------------------------------------------------------------------------

  resultado <- broom::tidy(
    modelo,
    conf.int = TRUE
  ) |>
    dplyr::filter(
      term == "did"
    ) |>
    dplyr::mutate(
      excluido =
        ifelse(
          is.na(id_excluido),
          "Ninguno",
          id_excluido
        ),

      n_tratamientos =
        n_tratamientos,

      n_controles =
        dplyr::n_distinct(
          controles$id_ubicacion
        ),

      peso_total_tratados =
        n_tratamientos,

      peso_total_controles =
        sum(
          pesos_controles$peso_matching
        ),

      media_pre_tratados =
        media_pre,

      efecto_porcentual_aprox =
        100 *
        estimate /
        media_pre
    ) |>
    dplyr::select(
      excluido,
      n_tratamientos,
      n_controles,
      peso_total_tratados,
      peso_total_controles,
      estimate,
      std.error,
      statistic,
      p.value,
      conf.low,
      conf.high,
      media_pre_tratados,
      efecto_porcentual_aprox
    )


  list(
    modelo =
      modelo,

    resultado =
      resultado
  )
}


# ------------------------------------------------------------------------------
# 6. Estimación principal completa
# ------------------------------------------------------------------------------

cat(
  "\n==============================================================\n"
)

cat(
  "MODELO COMPLETO\n"
)

cat(
  "==============================================================\n"
)


resultado_completo <- estimar_did(
  id_excluido =
    NA_character_
)


print(
  resultado_completo$resultado,
  width = Inf
)


# ------------------------------------------------------------------------------
# 7. Leave-one-out
# ------------------------------------------------------------------------------

cat(
  "\n==============================================================\n"
)

cat(
  "LEAVE-ONE-OUT\n"
)

cat(
  "==============================================================\n"
)


resultados_loo <- vector(
  mode = "list",
  length =
    length(ids_tratamientos)
)


for (
  i in seq_along(
    ids_tratamientos
  )
) {

  id_actual <- ids_tratamientos[
    i
  ]


  cat(
    "\n[",
    i,
    "/",
    length(ids_tratamientos),
    "] Excluyendo:",
    id_actual,
    "\n",
    sep = ""
  )


  resultado_actual <- estimar_did(
    id_excluido =
      id_actual
  )


  resultados_loo[[i]] <-
    resultado_actual$resultado


  cat(
    "Coeficiente:",
    round(
      resultado_actual$resultado$estimate,
      3
    ),
    "| SE:",
    round(
      resultado_actual$resultado$std.error,
      3
    ),
    "| p:",
    round(
      resultado_actual$resultado$p.value,
      3
    ),
    "\n"
  )
}


resultados_loo <- dplyr::bind_rows(
  resultados_loo
)


# ------------------------------------------------------------------------------
# 8. Combinar con resultado principal
# ------------------------------------------------------------------------------

resultados_completos <- dplyr::bind_rows(
  resultado_completo$resultado,
  resultados_loo
)


cat(
  "\n==============================================================\n"
)

cat(
  "RESULTADOS LEAVE-ONE-OUT\n"
)

cat(
  "==============================================================\n\n"
)


print(
  resultados_completos,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 9. Resumen de sensibilidad
# ------------------------------------------------------------------------------

coef_principal <- resultado_completo$resultado$
  estimate


resumen_loo <- resultados_loo |>
  dplyr::summarise(
    coef_principal =
      coef_principal,

    coef_min =
      min(
        estimate
      ),

    coef_max =
      max(
        estimate
      ),

    coef_promedio =
      mean(
        estimate
      ),

    coef_mediano =
      stats::median(
        estimate
      ),

    sd_coeficientes =
      stats::sd(
        estimate
      ),

    max_desviacion_principal =
      max(
        abs(
          estimate -
            coef_principal
        )
      ),

    n_coef_negativos =
      sum(
        estimate < 0
      ),

    n_coef_positivos =
      sum(
        estimate > 0
      )
  )


cat(
  "\nResumen de sensibilidad:\n\n"
)

print(
  resumen_loo,
  width = Inf
)


# ------------------------------------------------------------------------------
# 10. Identificar radar con mayor influencia
# ------------------------------------------------------------------------------

influencia <- resultados_loo |>
  dplyr::mutate(
    diferencia_vs_principal =
      estimate -
      coef_principal,

    cambio_absoluto =
      abs(
        diferencia_vs_principal
      )
  ) |>
  dplyr::arrange(
    dplyr::desc(
      cambio_absoluto
    )
  )


cat(
  "\nInfluencia por radar excluido:\n\n"
)

print(
  influencia |>
    dplyr::select(
      excluido,
      estimate,
      std.error,
      p.value,
      diferencia_vs_principal,
      cambio_absoluto
    ),
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 11. Gráfica leave-one-out
# ------------------------------------------------------------------------------

grafica_loo <- influencia |>
  dplyr::mutate(
    excluido =
      stats::reorder(
        excluido,
        estimate
      )
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x =
        excluido,

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
  ggplot2::geom_hline(
    yintercept =
      coef_principal,

    linetype =
      "dotted"
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin =
        conf.low,

      ymax =
        conf.high
    ),

    width =
      0.15
  ) +
  ggplot2::geom_point(
    size =
      2.7
  ) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title =
      "Robustez leave-one-out del efecto DiD",

    subtitle =
      paste0(
        "Cada estimación excluye un radar tratado · ",
        "línea punteada = estimación con los 8 tratamientos"
      ),

    x =
      "Radar excluido",

    y =
      "Efecto DiD estimado\n(incidentes mensuales a 500 m)"
  ) +
  ggplot2::theme_minimal()


ggplot2::ggsave(
  fs::path(
    rutas$output_figures,
    "600_leave_one_out.png"
  ),

  plot =
    grafica_loo,

  width =
    9,

  height =
    6,

  dpi =
    300
)


# ------------------------------------------------------------------------------
# 12. Gráfica de desviación respecto del modelo principal
# ------------------------------------------------------------------------------

grafica_influencia <- influencia |>
  dplyr::mutate(
    excluido =
      stats::reorder(
        excluido,
        cambio_absoluto
      )
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x =
        excluido,

      y =
        diferencia_vs_principal
    )
  ) +
  ggplot2::geom_hline(
    yintercept =
      0,

    linetype =
      "dashed"
  ) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title =
      "Influencia de cada radar sobre la estimación principal",

    subtitle =
      "Cambio en el coeficiente DiD al excluir cada ubicación",

    x =
      "Radar excluido",

    y =
      "Cambio respecto del coeficiente principal"
  ) +
  ggplot2::theme_minimal()


ggplot2::ggsave(
  fs::path(
    rutas$output_figures,
    "600_leave_one_out_influencia.png"
  ),

  plot =
    grafica_influencia,

  width =
    9,

  height =
    6,

  dpi =
    300
)


# ------------------------------------------------------------------------------
# 13. Guardar tablas
# ------------------------------------------------------------------------------

readr::write_csv(
  resultados_completos,

  fs::path(
    rutas$output_tables,
    "600_resultados_leave_one_out.csv"
  )
)


readr::write_csv(
  influencia,

  fs::path(
    rutas$output_tables,
    "600_influencia_radares.csv"
  )
)


readr::write_csv(
  resumen_loo,

  fs::path(
    rutas$output_tables,
    "600_resumen_leave_one_out.csv"
  )
)


# ------------------------------------------------------------------------------
# 14. Guardar resultado principal como referencia
# ------------------------------------------------------------------------------

saveRDS(
  resultado_completo$modelo,

  fs::path(
    rutas$data_processed,
    "600_modelo_referencia_leave_one_out.rds"
  )
)


# ------------------------------------------------------------------------------
# 15. Resumen final
# ------------------------------------------------------------------------------

cat(
  "\n==============================================================\n"
)

cat(
  "LEAVE-ONE-OUT TERMINADO\n"
)

cat(
  "==============================================================\n"
)

cat(
  "Tratamientos originales:",
  length(
    ids_tratamientos
  ),
  "\n"
)

cat(
  "Modelos leave-one-out:",
  nrow(
    resultados_loo
  ),
  "\n"
)

cat(
  "Coeficiente principal:",
  round(
    coef_principal,
    3
  ),
  "\n"
)

cat(
  "Rango leave-one-out:",
  round(
    min(
      resultados_loo$estimate
    ),
    3
  ),
  "a",
  round(
    max(
      resultados_loo$estimate
    ),
    3
  ),
  "\n"
)

cat(
  "==============================================================\n"
)