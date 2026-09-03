# ==============================================================================
# 505_did_definiciones.R
#
# Objetivo:
#   Evaluar la robustez del resultado principal ante distintas definiciones
#   espaciales del tratamiento.
#
# Definiciones:
#   >250 m  : 10 tratamientos
#   >500 m  :  8 tratamientos (principal)
#   >1000 m :  5 tratamientos (estricto)
#
# Outcome:
#   Incidentes C5 mensuales dentro de 500 m.
#
# Diseño:
#   - Post: enero 2023
#   - FE ubicación
#   - FE mes-año
#   - Matching 1:5 específico para cada definición
#   - Pesos según reutilización del control
#   - SE agrupados por ubicación
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
# 3. Archivos comunes
# ------------------------------------------------------------------------------

archivo_radares <- fs::path(
  rutas$data_processed,
  "300_panel_radares_c5.csv"
)

archivo_controles <- fs::path(
  rutas$data_processed,
  "301_panel_controles_c5.csv"
)


stopifnot(
  file.exists(archivo_radares),
  file.exists(archivo_controles)
)


# ------------------------------------------------------------------------------
# 4. Leer paneles
# ------------------------------------------------------------------------------

panel_radares <- readr::read_csv(
  archivo_radares,
  show_col_types = FALSE
)

panel_controles <- readr::read_csv(
  archivo_controles,
  show_col_types = FALSE
)


# ------------------------------------------------------------------------------
# 5. Función para estimar una definición
# ------------------------------------------------------------------------------

estimar_definicion <- function(
    variable_tratamiento,
    archivo_matching,
    nombre_definicion
) {

  cat(
    "\n==============================================================\n"
  )

  cat(
    "DID DEFINICIÓN:",
    nombre_definicion,
    "\n"
  )

  cat(
    "==============================================================\n"
  )


  # --------------------------------------------------------------------------
  # 5.1. Leer matching
  # --------------------------------------------------------------------------

  matching <- readr::read_csv(
    archivo_matching,
    show_col_types = FALSE
  )


  n_tratamientos <- dplyr::n_distinct(
    matching$id_tratamiento
  )

  n_controles <- dplyr::n_distinct(
    matching$id_control
  )


  cat(
    "Tratamientos:",
    n_tratamientos,
    "\n"
  )

  cat(
    "Controles únicos:",
    n_controles,
    "\n"
  )


  # --------------------------------------------------------------------------
  # 5.2. Pesos controles
  # --------------------------------------------------------------------------

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
    "Peso total controles:",
    sum(
      pesos_controles$peso_matching
    ),
    "\n"
  )


  # --------------------------------------------------------------------------
  # 5.3. Tratamientos
  # --------------------------------------------------------------------------

  tratados <- panel_radares |>
    dplyr::filter(
      .data[[variable_tratamiento]]
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
  # 5.4. Controles seleccionados
  # --------------------------------------------------------------------------

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

      accidentes_500m,

      tratado =
        0L,

      peso_matching
    )


  # --------------------------------------------------------------------------
  # 5.5. Panel DiD
  # --------------------------------------------------------------------------

  panel <- dplyr::bind_rows(
    tratados,
    controles
  ) |>
    dplyr::mutate(
      post =
        as.integer(
          mes >= fecha_post
        ),

      did =
        tratado * post
    )


  # --------------------------------------------------------------------------
  # 5.6. Diagnóstico
  # --------------------------------------------------------------------------

  resumen_muestra <- panel |>
    dplyr::summarise(
      ubicaciones =
        dplyr::n_distinct(
          id_ubicacion
        ),

      tratamientos =
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


  print(
    resumen_muestra,
    width = Inf
  )


  # --------------------------------------------------------------------------
  # 5.7. Media pretratamiento
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


  cat(
    "Media pre tratamientos:",
    round(
      media_pre,
      2
    ),
    "\n"
  )


  # --------------------------------------------------------------------------
  # 5.8. Modelo
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
  # 5.9. Extraer resultado
  # --------------------------------------------------------------------------

  resultado <- broom::tidy(
    modelo,
    conf.int = TRUE
  ) |>
    dplyr::filter(
      term == "did"
    ) |>
    dplyr::mutate(
      definicion =
        nombre_definicion,

      n_tratamientos =
        n_tratamientos,

      n_controles =
        n_controles,

      media_pre_tratados =
        media_pre,

      efecto_porcentual_aprox =
        100 *
        estimate /
        media_pre
    ) |>
    dplyr::select(
      definicion,
      n_tratamientos,
      n_controles,
      estimate,
      std.error,
      statistic,
      p.value,
      conf.low,
      conf.high,
      media_pre_tratados,
      efecto_porcentual_aprox
    )


  cat(
    "\nResultado:\n\n"
  )

  print(
    resultado,
    width = Inf
  )


  list(
    modelo =
      modelo,

    resultado =
      resultado,

    panel =
      panel
  )
}


# ------------------------------------------------------------------------------
# 6. Definición >250 m
# ------------------------------------------------------------------------------

resultado_250 <- estimar_definicion(

  variable_tratamiento =
    "tratamiento_250m",

  archivo_matching =
    fs::path(
      rutas$data_processed,
      "504_matching_250m.csv"
    ),

  nombre_definicion =
    ">250 m"
)


# ------------------------------------------------------------------------------
# 7. Definición >500 m
# ------------------------------------------------------------------------------

resultado_500 <- estimar_definicion(

  variable_tratamiento =
    "tratamiento_500m",

  archivo_matching =
    fs::path(
      rutas$data_processed,
      "504_matching_500m.csv"
    ),

  nombre_definicion =
    ">500 m"
)


# ------------------------------------------------------------------------------
# 8. Definición >1000 m
# ------------------------------------------------------------------------------

resultado_1000 <- estimar_definicion(

  variable_tratamiento =
    "tratamiento_1000m",

  archivo_matching =
    fs::path(
      rutas$data_processed,
      "504_matching_1000m.csv"
    ),

  nombre_definicion =
    ">1000 m"
)


# ------------------------------------------------------------------------------
# 9. Combinar resultados
# ------------------------------------------------------------------------------

resultados_definiciones <- dplyr::bind_rows(
  resultado_250$resultado,
  resultado_500$resultado,
  resultado_1000$resultado
)


cat(
  "\n==============================================================\n"
)

cat(
  "RESULTADOS DID POR DEFINICIÓN DE TRATAMIENTO\n"
)

cat(
  "==============================================================\n\n"
)


print(
  resultados_definiciones,
  width = Inf
)


# ------------------------------------------------------------------------------
# 10. Tabla conjunta de modelos
# ------------------------------------------------------------------------------

fixest::etable(
  resultado_250$modelo,
  resultado_500$modelo,
  resultado_1000$modelo,

  headers = c(
    ">250 m",
    ">500 m",
    ">1000 m"
  ),

  dict = c(
    did =
      "Tratado × Post"
  ),

  fitstat =
    ~ n + r2 + wr2
)


# ------------------------------------------------------------------------------
# 11. Gráfica de coeficientes
# ------------------------------------------------------------------------------

resultados_grafica <- resultados_definiciones |>
  dplyr::mutate(
    definicion =
      factor(
        definicion,
        levels = c(
          ">250 m",
          ">500 m",
          ">1000 m"
        )
      )
  )


grafica_definiciones <- ggplot2::ggplot(
  resultados_grafica,
  ggplot2::aes(
    x = definicion,
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
      "DiD según definición espacial del tratamiento",

    subtitle =
      "Outcome: incidentes C5 dentro de 500 m",

    x =
      "Distancia mínima a infraestructura Fotocívicas 2022",

    y =
      "Efecto estimado\n(incidentes mensuales)"
  ) +
  ggplot2::theme_minimal()


ggplot2::ggsave(
  fs::path(
    rutas$output_figures,
    "505_did_definiciones_coeficientes.png"
  ),
  plot =
    grafica_definiciones,
  width =
    8,
  height =
    5.5,
  dpi =
    300
)


# ------------------------------------------------------------------------------
# 12. Gráfica porcentual
# ------------------------------------------------------------------------------

grafica_definiciones_pct <- ggplot2::ggplot(
  resultados_grafica,
  ggplot2::aes(
    x =
      definicion,

    y =
      efecto_porcentual_aprox
  )
) +
  ggplot2::geom_hline(
    yintercept =
      0,

    linetype =
      "dashed"
  ) +
  ggplot2::geom_point(
    size =
      2.8
  ) +
  ggplot2::labs(
    title =
      "Efecto relativo según definición de tratamiento",

    subtitle =
      "Porcentaje aproximado respecto del nivel pretratamiento",

    x =
      "Definición del tratamiento",

    y =
      "Efecto aproximado (%)"
  ) +
  ggplot2::theme_minimal()


ggplot2::ggsave(
  fs::path(
    rutas$output_figures,
    "505_did_definiciones_porcentaje.png"
  ),
  plot =
    grafica_definiciones_pct,
  width =
    8,
  height =
    5.5,
  dpi =
    300
)


# ------------------------------------------------------------------------------
# 13. Guardar resultados
# ------------------------------------------------------------------------------

readr::write_csv(
  resultados_definiciones,
  fs::path(
    rutas$output_tables,
    "505_resultados_did_definiciones.csv"
  )
)


# ------------------------------------------------------------------------------
# 14. Guardar tabla HTML
# ------------------------------------------------------------------------------

fixest::etable(
  resultado_250$modelo,
  resultado_500$modelo,
  resultado_1000$modelo,

  headers = c(
    ">250 m",
    ">500 m",
    ">1000 m"
  ),

  dict = c(
    did =
      "Tratado × Post"
  ),

  fitstat =
    ~ n + r2 + wr2,

  tex =
    FALSE,

  file =
    fs::path(
      rutas$output_tables,
      "505_tabla_did_definiciones.html"
    )
)


# ------------------------------------------------------------------------------
# 15. Guardar modelos
# ------------------------------------------------------------------------------

saveRDS(
  list(
    modelo_250 =
      resultado_250$modelo,

    modelo_500 =
      resultado_500$modelo,

    modelo_1000 =
      resultado_1000$modelo
  ),

  fs::path(
    rutas$data_processed,
    "505_modelos_did_definiciones.rds"
  )
)


# ------------------------------------------------------------------------------
# 16. Resumen final
# ------------------------------------------------------------------------------

cat(
  "\n==============================================================\n"
)

cat(
  "DID POR DEFINICIÓN DE TRATAMIENTO TERMINADO\n"
)

cat(
  "==============================================================\n"
)

cat(
  "Definiciones: >250 / >500 / >1000 m\n"
)

cat(
  "Outcome: incidentes C5 dentro de 500 m\n"
)

cat(
  "Post: enero 2023\n"
)

cat(
  "Matching específico por definición: sí\n"
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