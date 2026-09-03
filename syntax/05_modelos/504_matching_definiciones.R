# ==============================================================================
# 504_matching_definiciones.R
#
# Objetivo:
#   Construir matchings separados para distintas definiciones espaciales
#   del tratamiento:
#
#   - >250 m  : 10 radares
#   - >500 m  :  8 radares (principal)
#   - >1000 m :  5 radares (estricto)
#
# Estrategia:
#   - Controles potenciales: puntos Fotocívicas preexistentes del panel 301
#   - Outcome utilizado para matching: accidentes C5 dentro de 500 m
#   - Periodo pretratamiento: enero 2014 – diciembre 2022
#   - Matching 1:5 con reemplazo
#
# Características:
#   - media pre
#   - desviación estándar pre
#   - tendencia lineal pre
#   - patrón estacional mensual
#
# IMPORTANTE:
#   El matching se reconstruye independientemente para cada definición de
#   tratamiento. No se reutiliza el matching >500 m para las otras muestras.
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

fecha_inicio_pre <- as.Date(
  "2014-01-01"
)

fecha_fin_pre <- as.Date(
  "2022-12-01"
)

k_controles <- 5L


# ------------------------------------------------------------------------------
# 3. Archivos
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


cat(
  "\nRadares disponibles:",
  dplyr::n_distinct(
    panel_radares$id_radar
  ),
  "\n"
)

cat(
  "Controles potenciales:",
  dplyr::n_distinct(
    panel_controles$id_control
  ),
  "\n"
)


# ------------------------------------------------------------------------------
# 5. Preparar controles pretratamiento
# ------------------------------------------------------------------------------

controles_pre <- panel_controles |>
  dplyr::transmute(
    id =
      id_control,

    mes =
      as.Date(mes),

    accidentes =
      accidentes_500m
  ) |>
  dplyr::filter(
    mes >= fecha_inicio_pre,
    mes <= fecha_fin_pre
  )


# ------------------------------------------------------------------------------
# 6. Función para construir características
# ------------------------------------------------------------------------------

construir_caracteristicas <- function(
    datos
) {

  caracteristicas_generales <- datos |>
    dplyr::arrange(
      id,
      mes
    ) |>
    dplyr::group_by(
      id
    ) |>
    dplyr::mutate(
      tendencia_t =
        dplyr::row_number()
    ) |>
    dplyr::group_modify(
      ~ {

        modelo <- stats::lm(
          accidentes ~ tendencia_t,
          data = .x
        )

        tibble::tibble(
          media_pre =
            mean(
              .x$accidentes,
              na.rm = TRUE
            ),

          sd_pre =
            stats::sd(
              .x$accidentes,
              na.rm = TRUE
            ),

          tendencia_pre =
            unname(
              stats::coef(
                modelo
              )[["tendencia_t"]]
            ),

          n_meses =
            sum(
              !is.na(
                .x$accidentes
              )
            )
        )
      }
    ) |>
    dplyr::ungroup()


  estacionalidad <- datos |>
    dplyr::mutate(
      mes_calendario =
        lubridate::month(
          mes
        )
    ) |>
    dplyr::group_by(
      id,
      mes_calendario
    ) |>
    dplyr::summarise(
      media_mes =
        mean(
          accidentes,
          na.rm = TRUE
        ),

      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from =
        mes_calendario,

      values_from =
        media_mes,

      names_prefix =
        "est_mes_"
    )


  caracteristicas_generales |>
    dplyr::left_join(
      estacionalidad,
      by = "id"
    )
}


# ------------------------------------------------------------------------------
# 7. Características de controles
# ------------------------------------------------------------------------------

carac_controles <- construir_caracteristicas(
  controles_pre
)


# ------------------------------------------------------------------------------
# 8. Variables utilizadas en matching
# ------------------------------------------------------------------------------

variables_matching <- c(
  "media_pre",
  "sd_pre",
  "tendencia_pre",
  paste0(
    "est_mes_",
    1:12
  )
)


# ------------------------------------------------------------------------------
# 9. Función principal de matching
# ------------------------------------------------------------------------------

hacer_matching <- function(
    panel_radares,
    carac_controles,
    variable_tratamiento,
    nombre_definicion,
    k = 5L
) {

  cat(
    "\n==============================================================\n"
  )

  cat(
    "MATCHING:",
    nombre_definicion,
    "\n"
  )

  cat(
    "==============================================================\n"
  )


  # --------------------------------------------------------------------------
  # 9.1. Seleccionar tratamientos
  # --------------------------------------------------------------------------

  tratados_pre <- panel_radares |>
    dplyr::filter(
      .data[[variable_tratamiento]]
    ) |>
    dplyr::transmute(
      id =
        id_radar,

      mes =
        as.Date(mes),

      accidentes =
        accidentes_500m
    ) |>
    dplyr::filter(
      mes >= fecha_inicio_pre,
      mes <= fecha_fin_pre
    )


  cat(
    "Tratamientos:",
    dplyr::n_distinct(
      tratados_pre$id
    ),
    "\n"
  )


  # --------------------------------------------------------------------------
  # 9.2. Características de tratados
  # --------------------------------------------------------------------------

  carac_tratados <- construir_caracteristicas(
    tratados_pre
  )


  # --------------------------------------------------------------------------
  # 9.3. Combinar tratados y controles antes de estandarizar
  #
  # Esto asegura que la escala sea común dentro de cada definición.
  # --------------------------------------------------------------------------

  caracteristicas <- dplyr::bind_rows(

    carac_tratados |>
      dplyr::mutate(
        grupo =
          "tratamiento"
      ),

    carac_controles |>
      dplyr::mutate(
        grupo =
          "control"
      )

  )


  # --------------------------------------------------------------------------
  # 9.4. Revisar faltantes
  # --------------------------------------------------------------------------

  n_faltantes <- caracteristicas |>
    dplyr::select(
      dplyr::all_of(
        variables_matching
      )
    ) |>
    is.na() |>
    sum()


  cat(
    "Valores faltantes en matching:",
    n_faltantes,
    "\n"
  )


  if (
    n_faltantes > 0
  ) {

    stop(
      paste0(
        "Hay valores faltantes en las características de ",
        nombre_definicion
      )
    )
  }


  # --------------------------------------------------------------------------
  # 9.5. Estandarizar
  # --------------------------------------------------------------------------

  matriz_x <- caracteristicas |>
    dplyr::select(
      dplyr::all_of(
        variables_matching
      )
    ) |>
    as.matrix()


  matriz_x_std <- scale(
    matriz_x
  )


  caracteristicas_std <- tibble::as_tibble(
    matriz_x_std
  )


  names(
    caracteristicas_std
  ) <- paste0(
    variables_matching,
    "_z"
  )


  caracteristicas_std <- dplyr::bind_cols(

    caracteristicas |>
      dplyr::select(
        id,
        grupo
      ),

    caracteristicas_std

  )


  # --------------------------------------------------------------------------
  # 9.6. Separar matrices
  # --------------------------------------------------------------------------

  x_tratados <- caracteristicas_std |>
    dplyr::filter(
      grupo ==
        "tratamiento"
    )


  x_controles <- caracteristicas_std |>
    dplyr::filter(
      grupo ==
        "control"
    )


  vars_z <- paste0(
    variables_matching,
    "_z"
  )


  # --------------------------------------------------------------------------
  # 9.7. Distancias Euclidianas tratado-control
  #
  # La muestra es pequeña, así que podemos hacerlo directamente.
  # --------------------------------------------------------------------------

  matriz_tratados <- x_tratados |>
    dplyr::select(
      dplyr::all_of(
        vars_z
      )
    ) |>
    as.matrix()


  matriz_controles <- x_controles |>
    dplyr::select(
      dplyr::all_of(
        vars_z
      )
    ) |>
    as.matrix()


  distancias <- matrix(
    NA_real_,
    nrow = nrow(
      matriz_tratados
    ),
    ncol = nrow(
      matriz_controles
    )
  )


  for (
    i in seq_len(
      nrow(
        matriz_tratados
      )
    )
  ) {

    diferencias <- sweep(
      matriz_controles,
      2,
      matriz_tratados[
        i,
      ],
      FUN = "-"
    )


    distancias[
      i,
    ] <- sqrt(
      rowSums(
        diferencias^2
      )
    )
  }


  # --------------------------------------------------------------------------
  # 9.8. Pasar matriz a formato largo
  # --------------------------------------------------------------------------

  matching_completo <- expand.grid(
    fila_tratado =
      seq_len(
        nrow(
          matriz_tratados
        )
      ),

    fila_control =
      seq_len(
        nrow(
          matriz_controles
        )
      )
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      id_tratamiento =
        x_tratados$id[
          fila_tratado
        ],

      id_control =
        x_controles$id[
          fila_control
        ],

      distancia_matching =
        distancias[
          cbind(
            fila_tratado,
            fila_control
          )
        ]
    ) |>
    dplyr::select(
      id_tratamiento,
      id_control,
      distancia_matching
    ) |>
    dplyr::arrange(
      id_tratamiento,
      distancia_matching
    )


  # --------------------------------------------------------------------------
  # 9.9. Seleccionar k mejores controles
  # --------------------------------------------------------------------------

  matching_seleccionado <- matching_completo |>
    dplyr::group_by(
      id_tratamiento
    ) |>
    dplyr::slice_min(
      order_by =
        distancia_matching,

      n =
        k,

      with_ties =
        FALSE
    ) |>
    dplyr::mutate(
      ranking_control =
        dplyr::row_number()
    ) |>
    dplyr::ungroup()


  # --------------------------------------------------------------------------
  # 9.10. Diagnóstico de controles seleccionados
  # --------------------------------------------------------------------------

  ids_controles_seleccionados <- unique(
    matching_seleccionado$id_control
  )


  uso_controles <- matching_seleccionado |>
    dplyr::count(
      id_control,
      name =
        "veces_seleccionado",
      sort = TRUE
    )


  cat(
    "Pares seleccionados:",
    nrow(
      matching_seleccionado
    ),
    "\n"
  )

  cat(
    "Controles únicos:",
    length(
      ids_controles_seleccionados
    ),
    "\n"
  )


  # --------------------------------------------------------------------------
  # 9.11. Resumen de balance
  # --------------------------------------------------------------------------

  resumen_balance <- caracteristicas |>
    dplyr::mutate(
      muestra =
        dplyr::case_when(

          grupo ==
            "tratamiento" ~
            "Tratamientos",

          grupo ==
            "control" &
            id %in%
              ids_controles_seleccionados ~
            "Controles seleccionados",

          TRUE ~
            "Controles no seleccionados"
        )
    ) |>
    dplyr::group_by(
      muestra
    ) |>
    dplyr::summarise(
      n =
        dplyr::n(),

      media_incidentes_pre =
        mean(
          media_pre,
          na.rm = TRUE
        ),

      sd_incidentes_pre =
        mean(
          sd_pre,
          na.rm = TRUE
        ),

      tendencia_media_pre =
        mean(
          tendencia_pre,
          na.rm = TRUE
        ),

      .groups =
        "drop"
    ) |>
    dplyr::mutate(
      definicion =
        nombre_definicion
    )


  cat(
    "\nBalance:\n\n"
  )

  print(
    resumen_balance,
    width = Inf
  )


  # --------------------------------------------------------------------------
  # 9.12. Resultado
  # --------------------------------------------------------------------------

  list(
    matching_completo =
      matching_completo,

    matching_seleccionado =
      matching_seleccionado,

    caracteristicas =
      caracteristicas,

    resumen_balance =
      resumen_balance,

    uso_controles =
      uso_controles
  )
}


# ------------------------------------------------------------------------------
# 10. Ejecutar matching >250 m
# ------------------------------------------------------------------------------

resultado_250 <- hacer_matching(
  panel_radares =
    panel_radares,

  carac_controles =
    carac_controles,

  variable_tratamiento =
    "tratamiento_250m",

  nombre_definicion =
    "250m",

  k =
    k_controles
)


# ------------------------------------------------------------------------------
# 11. Ejecutar matching >500 m
# ------------------------------------------------------------------------------

resultado_500 <- hacer_matching(
  panel_radares =
    panel_radares,

  carac_controles =
    carac_controles,

  variable_tratamiento =
    "tratamiento_500m",

  nombre_definicion =
    "500m",

  k =
    k_controles
)


# ------------------------------------------------------------------------------
# 12. Ejecutar matching >1000 m
# ------------------------------------------------------------------------------

resultado_1000 <- hacer_matching(
  panel_radares =
    panel_radares,

  carac_controles =
    carac_controles,

  variable_tratamiento =
    "tratamiento_1000m",

  nombre_definicion =
    "1000m",

  k =
    k_controles
)


# ------------------------------------------------------------------------------
# 13. Comparar cantidad de tratamientos y controles
# ------------------------------------------------------------------------------

resumen_definiciones <- tibble::tibble(

  definicion =
    c(
      "250m",
      "500m",
      "1000m"
    ),

  tratamientos =
    c(
      dplyr::n_distinct(
        resultado_250$
          matching_seleccionado$
          id_tratamiento
      ),

      dplyr::n_distinct(
        resultado_500$
          matching_seleccionado$
          id_tratamiento
      ),

      dplyr::n_distinct(
        resultado_1000$
          matching_seleccionado$
          id_tratamiento
      )
    ),

  pares =
    c(
      nrow(
        resultado_250$
          matching_seleccionado
      ),

      nrow(
        resultado_500$
          matching_seleccionado
      ),

      nrow(
        resultado_1000$
          matching_seleccionado
      )
    ),

  controles_unicos =
    c(
      dplyr::n_distinct(
        resultado_250$
          matching_seleccionado$
          id_control
      ),

      dplyr::n_distinct(
        resultado_500$
          matching_seleccionado$
          id_control
      ),

      dplyr::n_distinct(
        resultado_1000$
          matching_seleccionado$
          id_control
      )
    )
)


cat(
  "\n==============================================================\n"
)

cat(
  "RESUMEN DE MATCHINGS POR DEFINICIÓN\n"
)

cat(
  "==============================================================\n\n"
)


print(
  resumen_definiciones,
  width = Inf
)


# ------------------------------------------------------------------------------
# 14. Combinar balance
# ------------------------------------------------------------------------------

balance_definiciones <- dplyr::bind_rows(
  resultado_250$resumen_balance,
  resultado_500$resumen_balance,
  resultado_1000$resumen_balance
) |>
  dplyr::select(
    definicion,
    dplyr::everything()
  )


cat(
  "\nBalance por definición:\n\n"
)

print(
  balance_definiciones,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 15. Guardar matching >250 m
# ------------------------------------------------------------------------------

readr::write_csv(
  resultado_250$
    matching_seleccionado,

  fs::path(
    rutas$data_processed,
    "504_matching_250m.csv"
  )
)


# ------------------------------------------------------------------------------
# 16. Guardar matching >500 m
# ------------------------------------------------------------------------------

readr::write_csv(
  resultado_500$
    matching_seleccionado,

  fs::path(
    rutas$data_processed,
    "504_matching_500m.csv"
  )
)


# ------------------------------------------------------------------------------
# 17. Guardar matching >1000 m
# ------------------------------------------------------------------------------

readr::write_csv(
  resultado_1000$
    matching_seleccionado,

  fs::path(
    rutas$data_processed,
    "504_matching_1000m.csv"
  )
)


# ------------------------------------------------------------------------------
# 18. Guardar resumen de definiciones
# ------------------------------------------------------------------------------

readr::write_csv(
  resumen_definiciones,

  fs::path(
    rutas$output_tables,
    "504_resumen_matching_definiciones.csv"
  )
)


readr::write_csv(
  balance_definiciones,

  fs::path(
    rutas$output_tables,
    "504_balance_matching_definiciones.csv"
  )
)


# ------------------------------------------------------------------------------
# 19. Guardar uso de controles
# ------------------------------------------------------------------------------

uso_controles_definiciones <- dplyr::bind_rows(

  resultado_250$
    uso_controles |>
    dplyr::mutate(
      definicion =
        "250m"
    ),

  resultado_500$
    uso_controles |>
    dplyr::mutate(
      definicion =
        "500m"
    ),

  resultado_1000$
    uso_controles |>
    dplyr::mutate(
      definicion =
        "1000m"
    )

) |>
  dplyr::select(
    definicion,
    dplyr::everything()
  )


readr::write_csv(
  uso_controles_definiciones,

  fs::path(
    rutas$output_tables,
    "504_uso_controles_definiciones.csv"
  )
)


# ------------------------------------------------------------------------------
# 20. Comprobar consistencia con matching principal del 302
#
# El matching de 500 m debería ser muy parecido al 302.
# Puede no ser idéntico si cambió algún detalle de estandarización.
# ------------------------------------------------------------------------------

archivo_matching_302 <- fs::path(
  rutas$data_processed,
  "302_matching_controles_seleccionados.csv"
)


if (
  file.exists(
    archivo_matching_302
  )
) {

  matching_302 <- readr::read_csv(
    archivo_matching_302,
    show_col_types = FALSE
  )


  comparacion_302_504 <- resultado_500$
    matching_seleccionado |>
    dplyr::select(
      id_tratamiento,
      id_control
    ) |>
    dplyr::mutate(
      seleccionado_504 =
        TRUE
    ) |>
    dplyr::full_join(

      matching_302 |>
        dplyr::select(
          id_tratamiento,
          id_control
        ) |>
        dplyr::mutate(
          seleccionado_302 =
            TRUE
        ),

      by = c(
        "id_tratamiento",
        "id_control"
      )
    ) |>
    dplyr::mutate(
      seleccionado_504 =
        tidyr::replace_na(
          seleccionado_504,
          FALSE
        ),

      seleccionado_302 =
        tidyr::replace_na(
          seleccionado_302,
          FALSE
        )
    )


  cat(
    "\nCoincidencias matching 302 vs 504 (>500 m):\n"
  )


  print(
    table(
      comparacion_302_504$
        seleccionado_302,

      comparacion_302_504$
        seleccionado_504
    )
  )


  readr::write_csv(
    comparacion_302_504,

    fs::path(
      rutas$output_tables,
      "504_comparacion_matching_302_500m.csv"
    )
  )

}


# ------------------------------------------------------------------------------
# 21. Resumen final
# ------------------------------------------------------------------------------

cat(
  "\n==============================================================\n"
)

cat(
  "MATCHING DE DEFINICIONES TERMINADO\n"
)

cat(
  "==============================================================\n"
)

cat(
  "Definiciones evaluadas: >250 / >500 / >1000 m\n"
)

cat(
  "Outcome matching: accidentes C5 a 500 m\n"
)

cat(
  "Periodo matching: 2014-01 → 2022-12\n"
)

cat(
  "Controles por tratamiento:",
  k_controles,
  "\n"
)

cat(
  "Matching con reemplazo: sí\n"
)

cat(
  "==============================================================\n"
)