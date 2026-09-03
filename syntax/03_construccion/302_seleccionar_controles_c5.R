# ==============================================================================
# 302_seleccionar_controles_c5.R
#
# Objetivo:
#   Seleccionar controles comparables para los radares tratados en 2023
#   utilizando exclusivamente información pretratamiento.
#
# Estrategia:
#   - Resultado principal: incidentes C5 dentro de 500 m
#   - Periodo pretratamiento: 2014-01 a 2022-12
#   - Características:
#       * nivel medio
#       * desviación estándar
#       * tendencia temporal
#       * estacionalidad mensual
#   - Distancia entre tratados y controles sobre características estandarizadas
#   - Selección inicial: 5 controles más cercanos por tratamiento
#
# IMPORTANTE:
#   Ninguna observación de 2023 se utiliza para seleccionar controles.
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
fecha_fin_pre    <- as.Date("2022-12-01")

k_controles <- 5L

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


cat(
  "\nArchivo tratamientos:\n",
  archivo_tratados,
  "\n"
)

cat(
  "\nArchivo controles:\n",
  archivo_controles,
  "\n"
)


stopifnot(
  file.exists(archivo_tratados),
  file.exists(archivo_controles)
)

# ------------------------------------------------------------------------------
# 4. Leer paneles
# ------------------------------------------------------------------------------

panel_tratados <- readr::read_csv(
  archivo_tratados,
  show_col_types = FALSE
)

panel_controles <- readr::read_csv(
  archivo_controles,
  show_col_types = FALSE
)


cat(
  "\nFilas panel tratamientos:",
  nrow(panel_tratados),
  "\n"
)

cat(
  "Filas panel controles:",
  nrow(panel_controles),
  "\n"
)


cat(
  "\nVariables tratamientos:\n"
)

print(
  names(panel_tratados)
)


cat(
  "\nVariables controles:\n"
)

print(
  names(panel_controles)
)


# ------------------------------------------------------------------------------
# 5. Homogeneizar paneles
# ------------------------------------------------------------------------------

# Ajusta estos nombres únicamente si el 300/301 los guardó con nombres distintos.

tratados <- panel_tratados |>
  dplyr::filter(
    tratamiento_500m
  ) |>
  dplyr::transmute(
    id = id_radar,
    mes = as.Date(mes),
    accidentes = accidentes_500m,
    grupo = "tratamiento"
  )


controles <- panel_controles |>
  dplyr::transmute(
    id = id_control,
    mes = as.Date(mes),
    accidentes = accidentes_500m,
    grupo = "control"
  )


cat(
  "\nTratamientos:",
  dplyr::n_distinct(tratados$id),
  "\n"
)

cat(
  "Controles:",
  dplyr::n_distinct(controles$id),
  "\n"
)


# ------------------------------------------------------------------------------
# 6. Restringir al periodo pretratamiento
# ------------------------------------------------------------------------------

pre_tratados <- tratados |>
  dplyr::filter(
    mes >= fecha_inicio_pre,
    mes <= fecha_fin_pre
  )


pre_controles <- controles |>
  dplyr::filter(
    mes >= fecha_inicio_pre,
    mes <= fecha_fin_pre
  )


cat(
  "\nPeriodo utilizado para matching:\n"
)

cat(
  format(fecha_inicio_pre, "%Y-%m"),
  "→",
  format(fecha_fin_pre, "%Y-%m"),
  "\n"
)


cat(
  "\nMeses pretratamiento tratamientos:",
  dplyr::n_distinct(pre_tratados$mes),
  "\n"
)

cat(
  "Meses pretratamiento controles:",
  dplyr::n_distinct(pre_controles$mes),
  "\n"
)


# ------------------------------------------------------------------------------
# 7. Función para construir características pretratamiento
# ------------------------------------------------------------------------------

construir_caracteristicas <- function(datos) {

  datos |>
    dplyr::arrange(
      id,
      mes
    ) |>
    dplyr::group_by(
      id
    ) |>
    dplyr::mutate(
      tendencia_t = dplyr::row_number()
    ) |>
    dplyr::group_modify(
      ~ {

        modelo_tendencia <- stats::lm(
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

          minimo_pre =
            min(
              .x$accidentes,
              na.rm = TRUE
            ),

          maximo_pre =
            max(
              .x$accidentes,
              na.rm = TRUE
            ),

          tendencia_pre =
            unname(
              stats::coef(
                modelo_tendencia
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
}


# ------------------------------------------------------------------------------
# 8. Características generales
# ------------------------------------------------------------------------------

carac_tratados <- construir_caracteristicas(
  pre_tratados
)


carac_controles <- construir_caracteristicas(
  pre_controles
)


# ------------------------------------------------------------------------------
# 9. Construir características estacionales
# ------------------------------------------------------------------------------

estacionalidad_tratados <- pre_tratados |>
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
    names_from = mes_calendario,
    values_from = media_mes,
    names_prefix = "est_mes_"
  )


estacionalidad_controles <- pre_controles |>
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
    names_from = mes_calendario,
    values_from = media_mes,
    names_prefix = "est_mes_"
  )


# ------------------------------------------------------------------------------
# 10. Combinar características
# ------------------------------------------------------------------------------

carac_tratados <- carac_tratados |>
  dplyr::left_join(
    estacionalidad_tratados,
    by = "id"
  ) |>
  dplyr::mutate(
    grupo = "tratamiento"
  )


carac_controles <- carac_controles |>
  dplyr::left_join(
    estacionalidad_controles,
    by = "id"
  ) |>
  dplyr::mutate(
    grupo = "control"
  )


caracteristicas <- dplyr::bind_rows(
  carac_tratados,
  carac_controles
)


cat(
  "\nCaracterísticas construidas:",
  nrow(caracteristicas),
  "ubicaciones\n"
)


# ------------------------------------------------------------------------------
# 11. Variables utilizadas para matching
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
# 12. Revisar valores faltantes
# ------------------------------------------------------------------------------

faltantes <- caracteristicas |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(
        variables_matching
      ),
      ~ sum(
        is.na(.x)
      )
    )
  )


cat(
  "\nValores faltantes en características:\n"
)

print(
  faltantes,
  width = Inf
)


# ------------------------------------------------------------------------------
# 13. Estandarizar características
# ------------------------------------------------------------------------------

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


# ------------------------------------------------------------------------------
# 14. Separar tratados y controles
# ------------------------------------------------------------------------------

x_tratados <- caracteristicas_std |>
  dplyr::filter(
    grupo == "tratamiento"
  )


x_controles <- caracteristicas_std |>
  dplyr::filter(
    grupo == "control"
  )


vars_z <- paste0(
  variables_matching,
  "_z"
)


# ------------------------------------------------------------------------------
# 15. Calcular todas las distancias tratado-control
# ------------------------------------------------------------------------------

cat(
  "\nCalculando similitud entre",
  nrow(x_tratados),
  "tratamientos y",
  nrow(x_controles),
  "controles...\n"
)


matching_completo <- tidyr::crossing(
  fila_tratado = seq_len(
    nrow(x_tratados)
  ),
  fila_control = seq_len(
    nrow(x_controles)
  )
) |>
  dplyr::rowwise() |>
  dplyr::mutate(

    distancia_matching =
      sqrt(
        sum(
          (
            as.numeric(
              x_tratados[
                fila_tratado,
                vars_z
              ]
            ) -
              as.numeric(
                x_controles[
                  fila_control,
                  vars_z
                ]
              )
          )^2
        )
      ),

    id_tratamiento =
      x_tratados$id[
        fila_tratado
      ],

    id_control =
      x_controles$id[
        fila_control
      ]

  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    id_tratamiento,
    id_control,
    distancia_matching
  ) |>
  dplyr::arrange(
    id_tratamiento,
    distancia_matching
  )


# ------------------------------------------------------------------------------
# 16. Seleccionar k controles más cercanos por tratamiento
# ------------------------------------------------------------------------------

matching_seleccionado <- matching_completo |>
  dplyr::group_by(
    id_tratamiento
  ) |>
  dplyr::slice_min(
    order_by = distancia_matching,
    n = k_controles,
    with_ties = FALSE
  ) |>
  dplyr::mutate(
    ranking_control =
      dplyr::row_number()
  ) |>
  dplyr::ungroup()


cat(
  "\nMatching inicial terminado.\n"
)


cat(
  "\nControles seleccionados por tratamiento:\n"
)


print(
  matching_seleccionado,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 17. Diagnóstico de reutilización de controles
# ------------------------------------------------------------------------------

uso_controles <- matching_seleccionado |>
  dplyr::count(
    id_control,
    name = "veces_seleccionado",
    sort = TRUE
  )


cat(
  "\nNúmero de controles únicos seleccionados:",
  dplyr::n_distinct(
    matching_seleccionado$id_control
  ),
  "\n"
)


cat(
  "\nControles reutilizados:\n"
)


print(
  uso_controles,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 18. Comparar características antes/después del matching
# ------------------------------------------------------------------------------

ids_controles_seleccionados <- unique(
  matching_seleccionado$id_control
)


resumen_matching <- caracteristicas |>
  dplyr::mutate(
    muestra =
      dplyr::case_when(

        grupo == "tratamiento" ~
          "Tratamientos",

        grupo == "control" &
          id %in% ids_controles_seleccionados ~
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

    .groups = "drop"
  )


cat(
  "\nResumen de balance inicial:\n"
)


print(
  resumen_matching,
  width = Inf
)


# ------------------------------------------------------------------------------
# 19. Guardar resultados
# ------------------------------------------------------------------------------

readr::write_csv(
  matching_completo,
  fs::path(
    rutas$data_processed,
    "302_matching_controles_completo.csv"
  )
)


readr::write_csv(
  matching_seleccionado,
  fs::path(
    rutas$data_processed,
    "302_matching_controles_seleccionados.csv"
  )
)


readr::write_csv(
  caracteristicas,
  fs::path(
    rutas$data_processed,
    "302_caracteristicas_pretratamiento.csv"
  )
)


readr::write_csv(
  resumen_matching,
  fs::path(
    rutas$output_tables,
    "302_resumen_matching.csv"
  )
)


# ------------------------------------------------------------------------------
# 20. Resumen final
# ------------------------------------------------------------------------------

cat(
  "\n==============================================================\n"
)

cat(
  "SELECCIÓN DE CONTROLES TERMINADA\n"
)

cat(
  "==============================================================\n"
)

cat(
  "Tratamientos:",
  nrow(x_tratados),
  "\n"
)

cat(
  "Controles candidatos:",
  nrow(x_controles),
  "\n"
)

cat(
  "k por tratamiento:",
  k_controles,
  "\n"
)

cat(
  "Controles únicos seleccionados:",
  dplyr::n_distinct(
    matching_seleccionado$id_control
  ),
  "\n"
)

cat(
  "Periodo matching:",
  format(
    fecha_inicio_pre,
    "%Y-%m"
  ),
  "→",
  format(
    fecha_fin_pre,
    "%Y-%m"
  ),
  "\n"
)

cat(
  "==============================================================\n"
)