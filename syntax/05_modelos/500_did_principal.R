# ==============================================================================
# 500_did_principal.R
#
# Objetivo:
#   Estimar una primera especificación DiD para el efecto de las nuevas
#   ubicaciones de radares sobre incidentes viales C5.
#
# Especificación principal:
#   - Tratamientos: 8 ubicaciones validadas (>500 m de infraestructura 2022)
#   - Controles: controles seleccionados mediante matching 1:5
#   - Outcome: incidentes C5 mensuales dentro de 500 m
#   - Efectos fijos:
#       * ubicación
#       * mes calendario (mes-año)
#   - Errores estándar agrupados por ubicación
#
# Sensibilidad temporal:
#   - Post desde enero 2023
#   - Post desde abril 2023
#   - Post desde julio 2023
#
# IMPORTANTE:
#   Las fechas alternativas NO representan fechas confirmadas de instalación.
#   Se utilizan para evaluar sensibilidad ante la incertidumbre temporal.
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

fecha_post_enero <- as.Date("2023-01-01")
fecha_post_abril <- as.Date("2023-04-01")
fecha_post_julio <- as.Date("2023-07-01")

k_matching <- 5


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
# 5. Construir pesos de controles
#
# Un control seleccionado varias veces recibe mayor peso.
#
# Ejemplo:
#   seleccionado 1 vez  -> peso 1/5
#   seleccionado 2 veces -> peso 2/5
#
# Como existen 40 matches:
#
#   suma pesos controles = 40 / 5 = 8
#
# lo que iguala el peso total de los 8 tratados.
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
  "\nSuma de pesos controles:",
  sum(pesos_controles$peso_matching),
  "\n"
)


# ------------------------------------------------------------------------------
# 6. Preparar panel de tratamientos
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
# 7. Preparar panel de controles seleccionados
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
# 8. Combinar muestra analítica
# ------------------------------------------------------------------------------

panel_did <- dplyr::bind_rows(
  tratados,
  controles
) |>
  dplyr::mutate(
    post_enero =
      as.integer(
        mes >= fecha_post_enero
      ),

    post_abril =
      as.integer(
        mes >= fecha_post_abril
      ),

    post_julio =
      as.integer(
        mes >= fecha_post_julio
      ),

    did_enero =
      tratado * post_enero,

    did_abril =
      tratado * post_abril,

    did_julio =
      tratado * post_julio,

    anio =
      lubridate::year(mes),

    numero_mes =
      lubridate::month(mes)
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
  "\nResumen muestra DiD:\n\n"
)

print(
  resumen_muestra,
  width = Inf
)


# ------------------------------------------------------------------------------
# 10. Comprobar balance de pesos
# ------------------------------------------------------------------------------

resumen_pesos <- panel_did |>
  dplyr::distinct(
    id_ubicacion,
    tratado,
    peso_matching
  ) |>
  dplyr::group_by(
    tratado
  ) |>
  dplyr::summarise(
    ubicaciones =
      dplyr::n(),

    peso_total =
      sum(
        peso_matching
      ),

    .groups = "drop"
  )


cat(
  "\nBalance de pesos:\n\n"
)

print(
  resumen_pesos,
  width = Inf
)


# ------------------------------------------------------------------------------
# 11. Promedios descriptivos pre/post
#
# Solo para interpretación. No son el estimador DiD.
# ------------------------------------------------------------------------------

descriptivos_pre_post <- panel_did |>
  dplyr::mutate(
    periodo =
      ifelse(
        post_enero == 1,
        "Post",
        "Pre"
      ),

    grupo =
      ifelse(
        tratado == 1,
        "Tratamientos",
        "Controles"
      )
  ) |>
  dplyr::group_by(
    grupo,
    periodo
  ) |>
  dplyr::summarise(
    media_500m =
      weighted.mean(
        accidentes_500m,
        w = peso_matching,
        na.rm = TRUE
      ),

    .groups = "drop"
  )


cat(
  "\nPromedios descriptivos pre/post:\n\n"
)

print(
  descriptivos_pre_post,
  width = Inf
)


# ------------------------------------------------------------------------------
# 12. Modelo principal — post enero 2023
#
# Y_it = alpha_i + lambda_t
#        + beta (Tratado_i × Post_t)
#        + e_it
#
# alpha_i  = FE ubicación
# lambda_t = FE mes-año
# ------------------------------------------------------------------------------

modelo_enero <- fixest::feols(
  accidentes_500m ~ did_enero |
    id_ubicacion + mes,
  data = panel_did,
  weights = ~ peso_matching,
  cluster = ~ id_ubicacion
)


# ------------------------------------------------------------------------------
# 13. Sensibilidad — post abril 2023
# ------------------------------------------------------------------------------

modelo_abril <- fixest::feols(
  accidentes_500m ~ did_abril |
    id_ubicacion + mes,
  data = panel_did,
  weights = ~ peso_matching,
  cluster = ~ id_ubicacion
)


# ------------------------------------------------------------------------------
# 14. Sensibilidad — post julio 2023
# ------------------------------------------------------------------------------

modelo_julio <- fixest::feols(
  accidentes_500m ~ did_julio |
    id_ubicacion + mes,
  data = panel_did,
  weights = ~ peso_matching,
  cluster = ~ id_ubicacion
)


# ------------------------------------------------------------------------------
# 15. Mostrar resultados
# ------------------------------------------------------------------------------

cat(
  "\n==============================================================\n"
)

cat(
  "DID PRINCIPAL — INCIDENTES C5 A 500 m\n"
)

cat(
  "==============================================================\n\n"
)


fixest::etable(
  modelo_enero,
  modelo_abril,
  modelo_julio,

  headers = c(
    "Post: ene-2023",
    "Post: abr-2023",
    "Post: jul-2023"
  ),

  dict = c(
    did_enero =
      "Tratado × Post",

    did_abril =
      "Tratado × Post",

    did_julio =
      "Tratado × Post"
  ),

  fitstat = ~ n + r2 + wr2
)


# ------------------------------------------------------------------------------
# 16. Extraer coeficientes
# ------------------------------------------------------------------------------

extraer_coeficiente <- function(
    modelo,
    termino,
    especificacion
) {

  broom::tidy(
    modelo,
    conf.int = TRUE
  ) |>
    dplyr::filter(
      term == termino
    ) |>
    dplyr::mutate(
      especificacion =
        especificacion
    ) |>
    dplyr::select(
      especificacion,
      term,
      estimate,
      std.error,
      statistic,
      p.value,
      conf.low,
      conf.high
    )
}


resultados_did <- dplyr::bind_rows(

  extraer_coeficiente(
    modelo_enero,
    "did_enero",
    "Post desde enero 2023"
  ),

  extraer_coeficiente(
    modelo_abril,
    "did_abril",
    "Post desde abril 2023"
  ),

  extraer_coeficiente(
    modelo_julio,
    "did_julio",
    "Post desde julio 2023"
  )
)


cat(
  "\nCoeficientes DiD:\n\n"
)

print(
  resultados_did,
  width = Inf
)


# ------------------------------------------------------------------------------
# 17. Convertir efecto a porcentaje del nivel pretratamiento tratado
#
# Esto sirve únicamente para facilitar interpretación.
# ------------------------------------------------------------------------------

media_pre_tratados <- panel_did |>
  dplyr::filter(
    tratado == 1,
    mes < fecha_post_enero
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


resultados_did <- resultados_did |>
  dplyr::mutate(
    media_pre_tratados =
      media_pre_tratados,

    efecto_porcentual_aprox =
      100 *
      estimate /
      media_pre_tratados
  )


cat(
  "\nMedia mensual pretratamiento — tratados:",
  round(
    media_pre_tratados,
    2
  ),
  "\n"
)


cat(
  "\nResultados con interpretación porcentual aproximada:\n\n"
)

print(
  resultados_did,
  width = Inf
)


# ------------------------------------------------------------------------------
# 18. Modelo sin pesos para sensibilidad
#
# Todos los controles seleccionados reciben peso 1.
# ------------------------------------------------------------------------------

modelo_sin_pesos <- fixest::feols(
  accidentes_500m ~ did_enero |
    id_ubicacion + mes,
  data = panel_did,
  cluster = ~ id_ubicacion
)


cat(
  "\n==============================================================\n"
)

cat(
  "SENSIBILIDAD — SIN PESOS DE MATCHING\n"
)

cat(
  "==============================================================\n\n"
)


fixest::etable(
  modelo_enero,
  modelo_sin_pesos,

  headers = c(
    "Matching ponderado",
    "Sin ponderar"
  ),

  dict = c(
    did_enero =
      "Tratado × Post"
  ),

  fitstat = ~ n + r2 + wr2
)


# ------------------------------------------------------------------------------
# 19. Guardar base analítica
# ------------------------------------------------------------------------------

readr::write_csv(
  panel_did,
  fs::path(
    rutas$data_processed,
    "500_panel_did_principal.csv"
  )
)


# ------------------------------------------------------------------------------
# 20. Guardar resultados
# ------------------------------------------------------------------------------

readr::write_csv(
  resultados_did,
  fs::path(
    rutas$output_tables,
    "500_resultados_did_principal.csv"
  )
)


readr::write_csv(
  descriptivos_pre_post,
  fs::path(
    rutas$output_tables,
    "500_descriptivos_pre_post.csv"
  )
)


readr::write_csv(
  resumen_pesos,
  fs::path(
    rutas$output_tables,
    "500_resumen_pesos_matching.csv"
  )
)


# ------------------------------------------------------------------------------
# 21. Guardar tabla de modelos
# ------------------------------------------------------------------------------

fixest::etable(
  modelo_enero,
  modelo_abril,
  modelo_julio,

  headers = c(
    "Post: ene-2023",
    "Post: abr-2023",
    "Post: jul-2023"
  ),

  dict = c(
    did_enero =
      "Tratado × Post",

    did_abril =
      "Tratado × Post",

    did_julio =
      "Tratado × Post"
  ),

  fitstat = ~ n + r2 + wr2,

  tex = FALSE,

  file = fs::path(
    rutas$output_tables,
    "500_tabla_did_principal.html"
  )
)


# ------------------------------------------------------------------------------
# 22. Resumen final
# ------------------------------------------------------------------------------

cat(
  "\n==============================================================\n"
)

cat(
  "MODELO DID PRINCIPAL TERMINADO\n"
)

cat(
  "==============================================================\n"
)

cat(
  "Outcome principal: incidentes C5 dentro de 500 m\n"
)

cat(
  "Tratamientos: 8\n"
)

cat(
  "Controles únicos:",
  dplyr::n_distinct(
    controles$id_ubicacion
  ),
  "\n"
)

cat(
  "FE ubicación: sí\n"
)

cat(
  "FE mes-año: sí\n"
)

cat(
  "Clustering: ubicación\n"
)

cat(
  "Sensibilidad temporal: enero / abril / julio 2023\n"
)

cat(
  "==============================================================\n"
)