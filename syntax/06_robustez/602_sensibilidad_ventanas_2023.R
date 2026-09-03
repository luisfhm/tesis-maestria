# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 602_sensibilidad_ventanas_2023.R
# Objetivo:
#   - Evaluar la sensibilidad del event study de septiembre de 2023
#     a distintas ventanas pretratamiento
#   - Comparar las pruebas conjuntas de pre-tendencias
#   - Comparar la dinámica de los coeficientes postratamiento
#
# Entrada:
#   data/final/panel_evento_2023.parquet
#
# Salidas:
#   output/tables/602_sensibilidad_ventanas_2023.csv
#   output/tables/602_coeficientes_ventanas_2023.csv
#   output/figures/602_sensibilidad_ventanas_2023.png
#
# Especificación:
#   Efectos fijos: colonia + mes
#   Errores estándar: cluster por colonia
#   Periodo de referencia: mes -1
#
# Nota:
#   Las distintas ventanas constituyen un ejercicio de sensibilidad.
#   No se selecciona una especificación en función de su significancia.
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
# 1. Leer panel
# ------------------------------------------------------------------------------

archivo_panel <- fs::path(
  rutas$data_final,
  "panel_evento_2023.parquet"
)

validar_archivo(archivo_panel)

panel <- arrow::read_parquet(
  archivo_panel
) |>
  tibble::as_tibble()


# ------------------------------------------------------------------------------
# 2. Definir ventanas
# ------------------------------------------------------------------------------

ventanas_pre <- c(
  12,
  10,
  8,
  6
)

ventana_post <- 5


# ------------------------------------------------------------------------------
# 3. Función para estimar una ventana
# ------------------------------------------------------------------------------

estimar_ventana <- function(ventana_pre) {

  datos <- panel |>
    dplyr::filter(
      tiempo_evento >= -ventana_pre,
      tiempo_evento <= ventana_post
    )

  modelo <- fixest::feols(
    n_incidentes ~
      fixest::i(
        tiempo_evento,
        tratado,
        ref = -1
      ) |
      id_colonia + mes,
    cluster = ~ id_colonia,
    data = datos
  )

  # --------------------------------------------------------------------------
  # Prueba conjunta de pre-tendencias
  # --------------------------------------------------------------------------

  prueba_pre <- fixest::wald(
    modelo,
    keep = "tiempo_evento::-[0-9]+:tratado",
    print = FALSE
  )

  # --------------------------------------------------------------------------
  # Extraer coeficientes
  # --------------------------------------------------------------------------

  coeficientes <- broom::tidy(
    modelo,
    conf.int = TRUE
  ) |>
    dplyr::filter(
      stringr::str_detect(
        term,
        "tiempo_evento"
      )
    ) |>
    dplyr::mutate(
      tiempo_evento = stringr::str_extract(
        term,
        "-?\\d+"
      ) |>
        as.integer(),

      ventana_pre = ventana_pre,

      ventana = paste0(
        "-",
        ventana_pre,
        " a +",
        ventana_post
      )
    )

  # --------------------------------------------------------------------------
  # Agregar referencia k = -1
  # --------------------------------------------------------------------------

  referencia <- tibble::tibble(
    term = "referencia",
    estimate = 0,
    std.error = NA_real_,
    statistic = NA_real_,
    p.value = NA_real_,
    conf.low = 0,
    conf.high = 0,
    tiempo_evento = -1L,
    ventana_pre = ventana_pre,
    ventana = paste0(
      "-",
      ventana_pre,
      " a +",
      ventana_post
    )
  )

  coeficientes <- dplyr::bind_rows(
    coeficientes,
    referencia
  ) |>
    dplyr::arrange(
      tiempo_evento
    )

  # --------------------------------------------------------------------------
  # Resumen de la especificación
  # --------------------------------------------------------------------------

  resumen <- tibble::tibble(
    ventana_pre = ventana_pre,

    ventana = paste0(
      "-",
      ventana_pre,
      " a +",
      ventana_post
    ),

    n_meses_pre = ventana_pre - 1,

    n_meses_post = ventana_post + 1,

    n_colonias = dplyr::n_distinct(
      datos$id_colonia
    ),

    n_tratadas = dplyr::n_distinct(
      datos$id_colonia[
        datos$tratado == 1
      ]
    ),

    n_controles = dplyr::n_distinct(
      datos$id_colonia[
        datos$tratado == 0
      ]
    ),

    observaciones = nrow(datos),

    wald_pre = unname(
      prueba_pre$stat
    ),

    p_wald_pre = prueba_pre$p
  )

  list(
    modelo = modelo,
    coeficientes = coeficientes,
    resumen = resumen
  )
}


# ------------------------------------------------------------------------------
# 4. Estimar todas las ventanas
# ------------------------------------------------------------------------------

resultados <- purrr::map(
  ventanas_pre,
  estimar_ventana
)


# ------------------------------------------------------------------------------
# 5. Construir tabla de sensibilidad
# ------------------------------------------------------------------------------

tabla_sensibilidad <- purrr::map_dfr(
  resultados,
  "resumen"
)

print(
  tabla_sensibilidad,
  n = Inf
)


# ------------------------------------------------------------------------------
# 6. Reunir coeficientes
# ------------------------------------------------------------------------------

coeficientes_ventanas <- purrr::map_dfr(
  resultados,
  "coeficientes"
)


# ------------------------------------------------------------------------------
# 7. Extraer coeficientes post
# ------------------------------------------------------------------------------

coeficientes_post <- coeficientes_ventanas |>
  dplyr::filter(
    tiempo_evento >= 0
  )


# ------------------------------------------------------------------------------
# 8. Tabla resumida de efectos post
# ------------------------------------------------------------------------------

tabla_post <- coeficientes_post |>
  dplyr::select(
    ventana,
    tiempo_evento,
    estimate,
    std.error,
    p.value,
    conf.low,
    conf.high
  ) |>
  dplyr::arrange(
    ventana,
    tiempo_evento
  )

print(
  tabla_post,
  n = Inf
)


# ------------------------------------------------------------------------------
# 9. Gráfica comparativa
# ------------------------------------------------------------------------------

coeficientes_ventanas <- coeficientes_ventanas |>
  dplyr::mutate(
    ventana = factor(
      ventana,
      levels = paste0(
        "-",
        ventanas_pre,
        " a +",
        ventana_post
      )
    )
  )


grafica_sensibilidad <- ggplot2::ggplot(
  coeficientes_ventanas,
  ggplot2::aes(
    x = tiempo_evento,
    y = estimate
  )
) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dotted"
  ) +
  ggplot2::geom_vline(
    xintercept = -0.5,
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
    size = 1.7
  ) +
  ggplot2::facet_wrap(
    ~ ventana,
    scales = "free_x"
  ) +
  ggplot2::labs(
    title = "Sensibilidad del estudio de evento a la ventana temporal",
    subtitle = "Evento: septiembre de 2023; referencia: agosto de 2023",
    x = "Mes relativo al evento",
    y = "Diferencia en incidentes C5"
  ) +
  ggplot2::theme_minimal()


grafica_sensibilidad


# ------------------------------------------------------------------------------
# 10. Guardar gráfica
# ------------------------------------------------------------------------------

archivo_grafica <- fs::path(
  rutas$output_figures,
  "602_sensibilidad_ventanas_2023.png"
)

ggplot2::ggsave(
  filename = archivo_grafica,
  plot = grafica_sensibilidad,
  width = 12,
  height = 8,
  dpi = 300
)


# ------------------------------------------------------------------------------
# 11. Guardar tabla de sensibilidad
# ------------------------------------------------------------------------------

archivo_sensibilidad <- fs::path(
  rutas$output_tables,
  "602_sensibilidad_ventanas_2023.csv"
)

readr::write_csv(
  tabla_sensibilidad,
  archivo_sensibilidad
)


# ------------------------------------------------------------------------------
# 12. Guardar todos los coeficientes
# ------------------------------------------------------------------------------

archivo_coeficientes <- fs::path(
  rutas$output_tables,
  "602_coeficientes_ventanas_2023.csv"
)

readr::write_csv(
  coeficientes_ventanas,
  archivo_coeficientes
)


# ------------------------------------------------------------------------------
# 13. Guardar coeficientes post
# ------------------------------------------------------------------------------

archivo_post <- fs::path(
  rutas$output_tables,
  "602_coeficientes_post_ventanas_2023.csv"
)

readr::write_csv(
  tabla_post,
  archivo_post
)


# ------------------------------------------------------------------------------
# 14. Resumen en consola
# ------------------------------------------------------------------------------

message("")
message("==============================================================")
message("SENSIBILIDAD DE VENTANAS - EVENTO 2023")
message("==============================================================")

for (i in seq_len(nrow(tabla_sensibilidad))) {

  message(
    "Ventana ",
    tabla_sensibilidad$ventana[i],
    ": Wald = ",
    round(
      tabla_sensibilidad$wald_pre[i],
      3
    ),
    "; p = ",
    format.pval(
      tabla_sensibilidad$p_wald_pre[i],
      digits = 4
    )
  )
}

message("==============================================================")
message("Gráfica:      ", archivo_grafica)
message("Sensibilidad: ", archivo_sensibilidad)
message("Coeficientes: ", archivo_coeficientes)
message("Post:         ", archivo_post)
message("==============================================================")