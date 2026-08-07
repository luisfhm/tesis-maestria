# ==============================================================================
# 110_explorar_accidentes_alcaldias.R
# Exploración inicial de accidentes terrestres por alcaldía
# ==============================================================================

source("syntax/00_setup/000_setup.R")


# ------------------------------------------------------------------------------
# 1. Ruta
# ------------------------------------------------------------------------------

ruta_accidentes_alcaldias <- fs::path(
  rutas$data_raw,
  "12_accidentes_alcaldias"
)

archivos_accidentes_alcaldias <- fs::dir_ls(
  ruta_accidentes_alcaldias,
  regexp = "\\.csv$",
  type = "file"
)


cat("\nArchivos encontrados:\n\n")

print(
  fs::path_file(
    archivos_accidentes_alcaldias
  )
)


# ------------------------------------------------------------------------------
# 2. Inventario
# ------------------------------------------------------------------------------

inventario_accidentes_alcaldias <- tibble::tibble(
  ruta = archivos_accidentes_alcaldias,

  archivo = fs::path_file(
    archivos_accidentes_alcaldias
  ),

  tamano_mb = round(
    as.numeric(
      fs::file_size(
        archivos_accidentes_alcaldias
      )
    ) / 1024^2,
    3
  )
)


cat("\nInventario:\n\n")

print(
  inventario_accidentes_alcaldias,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 3. Revisar tamaño real del archivo
# ------------------------------------------------------------------------------

cat("\nTamaño en bytes:\n\n")

print(
  fs::file_size(
    archivos_accidentes_alcaldias
  )
)


# ------------------------------------------------------------------------------
# 4. Leer muestra
# ------------------------------------------------------------------------------

muestra_accidentes_alcaldias <- tryCatch(

  readr::read_csv(
    archivos_accidentes_alcaldias[1],
    n_max = 2000,
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "unique"
  ),

  error = function(e) {

    cat(
      "\nError de lectura:\n",
      conditionMessage(e),
      "\n"
    )

    return(NULL)
  }
)


# ------------------------------------------------------------------------------
# 5. Comprobar lectura
# ------------------------------------------------------------------------------

if (is.null(muestra_accidentes_alcaldias)) {

  stop(
    "No fue posible leer el archivo de accidentes por alcaldía."
  )

}


cat("\nDimensiones de la muestra:\n\n")

cat(
  "Filas:",
  nrow(muestra_accidentes_alcaldias),
  "\n"
)

cat(
  "Columnas:",
  ncol(muestra_accidentes_alcaldias),
  "\n"
)


# ------------------------------------------------------------------------------
# 6. Variables
# ------------------------------------------------------------------------------

cat("\nVariables disponibles:\n\n")

print(
  names(
    muestra_accidentes_alcaldias
  )
)


# ------------------------------------------------------------------------------
# 7. Tipos de variables
# ------------------------------------------------------------------------------

tipos_accidentes_alcaldias <- tibble::tibble(
  variable = names(
    muestra_accidentes_alcaldias
  ),

  clase = purrr::map_chr(
    muestra_accidentes_alcaldias,
    ~ paste(
      class(.x),
      collapse = "/"
    )
  )
)


cat("\nTipos de variables:\n\n")

print(
  tipos_accidentes_alcaldias,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 8. Primeras observaciones
# ------------------------------------------------------------------------------

cat("\nPrimeras observaciones:\n\n")

print(
  muestra_accidentes_alcaldias |>
    dplyr::slice_head(
      n = 30
    ),
  width = Inf
)


# ------------------------------------------------------------------------------
# 9. Cardinalidad y faltantes
# ------------------------------------------------------------------------------

calidad_muestra_accidentes_alcaldias <- tibble::tibble(
  variable = names(
    muestra_accidentes_alcaldias
  ),

  valores_unicos = purrr::map_int(
    muestra_accidentes_alcaldias,
    ~ dplyr::n_distinct(
      .x,
      na.rm = TRUE
    )
  ),

  faltantes = purrr::map_int(
    muestra_accidentes_alcaldias,
    ~ sum(
      is.na(.x)
    )
  ),

  porcentaje_faltantes = round(
    faltantes /
      nrow(muestra_accidentes_alcaldias) *
      100,
    2
  )
)


cat("\nCardinalidad y faltantes:\n\n")

print(
  calidad_muestra_accidentes_alcaldias,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 10. Buscar variables potencialmente relevantes
# ------------------------------------------------------------------------------

patron_relevante <- paste(
  c(
    "anio",
    "año",
    "fecha",
    "mes",
    "alcal",
    "deleg",
    "accid",
    "evento",
    "lesion",
    "herid",
    "falle",
    "muert",
    "victim",
    "peat",
    "cic",
    "moto",
    "vehic",
    "choque",
    "colision"
  ),
  collapse = "|"
)


variables_relevantes_accidentes_alcaldias <- tibble::tibble(
  variable = names(
    muestra_accidentes_alcaldias
  )
) |>
  dplyr::filter(
    stringr::str_detect(
      stringr::str_to_lower(variable),
      patron_relevante
    )
  )


cat("\nVariables potencialmente relevantes:\n\n")

print(
  variables_relevantes_accidentes_alcaldias,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 11. Buscar variables temporales
# ------------------------------------------------------------------------------

variables_temporales_accidentes_alcaldias <- names(
  muestra_accidentes_alcaldias
)[
  stringr::str_detect(
    stringr::str_to_lower(
      names(
        muestra_accidentes_alcaldias
      )
    ),
    "anio|año|fecha|mes|trimestre|periodo"
  )
]


cat("\nVariables temporales candidatas:\n\n")

print(
  variables_temporales_accidentes_alcaldias
)


# ------------------------------------------------------------------------------
# 12. Buscar variables geográficas
# ------------------------------------------------------------------------------

variables_geo_accidentes_alcaldias <- names(
  muestra_accidentes_alcaldias
)[
  stringr::str_detect(
    stringr::str_to_lower(
      names(
        muestra_accidentes_alcaldias
      )
    ),
    "alcal|deleg|municip|entidad"
  )
]


cat("\nVariables geográficas candidatas:\n\n")

print(
  variables_geo_accidentes_alcaldias
)


# ------------------------------------------------------------------------------
# 13. Guardar auditoría inicial
# ------------------------------------------------------------------------------

readr::write_csv(
  inventario_accidentes_alcaldias,
  fs::path(
    rutas$output_tables,
    "110_accidentes_alcaldias_inventario.csv"
  )
)

readr::write_csv(
  tipos_accidentes_alcaldias,
  fs::path(
    rutas$output_tables,
    "110_accidentes_alcaldias_tipos_variables.csv"
  )
)

readr::write_csv(
  calidad_muestra_accidentes_alcaldias,
  fs::path(
    rutas$output_tables,
    "110_accidentes_alcaldias_calidad_muestra.csv"
  )
)


message(
  "Exploración estructural inicial de accidentes por alcaldía terminada."
)

# ------------------------------------------------------------------------------
# 14. Leer base completa
# ------------------------------------------------------------------------------

accidentes_alcaldias <- readr::read_csv(
  archivos_accidentes_alcaldias[1],
  show_col_types = FALSE,
  progress = FALSE,
  name_repair = "unique"
)


cat("\nDimensiones completas:\n\n")

cat(
  "Filas:",
  nrow(accidentes_alcaldias),
  "\n"
)

cat(
  "Columnas:",
  ncol(accidentes_alcaldias),
  "\n"
)


# ------------------------------------------------------------------------------
# 15. Mostrar base completa
# ------------------------------------------------------------------------------

cat("\nBase completa:\n\n")

print(
  accidentes_alcaldias,
  n = Inf
)


# ------------------------------------------------------------------------------
# 16. Total de accidentes
# ------------------------------------------------------------------------------

resumen_accidentes_alcaldias <- accidentes_alcaldias |>
  dplyr::summarise(
    categorias = dplyr::n(),

    accidentes_totales = sum(
      `Número de accidente`,
      na.rm = TRUE
    ),

    minimo = min(
      `Número de accidente`,
      na.rm = TRUE
    ),

    maximo = max(
      `Número de accidente`,
      na.rm = TRUE
    )
  )


cat("\nResumen:\n\n")

print(
  resumen_accidentes_alcaldias,
  width = Inf
)


# ------------------------------------------------------------------------------
# 17. Ranking
# ------------------------------------------------------------------------------

ranking_accidentes_alcaldias <- accidentes_alcaldias |>
  dplyr::arrange(
    dplyr::desc(
      `Número de accidente`
    )
  ) |>
  dplyr::mutate(
    porcentaje = round(
      `Número de accidente` /
        sum(`Número de accidente`) *
        100,
      2
    ),

    ranking = dplyr::row_number()
  ) |>
  dplyr::relocate(
    ranking
  )


cat("\nAccidentes por alcaldía:\n\n")

print(
  ranking_accidentes_alcaldias,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 18. Detectar categorías que no correspondan a las 16 alcaldías
# ------------------------------------------------------------------------------

alcaldias_cdmx <- c(
  "ÁLVARO OBREGÓN",
  "AZCAPOTZALCO",
  "BENITO JUÁREZ",
  "COYOACÁN",
  "CUAJIMALPA DE MORELOS",
  "CUAUHTÉMOC",
  "GUSTAVO A. MADERO",
  "IZTACALCO",
  "IZTAPALAPA",
  "LA MAGDALENA CONTRERAS",
  "MIGUEL HIDALGO",
  "MILPA ALTA",
  "TLÁHUAC",
  "TLALPAN",
  "VENUSTIANO CARRANZA",
  "XOCHIMILCO"
)


categorias_normalizadas <- accidentes_alcaldias |>
  dplyr::mutate(
    alcaldia_normalizada = Alcaldía |>
      stringr::str_to_upper() |>
      stringi::stri_trans_general(
        "Latin-ASCII"
      )
  )


alcaldias_cdmx_normalizadas <- alcaldias_cdmx |>
  stringi::stri_trans_general(
    "Latin-ASCII"
  )


categorias_extra <- categorias_normalizadas |>
  dplyr::filter(
    !alcaldia_normalizada %in%
      alcaldias_cdmx_normalizadas
  )


cat("\nCategorías distintas de las 16 alcaldías:\n\n")

print(
  categorias_extra,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 19. Guardar resultados
# ------------------------------------------------------------------------------

readr::write_csv(
  resumen_accidentes_alcaldias,
  fs::path(
    rutas$output_tables,
    "110_accidentes_alcaldias_resumen.csv"
  )
)

readr::write_csv(
  ranking_accidentes_alcaldias,
  fs::path(
    rutas$output_tables,
    "110_accidentes_alcaldias_ranking.csv"
  )
)


message(
  "Exploración de accidentes por alcaldía terminada."
)

# ------------------------------------------------------------------------------
# 20. Separar total CDMX de alcaldías
# ------------------------------------------------------------------------------

accidentes_alcaldias_detalle <- accidentes_alcaldias |>
  dplyr::filter(
    stringr::str_to_upper(Alcaldía) != "CDMX"
  )


total_reportado_cdmx <- accidentes_alcaldias |>
  dplyr::filter(
    stringr::str_to_upper(Alcaldía) == "CDMX"
  ) |>
  dplyr::pull(
    `Número de accidente`
  )


total_calculado_alcaldias <- sum(
  accidentes_alcaldias_detalle$`Número de accidente`,
  na.rm = TRUE
)


cat("\nTotal reportado CDMX:", total_reportado_cdmx, "\n")

cat(
  "Suma de las 16 alcaldías:",
  total_calculado_alcaldias,
  "\n"
)

cat(
  "Diferencia:",
  total_reportado_cdmx - total_calculado_alcaldias,
  "\n"
)


# ------------------------------------------------------------------------------
# 21. Ranking corregido
# ------------------------------------------------------------------------------

ranking_accidentes_alcaldias <- accidentes_alcaldias_detalle |>
  dplyr::arrange(
    dplyr::desc(`Número de accidente`)
  ) |>
  dplyr::mutate(
    porcentaje = round(
      `Número de accidente` /
        total_calculado_alcaldias *
        100,
      2
    ),

    ranking = dplyr::row_number()
  ) |>
  dplyr::relocate(
    ranking
  )


cat("\nRanking corregido:\n\n")

print(
  ranking_accidentes_alcaldias,
  n = Inf,
  width = Inf
)


# ------------------------------------------------------------------------------
# 22. Resumen corregido
# ------------------------------------------------------------------------------

resumen_accidentes_alcaldias <- tibble::tibble(
  alcaldias = nrow(accidentes_alcaldias_detalle),

  accidentes_totales = total_calculado_alcaldias,

  minimo = min(
    accidentes_alcaldias_detalle$`Número de accidente`,
    na.rm = TRUE
  ),

  maximo = max(
    accidentes_alcaldias_detalle$`Número de accidente`,
    na.rm = TRUE
  ),

  total_reportado_cdmx = total_reportado_cdmx,

  diferencia = total_reportado_cdmx -
    total_calculado_alcaldias
)


print(
  resumen_accidentes_alcaldias,
  width = Inf
)


# ------------------------------------------------------------------------------
# 23. Sobrescribir resultados corregidos
# ------------------------------------------------------------------------------

readr::write_csv(
  resumen_accidentes_alcaldias,
  fs::path(
    rutas$output_tables,
    "110_accidentes_alcaldias_resumen.csv"
  )
)

readr::write_csv(
  ranking_accidentes_alcaldias,
  fs::path(
    rutas$output_tables,
    "110_accidentes_alcaldias_ranking.csv"
  )
)


message(
  "Resultados de accidentes por alcaldía corregidos."
)