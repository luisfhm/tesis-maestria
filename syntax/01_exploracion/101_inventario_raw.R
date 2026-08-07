# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 101_inventario_raw.R
# Objetivo: Inventariar los archivos disponibles en data/raw
# ==============================================================================

source(
  here::here(
    "syntax",
    "00_setup",
    "000_setup.R"
  ),
  encoding = "UTF-8"
)

# ------------------------------------------------------------------------------
# 1. Listar archivos
# ------------------------------------------------------------------------------

archivos_raw <- fs::dir_info(
  rutas$data_raw,
  recurse = TRUE
) |>
  dplyr::filter(type == "file") |>
  dplyr::mutate(
    ruta_relativa = fs::path_rel(path, start = rutas$data_raw),
    carpeta = fs::path_dir(ruta_relativa),
    archivo = fs::path_file(path),
    extension = tolower(fs::path_ext(path)),
    tamano_bytes = as.numeric(size),
    tamano_mb = round(as.numeric(size) / 1024^2, 3)
  ) |>
  dplyr::select(
    carpeta,
    archivo,
    extension,
    tamano_mb,
    modification_time,
    ruta_relativa
  ) |>
  dplyr::arrange(carpeta, archivo)

# ------------------------------------------------------------------------------
# 2. Resumen
# ------------------------------------------------------------------------------

print(archivos_raw, n = Inf)

cat("\nNúmero total de archivos:", nrow(archivos_raw), "\n")
cat(
  "Tamaño total:",
  round(sum(archivos_raw$tamano_mb), 2),
  "MB\n"
)

# Archivos por extensión
archivos_raw |>
  dplyr::count(extension, sort = TRUE) |>
  print(n = Inf)

# Archivos por carpeta
archivos_raw |>
  dplyr::count(carpeta, sort = TRUE) |>
  print(n = Inf)

# ------------------------------------------------------------------------------
# 3. Guardar inventario
# ------------------------------------------------------------------------------

readr::write_csv(
  archivos_raw,
  fs::path(
    rutas$output_tables,
    "inventario_data_raw.csv"
  )
)

message("Inventario generado correctamente.")
